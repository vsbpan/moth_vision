# Wrapper for computing challenge metrics for binary masks using the polygons. Prove a list of inlist for each prediction and ground truth
mask_evaluator <- function(predictions, ground_truths, 
                           thresh = c(0.5, 0.75, 0.9), 
                           categories = c("body", "forewing", "hindwing", 
                                          "color_checker","ruler", "tag"),
                           cores = 1,
                           use_bbox = FALSE){
  
  l <- validate_evaluator_input(predictions, ground_truths)
  
  z <- vmisc::pb_par_lapply(seq_along(l$pred), 
                            function(i, pred_l, gt_l){
                              mask_evaluator_engine(
                                pred_l[[i]],gt_l[[i]], 
                                categories = categories,
                                use_bbox = use_bbox
                              )
  }, 
  pred_l = l$pred,
  gt_l = l$gt,
  cores = cores, 
  inorder = TRUE) %>% 
    unlist(TRUE, FALSE)
  
  out <- .evaluator_calc(z, thresh, thresh_name = "IOU")
  type <- if(use_bbox) "bbox" else "mask"
  .evaluator_report(out, l$pred, categories, type)
  invisible(out)
}

.evaluator_report <- function(calc_out, validated_pred, categories, type){
  nanno <- calc_out$n[1]
  nimg <- length(validated_pred)
  type <- cli::col_blue(type)
  
  has_things <- unname(unique(unlist(purrr::map_depth(validated_pred, 2, "thing_class"))))
  things <- categories[categories %in% has_things]
  things <- cli::col_yellow(things)
  
  cat(cli::cli_text("COCO {type} evaluation for {nanno} annotation{?s} across {nimg} image{?s}."))
  cat(cli::cli_text("Things evaluated: {things}"))
  cat("\n")
  print(calc_out$result)
}


bbox_evaluator <- function(prediction_list, ground_truth_list, 
                           thresh = c(0.5, 0.75, 0.9), 
                           categories = c("body", "forewing", "hindwing", 
                                          "color_checker","ruler", "tag"),
                           cores = 1){
  mask_evaluator(prediction_list, ground_truth_list, 
                 thresh = thresh, 
                 categories = categories,
                 cores = cores,
                 use_bbox = TRUE)
}

# Mask evaluator for a single inlist. Output the iou outcomes
mask_evaluator_engine <- function(prediction, ground_truth, 
                                  categories = c("body", "forewing", "hindwing", 
                                                 "color_checker","ruler", "tag"), 
                                  use_bbox = FALSE){
  
  o <- order(do.call("c",map(prediction, "score")))
  prediction <- prediction[o]
  geom <- if(use_bbox) "bbox" else "polygon"
  
  out_list <- vector(mode = "list", length = length(categories))
  
  for (i in seq_along(categories)){
    cati <- categories[i]
    pred_classi <- select_category(prediction, cati)
    gt_classi <- select_category(ground_truth, cati)
    
    n_predi <- length(pred_classi)
    n_gti <- length(gt_classi)
    
    
    if(n_predi == 0 && n_gti == 0){
      next
    } else if(n_predi == 0){
      out_list[[i]] <- rep("False_negative", n_gti)
      names(out_list)[i] <- categories[i]
      next
    } else if(n_gti == 0){
      out_list[[i]] <- rep("False_positive", n_predi)
      names(out_list)[i] <- categories[i]
      next
    }
    
    
    grid <- expand.grid(seq_len(n_predi), seq_len(n_gti))
    
    mat <- lapply(seq_len(nrow(grid)), function(i){
      pred_geom <- pred_classi[[grid[i,1]]][[geom]]
      gt_geom <- gt_classi[[grid[i,2]]][[geom]]
      if(is.null(pred_geom) && is.null(gt_geom)){
        return(NULL)
      } else if(is.null(pred_geom)){
        iou <- 0
      } else if(is.null(gt_geom)){
        iou <- 0
      } else {
        iou <- IOU(pred_geom,gt_geom)
      }
      
      return(iou)
    }) %>% 
      unlist() %>%  
      matrix(nrow = n_predi, 
             ncol = n_gti)
    match_ids <- .which_max_no_replace(mat)
    out_list[[i]] <- purrr::map2(seq_along(match_ids),match_ids, function(x,y){
      if(y == "unmatched_row"){
        return("False_positive")
      } else if(y == "unmatched_column"){
        return("False_negative")
      } else {
        return(as.character(mat[as.integer(x),as.integer(y)]))
      }
    }) %>% 
      do.call("c", .)
    names(out_list)[i] <- categories[i]
  }
  out_list <- purrr::keep(out_list, function(x){!is.null(x)})
  return(out_list)
}

# Find which max but use up that column after each row is looped through
.which_max_no_replace <- function(mat) {
  nrow_mat <- nrow(mat)
  ncol_mat <- ncol(mat)
  
  vmisc::warnifnot(all(mat!=-Inf))
  
  largest_indices <- numeric(nrow_mat)
  
  for (i in 1:nrow_mat) {
    row_vector <- mat[i, ]
    if(all(row_vector == -Inf)){
      largest_idx <- "unmatched_row"
      largest_indices[i] <- largest_idx
      next
    } else {
      largest_idx <- which.max(row_vector)
      largest_indices[i] <- largest_idx
    }
    mat[, largest_idx] <- -Inf
  }
  
  largest_indices <- c(largest_indices, rep("unmatched_column", max(c(ncol_mat - nrow_mat, 0))))
  
  return(largest_indices)
}


# Compute the summary of challenge scores as error rates
.evaluator_calc <- function(score, thresh, thresh_name = NULL){
  if(is.null(thresh_name)){
    thresh_name <- "score"
  }
  
  mat <- outer(score, thresh, 
                Vectorize(function(x, y){
                  if(x == "False_positive"){
                    return("False_positive")
                  } else if(x == "False_negative"){
                    return("False_negative")
                  } else {
                    return(ifelse(as.numeric(x) >= y, "True_positive", "False_positive"))
                  }
                }))
  
  count_class <- function(class_i){
    apply(mat, 1, function(x){
      x == class_i
    }, simplify = FALSE) %>% 
      do.call("cbind", .) %>% 
      rowSums()
  }
  
  
  
  TP <- count_class("True_positive")
  TN <- 0
  FP <- count_class("False_positive")
  FN <- count_class("False_negative")
  
  n <- length(score)
  
  eval_res <- data.frame(
    "score" = thresh, 
    "TP" = TP,
    "FP" = FP,
    "TN" = TN,
    "FN" = FN,
    "n" = n
  )
  names(eval_res)[1] <- thresh_name
  
  eval_res$precision <- eval_res$TP / (eval_res$TP + eval_res$FP)
  eval_res$recall <- eval_res$TP / (eval_res$TP + eval_res$FN)
  eval_res$accuracy <- (eval_res$TP + eval_res$TN) / (eval_res$TP + eval_res$TN + eval_res$FP + eval_res$FN)
  
  out <- list("result" = eval_res, "score" = score)
  names(out)[2] <- thresh_name
  return(out)
}


# Wrapper for computing challenge metrics for keypoints. Prove a list of inlist for each prediction and ground truth
keypoint_evaluator <- function(predictions, ground_truths, 
                               k = c(5, 5), # From visually assessing a sample whether this is reliable for distinguishing false positive and true positive.
                               categories = "forewing",
                               keypoints = c("inner", "outer"), 
                               thresh = c(0.5, 0.75, 0.9),
                               cores = 1){
  
  
  if(length(k) != length(keypoints)){
    cli::cli_abort("{.var k} and {.var keypoints} must be the same length!")
  }
  
  l <- validate_evaluator_input(predictions, ground_truths)
  
  
  keypoints <- match(match.arg(keypoints, several.ok = TRUE), c("inner", "outer"))
  
  o <- order(keypoints)
  keypoints <- keypoints[o]
  k <- k[o]
  
  
  z <- vmisc::pb_par_lapply(seq_along(l$pred), 
                            function(i, pred_l, gt_l){
                              keypoint_evaluator_engine(
                                pred_l[[i]],gt_l[[i]], 
                                categories = categories,
                                keypoints = keypoints,
                                k = k
                              )
                            }, 
                            pred_l = l$pred,
                            gt_l = l$gt,
                            cores = cores, 
                            inorder = TRUE) %>% 
    unlist(TRUE, FALSE)
  
  out <- .evaluator_calc(z, thresh, thresh_name = "OKS")
  .evaluator_report(out, l$pred, categories, "keypoint")
  invisible(out)
}

# keypoint evaluator for a single inlist. Output the oks outcomes
keypoint_evaluator_engine <- function(prediction, ground_truth, 
                                      categories = c("forewing"),
                                      keypoints = c("inner", "outer"),
                                      k = c(5, 5)){
  
  o <- order(do.call("c",map(prediction, "score")))
  prediction <- prediction[o]
  
  out_list <- vector(mode = "list", length = length(categories))
  
  for (i in seq_along(categories)){
    cati <- categories[i]
    pred_classi <- select_category(prediction, cati)
    gt_classi <- select_category(ground_truth, cati)
    
    n_predi <- length(pred_classi)
    n_gti <- length(gt_classi)
    
    
    if(n_predi == 0 && n_gti == 0){
      next
    } else if(n_predi == 0){
      out_list[[i]] <- rep("False_negative", n_gti)
      names(out_list)[i] <- categories[i]
      next
    } else if(n_gti == 0){
      out_list[[i]] <- rep("False_positive", n_predi)
      names(out_list)[i] <- categories[i]
      next
    }
    
    
    grid <- expand.grid(seq_len(n_predi), seq_len(n_gti))
    
    mat <- lapply(seq_len(nrow(grid)), function(i){
      kp1 <- pred_classi[[grid[i,1]]]$keypoints
      kp2 <- gt_classi[[grid[i,2]]]$keypoints
      s2 <- area(pred_classi[[grid[i,2]]]$polygon)
      
      if(is.null(kp1) || is.null(kp2)){
        oks <- 0
      } else {
        oks <- keypoint_OKS(
          kp1[keypoints,1:2, drop = FALSE],
          kp2[keypoints,1:2, drop = FALSE],
          s2 = s2, 
          k = k, 
          ground_truth_flag = kp2[keypoints,"score", drop = TRUE]
        )
        return(oks)
      }
    }) %>% 
      unlist() %>%  
      matrix(nrow = n_predi, 
             ncol = n_gti)
    match_ids <- .which_max_no_replace(mat)
    out_list[[i]] <- purrr::map2(seq_along(match_ids),match_ids, function(x,y){
      if(y == "unmatched_row"){
        return("False_positive")
      } else if(y == "unmatched_column"){
        return("False_negative")
      } else {
        return(as.character(mat[as.integer(x),as.integer(y)]))
      }
    }) %>% 
      do.call("c", .)
    names(out_list)[i] <- categories[i]
  }
  out_list <- purrr::keep(out_list, function(x){!is.null(x)})
  return(out_list)
}


# Object keypoint similarity score
keypoint_OKS <- function(kp1, kp2, s2, k, ground_truth_flag){
  has_lab <- as.numeric(ground_truth_flag > 0)
  
  dist <- (kp1 - kp2)^2
  if(is.null(nrow(dist))){
    dist <- sum(dist)
  } else {
    dist <- rowSums(dist)
  }
  
  n <- sum(has_lab)
  
  if(n == 0 || s2 == 0){
    oks <- 0
  } else {
    oks <- sum(exp(-(dist)^2 / (s2 * 2 * k^2)) * has_lab) / n 
  }
  return(oks)
}

# Defunct function mostly for testing
# Mask intersection area
mask_intersection_area <- function(img, img2, na.rm = FALSE){
  stopifnot(all.equal(dim(img), dim(img2)))
  stopifnot(dim(img)[3] == 1)
  sum(img & img2, na.rm = na.rm)
}

# Polygon intersection area using the polyclip package
polygon_intersection_area <- function(poly1, poly2){
  intersection_poly <- polyclip::polyclip(as.list(as.data.frame(poly1)), 
                     as.list(as.data.frame(poly2)), 
                     op = "intersection")
  
  if(length(intersection_poly) < 1){
    res <- 0
  } else {
    if(is.nested(intersection_poly)){
      res <- do.call("sum",lapply(intersection_poly, function(x){.polygon_area(as.data.frame(x))}))
    } else {
      res <- intersection_poly %>% 
        as.data.frame() %>% 
        .polygon_area() 
    }
  }
  return(res)
}

# Compute IoU from polygons
IOU.polygon <- function(x, y){
  a1 <- area(x)
  a2 <- area(y)
  i <- polygon_intersection_area(x, y)
  a <- (a1 + a2 - i)
  iou <- ifelse(a > 0, i / a, 0)
  return(iou)
}

# Defunct function mostly for testing
# Compute IoU from masks
IOU.pixset <- function(img, img2, na.rm = FALSE){
  if(is.null(img) | is.null(img2)){
    return(NA)
  }
  a <- sum(img | img2, na.rm = na.rm)
  i <- mask_intersection_area(img, img2, na.rm)
  return(ifelse(a > 0, i / a, 0))
}

# Compute IoU for two bounding boxes
IOU.bbox <- function(x, y){
  p1 <- as.polygon(x)
  p2 <- as.polygon(y)
  return(IOU(p1, p2))
}






