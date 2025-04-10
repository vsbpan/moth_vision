vmisc::load_all2("mothr")


parsed_full <- readRDS("cleaned_data/parsed_full.rds")


# Drop mini-moths with known problems
parsed_full <- parsed_full %>% 
  filter(
    !file_name %in% (fetch_inference_error() %>% unlist(FALSE, FALSE))
  )


l <- pb_par_lapply(1:nrow(parsed_full), function(i,parsed_full){
  out <- tryCatch({
    img_path <- parsed_full$file_name[i] %>% 
      mini_moth_path(root_path = "D:/")
    if(!file.exists(img_path)){
      return(NULL)
    }
    img <- fast_load_image(img_path)
    
    parsed_full$inlist[[i]] %>% 
      as_relative() %>% 
      select_things(c("forewing","hindwing", "body")) %>% 
      lapply(function(x){
        img[!as.pixset(x$polygon,dim_xy = dim(img)[1:2])] <- NA_real_
        res <- list(
          "image_id" = x$image_id,
          "instance_id" = x$instance_id,
          "thing_class" = x$thing_class
        )
        c(res, count_rgb_bin(img))
      })
  }, error = function(e){
    message(e$message)
    return(NULL)
  })
}, cores = 4, inorder = FALSE,
parsed_full = parsed_full)



l <- readRDS("invisible/color_counts.rds")
out <- collect_color_bins(l)





out2 <- out %>% 
  filter(thing_class %in% c("forewing", "hindwing")) %>% 
  group_by(image_id) %>% 
  summarise_all(function(x){
    if(is.character(x)){
      return(unique(x)[1])
    } else {
      return(sum(x))
    }
  })
out2 <- out2 %>% 
  select(image_id, `1-1-1`:`6-6-6`) %>% 
  gather(key = key, value = val, `1-1-1`:`6-6-6`) %>% 
  group_by(image_id) %>% 
  arrange(image_id, key) %>% 
  summarise(
    image_id = unique(image_id),
    val = list("val" = val)
  ) %>% 
  left_join(d_ref)






l <- pb_par_lapply(1:nrow(parsed_full), function(i,parsed_full){
  out <- tryCatch({
    img_path <- parsed_full$file_name[i] %>% 
      mini_moth_path(root_path = "D:/")
    if(!file.exists(img_path)){
      return(NULL)
    }
    img <- fast_load_image(img_path)
    
    parsed_full$inlist[[i]] %>% 
      as_relative() %>% 
      select_things(c("forewing","hindwing", "body")) %>% 
      lapply(function(x){
        res <- list(
          "image_id" = x$image_id,
          "instance_id" = x$instance_id,
          "thing_class" = x$thing_class
        )
        c(res, binary_count2(img, as.pixset(x, dim(img))))
      })
  }, error = function(e){
    message(e$message)
    return(NULL)
  })
}, cores = 4, inorder = FALSE,
parsed_full = parsed_full)


z <- unlist(l, FALSE) %>% 
  do.call("bind_rows", .)

saveRDS(z, "invisible/white_black_count.rds")

get_p <- function(n){
  w <- runif(n)
  w / sum(w)
}
rvm <- function(s, n){
  extraDistr::rcat(s, get_p(n))
}
get_margin <- function(x){
  -diff(sort(table(x), decreasing = TRUE))
}


z <- z %>% 
  mutate(n = white + black)

z <- z %>% 
  mutate(
    m = abs(white-black)
  )

lapply(z$n, function(x){
  rvm(ceiling(x/100),2) %>% get_margin()
}) %>% 
  do.call("c", .) -> a

z2 <- z %>% 
  mutate(
    #m_pred = a
  )

z2 

loghist(list(
  "a" = a,
  "m" = z$m
), log.p = TRUE, scale = TRUE)



z2 <- z2 %>% 
  left_join(d_ref)

z2 %>% 
  filter(thing_class != "body") %>% 
  filter(!is.na(genus)) %>% 
  mutate(
    group = family
  ) %>% 
  mutate(
    mu = m/n
  ) %>% 
  group_by(group) %>% 
  summarise(
    mu = list(mu),
    m = list(m),
    #m_pred = list(m_pred/ n)
   ) %>% 
  select(group, mu) %>% 
  as_named_vector() %>% 
  keep_len(1000) %>% 
  #plot_moments()
  loghist(scale = TRUE, geom = "line") +
  theme(legend.position = "none")


calc_hist2 <- function (x, breaks, log.x, log.p, scale, delta, phi, ...) 
{
  x <- x[!is.na(x)]
  if (log.x) {
    if (scale) {
      mu <- mean(x)
      x <- x/mu^phi
      p <- hist(log(x), plot = FALSE, breaks = breaks, 
                ...)
      d <- data.frame(x = exp(p$mids), p = p$density * 
                        exp(p$mids)^(delta - 1))
      attr(d, "scaling") <- list(mu = mu, phi = phi, 
                                 delta = delta)
    }
    else {
      p <- hist(log(x), plot = FALSE, breaks = breaks, 
                ...)
      d <- data.frame(x = exp(p$mids) / mean(x), p = p$density/exp(p$mids) * mean(x))
    }
  }
  else {
    if (scale) {
      mu <- mean(x)
      x <- x/mu^phi
      p <- hist(x, plot = FALSE, breaks = breaks, ...)
      d <- data.frame(x = p$mids, p = p$density * p$mids^delta)
      attr(d, "scaling") <- list(mu = mu, phi = phi, 
                                 delta = delta)
    }
    else {
      p <- hist(x, plot = FALSE, breaks = breaks, ...)
      d <- data.frame(x = p$mids, p = p$density)
    }
  }
  if (log.p) {
    d <- d[d$p > 0, ]
  }
  return(d)
}

loghist2 <- function (x, nclass = 50, by = NULL, log.p = FALSE, log.x = TRUE, 
                      scale = FALSE, delta = 1, phi = 1, geom = c("line", 
                                                                  "col"), linewidth = 1, distr_list = NULL, draw_distr_args = list(linewidth = linewidth, 
                                                                                                                                   linetype = "dashed", delta = delta, phi = phi, 
                                                                                                                                   scale = scale), hist_args = NULL, ...) 
{
  if (!missing(by)) {
    nclass <- NULL
  }
  if (!is.null(nclass)) {
    if (!is.null(by) && missing(by)) {
      stop("Only one of 'nclass' or 'by' should be supplied and the other set to NULL.")
    }
    breaks <- nclass
  }
  else {
    if (!is.null(by)) {
      if (scale) {
        x1 <- range(unlist(lapply(x, function(z) {
          z/mean(z, na.rm = TRUE)^phi
        }), TRUE, FALSE), na.rm = TRUE)
      }
      else {
        x1 <- range(unlist(x, TRUE, FALSE), na.rm = TRUE)
      }
      if (log.x) {
        x1 <- log(x1)
      }
      breaks <- seq_interval(x1, by = by, na.rm = TRUE)
      breaks <- c(min(breaks,) - by, breaks, max(breaks) + 
                    by)
    }
    else {
      stop("Must supply 'nclass' or 'by' to set the breaks.")
    }
  }
  nms <- names(x)
  if (is.null(nms)) {
    nms <- seq_along(x)
  }
  d <- lapply(seq_along(x), function(i) {
    do.call("calc_hist2", c(list(x = x[[i]], breaks = breaks, 
                                log.x = log.x, log.p = log.p, scale = scale, delta = delta, 
                                phi = phi), hist_args)) %>% cbind(group = as.factor(nms[i]))
  }) %>% do.call("rbind", .)
  if (!log.p && length(geom) == 2) {
    geom <- "col"
  }
  else {
    geom <- match.arg(geom)
  }
  if (scale) {
    x_lab <- tex("x / \\langle x \\rangle^\\phi")
    y_lab <- tex("x^\\Delta P(x)")
  }
  else {
    x_lab <- tex("x")
    y_lab <- tex("P(x)")
  }
  g <- d %>% ggplot2::ggplot(ggplot2::aes(x = x, y = p)) + 
    ggplot2::theme_bw(base_size = 15) + ggplot2::labs(x = x_lab, 
                                                      y = y_lab)
  if (log.x) {
    g <- g + ggplot2::scale_x_continuous(trans = "log10", 
                                         labels = fancy_scientificb)
  }
  if (log.p) {
    g <- g + ggplot2::scale_y_continuous(trans = "log10", 
                                         labels = fancy_scientificb)
  }
  if (geom == "line") {
    g <- g + ggplot2::geom_line(aes(color = group, group = group), 
                                linewidth = linewidth, ...)
  }
  if (geom == "col") {
    g <- g + ggplot2::geom_col(aes(group = group, fill = group), 
                               position = "dodge", ...)
  }
  if (!is.null(distr_list)) {
    g <- do.call("draw_distr", c(list(g = g, distr_list = distr_list, 
                                      x = unique(d$x)), draw_distr_args))
  }
  return(g)
}







