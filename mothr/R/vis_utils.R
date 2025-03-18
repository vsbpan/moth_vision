colorpal <- function(n){
  if(n > 20){
    stop("Not enough colors")
  }
  c("#88CCEE", "#DDCC77", "#CC6677", "#117733", "#332288", 
    "#AA4499", "#44AA99", "#999933", "#882255", "#661100", 
    "#6699CC", "#888888", "#E69F00", "#56B4E9", "#009E73", 
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")[seq_len(n)]
}


plot_moments <- function(data_list, choice = c(2:4), plot = TRUE, center = FALSE, max_plot = 5000){
  choice <- c(1, choice)
  
  if(length(data_list) > max_plot){
    cli::cli_alert_warning("Length of {.arg data_list} ({length(data_list)}) exceeds {.arg max_plot = {max_plot}} ")
    data_list <- sample(data_list, max_plot)
  }
  
  out <- lapply(
    data_list,
    function(x){
      res <- vapply(choice, FUN = function(p){
        vmisc::moment(x, p)
      }, FUN.VALUE = numeric(1))
      names(res)<- c("x", paste0(choice[-1]))
      res
    }
  ) %>% 
    vmisc::bind_vec() %>% 
    tidyr::gather(
      key = k, value = val, -x
    ) %>% 
    dplyr::mutate(
      k = as.numeric(k)
    )
  
  if(plot){
    out2 <- out
    
    
    if(center){
      y_lab <- latex2exp::TeX("$\\langle x^k \\rangle - \\langle x \\rangle^k$")
      out2 <- out2 %>% 
        dplyr::mutate(
          val = val - x^k
        )
    } else {
      y_lab <- latex2exp::TeX("$\\langle x^k \\rangle $")
    }
    
    g <- out2 %>% 
      ggplot2::ggplot(ggplot2::aes(x = x, y = val, color = as.factor(k), group = as.factor(k))) + 
      ggplot2::geom_point() + 
      ggpmisc::stat_ma_line() +
      ggpmisc::stat_ma_eq(eq.with.lhs = "italic(hat(y))~`=`~",
                          ggpmisc::use_label(c("eq", "R2")),
                          rr.digits = 3,
                          coef.digits = 3, 
                          coef.keep.zeros = TRUE) +
      ggplot2::scale_x_continuous(trans = "log10") +
      ggplot2::scale_y_continuous(trans = "log10", label = vmisc::fancy_scientific)+
      ggplot2::labs(x = latex2exp::TeX("$\\langle x \\rangle $"), 
           y = y_lab,
           color = "k") + 
      ggplot2::theme_bw() + 
      ggplot2::scale_color_viridis_d()
    
    
    suppressWarnings(print(g))
  }
  invisible(out)
}

ggbiplot <- function (x, choices = c(1, 2), scaling = 2, display = c("sites", 
                                                         "species", "biplot", "centroids"), ellipse = NA, group = NULL, 
          group2 = NULL, sites_alpha = 1, sites_size = 2, group_as_aes = TRUE, 
          species_color = "violetred", species_alpha = 1) 
{
  display <- match.arg(display, several.ok = TRUE)
  s <- vegan::scores(x, choices = choices, scaling = scaling)
  name <- names(s)
  if (is.null(name)) {
    name <- "sites"
    s <- list(s)
  }
  s <- lapply(seq_along(name), function(i, s, name) {
    x <- as.data.frame(s[[i]])
    x <- cbind(x, a = rownames(x))
    names(x)[length(x)] <- name[i]
    x
  }, s = s, name = name)
  names(s) <- name
  dim1 <- names(s$sites)[1]
  dim2 <- names(s$sites)[2]
  s$dummy <- data.frame(1, 2)
  names(s$dummy) <- c(dim1, dim2)
  egv <- vegan::eigenvals(x)
  prec_var <- signif((egv/sum(egv) * 100)[choices], digits = 2)
  g <- s$dummy %>% ggplot2::ggplot(ggplot2::aes_string(paste0("x = ", 
                                                              dim1), paste0("y = ", dim2))) + 
    ggplot2::geom_vline(ggplot2::aes(xintercept = 0), 
                        linetype = "dashed", color = "grey", size = 1) + 
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), 
                        linetype = "dashed", color = "grey", size = 1) + 
    ggplot2::theme_bw() + 
    ggplot2::labs(x = paste0(dim1, " (", prec_var[1], "%)"), 
                  y = paste0(dim2, " (", prec_var[2], "%)"))
  if ("sites" %in% display) {
    if (!is.null(group)) {
      if (!is.null(group2)) {
        s$sites <- cbind(s$sites, group = group, group2 = group2)
        if (group_as_aes) {
          g <- g + ggplot2::geom_point(data = s$sites, 
                                       ggplot2::aes(color = group, shape = group2), 
                                       alpha = sites_alpha, size = sites_size)
        }
        else {
          g <- g + ggplot2::geom_point(data = s$sites, 
                                       ggplot2::aes(shape = group2), color = group, 
                                       alpha = sites_alpha, size = sites_size)
        }
      }
      else {
        s$sites <- cbind(s$sites, group = group)
        if (group_as_aes) {
          g <- g + ggplot2::geom_point(data = s$sites, 
                                       ggplot2::aes(color = group), alpha = sites_alpha, 
                                       size = sites_size)
        }
        else {
          g <- g + ggplot2::geom_point(data = s$sites, 
                                       color = group, alpha = sites_alpha, size = sites_size)
        }
      }
    }
    else {
      g <- g + ggplot2::geom_point(data = s$sites, color = "deepskyblue", 
                                   alpha = sites_alpha)
    }
  }
  if (!is.na(ellipse)) {
    if (!is.null(group)) {
      s$sites <- cbind(s$sites, z = group)
      g <- g + ggplot2::stat_ellipse(data = s$sites, ggplot2::aes(color = z),linetype = 4, size = 1)
    }
    else {
      g <- g + ggplot2::stat_ellipse(data = s$sites, color = "navy", 
                                     linetype = 4, size = 1)
    }
  }
  if ("species" %in% display && !is.null(s$species)) {
    g <- g + ggplot2::geom_text(data = s$species, ggplot2::aes(label = species), 
                                color = species_color, alpha = species_alpha)
  }
  if ("biplot" %in% display && !is.null(s$biplot)) {
    s$biplot$x <- 0
    s$biplot$y <- 0
    s$biplot$xend <- s$biplot[, dim1]
    s$biplot$yend <- s$biplot[, dim2]
    g <- g + ggplot2::geom_text(data = s$biplot, ggplot2::aes(label = biplot), 
                                color = "black") + 
      ggplot2::geom_segment(data = s$biplot, 
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend), 
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.2,"cm")), 
                            color = "black", size = 1)
  }
  if ("centroids" %in% display && !is.null(s$centroids)) {
    g <- g + ggplot2::geom_text(data = s$centroids, ggplot2::aes(label = centroids), 
                                color = "darkolivegreen")
  }
  return(g)
}

plot_gradfield <- function(data, aggregate = 2, 
                           colours = c("#B2182B", "#E68469", "#D9E9F1", "#ACD2E5", 
                                       "#539DC8", "#3C8ABE", "#2E78B5")){
  foo <- function(data,aggregate,colours){
    rast <- raster::rasterFromXYZ(data)
    names(rast) <- "z"
    raster::projection(rast) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
    quiv <- raster::aggregate(rast, aggregate)
    terr <- raster::terrain(quiv, opt = c('slope', 'aspect'))
    quiv$u <- terr$slope[] * sin(terr$aspect[])
    quiv$v <- terr$slope[] * cos(terr$aspect[])
    quiv_df <- as.data.frame(quiv, xy = TRUE)
    rast_df <- as.data.frame(rast, xy = TRUE)
    nms <- colnames(data)
    g <- ggplot(mapping = aes(x = x, y = y, fill = z)) + 
      geom_raster(data = rast_df, na.rm = TRUE) + 
      ggquiver ::geom_quiver(data = quiv_df, aes(u = u, v = v), vecsize = 1.5) +
      scale_fill_gradientn(colours = colours, na.value = "transparent") +
      theme_bw() + 
      labs(x = nms[1], y = nms[2], fill = nms[3])
    g
  }
  
  environment(foo) <- asNamespace("raster")
  g <- foo(data,aggregate,colours)
  
  return(g)
}

