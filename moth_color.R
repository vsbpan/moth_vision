vmisc::load_all2("mothr")


parsed_full <- readRDS("cleaned_data/parsed_full.rds")


# Drop mini-moths with known problems
parsed_full <- parsed_full %>% 
  filter(
    !file_name %in% (fetch_inference_error() %>% unlist(FALSE, FALSE))
  )


l <- pb_par_lapply(10001:nrow(parsed_full), function(i,parsed_full){
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
        count_rgb_bin(img)
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
  #filter(thing_class %in% c("hindwing", "forewing")) %>% 
  group_by(image_id, thing_class) %>% 
  summarise_all(function(x){
    if(is.character(x)){
      return(unique(x)[1])
    } else {
      return(sum(x))
    }
  })


out2 %>% 
  filter(
    thing_class != "body"
  ) %>% 
  gather(key = bin, value = count, `1-1-1`:`6-6-6`) %>% 
  group_by(image_id,thing_class) %>% 
  filter(count > 0) %>% 
  mutate(
    p = count / sum(count)
  ) %>% 
  mutate(
    split_color_bin(bin)
  ) %>% 
  group_by(image_id, thing_class) %>% 
  summarise(
    r = mean_wt(r, p),
    g = mean_wt(g, p),
    b = mean_wt(b, p)
  ) -> z


z %$% 
  plotly::plot_ly(x = r, y = g, z = b, type = "scatter3d", size = 1, alpha = 0.3, color = thing_class) %>% 
  plotly::layout(title = 'Mean in channel intensity', 
                 scene = list(xaxis=list(title ="Red"),
                              yaxis=list(title ="Green"),
                              zaxis=list(title ="Blue")))




w <- out2 %>% 
  filter(
    thing_class != "body"
  ) %>% 
  gather(key = bin, value = count, `1-1-1`:`6-6-6`) %>% 
  group_by(image_id,thing_class) %>% 
  filter(count > 0) %>% 
  mutate(
    p = count / sum(count)
  ) %>% 
  filter(p > 0.01) %>% 
  mutate(
    p = count / sum(count)
  ) %>% 
  summarise(
    H = entropy(p),
    H_max = entropy(rep(1/length(p), length(p))),
    n = length(p),
    H_min = entropy(c(rep(0.01, length(p) - 1), 1 - sum(0.01 * (length(p) - 1)))),
    H_unif = entropy_null(p, "unif"),
    H_lnorm = entropy_null(p, "lnorm", meanlog = -2.730898,sdlog = 1.077979),
    H_gamma = entropy_null(p, "gamma", shape = 1.022666, rate = 8.938170)
  )

w %>% 
  left_join(
    ref_d, by = "image_id"
  ) %>% 
  ggplot(aes(x = n, y = H)) +
  geom_point(alpha = 0.05, position = position_jitter(width = 0.2), size = 2, color = "grey") +
  geom_smooth(
    data = 
      w %>% 
      gather(key = "type", value = H, c(H_max, H_min:H_gamma)),
    aes(color = type),
    linewidth = 1.2,
    se = FALSE, 
    formula = y ~ s(x, bs = "tp", k = 5)
  ) + 
  facet_wrap(~thing_class) + 
  geom_smooth(method = "gam", size= 2, color = "black",
              formula = y ~ s(x, bs = "tp", k = 5)) + 
  #scale_color_brewer(type = "qual") + 
  theme_bw(base_size = 15) + 
  labs(x = "Number of colors", y = "Shannon entropy", color = "Distribution")












w %>% 
  left_join(
    ref_d, by = "image_id"
  ) %>% 
  filter(
    Family %in% c("Erebidae", "Noctuidae", "Geometridae")
  ) %>% 
  ggplot(aes(x = n, y = H, color = Family)) +
  geom_point() + 
  geom_smooth(method = "gam") + 
  facet_wrap(~thing_class)











b %>% 
  filter(MONA == "10440") %>% 
  mutate(
    date = as.POSIXct(date, format = "%d-%B-%Y")
  ) %>% 
  mutate(
    month = lubridate::month(date)
  ) %>% 
  filter(
    is.between(month, c(8, 11), TRUE)
  ) %>% 
  with({
    list(
      "infected" = body_area[ear_mites %in% c("Left", "Right","Both")],
       "uninfected" = body_area[ear_mites %in% c("None")]
    )
  }) %>% 
  vmisc::loghist(log.x = FALSE) + 
  labs(x = "Moth body area", y = "Probability density")

   
  mutate(
    ear_mites = ifelse(ear_mites %in% c("Left", "Right"), "one", ear_mites),
    foo = ifelse(ear_mites == "one", 1, 0)
  ) %>%
  filter(ear_mites != "Both") %>% 
  glmmTMB::glmmTMB(
    foo ~ log(wl) + date, data= ., family = binomial()
  ) %>% summary()



  ggplot(aes(x = wl, y = ear_mites)) + 
  geom_point(position = position_jitter(height = 0.1), alpha = 0.1) + 
  geom_pointrange(color = "steelblue", stat = "summary", size = 1, linewidth = 2) + 
  scale_x_log10()


  






z %>% filter(thing_class == "forewing") %$% 
    plotly::plot_ly(x = r, y = g, z = b, type = "scatter3d", size = 1, alpha = 0.3, 
                    marker = list(color = rgb(r/6, g/6, b/6, maxColorValue = 1))) %>% 
    plotly::layout(title = 'Mean in channel intensity', 
                   scene = list(xaxis=list(title ="Red"),
                                yaxis=list(title ="Green"),
                                zaxis=list(title ="Blue")))





z %>% 
  left_join(b, by = "image_id") %>% 
  mutate(
    date = as.POSIXct(date, format = "%d-%B-%Y")
  ) %>% 
  mutate(
    month = lubridate::month(date)
  ) %>% 
  group_by(month) %>% 
  summarise(
    r = mean(r),
    g = mean(g),
    b = mean(b)
  ) %$% 
  {
    plotly::plot_ly(x = r,y = g, z = b) %>% 
      plotly::add_trace(mode = "lines+markers") %>% 
      plotly::add_text(text=~as.character(month), textposition="top center")
  }


z %>% 
  left_join(b, by = "image_id") %>% 
  group_by(location, month) %>% 
  filter(!is.na(month)) %>% 
  tally() %>% 
  summarise(
    r = median(r, na.rm = TRUE),
    g = median(g, na.rm = TRUE),
    b = median(b, na.rm = TRUE)
  ) %>% 
  filter(!is.na(month)) %>% 
  rowwise() %>% 
  mutate(
    hex = rgb(r/6, g/6, b/6)
  ) %>% 
  {
    ggplot(.,aes(x = month, y = location)) + 
      geom_tile(fill = .$hex)
  }


  gather(key = channel, value = v, -month) %>% 
  ggplot(aes(x = month, y = v, color = channel)) + 
  geom_line(size = 1) + 
  scale_color_manual(values = rev(c("red", "green","blue")))












  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  