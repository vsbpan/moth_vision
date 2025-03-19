


out2 %>% 
  left_join(d_ref) %>% 
  group_by(
    species
  ) %>% 
  mutate(
    weight = number/ n()
  ) %>% 
  # group_by(Genus_Species) %>% 
  # summarise_at(
  #   vars(weight,`1-1-1`:`6-6-6`), .funs = sum
  # ) %>% 
  select(species, image_id, weight, `1-1-1`:`6-6-6`) %>% 
  filter(!is.na(weight)) %>% 
  ungroup() %>% 
  gather(key = key, value = val, `1-1-1`:`6-6-6`) %>% 
  group_by(image_id) %>% 
  arrange(image_id, key) %>% 
  summarise(
    species = unique(species),
    val = list("val" = c(val / sum(val) * unique(weight)))
  ) %>% 
  left_join(d_ref) %>% 
  group_by(species) %>%
  mutate(
    n = n()
  ) %>% 
  filter(
    n > 9
  ) %>% 
  mutate(
    div = LOOKL(val, n = 10)
  )-> z2


debug(LOOKL)

z2$n


d_trait %>% 
  group_by(species) %>% 
  left_join(
    z2
  ) -> w
  #group_by(species) %>%
  # summarise_all(function(x){
  #   if(!is.numeric(x)){
  #     unique(x)[1]
  #   } else {
  #     exp(mean(log(x), na.rm = TRUE))
  #   }
  # }) %>%
  #filter(family %in% c("Erebidae", "Geometridae", "Noctuidae")) %>% 
  glmmTMB::glmmTMB(
    div ~ 
      log(number) + #(log(db_family)) * log(body_area) + 
      (1|location) + 
      #(0 + log(db_family) + log(number)|family) + 
      (1|family/genus/species),
    data = w, 
    control = glmmTMB::glmmTMBControl(optimizer = "optim", optArgs = list(method = "BFGS")),
    family = Gamma(link = "log")
  ) -> m
summary(m)

sjPlot::plot_model(m, type = "eff", terms = c("body_area", "db_family[3,30]")) + 
  scale_xy_log()
marginal_effects(m,terms = c("body_area", "db_family[3, 30]"), n = 10) 
  

  ggplot(aes(x = number, y = div, color = family)) +
  geom_point() + 
  #geom_point(position = position_jitterdodge(jitter.width = 0.1), color = "grey", aes(fill = Family)) + 
  #scale_y_log10() + 
  #geom_pointrange(stat = "summary", position = position_dodge(width = 0.75))
  geom_lineeq(labels = c("eq", "R2", "P"), method = "OLS") + 
  #geom_smooth(method = "lm") +
  scale_xy_log()



mini_moth_path(parsed_full$file_name, root_path = "D:") 
  lapply(imagefluency::img_read) %>% 
  imagefluency::img_typicality()


  
  

  
  
  
  
  
  
  
  
  
  
  

  
