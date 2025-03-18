


out2 %>% 
  filter(
    thing_class != "body"
  ) %>% 
  left_join(w) %>% 
  # group_by(Genus_Species) %>% 
  # summarise_at(
  #   vars(weight,`1-1-1`:`6-6-6`), .funs = sum
  # ) %>% 
  select(Genus_Species, image_id, weight, `1-1-1`:`6-6-6`) %>% 
  filter(!is.na(weight)) %>% 
  gather(key = key, value = val, `1-1-1`:`6-6-6`) %>% 
  group_by(image_id) %>% 
  arrange(image_id, key) %>% 
  summarise(
    Genus_Species = unique(Genus_Species),
    val = list("val" = c(val / sum(val) * unique(weight)))
  ) %>% 
  left_join(
    ref_d %>% select(Family, Genus_Species) %>% unique()
  ) %>% 
  group_by(Family) %>%
  mutate(
    n = n()
  ) %>% 
  filter(
    n > 4
  ) %>% 
  mutate(
    div = LOOKL(val)
  )-> z

z





w %>% 
  group_by(Genus_Species) %>% 
  summarise(
    number = mean(number)
  ) %>% 
  left_join(
    z
  ) %>% 
  left_join(ref_d %>% 
              select(Genus_Species, Family, Genus) %>% 
              unique()) %>% 
  left_join(host_d2 %>% 
              rename(db = n)) %>% 
  filter(Family %in% c("Erebidae", "Geometridae", "Noctuidae")) %>% 
  # glmmTMB::glmmTMB(
  #   div ~ log(number) + log(db) + (1|Family/Genus/Genus_Species),
  #   data = .,
  #   family = Gamma(link = "log")
  # ) %>%
  # summary()
  # group_by(Genus_Species) %>% 
  # summarise_all(function(x){
  #   if(!is.numeric(x)){
  #     unique(x)[1]
  #   } else {
  #     exp(mean(log(x), na.rm = TRUE))
  #   }
  # }) %>% 
  ggplot(aes(x = number, y = div, color = Family)) +
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



  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
