vmisc::load_all2("mothr")
options(
  "database_path" = "D:"
)

parsed_full <- list.files("cleaned_data/all_batches/", full.names = TRUE, pattern = ".rds") %>% 
  lapply(function(x){
    readr::read_rds(x)
  }) %>% 
  do.call("rbind", .)


parsed_full <- parsed_full %>% 
  filter(
    !file_name %in% unlist(fetch_exclude_flags())
  ) %>% 
  mothr::update_tag_id() %>%
  mutate(
    tag_id = split_tag_id(tag_id)
  ) %>% 
  update_tick_size()

parsed_full$wing_length <- wing_length_calc(parsed_full, units = "mm")
parsed_full <- bind_cols(parsed_full, thing_area_calc(parsed_full, units = "mm"))


l <- readRDS("invisible/googlesheets_tables.rds")
l <- l[!grepl("not_spread", names(l))] %>% 
  do.call("rbind.fill", .) %>% 
  dplyr::select(
    tag_id, date, MONA, location, species
  ) %>% 
  mutate(
    tag_id = reformat_tag_id(tag_id)
  ) %>% 
  mutate(
    date = as.POSIXct(date, format = "%d-%B-%Y"),
    month = lubridate::month(date),
    year = lubridate::year(date),
    doy = lubridate::yday(date)
  ) 



parsed_full <- parsed_full %>% 
  group_by(tag_id) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  left_join(l, by = "tag_id")

parsed_full <- parsed_full %>% 
  mutate(
    historic = grepl("WPC", tag_id)
  ) %>% 
  left_join(
    read_csv("raw_data/MPG-Taxa_20230503.csv") %>% 
      mutate(
        MONA = as.character(MONA)
      ) %>% 
      select(
        Genus_Species, Genus, Family, Superfamily, MONA
      ), 
    by = "MONA"
  )


parsed_full %>% 
  select(
    id, file_name, path, height, width, num_annotations, version, inf_time,
    tag_id, tick_size, wing_length:Superfamily
  ) %>% 
  select(-species) %>% 
  mutate(
    path = gsub("D:/", "C:/", path)
  ) %>% 
  write_csv("cleaned_data/moth_database_size_measurements_May_22_2026.csv")



parsed_full %>% 
  filter(
    !is.na(wing_length) & !is.na(MONA)
  ) %>% 
  group_by(historic, MONA) %>% 
  summarise(
    m = mean(log(wing_length)),
    s = sd(log(wing_length)),
    n = n()
  ) %>% 
  ungroup() %>% 
  group_by(MONA) %>% 
  filter(
    n() == 2
  ) %>% 
  ungroup() %>% 
  arrange(MONA, historic) %>% 
  group_by(MONA) %>% 
  summarise(
    d = -diff(m),
    s = vmisc::pooled_SD(s^2, n) * sqrt(sum(1/n)),
    l = d - s * 1.96,
    u = d + s * 1.96
  ) %>% 
  filter(
    !is.na(s)
  ) %>% 
  group_by(Family) %>% 
  filter(
    n() > 10
  ) %>% 
  mutate(
    len = length(d)
  ) %>% 
  ggplot(aes(x = forcats::fct_reorder(MONA, d), y = exp(d))) + 
  geom_pointrange(
    aes(ymin = exp(l), ymax = exp(u), color = ifelse(
      u < 0,
      "Decreased",
      ifelse(
        l > 0,
        "Increased",
        "NS"
      )
    )),
    alpha = 0.5
  ) +
  scale_y_log10() + 
  coord_flip() + 
  geom_vline(aes(xintercept = len / 2), color = "black", linetype = "dashed", linewidth = 1) + 
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", linewidth = 1) + 
  labs(color = "Response", x  = "MONA", y = "Wing length response ratio (Present/Historic)") + 
  theme_minimal() + 
  facet_wrap(~Family, scales = "free_y") + 
  theme(legend.position = "top") -> g;g


ggsave("graphs/all_moth_wing_length_by_family.jpg", g, height = 8, width = 8, dpi = 400)
  


