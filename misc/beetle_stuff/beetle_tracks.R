vmisc::load_all2("mothr")

d <- read_csv("beetle_stuff/AC_10out_track.csv")



res <- parse_pytracks(d)

format_tracks(res) %>% 
  ggplot(aes(x = x, y = y, color = new_beetle_id)) + 
  geom_path(linewidth = 1.5) + 
  scale_color_manual(values = colorpal(14)) + 
  theme_bw()