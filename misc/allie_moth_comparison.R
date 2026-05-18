vmisc::load_all2("mothr")

# # Process Allie's historic specimen images
# d <- import_raw_inference(path_meta = "inference/allie_historic_image_meta_mask.csv",
#                           path_inference = "inference/allie_historic_inference_mask.csv")
# 
# 
# d2 <- import_raw_inference(path_meta = "inference/allie_historic_image_meta_keypoint.csv",
#                            path_inference = "inference/allie_historic_inference_keypoint.csv")
# 
# 
# register_image_id(d$meta$file_name)
# 
# parsed_mask <- as.parsed_inference(d)
# parsed_kp <- as.parsed_inference(d2)
# 
# 
# parsed_full <- merge_parsed_inference(parsed_mask, parsed_kp)
# 
# # Make mini moth
# pb_par_lapply(
#   1:nrow(parsed_full),
#   function(i, parsed_full){
#     if(parsed_full[i, "empty_instance", drop = TRUE]){
#       return(invisible(NULL))
#     }
# 
#     tryCatch({
#       path <- parsed_full[i, "path", drop = TRUE]
#       new_fn <- gsub("img_moth","mini_moth",basename(path))
#       write_path <- paste(get_mini_moth_path("D:"), new_fn,sep = "/")
#       img <- fast_load_image(path)
#       img <- bbox_crop(img, moth_bbox(parsed_full[i, "inlist", drop = TRUE][[1]]))
#       write_jpg(img, write_path)
#     }, error = function(e){
#       message(e$message)
#       return(NULL)
#     })
#     return(invisible(NULL))
#   }, cores = 4, inorder = FALSE,
#   parsed_full = parsed_full
# )
# 
# saveRDS(parsed_full, file = "cleaned_data/allie_historic_parsed.rds")


# Read in parsed inference
parsed_historic <- readRDS("cleaned_data/allie_historic_parsed.rds")

# Drop mini-moths with known problems
parsed_historic <- parsed_historic %>% 
  filter(
    !file_name %in% (fetch_inference_error() %>% unlist(FALSE, FALSE)) & 
      !file_name %in% (fetch_exclude_flags() %>% unlist(FALSE, FALSE))
  )

# Read in WPC meta data
WPC_d <- import_sheets("https://docs.google.com/spreadsheets/d/1llqU0s4pNzkLHiev1MquWIZSiBvd186V1YclLt1TCNs/edit?gid=0#gid=0", "Sheet1")

# Read in taxon info
d_taxon <- read_csv("raw_data/MPG-Taxa_20230503.csv") %>% 
  select(MONA, Superfamily, Family, Subfamily, Tribe, Subtribe, Genus, Species) %>% 
  rename_at(vars(-MONA), tolower) %>% 
  mutate_at(vars(-MONA), clean_taxon_name) %>% 
  mutate(
    sp = species,
    species = paste(genus, species, sep = " ")
  )

WPC_d <- WPC_d %>% 
  dplyr::select(date, species,genus, family, sex, tag_id, MONA, location) %>% 
  rename(sp = species) %>% 
  mutate(
    date = as.POSIXct(date, format = "%d-%B-%Y"),
    month = lubridate::month(date),
    year = lubridate::year(date),
    doy = lubridate::yday(date)
  ) %>% 
  rename_at(vars(family, genus, sp), function(x){paste0(x,"_guess")}) %>% 
  left_join(
    d_taxon %>% 
      mutate(MONA = as.character(MONA)), by = "MONA"
  ) %>% 
  mutate(
    # Fill in the taxon info if there is manual entry and no matched MONA entry. Otherwise, MONA taxon takes precedence.
    family = ifelse(!is.na(family_guess) & is.na(family), family_guess, family),
    genus = ifelse(!is.na(genus_guess) & is.na(genus), genus_guess, genus),
    sp = ifelse(!is.na(sp_guess) & is.na(sp), sp_guess, sp),
    sp = ifelse(is.na(sp), "sp", sp),
    species = paste(genus, sp)
  ) %>% 
  dplyr::select(-contains("_guess")) %>% 
  filter(!is.na(date))

# Compute image record level meta-data
d_ref2 <- parsed_historic %>% 
  dplyr::select(tag_id_guess, id, file_name, path, tick_size) %>% 
  mutate(
    image_id = paste0("img", id),
    tag_id = tag_id_guess
  ) %>% 
  mothr::update_tag_id() %>%
  mutate(
    tag_id = split_tag_id(tag_id)
  ) %>% 
  left_join(
    WPC_d %>% 
      mutate(
        tag_id = reformat_tag_id(tag_id)
      ) %>% 
      filter(!is.na(tag_id)),
    by = "tag_id"
  ) 

d_ref %>% 
  filter(MONA %in% c(8170, 8169, 8196)) %>% 
  .$file_name -> ids

# Add the two together for quicker processing
parsed_joined <- parsed_full %>% 
  filter(
    file_name %in% ids
  ) %>% 
  bind_rows(parsed_historic)



parsed_joined$wing_length <- wing_length_calc(parsed_joined, units = "mm")
parsed_joined <- bind_cols(parsed_joined, thing_area_calc(parsed_joined, units = "mm"))

options(
  "database_path" = "D:"
)

parsed_joined <- bind_cols(parsed_joined, thing_black_area_calc(parsed_joined, units = "mm", cores = 6))

parsed_joined <- parsed_joined %>% 
  mutate(
    hindwing_area_other = hindwing_area - hindwing_area_black,
    body_area_other = body_area - body_area_black,
    forewing_area_other = forewing_area - forewing_area_black
  )

a <- thing_not_black_color(parsed_joined, cores = 6)

parsed_joined <- bind_cols(parsed_joined,
                           a %>% 
                             reframe(
                               expand_hex(body_hex),
                               expand_hex(hindwing_hex),
                               expand_hex(forewing_hex)
                             ) %>% 
                             rename_all(function(x){
                               gsub("hex_","",x)
                             }) %>% 
                             bind_cols(a))




parsed_joined


names(parsed_joined)



d_final <- d_ref %>% 
  select(file_name, MONA, tag_id, date,location,month, year, doy,
         superfamily,family,subfamily,genus,sp, sex) %>% 
  bind_rows(
    d_ref2 %>% 
      select(file_name, MONA, tag_id, date,location,month, year, doy,
             superfamily,family,subfamily,genus,sp,sex)
  ) %>% 
  filter(
    file_name %in% parsed_joined$file_name
  ) %>% 
  left_join(parsed_joined %>% 
              select(id, file_name, path, tick_size,
                     contains("body"), contains("forewing"), contains("hindwing"), 
                     wing_length))

names(d_final)



# write_csv(d_final, "cleaned_data/Allie_moth_data.csv")


d_final <- read_csv("cleaned_data/Allie_moth_data.csv")


d_final %>% 
  mutate(
    source = ifelse(year > 2000, "modern", "historic")
  ) %>% 
  ggplot(aes(x = source, y = wing_length, color = sex)) + 
  geom_boxplot() + 
  geom_point(position = "jitter") + 
  labs(x = "Collection", y = "Wing length (mm)") + 
  theme_bw() + 
  facet_wrap(~MONA, scales = "free")

library(glmmTMB)

glmmTMB(
  wing_length ~ source + s(month) + (1|MONA), 
  data = d_final %>% 
    mutate(
      source = ifelse(year > 2000, "modern", "historic")
    )
) %>% 
  summary()




par(mfrow=c(3,2))
par(mar = c(0.1, 0, 0.1, 0))
fn <- d_final$file_name[40]
fn2 <- d_final$file_name[111]
img <- fn %>% mini_moth_path() %>% fast_load_image()
img2 <- fn2 %>% mini_moth_path() %>% fast_load_image()
w <- parsed_full %>% 
  filter(file_name == fn)
w2 <- parsed_full %>% 
  filter(file_name == fn2)
l <- as_relative(w$inlist[[1]]) %>% select_things(things = c("body","forewing","hindwing"))
l2 <- as_relative(w2$inlist[[1]]) %>% select_things(things = c("body","forewing","hindwing"))

plot(img, axes = FALSE)
plot(img2, axes = FALSE)
plot(img, axes = FALSE)
plot(l, kp_pch = )
plot(img2, axes = FALSE)
plot(l2)

threshold_black(img, as.pixset(l, dim(img))) %>% 
  color_invert() %>%
  immask(!as.pixset(l, dim(img)), background = 0.3) %>% 
  plot(axes = FALSE)


threshold_black(img2, as.pixset(l2, dim(img2))) %>% 
  color_invert() %>%
  immask(!as.pixset(l2, dim(img2)), background = 0.3) %>% 
  plot(axes = FALSE)

dev.off()


