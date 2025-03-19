vmisc::load_all2("mothr")


parsed_full <- readRDS("cleaned_data/parsed_full.rds")


# Drop mini-moths with known problems
parsed_full <- parsed_full %>% 
  filter(
    !file_name %in% (fetch_inference_error() %>% unlist(FALSE, FALSE)) & 
      !file_name %in% (fetch_exclude_flags() %>% unlist(FALSE, FALSE))
  )


# Read in taxon info
d_taxon <- read_csv("raw_data/MPG-Taxa_20230503.csv") %>% 
  select(MONA, Superfamily, Family, Subfamily, Tribe, Subtribe, Genus, Species) %>% 
  rename_at(vars(-MONA), tolower) %>% 
  mutate_at(vars(-MONA), clean_taxon_name) %>% 
  mutate(
    sp = species,
    species = paste(genus, species, sep = " ")
  )


# Pull partial data for ones with assigned tag ID
d <- rbind.fill(import_sheets("https://docs.google.com/spreadsheets/d/1-3_FM7t40Iv10BM3XLxWX0Qs__VFkVIP5Deqg0CcmUg/edit?gid=0#gid=0", "data"), 
                import_sheets("https://docs.google.com/spreadsheets/d/1i8C-greXGMThg-0AqQRqaPMHT3KTXxR7x-QTDj5qGbE/edit?gid=0#gid=0", "data"))

d <- d %>% 
  dplyr::select(tag_id, MONA, location, date, collection_method, photo, ear_mites,light_type,notes, 
                family, genus, species) %>%
  mutate(
    date = as.POSIXct(date, format = "%d-%B-%Y"),
    month = lubridate::month(date),
    year = lubridate::year(date),
    doy = lubridate::yday(date)
  ) %>% 
  rename(sp = species) %>% 
  as_tibble()

# Counted not spread
d_count <- import_sheets("https://docs.google.com/spreadsheets/d/1Y-0oaVZ2WBF3FntlgCh6pzlGc8neRGQUQIQnyeXEgd8/edit?gid=0#gid=0", sheet = "Sheet1")

d_count <- d_count %>% 
  mutate(
    date = as.POSIXct(date, format = "%d-%B-%Y"),
    number = as.numeric(number),
  ) %>% 
  mutate(
    species = clean_taxon_name(species),
    genus = gsub(" .*", "", species),
    sp = gsub(".* ", "", species)
  ) %>% 
  dplyr::select(
    -c(collector, species)
  )
  
# combine the spread and counted data sheets by uncounting the counted ones
d <- d_count %>% 
  filter(!is.na(number)) %>% 
  left_join(d_taxon %>% 
              dplyr::select(MONA, genus, sp), by = c("genus", "sp")) %>% 
  uncount(number) %>% 
  rbind.fill(d) %>% 
  as_tibble() %>% 
  rename_at(vars(family, genus, sp), function(x){paste0(x,"_guess")}) %>% 
  left_join(
    d_taxon, by = "MONA"
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

# No longer needed
rm("d_count")

#Append species level attributes
d_host <- read_csv("raw_data/HOST_download.csv") %>% 
  rename_all(function(x){tolower(gsub(" ", "_", x))}) %>%
  mutate_at(vars(
    contains("insect_"), contains("hostplant_")
  ), function(x){
    x <- gsub("\\(.*| ", "", x)
    clean_taxon_name(x)
  }) %>% 
  mutate(
    sp = insect_species,
    genus = insect_genus,
    sp = ifelse(is.na(sp), "sp", sp),
    hostplant_species = ifelse(is.na(hostplant_species), "sp", hostplant_species),
    species = paste(genus,sp, sep = " ")
  ) %>% 
  rename_at(vars(contains("hostplant")), function(x){gsub("hostplant","plant",x)}) %>% 
  rename(
    plant_sp = plant_species
  ) %>% 
  dplyr::select(
    plant_family, plant_genus, plant_sp, damage, species
  ) %>% 
  group_by(species) %>% 
  summarise(
    db_family = unique_len(na.omit(plant_family)),
    db_genus = unique_len(na.omit(plant_genus)),
    db_sp = unique_len(na.omit(plant_sp))
  ) %>% 
  mutate(
    db_genus = ifelse(db_genus == 0, NA, db_genus),
    db_sp = ifelse(db_sp == 0, NA, db_sp)
  )


# Compute species level traits
d_trait <- d %>% 
  group_by(species) %>% 
  summarise(number = n()) %>% 
  mutate(
    no_id = grepl(" sp", species)
  ) %>% 
  left_join(d_host, by = "species")

# No longer needed
rm("d_host")

# Compute image record level meta-data
d_ref <- parsed_full %>% 
  dplyr::select(tag_id_guess, id, file_name, tick_size) %>% 
  mutate(
    image_id = paste0("img", id),
    tag_id = tag_id_guess
  ) %>% 
  mothr::update_tag_id() %>%
  mutate(
    tag_id = split_tag_id(tag_id)
  ) %>% 
  left_join(
    d %>% 
      mutate(
        tag_id = reformat_tag_id(tag_id)
      ) %>% 
      filter(!is.na(tag_id)),
    by = "tag_id"
  ) %>% 
  left_join(
    d_trait, by = "species"
  )

d_ref$wl <- wing_length_calc(parsed_full)
d_ref <- bind_cols(d_ref, thing_area_calc(parsed_full))



tree <- readRDS("cleaned_data/lep_mega_tree.rds")
phylo <- get_tree2(d_ref %>% 
                    dplyr::select(species, genus, family) %>% 
                    distinct(), 
                  tree$tree)
rm("tree")

