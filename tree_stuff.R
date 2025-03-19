vmisc::load_all2("mothr")
tre <- ape::read.tree("raw_data/Kawahara_lep_phylo.tre")

d <- read_csv("raw_data/Kawahara_lep_phylo_info.csv")

tre$tip.label
tre <- ape::root(tre, 
               outgroup = "Diptera_Drosophilidae_Drosophilinae_Drosophila_melanogaster", 
               resolve.root = TRUE)

tre$tip.label <- tre$tip.label %>% 
  match(d$tip_lab) %>% 
  d$species[.]
tre$tip.label <- gsub(" ", "_", tre$tip.label)
d$species <- gsub(" ", "_", d$species)

tre2 <- rtrees::add_root_info(
  tre, 
  classification = d %>% 
    select(genus, family) %>% 
    as.data.frame()
)

# Remove duplicates otherwise there would be an error.
tre2$genus_family_root <- distinct(tre2$genus_family_root)

tre2$genus_family_root %>% View()

read_csv("raw_data/MPG-Taxa_20230503.csv") %>% 
  select(Genus, Species, Family) %>% 
  mutate(
    species = paste(Genus, Species, sep = "_")
  ) %>% 
  transmute(
    species = species,
    genus = Genus,
    family = Family
  ) %>% 
  slice_sample(n = 300) -> w

w$species


read_csv("raw_data/MPG-Taxa_20230503.csv")$Superfamily %>% unique()

d2 <- read_csv("raw_data/MPG-Taxa_20230503.csv") %>% 
  transmute(
    order = "Lepidoptera",
    superfamily = Superfamily,
    family = Family,
    genus = Genus,
    sp = Species,
    species = paste(Genus, Species, sep = "_")
  )


tre3 <- append_family_nodes(tre2, d, d2)

tre4 <- w %>% 
  rtrees::get_tree(., 
                   tree = tre3, 
                   taxon = NULL, 
                   scenario = "ran")



phytools::vcvPhylo(tre4, FALSE) 
