vmisc::load_all2("mothr")

# Install 'rtrees' to prune tree. Cannot get from CRAN directly
# install.packages('rtrees', repos=c(
#   
#   rtrees='https://daijiang.r-universe.dev',
#   
#   CRAN='https://cloud.r-project.org'
#   
# ))

# Read in tree file
tre <- ape::read.tree("raw_data/Kawahara_lep_phylo.tre")


# Taxon infor for tree
d <- read_csv("raw_data/Kawahara_lep_phylo_info.csv")

# Set D. melanogaster as the outgroup
tre <- ape::root(tre, 
               outgroup = "Diptera_Drosophilidae_Drosophilinae_Drosophila_melanogaster", 
               resolve.root = TRUE)

# Some wrangling
tre$tip.label <- tre$tip.label %>% 
  match(d$tip_lab) %>% 
  d$species[.]
tre$tip.label <- gsub(" ", "_", tre$tip.label)
d$species <- gsub(" ", "_", d$species)

# Add root_info() per the instructions of `rtrees`
# Using custom function here so that tidytree and force the tree to be ultrametric, an assumption of ape::branch.times()
tre <- add_root_info2(
  tre, 
  classification = d %>% 
    dplyr::select(genus, family) %>% 
    as.data.frame()
)

# Remove duplicates otherwise there would be an error.
tre$genus_family_root <- distinct(tre$genus_family_root)

# Set as ultrametric
tre <- tidytree::as.ultrametric(tre)


# Get taxon classification for species in MONA database
d_MONA <- read_csv("raw_data/MPG-Taxa_20230503.csv") %>% 
  transmute(
    order = "Lepidoptera",
    superfamily = clean_taxon_name(Superfamily),
    family = clean_taxon_name(Family),
    genus = clean_taxon_name(Genus),
    sp = Species,
    species = paste(Genus, Species, sep = "_")
  )

# Update the root info by adding families not found the in phylo tree based on taxon classifications above the family
# Bind the new taxa to the lowest classification basal node that matches
tre <- append_family_nodes(tre, # Tree
                           taxon_info = d, # Taxon classification of the species in the tree
                           new_taxon_info = d_MONA, # New species to append
                           taxon_order = c("order","superfamily","family","genus","sp"))

l <- list(
  "taxon_info" = d[,names(d) != "tip_lab"],
  "tree" = tre
)


# saveRDS(l, "cleaned_data/lep_mega_tree.rds")

tree <- readRDS("cleaned_data/lep_mega_tree.rds")



# Example 
# Some candidate species for pruning
read_csv("raw_data/MPG-Taxa_20230503.csv") %>% 
  select(Genus, Species, Family) %>% 
  mutate(
    species = paste(Genus, Species, sep = "_")
  ) %>% 
  transmute(
    species = species,
    genus = clean_taxon_name(Genus),
    family = clean_taxon_name(Family)
  ) %>% 
  slice_sample(n = 100) -> w


tre2 <- get_tree2(w, tree$tree)

plot(tre2, type = "fan")

