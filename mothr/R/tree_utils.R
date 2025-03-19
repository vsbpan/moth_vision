find_lowest_taxon <- function(x, y){
  for(i in seq_along(y)){
    if(isTRUE(any(y[,i] == x[i]))){
      next
    } else {
      res <- i - 1
      break
    }
  }
  if((res == 0) &&  !isTRUE(any(y[,1] == x[1]))){
    stop("Highest Taxonomic level not matched!")
  }
  return(res)
}


inode_root_dist <- function(tree){
  x <- ape::dist.nodes(tree)[,1]
  x <- x[seq_len(ape::Nnode(tree))]
  names(x) <- tree$node.label
  x
}

# Expects a sorted taxon info from order to species 
append_family_nodes <- function(tree, taxon_info, new_taxon_info, taxon_order){
  expected_vars <- c(taxon_order, "species")
  assert_variable_in_df(taxon_info, variable = expected_vars)
  assert_variable_in_df(new_taxon_info, variable = expected_vars)
  taxon_info <- dplyr::select(taxon_info, dplyr::all_of(expected_vars))
  new_taxon_info <- dplyr::select(new_taxon_info, dplyr::all_of(expected_vars))
  
  stopifnot(
    identical(names(taxon_info),names(new_taxon_info))
  )
  new_taxon_info <- new_taxon_info[!new_taxon_info$family %in% taxon_info$family,]
  low_index <- apply(new_taxon_info, 1, function(x){
    find_lowest_taxon(x, taxon_info)
  })
  # Find the root node of the closest family
  res <- lapply(seq_along(low_index), function(i){
    taxon_i <- new_taxon_info[i,low_index[i], drop = TRUE]
    sps <- taxon_info[which(taxon_info[,low_index[i], drop = TRUE] == taxon_i), "species", drop = TRUE]
    a <- tree$genus_family_root %>% 
      dplyr::filter(
        (only_sp %in% sps) & is.na(genus)
      )
    if(nrow(a) > 1){
      a <- a[which.min(inode_root_dist(tree)[a$basal_node]),]
    }
    a$family <- new_taxon_info[i,"family", drop = TRUE]
    a$family_graft <- names(new_taxon_info)[low_index[i]]
    return(a)
  }) %>% 
    dplyr::bind_rows()
  tree$genus_family_root$family_graft <- "native"
  tree$genus_family_root <- dplyr::bind_rows(
    tree$genus_family_root,
    res
  ) %>% 
    dplyr::distinct()
  
  tree
}


get_tree2 <- function(sp_list, tree, show_grafted = FALSE, 
                      .progress = "text", dt = TRUE){
  taxon_lab <- c("family","genus","species")
  assert_variable_in_df(sp_list, taxon_lab)
  tre <- get_one_tree2(sp_list, 
                          tree = tree, 
                          show_grafted = show_grafted,
                          tree_by_user = FALSE,
                          dt = dt,
                          .progress = .progress)
  
  tre$graft_status <- tre$graft_status %>% 
    dplyr::left_join(sp_list, by = "species") %>% 
    dplyr::left_join(
      tree$genus_family_root %>% 
        dplyr::select(family, family_graft) %>% 
        dplyr::filter(family_graft != "native"),
      by = "family"
    ) %>% 
    dplyr::mutate(
      status = ifelse(!is.na(family_graft),
                      sprintf("grafted at %s level", family_graft)
                      ,status)
    ) %>% 
    dplyr::select(
      dplyr::all_of(c(taxon_lab,"tip_label","status"))
    )
  tidytree::as.ultrametric(tre)
}

# Using custom function here so that tidytree and force the tree to be ultrametric, an assumption of ape::branch.times()
get_one_tree2 <- function(sp_list, tree, show_grafted = FALSE, tree_by_user = FALSE, 
                          .progress = "text", dt = TRUE){
  foo <- function (sp_list, tree, taxon = NULL, scenario = "at_basal_node", show_grafted = FALSE, tree_by_user = FALSE, 
                   .progress = "text", dt = TRUE) 
  {
    if (tree_by_user & all(!grepl("_", tree$tip.label))) 
      stop("Please change the tree's tip labels to be the format of genus_sp.")
    if (tree_by_user) 
      tree = rm_stars(tree)
    tree_genus = unique(gsub("^([-A-Za-z]*)_.*$", "\\1", tree$tip.label))
    sp_list = sp_list_df(unique(sp_list))
    all_genus_in_tree = all(unique(sp_list$genus) %fin% tree_genus)
    if (!all_genus_in_tree) {
      if ((!"family" %fin% names(sp_list))) {
        if (missing(taxon) | is.null(taxon)) 
          stop("Please specify `taxon` as not all genus are in the tree.")
        sp_list = sp_list_df(sp_list$species, taxon)
      }
    }
    if (!is.null(taxon)) {
      if (!taxon %fin% rtrees::taxa_supported & !all_genus_in_tree) {
        new_cls = unique(dplyr::select(sp_list, genus, family))
        new_cls$taxon = taxon
        classifications <- dplyr::bind_rows(rtrees::classifications, 
                                            new_cls)
      }
    }
    sp_out_tree = sp_list[!sp_list$species %fin% tree$tip.label, 
    ]
    subsp_in_tree = grep("^.*_.*_.*$", x = tree$tip.label, value = T)
    if (length(subsp_in_tree)) {
      sp_out_tree = dplyr::mutate(sp_out_tree, re_matched = NA, 
                                  matched_name = NA)
      for (i in 1:length(sp_out_tree$species)) {
        name_in_tree = grep(paste0("^", sp_out_tree$species[i], 
                                   "_"), x = subsp_in_tree, ignore.case = T, value = T)
        if (length(name_in_tree)) {
          sp_out_tree$re_matched[i] = TRUE
          sp_out_tree$matched_name[i] = sample(name_in_tree, 
                                               1)
          tree$tip.label[tree$tip.label == sp_out_tree$matched_name[i]] = sp_out_tree$species[i]
          if (!is.null(tree$genus_family_root)) {
            tree$genus_family_root$only_sp[tree$genus_family_root$only_sp == 
                                             sp_out_tree$matched_name[i]] = sp_out_tree$species[i]
          }
        }
      }
      sp_out_tree = dplyr::distinct(sp_list[!sp_list$species %fin% 
                                              tree$tip.label, ])
    }
    close_sp_specified = close_genus_specified = FALSE
    if ("close_sp" %fin% names(sp_out_tree)) {
      close_sp_specified = TRUE
      sp_out_tree$close_sp = cap_first_letter(gsub(" +", "_", 
                                                   sp_out_tree$close_sp))
    }
    if ("close_genus" %fin% names(sp_out_tree)) 
      close_genus_specified = TRUE
    if (nrow(sp_out_tree) == 0) {
      message("Wow, all species are already in the mega-tree!")
      tree_sub = ape::drop.tip(tree, setdiff(tree$tip.label, 
                                             sp_list$species))
      return(tree_sub)
    }
    if (tree_by_user) {
      if (!is.null(tree$genus_family_root)) 
        warning("The phylogeny has basal node information, are you sure this is an user provided tree?")
      if (all_genus_in_tree) {
        tree = add_root_info(tree, process_all_tips = FALSE, 
                             genus_list = unique(sp_out_tree$genus), show_warning = FALSE)
      }
      else {
        if (missing(taxon)) 
          stop("Please specify `taxon`.")
        message("Not all genus can be found in the phylogeny.")
        if (is.null(tree$genus_family_root)) {
          warning("For user provided phylogeny, without a classification for all genus of species in the phylogeny,\n              it is unlikely to find the most recent ancestor for genus and family; here we proceed the phylogeny\n              by adding root information for genus and family that can be found in the phylogeny or species list but\n              we recommend to prepare the phylogeny using `add_root_info()` with a classification\n              data frame with all tips first.", 
                  call. = FALSE, immediate. = TRUE)
        }
        genus_not_in_tree = dplyr::filter(sp_out_tree, !genus %fin% 
                                            tree_genus)
        tree = add_root_info(tree, classification = if (is.null(taxon) & 
                                                        inherits(sp_list, "data.frame") & all(c("genus", 
                                                                                                "family") %fin% names(sp_list))) {
          unique(sp_list[, c("genus", "family")])
        }
        else {
          if ((!taxon %fin% rtrees::taxa_supported) & !all_genus_in_tree) {
            unique(classifications[classifications$taxon == 
                                     taxon, ])
          }
          else {
            unique(rtrees::classifications[rtrees::classifications$taxon == 
                                             taxon, ])
          }
        }, process_all_tips = FALSE, genus_list = if (length(setdiff(sp_out_tree$genus, 
                                                                     genus_not_in_tree$genus))) 
          setdiff(sp_out_tree$genus, genus_not_in_tree$genus)
        else NULL, family_list = if (nrow(genus_not_in_tree) > 
                                     0) 
          unique(genus_not_in_tree$family)
        else NULL, show_warning = FALSE)
      }
    }
    scenario = match.arg(scenario)
    if (is.null(tree$genus_family_root)) 
      stop("Did you use your own phylogeny? If so, please set `tree_by_user = TRUE`.")
    sp_out_tree$status = ""
    tree_df = tidytree::as_tibble(tree)
    tree_df$is_tip = !(tree_df$node %fin% tree_df$parent)
    tree <- tidytree::as.ultrametric(tree)
    node_hts = ape::branching.times(tree)
    all_eligible_nodes = unique(c(tree$genus_family_root$basal_node, 
                                  tree$genus_family_root$root_node))
    n_spp_to_show_progress = 200
    if (nrow(sp_out_tree) > n_spp_to_show_progress) {
      progress <- create_progress_bar(.progress)
      progress$init(nrow(sp_out_tree))
      on.exit(progress$term())
    }
    for (i in 1:nrow(sp_out_tree)) {
      if (nrow(sp_out_tree) > n_spp_to_show_progress) 
        progress$step()
      where_loc_i = where_loc_i2 = NA
      if (close_sp_specified) {
        if (!is.na(sp_out_tree$close_sp[i]) & sp_out_tree$close_sp[i] %fin% 
            tree$tip.label) {
          where_loc_i = sp_out_tree$close_sp[i]
        }
      }
      if (close_genus_specified) {
        if (!is.na(sp_out_tree$close_genus[i]) & sp_out_tree$close_genus[i] != 
            "" & sp_out_tree$close_genus[i] %fin% tree_genus) {
          sp_out_tree$genus[i] = sp_out_tree$close_genus[i]
          where_loc_i2 = sp_out_tree$close_genus[i]
        }
        else {
          if (!is.na(sp_out_tree$close_genus[i])) 
            warning("The genus specified for ", sp_out_tree$species[i], 
                    " is not in the phylogeny.")
        }
      }
      if (!all_genus_in_tree & is.na(where_loc_i) & is.na(where_loc_i2)) {
        if (is.na(sp_out_tree$family[i]) | !sp_out_tree$family[i] %fin% 
            tree$genus_family_root$family) {
          sp_out_tree$status[i] = "No co-family species in the mega-tree"
          (next)()
        }
      }
      node_label_new = NULL
      add_above_node = FALSE
      fraction = 1/2
      if (sp_out_tree$genus[i] %fin% tree$genus_family_root$genus | 
          !is.na(where_loc_i2) | !is.na(where_loc_i)) {
        sp_out_tree$status[i] = "*"
        idx_row = which(tree$genus_family_root$genus == sp_out_tree$genus[i])
        root_sub = tree$genus_family_root[idx_row, ]
        if (root_sub$n_spp == 1 | !is.na(where_loc_i)) {
          if (!is.na(where_loc_i)) {
            where_loc = where_loc_i
            new_ht = tree_df$branch.length[tree_df$label == 
                                             where_loc_i] * (1 - fraction)
            node_hts = c(new_ht, node_hts)
            node_label_new = paste0("N", length(node_hts))
            names(node_hts)[1] = node_label_new
            all_eligible_nodes = c(all_eligible_nodes, 
                                   node_label_new)
            add_above_node = TRUE
            if (!sp_out_tree$genus[i] %fin% tree$genus_family_root$genus) {
              tree$genus_family_root = tibble::add_row(tree$genus_family_root, 
                                                       family = sp_out_tree$family[i], genus = sp_out_tree$genus[i], 
                                                       basal_node = node_label_new, basal_time = new_ht, 
                                                       root_node = tree_df$label[tree_df$node == 
                                                                                   tree_df$parent[tree_df$label == where_loc_i]], 
                                                       root_time = tree_df$branch.length[tree_df$node == 
                                                                                           tree_df$parent[tree_df$label == where_loc_i]], 
                                                       n_genus = 1, n_spp = 1, only_sp = sp_out_tree$species[i])
            }
          }
          else {
            where_loc = root_sub$only_sp
            new_ht = root_sub$basal_time * (1 - fraction)
            node_hts = c(new_ht, node_hts)
            node_label_new = paste0("N", length(node_hts))
            names(node_hts)[1] = node_label_new
            all_eligible_nodes = c(all_eligible_nodes, 
                                   node_label_new)
            add_above_node = TRUE
            tree$genus_family_root$only_sp[idx_row] = NA
            tree$genus_family_root$basal_node[idx_row] = node_label_new
            tree$genus_family_root$basal_time[idx_row] = unname(new_ht)
          }
        }
        else {
          where_loc = root_sub$basal_node
          if (scenario == "random_below_basal") {
            tree_df_sub = tidytree::offspring(tree_df, 
                                              where_loc)
            tree_df_sub = tree_df_sub[tree_df_sub$is_tip == 
                                        FALSE, ]
            if (nrow(tree_df_sub) > 0) {
              potential_locs = c(where_loc, tree_df_sub$label)
              bls = tree_df_sub$branch.length
              names(bls) = tree_df_sub$label
              bls = c(root_sub$root_time - root_sub$basal_time, 
                      bls)
              names(bls)[1] = root_sub$basal_node
              prob = bls/sum(bls)
              where_loc = sample(potential_locs, 1, prob = prob)
            }
          }
        }
      }
      else {
        sp_out_tree$status[i] = "**"
        idx_row = which(tree$genus_family_root$family == 
                          sp_out_tree$family[i] & is.na(tree$genus_family_root$genus))
        root_sub = tree$genus_family_root[idx_row, ]
        if (root_sub$n_spp == 1) {
          where_loc = root_sub$only_sp
          new_ht = root_sub$basal_time * (1 - fraction)
          node_hts = c(new_ht, node_hts)
          node_label_new = paste0("N", length(node_hts))
          names(node_hts)[1] = node_label_new
          all_eligible_nodes = c(all_eligible_nodes, node_label_new)
          add_above_node = TRUE
          tree$genus_family_root = tibble::add_row(tree$genus_family_root, 
                                                   family = sp_out_tree$family[i], genus = sp_out_tree$genus[i], 
                                                   basal_node = node_label_new, basal_time = unname(new_ht), 
                                                   root_node = node_label_new, root_time = unname(new_ht), 
                                                   n_genus = 1, n_spp = 1, only_sp = sp_out_tree$species[i])
        }
        else {
          where_loc = root_sub$basal_node
          if (scenario == "random_below_basal") {
            tree_df_sub = tidytree::offspring(tree_df, 
                                              where_loc)
            tree_df_sub = tree_df_sub[tree_df_sub$is_tip == 
                                        FALSE, ]
            if (nrow(tree_df_sub) > 0) {
              potential_locs = intersect(c(where_loc, tree_df_sub$label), 
                                         all_eligible_nodes)
              locs_bl = tree_df_sub[tree_df_sub$label %fin% 
                                      potential_locs, ]
              bls = locs_bl$branch.length
              names(bls) = locs_bl$label
              bls = c(root_sub$root_time - root_sub$basal_time, 
                      bls)
              names(bls)[1] = root_sub$basal_node
              prob = bls/sum(bls)
              where_loc = sample(potential_locs, 1, prob = prob)
            }
          }
        }
        tree$genus_family_root$n_genus[idx_row] = tree$genus_family_root$n_genus[idx_row] + 
          1
      }
      if (root_sub$n_spp > 3) 
        use_castor = TRUE
      else use_castor = FALSE
      if (dt) {
        tree_df = bind_tip(tree_tbl = tree_df, node_heights = node_hts, 
                           where = where_loc, new_node_above = add_above_node, 
                           tip_label = sp_out_tree$species[i], frac = fraction, 
                           return_tree = FALSE, node_label = node_label_new, 
                           use_castor = use_castor)
      }
      else {
        tree_df = bind_tip_df(tree_tbl = tree_df, node_heights = node_hts, 
                              where = where_loc, new_node_above = add_above_node, 
                              tip_label = sp_out_tree$species[i], frac = fraction, 
                              return_tree = FALSE, node_label = node_label_new, 
                              use_castor = use_castor)
      }
      tree$genus_family_root$n_spp[idx_row] = tree$genus_family_root$n_spp[idx_row] + 
        1
    }
    tree_df = dplyr::arrange(tree_df, node)
    if (any(sp_out_tree$status == "*")) {
      message("\n", sum(sp_out_tree$status == "*"), " species added at genus level (*) \n")
    }
    if (any(sp_out_tree$status == "**")) {
      message(sum(sp_out_tree$status == "**"), " species added at family level (**) \n")
    }
    tree_sub = castor::get_subtree_with_tips(tidytree::as.phylo(tree_df), 
                                             sp_list$species)$subtree
    grafted = sp_out_tree[sp_out_tree$status %fin% c("*", "**"), 
    ]
    grafted$sp2 = paste0(grafted$species, grafted$status)
    wid = which(tree_sub$tip.label %fin% grafted$species)
    tree_sub$tip.label[wid] = dplyr::left_join(tibble::tibble(species = tree_sub$tip.label[wid]), 
                                               grafted, by = "species")$sp2
    graft_status = tibble::tibble(tip_label = tree_sub$tip.label)
    graft_status$species = gsub("\\*", "", graft_status$tip_label)
    graft_status$status = ifelse(grepl("\\*{2}$", graft_status$tip_label), 
                                 "grafted at family level", ifelse(grepl("[^*]\\*{1}$", 
                                                                         graft_status$tip_label), "grafted at genus level", 
                                                                   "exisiting species in the megatree"))
    if (any(sp_out_tree$status == "No co-family species in the mega-tree")) {
      sp_no_family = sp_out_tree$species[sp_out_tree$status == 
                                           "No co-family species in the mega-tree"]
      message(length(sp_no_family), " species have no co-family species in the mega-tree, skipped\n(if you know their family, prepare and edit species list with `rtrees::sp_list_df()` may help): \n", 
              paste(sp_no_family, collapse = ", "))
      graft_status = dplyr::bind_rows(graft_status, data.frame(species = sp_no_family, 
                                                               status = rep("skipped as no co-family in the megatree", 
                                                                            length(sp_no_family))))
    }
    if (!show_grafted) {
      tree_sub = rm_stars(tree_sub)
      graft_status$tip_label = gsub("\\*", "", graft_status$tip_label)
    }
    tree_sub$graft_status = graft_status
    tree_sub = ape::ladderize(tree_sub)
    return(tree_sub)
  }
  environment(foo) <- asNamespace("rtrees")

  foo(sp_list, tree, show_grafted = show_grafted, tree_by_user = tree_by_user, 
      .progress = .progress, dt = dt)
}

