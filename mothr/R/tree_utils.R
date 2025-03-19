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
append_family_nodes <- function(tree, taxon_info, new_taxon_info){
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
      filter(
        (only_sp %in% sps) & is.na(genus)
      )
    if(nrow(a) > 1){
      a <- a[which.min(inode_root_dist(tree)[a$basal_node]),]
    }
    a$family <- new_taxon_info[i,"family", drop = TRUE]
    a
  }) %>% 
    dplyr::bind_rows()
  tree$genus_family_root <- dplyr::bind_rows(
    tree$genus_family_root,
    res
  ) %>% 
    dplyr::distinct()
  tree
}
