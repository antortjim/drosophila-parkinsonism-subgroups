library(jsonlite)

### Jaccard similarity (IoU) with tree a and subgroups defined by sleep
jaccard <- function(sleepgroup, GIgroup) {
  intersection <- length(intersect(sleepgroup, GIgroup))
  union <- length(sleepgroup) + length(GIgroup) - intersection
  return (intersection/union)
}

compute_jaccard_matrix <- function(sleep_groups, gi_groups) {
  
  jac_matrix <- matrix(rep(NA, 9), nrow=3, ncol=3)
  
  jac_matrix[1,1]<-jaccard(sleep_groups$behA, gi_groups$GI1)
  jac_matrix[1,2]<-jaccard(sleep_groups$behB, gi_groups$GI1)
  jac_matrix[1,3]<-jaccard(sleep_groups$behC, gi_groups$GI1)
  
  jac_matrix[2,1]<-jaccard(sleep_groups$behA, gi_groups$GI2)
  jac_matrix[2,2]<-jaccard(sleep_groups$behB, gi_groups$GI2)
  jac_matrix[2,3]<-jaccard(sleep_groups$behC, gi_groups$GI2)
  
  
  jac_matrix[3,1]<-jaccard(sleep_groups$behA, gi_groups$GI3)
  jac_matrix[3,2]<-jaccard(sleep_groups$behB, gi_groups$GI3)
  jac_matrix[3,3]<-jaccard(sleep_groups$behC, gi_groups$GI3)
  
  rownames(jac_matrix) <- names(gi_groups)
  colnames(jac_matrix) <- names(sleep_groups)
  
  return(round(t(jac_matrix), 2))
}


shuffle_groups <- function(genes, sizes, shuffle_sizes) {
  
  selected <- rep(x = FALSE, length=length(genes))
  # in the beginning, all genes are remaining in the pool
  remaining <- rep(x = TRUE, length=length(genes))
  gi_groups <- list()
  
  remaining_ids <- seq(1, length(sizes))
  
  for (i in 1:length(sizes)) {
    
    if (shuffle_sizes) {
      id <- unlist(sample(x = as.list(remaining_ids), size=1, replace=FALSE))
      size <- sizes[id]
      remaining_ids <- remaining_ids[!(remaining_ids==id)]
    } else {
      size <- sizes[i]
    }
    
    selected <- sample(x = which(remaining), size=size, replace=FALSE)
    remaining[selected]<-FALSE
    group_name <- paste0("GI", i)
    gi_groups[[group_name]] <- unname(genes[selected])   
  }
  stopifnot(sum(remaining)==0)
  return(gi_groups)
}

main <- function() {
  groups <- jsonlite::read_json(path = "database.json")
  target <- compute_jaccard_matrix(groups$sleep, groups$gi)
  
  genes <- unlist(groups$sleep)
  sleep_sizes <- sapply(groups$sleep, length)
  gi_sizes <- sapply(groups$gi, length)
  
  
  n_simulations<-10000
  
  # make n_simulations copies of the table with the real non shuffled data
  target_table <- simplify2array(lapply(1:n_simulations, function(i) {target}))
  
  # Shuffle protocol 1
  # randomize order of group sizes
  # this means the three shuffled groups have always the 3 same sizes but in a random order
  out <- lapply(1:n_simulations, function(i) {
    groups[["gi"]] <- shuffle_groups(genes, gi_sizes, TRUE)
    compute_jaccard_matrix(groups$sleep, groups$gi)
  })
  simulation_table <- simplify2array(out)
  
  # p-val = frequency with which the jaccard index for a given pair of clusters
  # is higher in the shuffled groups than in the target (real) groups
  # to compute that
  # 1. generate a boolean array of shape n_groups_sleep x n_groups_gi x n_simulations
  # with true in the i,j,k cell if the jaccard of the i,j pair of the kth simulation
  # is higher than the jaccard of the i,j pair of the real data
  # 2. count the fraction of true out of the total (mean) along the simulation axis
  # i.e. keeping the 1 and 2 axes
  p_values <- apply(simulation_table > target_table, c(1, 2), mean)
  print(p_values)
  
  # GI1    GI2    GI3
  # behA 0.2358 0.0290 0.9862
  # behB 0.9283 0.5528 0.0111
  # behC 0.0457 0.9186 0.5412
  
  
  # Shuffle protocol 2
  # don't randomize order of group sizes
  # this means the three shuffled groups have always the 3 same sizes in the same order
  out <- lapply(1:n_simulations, function(i) {
    groups[["gi"]] <- shuffle_groups(genes, gi_sizes, FALSE)
    compute_jaccard_matrix(groups$sleep, groups$gi)
  })
  simulation_table <- simplify2array(out)
  p_values <- apply(simulation_table > target_table, c(1, 2), mean)
  print(p_values)
  
  
  # GI1    GI2    GI3
  # behA 0.2574 0.0137 0.9637
  # behB 0.9072 0.4271 0.0025
  # behC 0.0273 0.8539 0.3015
}

main()