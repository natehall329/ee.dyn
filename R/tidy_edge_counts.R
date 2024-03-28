
tidy_edge_counts <- function(spaths, lagged = FALSE){

  emos <- as.character(unique(spaths$lhs))
  possible_edges <- arrangements::permutations(emos, 2) |> data.frame()
  colnames(possible_edges) <- c("from", "to")

  ccols <- colnames(spaths)[which(grepl("count", colnames(spaths)))]
  count_cols <- data.frame(matrix(ncol = length(ccols), nrow = nrow(possible_edges), 0))
  names(count_cols) <- ccols

  possible_edges <- cbind(possible_edges, count_cols)
  possible_edges$from <- as.character(possible_edges$from); possible_edges$to <- as.character(possible_edges$to)

  if(!lagged){
    # contemporaneous paths
    est_paths <- spaths |> filter(!grepl("lag", rhs) & op == "~")
  } else{
    # cross-lagged paths
    est_paths <- spaths |> filter(grepl("lag", rhs) & op == "~") |> filter(lhs != sub("lag", "", rhs)) |> mutate(rhs = sub("lag", "", rhs))# additionally need to filter AR1 and remove lag from rhs
  }

  for(i in 1:nrow(possible_edges)){
    fr <- possible_edges[i,"from"]; to <- possible_edges[i,"to"]
    for(j in ccols){
      nedges <- est_paths[which(est_paths$rhs == fr & est_paths$lhs == to),j]

      if(length(nedges) == 0) nedges <- 0
      possible_edges[which(possible_edges$from == fr & possible_edges$to == to),j] <- nedges
    }
  }
  return(possible_edges)
}










