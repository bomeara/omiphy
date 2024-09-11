#' Function to drop very short terminal branches
#' 
#' This will drop the shortest terminal branch, collapse any nodes that only have one descendant, and repeat. The goal is to get rid of terminal polytomies. 
#' 
#' @param phy A phylo object
#' @param threshold The maximum length of the branch to drop
phylo_drop_short_branches <- function(phy, threshold = 0.01) {
 terminal_branches <- which(!(phy$edge[,2] %in% phy$edge[,1]))
 min_length <- min(phy$edge.length[terminal_branches])
 if(max(phytools::nodeHeights(phy)) < threshold) {
   stop("Tree height overall is less than the threshold")
 }
 while(min_length < threshold) {
	  # Find the shortest terminal branch
	  edge_and_brlen <- cbind(phy$edge[terminal_branches,], phy$edge.length[terminal_branches])
	  min_edge <- which.min(edge_and_brlen[,3])
	  phy <- ape::drop.tip(phy, edge_and_brlen[min_edge,2], trim.internal=TRUE, collapse.singles=TRUE)
	   terminal_branches <- which(!(phy$edge[,2] %in% phy$edge[,1]))
 	min_length <- min(phy$edge.length[terminal_branches])
 }
  return(phy)
}