test_that("phylo_drop_short_branches works", {
	phy <- ape::compute.brlen(ape::stree(100, "right"))
	phy$edge.length <- phy$edge.length*.05
	phy2 <- phylo_drop_short_branches(phy, threshold = 0.01)
 	terminal_branches <- which(!(phy2$edge[,2] %in% phy2$edge[,1]))
	expect_true(all(phy2$edge.length[terminal_branches] >= 0.01))
}
)