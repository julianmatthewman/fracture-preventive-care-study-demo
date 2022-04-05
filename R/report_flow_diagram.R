#' Create a simple flow diagram
#'
#' @param x A dataframe containing counts at different steps of the data management process.
#' @param node The name of the column containing the node labels
#' @param edge The name of the columm containing the edge labels; the first edge label will be dropped
#'
#' @return A diagram
report_flow_diagram <- function(x, node, edge) {

	nodes <- create_node_df(n=nrow(x),
													type = "a",
													label=x[[node]],
													shape="rectangle",
													fixedsize=FALSE,
													style = "filled")
	
	edges<-create_edge_df(from = head(nodes$id, nrow(nodes)-1),
												to= tail(nodes$id, nrow(nodes)-1),
												label = tail(x[[edge]], nrow(nodes)-1),
												color= "black",
												constraint = (rep('true', nrow(nodes)-1)))
	
	
	graph<-create_graph(nodes, edges)
	render_graph(graph, layout = "tree")
}