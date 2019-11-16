pcs.tree <- function(graph, terminals) {

        # Check graph

        if (is.null(graph))
                stop("Error: Graph is Null")

        if (length(V(graph)) == 0 )
                stop("Error: Graph does not contain vertices")

        if (is.connected(graph))
                stop("Error: The graph is not connected. Run pcs.forest instead")

        if (is.null(V(graph)$prizes))
                stop("Error: Vertices do not have prizes")

        if (is.null(E(graph)$costs))
                stop("Error: Edges do not have costs")

        # Check terminals

        if (is.null(terminals) || is.na(terminals) || length(terminals) == 0)
                stop("Error: Terminals not found")

        return(0)
}
