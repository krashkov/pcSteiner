check_input <- function (graph, terminals, lambda, root, depth, eps, max_iter) {

        # Check graph
        if (! is.igraph(graph))
                stop("Error: Graph is not an igraph graph")

        if (length(V(graph)) == 0 )
                stop("Error: Graph does not contain vertices")

        if (! is.connected(graph))
                stop("Error: The graph is not connected. Run pcs.forest instead")

        if (is.null(V(graph)$prizes))
                stop("Error: Vertices do not have prizes")

        if (is.null(E(graph)$costs))
                stop("Error: Edges do not have costs")


        # Check terminals
        if (is.null(terminals) || is.na(terminals) || length(terminals) == 0)
                stop("Error: Terminals not found")


        # Check lambda
        if (! class(lambda) == "numeric" & ! class(lambda) == "integer") {
                stop("Error: lambda is not a number")
                if (lambda <= 0)
                        stop("Error: lambda <= 0")
        }


        # Check root
        if (is.null(root) || is.na(root) || length(root) != 1)
                stop("Error: Root not found")


        # Check names of vertices
        if (is.null(V(graph)$name)) {
                # creating name attribute
                V(graph)$name <- as.character(1:length(V(graph)))
        } else {
                # creating new name and realname attributes
                V(graph)$realname <- V(graph)$name
                V(graph)$name     <- as.character(1:length(V(graph)))
        }


        # Mathcing names of vertices and terminals if possible
        if (class(terminals) == "character") {
                # terminals contain realname of vertices
                if (sum(terminals %in% V(graph)$realname) != length(terminals)) {
                        stop("Error: Vertices names do not contain terminal names")
                } else {
                        # Convert realnames of terminals to integer id's (character id's)
                        terminal_id   <- match(terminals, V(graph)$realname)
                        terminal_name <- V(graph)$name[terminal_id]
                }
        } else if (class(terminals) == "numeric" | class(terminals) == "integer") {
                # terminals contains id's of vertices
                terminal_id   <- terminals
                terminal_name <- V(graph)$name[terminal_id]
        } else
                stop("Error: Invalid type of terminals")


        # Mathcing names of vertices and root if possible
        if (class(root) == "character") {
                if (sum(root %in% V(graph)$realname) != 1) {
                        stop("Error: Vertices names do not contain root name")
                } else {
                        root_id   <- match(root, V(graph)$realname)
                        root_name <- V(graph)$name[root_id]
                }
        } else if (class(terminals) == "numeric" | class(terminals) == "integer") {
                root_id   <- root
                root_name <- V(graph)$name[root_id]
        } else
                stop("Error: Invalid type of root")


        # Check depth
        if (! class(depth) == "numeric" & ! class(depth) == "integer") {
                stop("Error: depth is not a number")
                if (depth <= 0)
                        stop("Error: depth <= 0")
        }


        # Check eps
        if (! class(eps) == "numeric" & ! class(eps) == "integer") {
                stop("Error: eps is not a number")
                if (eps <= 0)
                        stop("Error: eps <= 0")
        }


        # Check max_iter
        if (! class(max_iter) == "numeric" & ! class(max_iter) == "integer") {
                stop("Error: max_iter is not a number")
                if (max_iter <= 0)
                        stop("Error: max_iter <= 0")
        }


        checkInput_res <- list()
        checkInput_res$vert_names <- V(graph)$name

        checkInput_res$terminal_id   <- terminal_id
        checkInput_res$terminal_name <- terminal_name

        checkInput_res$root_id   <- root_id
        checkInput_res$root_name <- root_name

        return(checkInput_res)
}
