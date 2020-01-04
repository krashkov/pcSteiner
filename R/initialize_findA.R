initialize_findA <- function (depth, size) {

        findA$values      <- list()
        findA$values[[1]] <- rep(-Inf, size)
        for (i in 2:depth) {
                findA$values[[i]] <<- rep(-Inf, size)
        }

}
