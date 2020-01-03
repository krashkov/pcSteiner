initialize_findA <- function (depth, size) {

        assign(values,      list(), envir = findA)
        assign(values[[1]], -Inf,   envir = findA)

        for (i in 2:depth) {
                assign(values[[i]], rep(-Inf, size),   envir = findA)
        }

}
