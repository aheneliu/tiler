#' Initialise a tiling
#'
#' @param dims Dimensions of the data matrix
#' @param columnwise_tiles Boolean indicating if the tiling should be initialised with columnwise tiles or not. Default is FALSE.
#'
#' @return A tiling, permuting using which leaves everything intact.
#'
#' @export
init_tiling <- function(dims, columnwise_tiles = FALSE) {
    tiles <- init_tiles(dims, columnwise_tiles = columnwise_tiles)
    index_row <- tapply(tiles[, "index"], tiles[, "rowindex"], function(x) x)

    options_list <- list("columnwise_tiles" = columnwise_tiles)

    list("dims" = dims,
         "tiles" = tiles,
         "tile_list_user" = list(),   ## user-defined tiles
         "tile_list_base" = list(),   ## base tiles (e.g., columnwise tiles)
         "tile_list_ignore" = list(), ## parts of the data matrix to ignore in the permutations, by setting these elements to NA
         "tile_list_focus" = list(),  ## second set of user-defined tiles
         "index_row" = index_row,
         "options" = options_list
         )
}


#' Initialise tiles in a tiling
#'
#' @param dims Dimensions of the data matrix
#' @param columnwise_tiles Boolean indicating if the tiling should be initialised with columnwise tiles or not. Default is FALSE.
#'
#' @return A matrix used to describe tiles
#'
#' @export
init_tiles <- function(dims, columnwise_tiles = FALSE) {
    N <- prod(dims)
    tiles <- matrix(c(seq.int(N),
                      rep(seq.int(dims[1]), dims[2]),
                      c(sapply(seq.int(dims[2]), function(i) rep(i, dims[1]))),
                      rep(NA, N)
                      ),

                    nrow = N, ncol = 4, dimnames = list(NULL, c("index", "rowindex", "columnindex", "permutation")))

    if (columnwise_tiles)
        tiles <- add_columnwise_tiles(tiles, dims)

    tiles
}


#' Create a tile
#'
#' @param rowset Set of rows in the tile
#' @param colset Set of columns in the tile
#' @param dims The dimensions of the data matrix
#'
#' @return A tiling
#'
#' @export
create_tile <- function(rowset, colset, dims) {
    out <- sub2ind_set(rowset, colset, dims)
    attr(out, "colset")  <- colset
    attr(out, "rowset")  <- rowset
    attr(out, "nrow")    <- length(rowset)
    attr(out, "ncol")    <- length(colset)

    out
}


#' Create columnwise tiles and return these as a list
#'
#' @param tiling A tiling
#' @param rowset The rowset of the columnwise tiles
#' @param colgroups The column groups of the columnwise tiles: all columns in one group belong to the same tile
#'
#' @return A list of tiles
#'
#' @export
create_columnwise_tiles <- function(tiling, rowset, colgroups) {
    out <- vector(mode = "list", length = length(colgroups))
    for (i in seq.int(length(colgroups)))
        out[[i]] <- create_tile(rowset = rowset, colset = colgroups[[i]], dims = tiling$dims)
    out
}


#' Create columnwise tiles in an empty tiling (from \code{init_tiling}).
#'
#' @param tiles A tile list (e.g., tiling$tiles)
#' @param dims The dimensions of the data matrix
#'
#' @return A tiling where each column is one tile
#'
#' @export
add_columnwise_tiles <- function(tiles, dims) {
    tiles[, "permutation"] <- c(sapply(seq.int(dims[2]), function(i) rep(i, dims[1])))
    tiles
}


#' Determine in how many pieces a new tile will be divided when added to an existing tiling
#'
#' When a new tile is added to an existing tiling, it must be broken up in horizontal slices
#' if it overlaps with existing tiles in the tiling. A slice indicates a set of overlapping
#' permutations that must be merged. This function determines in how many slices the new tile
#' will be broken up and enumerates these slices. One slice spans one or more rows, and
#' the same permutation is applied to all elements within a slice.
#'
#' @param tiling A tiling
#' @param tile A new tile to be added to the tiling
#'
#' @return A matrix containing for each row in the data matrix affected by the new tile:
#'           (i) a "slice number" enumerating the slices
#'          (ii) the indices of the rows in the data matrix affected by the new tile
#'         (iii) the existing permutations affected by the addition of the new tile
#'
#' @export
enum_slices <- function(tiling, tile) {
    x <- matrix(c(tiling$tiles[tile, "rowindex"][1:attr(tile, "nrow")],
                  tiling$tiles[tile, "permutation"]),
                ncol = attr(tile, "ncol")+1)

    x[is.na(x)] <- -1
    out <- new.env()
    for (i in seq.int(nrow(x))) {
        k <- paste0(x[i, 2:ncol(x)], collapse = "-")
        if (is.null(out[[k]])) {
            out[[k]] <- list("rows" = x[i, 1], "permutation" = unique(x[i, -1]))
        } else {
            out[[k]] <- list("rows" = c(out[[k]]$rows, x[i, 1]), "permutation" = out[[k]]$permutation)
        }
    }

    out
}


#' Add a new tile to a tiling
#'
#' @param tiling A tiling
#' @param tile A new tile to add to the tiling (from \code{create_tile})
#' @param target List to which the tile should be added. One of \code{tile_list_user}, \code{tile_list_base}, \code{tile_list_ignore}, or NULL if the tile should not be added anywhere.
#' @param store Should the tile be stored in the target
#'
#' @return The tiling with the new tile added
#'
#' @export
add_tile <- function(tiling, tile, target = "tile_list_user", store = TRUE) {
    ## Ignore-tiles do not need to be merged: just set the permutation to NA
    if (! is.null(target)) {
        if (target == "tile_list_ignore") {
            tiling$tiles[tile, "permutation"] <- NA
            if (store)
                tiling[[target]] <- c(tiling[[target]], list(tile))

            return(tiling)
        }
    }

    slices <- enum_slices(tiling, tile)
    pid_max <- smax(tiling$tiles[, "permutation"])


    for (k in ls(slices)) {
        ind_tmp <- unlist(tiling$index_row[as.character(slices[[k]]$rows)])
        ind     <- c(tiling$tiles[ind_tmp, "index"][tiling$tiles[ind_tmp, "permutation"] %in% slices[[k]]$permutation],
                     sub2ind_set(slices[[k]]$rows, attr(tile, "colset"), tiling$dims))


        tiling$tiles[ind, "permutation"] <- pid_max + 1
        pid_max <- pid_max + 1
    }

    if (store)
        tiling[[target]] <- c(tiling[[target]], list(tile))

    tiling
}


#' Special max function
#'
#' This function ignores NA values and returns a default value if the input vector is empty or if max would return -Inf/+Inf.
#'
#' @param x A vector
#' @param default Value to return if the input vector is empty or if max would return -Inf/+Inf.
#'
#' @return The vector in which the elements have been shuffled
#'
#' @export
smax <- function(x, default = 1) {

    if(! is.infinite(out <- suppressWarnings(max(x, na.rm = TRUE))))
        out
    else
        default
}


#' Permute a vector
#'
#' @param x A vector
#'
#' @return The vector in which the elements have been shuffled
#'
#' @export
permute_vector<- function(x) {
    x[sample.int(length(x))]
}


#' Convert a data frame to a matrix, returning the original column classes
#'
#' @param df A data frame
#'
#' @return A list containing the data frame as a matrix and the column classes
#'
#' @export
df2mat <- function(df) {
    list("matrix" = as.matrix(df), "col_classes" = sapply(df, class))
}


#' Convert a matrix to a data frame, also setting column classes
#'
#' @param x A data frame
#' @param col_classes A list containing the column class for each column in the matrix / data frame
#'
#' @return A data frame
#'
#' @export
mat2df <- function(x, col_classes) {
    df <- as.data.frame(x, stringsAsFactors = FALSE)

    for (colname in colnames(df)) {
        new_class <- col_classes[[colname]]

        if (new_class %in% c("integer", "numeric", "character")) {
            df[[colname]] <- as(object = df[[colname]], Class = new_class)
        }

        if (new_class == "factor") {
            df[[colname]] <- as.factor(df[[colname]])
        }
    }

    df
}


#' Permute a data matrix using a tiling
#'
#' @param x A data matrix or a data frame
#' @param tiling A tiling
#' @param rseed Random seed used for permuting.
#'
#' @return The data matrix permuted according to the tiling
#'
#' @export
permute_data <- function(x, tiling, rseed = NULL) {
    input_class = class(x)

    ## Handle data frame
    if (input_class == "data.frame") {
        tmp <- df2mat(x)
        x <- tmp$matrix
    }

    ## Initialise matrix
    x2 <- x
    colnames(x2) <- colnames(x)

    ## Set random seeds for permutations
    ## and generate list of random seeds, one seed per permutation function
    if (! is.null(rseed))
        set.seed(rseed)

    plist <- tapply(tiling$tiles[, "index"], tiling$tiles[, "permutation"], function(x) x)

    if (length(plist) > 0) {
        ind_rem <- which(sapply(plist, function(i) if(length(i) < 2) TRUE else FALSE))
        if (length(ind_rem) > 0)
            plist <- plist[-ind_rem]

        for (pid in names(plist)) {
            tile   <- tiling$tiles[plist[[pid]], ]
            rowset <- unique(tile[, "rowindex"])
            colset <- unique(tile[, "columnindex"])

            x2[rowset, colset] <- x[permute_vector(rowset), colset]
        }
    }


    ## Handle data frame
    if (input_class == "data.frame") {
        x2 <- mat2df(x2, col_classes = tmp$col_classes)
    }

    x2
}


#' Row and column sets to linear indexing
#'
#' @param rowset Set of rows
#' @param colset Set of columns
#' @param dims Dimensions of the data matrix
#'
#' @return The (column-major) linear indices corresponding to the row and column sets
#'
#' @export
sub2ind_set <- function(rowset, colset, dims) {
    out <- rep(NA, length(rowset) * length(colset))
    i <- 1
    for (cv in colset) {
        for (rv in rowset) {
            out[i] <- (cv - 1) * dims[1] + rv
            i <- i + 1
        }
    }
    out
}


#' Remove a given tile from the tiling
#'
#' @param tiling A tiling. If the tiling already has columnwise tiles set as an option, these are hounoured.
#' @param tile_ind Index of the tile(s) to be removed from the user tile list
#' @param target List from which the tile should be removed. One of \code{tile_list_user}, \code{tile_list_base} or \code{tile_list_ignore}
#'
#' @return The tiling without the given tile
#'
#' @export
remove_tiles <- function(tiling, tile_ind, target) {
    tiling[[target]] <- tiling[[target]][-tile_ind]
    rebuild_tiling(tiling)
}


#' Rebuild tiling.
#'
#' This function rebuilds a tiling from scratch, using both the user and base tiles
#'
#' @param tiling A tiling containing user and base tile. If the tiling has columnwise tiles set as an option, these are hounoured.
#'
#' @return A tiling
#'
#' @export
rebuild_tiling <- function(tiling) {

    tiling$tiles <- init_tiles(tiling$dims, columnwise_tiles = tiling$options$columnwise_tiles) ## clear tiling

    ## base tiles
    for (tile in tiling$tile_list_base)
        tiling <- add_tile(tiling, tile, target = "tile_list_base", store = FALSE)

    ## region to ignore (if any)
    for (tile in tiling$tile_list_ignore)
        tiling <- add_tile(tiling, tile, target = "tile_list_ignore", store = FALSE)

    ## user tiles
    for (tile in tiling$tile_list_user)
        tiling <- add_tile(tiling, tile, target = "tile_list_user", store = FALSE)

    ## focus tiles
    for (tile in tiling$tile_list_focus)
        tiling <- add_tile(tiling, tile, target = "tile_list_focus", store = FALSE)

    tiling
}


#' Linear index to subscript
#'
#' @param ind Linear index
#' @param nrows The number of rows in the data matrix
#'
#' @return The subscript index (row, col) corresponding to the linear index ind
#'
#' @export
ind2sub <- function(ind, nrows) {
    c(((ind-1) %% nrows) + 1, floor((ind-1) / nrows) + 1)
}


## ==================================================
## Below are some helper functions
## used during development
## ==================================================


#' Make a random tile
#'
#' @param nr The number of rows in the data matrix
#' @param nc The number of columns in the data matrix
#'
#' @return A tile
#'
#' @export
make_random_tile <- function(nr, nc) {
    r_tmp <- sample(seq.int(nr), 2, replace = FALSE)
    r_tmp <- seq.int(from = min(r_tmp), to = max(r_tmp))

    c_tmp <- sample(seq.int(nc), 2, replace = FALSE)
    c_tmp <- seq.int(from = min(c_tmp), to = max(c_tmp))

    create_tile(rowset = r_tmp, colset = c_tmp, dims = c(nr, nc))

}


#' Helper function for printing out a tiling
#'
#' This function prints out a specific column from a tiling.
#'
#' @param tiling A tiling containing user and base tile. If the tiling has columnwise tiles set as an option, these are hounoured.
#'
#' @return Nothing
printm <- function(tiling, col) {
    print(matrix(tiling$tiles[, col], nrow = tiling$dims[1], ncol = tiling$dims[2]))
}


#' Helper function for printing out a tiling
#'
#' This function prints the permutations in a tiling as a matrix
#'
#' @param tiling A tiling containing user and base tile. If the tiling has columnwise tiles set as an option, these are hounoured.
#'
#' @return Nothing
#'
#' @export
print_tiling <- function(tiling) {
    require(crayon)
    cat("\n")
    for (i in c("permutation")) {
        cat(green(i), "\n")
        printm(tiling, i)
        cat("\n")
    }
}


## --------------------------------------------------
## Different distances between matrices
## --------------------------------------------------

#' Calculate the correlation between all pairs of numberic columns in a dataset
#'
#' @param dataset A dataset (data frame or matrix)
#'
#' @return Correlation matrix
#'
#' @export
calculate_correlation <- function(dataset) {
    ## for each numeric variable, calculate the average correlation
    if (class(dataset) != "matrix") {
        col_numeric <- which(sapply(dataset, class) %in% c("integer", "numeric"))
        vnames <- names(dataset)[col_numeric]
        dataset <- as.matrix(dataset[, col_numeric])
    }
    cmat <- cor(dataset)
    cmat
}


#' Given two datasets ds1 and ds2, find the best variable pair for which  the difference in correlation between ds1 and ds2 is maximum
#'
#' @param ds1 A dataset (data frame or matrix)
#' @param ds2 A dataset (data frame or matrix)
#'
#' @return List containing (i) correlation values for the pairs, (ii) the correlation matrices, (iii) the pairs of matrices with the largest change in correlation, (iv) the names of the ordered pairs of columns
#'
#' @export
dist_maximum_change_in_correlation <- function(ds1, ds2, data = FALSE) {
    c1  <- calculate_correlation(ds1)
    c2  <- calculate_correlation(ds2)
    d   <- abs((c1**2 - c2**2))

    tmp <- lapply(order(d, decreasing = TRUE), function(i) ind2sub(i, nrows = nrow(d)))
    namelist <- lapply(tmp, function(i) c(colnames(d)[i[1]], colnames(d)[i[2]]))

    if (data)
        list("values" = tmp, "c1" = c1, "c2" = c2, "d" = d, "names_top_1" = colnames(d)[tmp[[1]]], "namelist" = namelist)
    else
        tmp[[1]]
}


#' Given two datasets ds1 and ds2, find the best variable pair for which  the difference in correlation between ds1 and ds2 is maximum
#'
#' @param ds1 A dataset (data frame or matrix)
#' @param ds2 A dataset (data frame or matrix)
#' @param data Should the data be returend (Boolean, default is \code{FALSE})
#'
#' @return List containing (i) correlation values for the pairs, (ii) the correlation matrices, (iii) the pairs of matrices with the largest change in correlation, (iv) the names of the ordered pairs of columns
#'
#' @export
find_top_k_variables <- function(ds1, ds2, k, data = FALSE) {
    v <- dist_maximum_change_in_correlation(ds1, ds2, data = TRUE)
    vk <- unlist(lapply(seq.int(length(v$namelist)), function(i) length(unique(unlist(v$namelist[1:i])))))
    top_k <- unique(unlist(v$namelist[1:which(vk >= k)[1]]))
    top_k <- top_k[1:min(length(top_k), k)]

    if (data)
        c(v, list("top_k" = top_k))
    else
        top_k
}


#' Given two datasets ds1 and ds2, find the best variable pair for which  the difference in correlation between ds1 and ds2 is maximum
#'
#' @param ds1 A dataset (data frame or matrix)
#' @param ds2 A dataset (data frame or matrix)
#' @param method Which method should be used (currenlty only \code{max_change_in_correlation} supported.
#' @param ... Extra arguments to be passed to the method used.
#'
#' @return Depends on the used method.
#'
#' @export
find_best_variable_pair <- function(ds1, ds2, method = "average_correlation", ...) {
    switch(method,
           "max_change_in_correlation" = dist_maximum_change_in_correlation(ds1, ds2, ...)
           )
}
