context("Permuting the data")

## Create data for test
nr <- 5
nc <- 3

x <- apply(matrix(rep(seq.int(nc), nr), nrow = nc, ncol = nr), 1, function(i) (i-1)*10 ) + matrix(rep(seq.int(nr), nc), nr, nc)

## Define tiles
t1 <- create_tile(rowset = c(1, 2), colset = c(2, 3), dims = c(nr, nc))
t2 <- create_tile(rowset = c(3, 4), colset = c(1, 2), dims = c(nr, nc))
t3 <- create_tile(rowset = c(2, 3, 4), colset = c(1, 2, 3), dims = c(nr, nc))
t4 <- create_tile(rowset = c(1,2, 3, 4, 5), colset = c(1, 2, 3), dims = c(nr, nc))
t5 <- create_tile(rowset = c(3), colset = c(2), dims = c(nr, nc))

test_that("Columnwise permutation", {
    tiling <- init_tiling(dim(x), columnwise_tiles = TRUE)

    set.seed(42)
    x2 <- permute_data(x, tiling)
    expect_equal(c(x2), c(5, 4, 1, 2, 3, 13, 15, 11, 12, 14, 23, 25, 24, 21, 22))
})

test_that("Different tiles -- columnwise tiles", {
    tiling <- init_tiling(dim(x), columnwise_tiles = TRUE)

    Tres1 <- add_tile(tiling, t1)
    Tres2 <- add_tile(add_tile(tiling, t1), t2)
    Tres3 <- add_tile(add_tile(add_tile(tiling, t1), t2), t3)
    Tres4 <- add_tile(tiling, t4)
    Tres5 <- add_tile(tiling, t5)

    res1 <- c(2, 1, 5, 4, 3, 11, 12, 15, 13, 14, 21, 22, 25, 23, 24)
    res2 <- c(5, 1, 4, 3, 2, 12, 11, 14, 13, 15, 22, 21, 23, 24, 25)
    res3 <- c(5, 2, 3, 4, 1, 11, 12, 13, 14, 15, 21, 22, 23, 24, 25)
    res4 <- c(1, 4, 2, 3, 5, 11, 14, 12, 13, 15, 21, 24, 22, 23, 25)
    res5 <- c(1, 2, 4, 5, 3, 11, 15, 13, 12, 14, 24, 22, 25, 23, 21)

    set.seed(11)
    expect_equal(c(permute_data(x, Tres1)), res1)

    set.seed(13)
    expect_equal(c(permute_data(x, Tres2)), res2)

    set.seed(15)
    expect_equal(c(permute_data(x, Tres3)), res3)

    set.seed(17)
    expect_equal(c(permute_data(x, Tres4)), res4)

    set.seed(19)
    expect_equal(c(permute_data(x, Tres5)), res5)

})

test_that("Different tiles -- empty tiling", {
    tiling <- init_tiling(dim(x), columnwise_tiles = FALSE)

    Tres1 <- add_tile(tiling, t1)
    Tres2 <- add_tile(add_tile(tiling, t1), t2)
    Tres3 <- add_tile(add_tile(add_tile(tiling, t1), t2), t3)
    Tres4 <- add_tile(tiling, t4)
    Tres5 <- add_tile(tiling, t5)

    res1 <- c(1, 2, 3, 4, 5, 12, 11, 13, 14, 15, 22, 21, 23, 24, 25)
    res2 <- c(1, 2, 4, 3, 5, 11, 12, 14, 13, 15, 21, 22, 23, 24, 25)
    res3 <- c(1, 2, 4, 3, 5, 11, 12, 14, 13, 15, 21, 22, 24, 23, 25)
    res4 <- c(1, 4, 2, 3, 5, 11, 14, 12, 13, 15, 21, 24, 22, 23, 25)
    res5 <- c(1, 2, 3, 4, 5, 11, 12, 13, 14, 15, 21, 22, 23, 24, 25)

    set.seed(13)
    expect_equal(c(permute_data(x, Tres1)), res1)

    set.seed(11)
    expect_equal(c(permute_data(x, Tres2)), res2)

    set.seed(15)
    expect_equal(c(permute_data(x, Tres3)), res3)

    set.seed(17)
    expect_equal(c(permute_data(x, Tres4)), res4)

    set.seed(19)
    expect_equal(c(permute_data(x, Tres5)), res5)

})



test_that("Test a few special cases", {
    nr <- 7
    nc <- 11

    t1 <- create_tile(rowset = c(2, 3, 4), colset = c(2, 3, 4), dims = c(nr, nc))
    t2 <- create_tile(rowset = c(3, 4, 5), colset = c(4, 5, 6), dims = c(nr, nc))
    t3 <- create_tile(rowset = c(3, 4), colset = c(6, 7, 8, 9), dims = c(nr, nc))
    t4 <- create_tile(rowset = c(6, 7), colset = c(8, 9, 10, 11), dims = c(nr, nc))

    x <- apply(matrix(rep(seq.int(nc), nr), nrow = nc, ncol = nr), 1, function(i) (i-1)*10 ) + matrix(rep(seq.int(nr), nc), nr, nc)

    tiling    <- init_tiling(c(nr, nc), columnwise_tiles = FALSE)
    tiling_cw <- init_tiling(c(nr, nc), columnwise_tiles = TRUE)

    
    Tres <- add_tile(tiling, t1)
    Tres <- add_tile(Tres, t2)
    Tres <- add_tile(Tres, t3)
    Tres <- add_tile(Tres, t4)

    Tres_cw <- add_tile(tiling_cw, t1)
    Tres_cw <- add_tile(Tres_cw, t2)
    Tres_cw <- add_tile(Tres_cw, t3)
    Tres_cw <- add_tile(Tres_cw, t4)

    
    set.seed(11)
    x2    <- permute_data(x, Tres)
    x2_cw <- permute_data(x, Tres_cw)

    ## Verify that column sums are intact
    expect_equal(colSums(x2), colSums(x))
    expect_equal(colSums(x2_cw), colSums(x))

    ## The matrices should not be identical
    expect_equal(identical(x2, x), FALSE)
    expect_equal(identical(x2_cw, x), FALSE)
})


test_that("Permute three tiles", {
    nr <- 7
    nc <- 11

    t1 <- create_tile(rowset = c(2, 3, 4), colset = c(2, 3, 4), dims = c(nr, nc))
    t3 <- create_tile(rowset = c(3, 4), colset = c(6, 7, 8, 9), dims = c(nr, nc))
    t4 <- create_tile(rowset = c(6, 7), colset = c(8, 9, 10, 11), dims = c(nr, nc))

    x <- apply(matrix(rep(seq.int(nc), nr), nrow = nc, ncol = nr), 1, function(i) (i-1)*10 ) + matrix(rep(seq.int(nr), nc), nr, nc)

    tiling    <- init_tiling(c(nr, nc), columnwise_tiles = FALSE)
    tiling_cw <- init_tiling(c(nr, nc), columnwise_tiles = TRUE)
    
    Tres <- add_tile(tiling, t1)
    Tres <- add_tile(Tres, t3)
    Tres <- add_tile(Tres, t4)

    Tres_cw <- add_tile(tiling_cw, t1)
    Tres_cw <- add_tile(Tres_cw, t3)
    Tres_cw <- add_tile(Tres_cw, t4)

    set.seed(11)
    x2    <- permute_data(x, Tres)
    x2_cw <- permute_data(x, Tres_cw)
    
    for (tile in list(t1, t3, t4)) {
        m <- x2[attr(tile, "rowset"), attr(tile, "colset")]
        m <- m[order(m[, 1], decreasing = FALSE), ]

        m_cw <- x2_cw[attr(tile, "rowset"), attr(tile, "colset")]
        m_cw <- m_cw[order(m_cw[, 1], decreasing = FALSE), ]

        
        m2 <- x[attr(tile, "rowset"), attr(tile, "colset")]
        expect_equal(identical(m, m2), TRUE)
        expect_equal(identical(m_cw, m2), TRUE)

    }
    
})
