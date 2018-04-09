context("Addition of tile")

## Create data for test
nr <- 5
nc <- 3


## Define tiles
t1 <- create_tile(rowset = c(1, 2), colset = c(2, 3), dims = c(nr, nc))
t2 <- create_tile(rowset = c(3, 4), colset = c(1, 2), dims = c(nr, nc))
t3 <- create_tile(rowset = c(2, 3, 4), colset = c(1, 2, 3), dims = c(nr, nc))
t4 <- create_tile(rowset = c(1,2, 3, 4, 5), colset = c(1, 2, 3), dims = c(nr, nc))
t5 <- create_tile(rowset = c(3), colset = c(2), dims = c(nr, nc))

## Test addition of tiles when the tiling is empty
tiling <- init_tiling(c(nr, nc), columnwise_tiles = FALSE)

test_that("Add single tiles", {
    Tres1 <- add_tile(tiling, t1)
    Tres2 <- add_tile(tiling, t2)
    Tres3 <- add_tile(tiling, t3)
    
    expect_equal(Tres1$tiles[, "permutation"], c(NA, NA, NA, NA, NA, 2, 2, NA, NA, NA, 2, 2, NA, NA, NA))
    expect_equal(Tres2$tiles[, "permutation"], c(NA, NA, 2, 2, NA, NA, NA, 2, 2, NA, NA, NA, NA, NA, NA))
    expect_equal(Tres3$tiles[, "permutation"], c(NA, 2, 2, 2, NA, NA, 2, 2, 2, NA, NA, 2, 2, 2, NA))
})

test_that("Add two tiles", {
    Tres <- add_tile(add_tile(tiling, t1), t2)
    expect_equal(Tres$tiles[, "permutation"], c(NA, NA, 3, 3, NA, 2, 2, 3, 3, NA, 2, 2, NA, NA, NA))
})

test_that("Add three tiles", {
    Tres <- add_tile(add_tile(add_tile(tiling, t1), t2), t3)
    expect_equal(Tres$tiles[, "permutation"], c(NA, 4, 5, 5, NA, 2, 4, 5, 5, NA, 2, 4, 5, 5, NA))
})

test_that("Add tile covering entire matrix to matrix with columnwise tiles", {
    Tres <- add_tile(tiling, t4)
    expect_equal(unique(Tres$tiles[, "permutation"]), 2)
})

test_that("Add single-element tile to columnwise tiling", {
    Tres <- add_tile(tiling, t5)
    expect_equal(Tres$tiles[, "permutation"], c(NA, NA, NA, NA, NA, NA, NA, 2, NA, NA, NA, NA, NA, NA, NA))
})

test_that("Add single-element tile to big tile covering entire matrix", {
    Tres <- add_tile(add_tile(tiling, t4), t5)
    expect_equal(Tres$tiles[, "permutation"], c(2, 2, 3, 2, 2, 2, 2, 3, 2, 2, 2, 2, 3, 2, 2))
})



## Test addition of tiles when there are columnwise tiles
tiling <- init_tiling(c(nr, nc), columnwise_tiles = TRUE)

test_that("Add single tiles", {
    Tres1 <- add_tile(tiling, t1)
    Tres2 <- add_tile(tiling, t2)
    Tres3 <- add_tile(tiling, t3)
    
    expect_equal(Tres1$tiles[, "permutation"], c(1, 1, 1, 1, 1, 4, 4, 2, 2, 2, 4, 4, 3, 3, 3))
    expect_equal(Tres2$tiles[, "permutation"], c(1, 1, 4, 4, 1, 2, 2, 4, 4, 2, 3, 3, 3, 3, 3))
    expect_equal(Tres3$tiles[, "permutation"], c(1, 4, 4, 4, 1, 2, 4, 4, 4, 2, 3, 4, 4, 4, 3))
})

test_that("Add two tiles", {
    Tres <- add_tile(add_tile(tiling, t1), t2)
    expect_equal(Tres$tiles[, "permutation"], c(1, 1, 5, 5, 1, 4, 4, 5, 5, 2, 4, 4, 3, 3, 3))
})

test_that("Add three tiles", {
    Tres <- add_tile(add_tile(add_tile(tiling, t1), t2), t3)
    expect_equal(Tres$tiles[, "permutation"], c(1, 6, 7, 7, 1, 4, 6, 7, 7, 2, 4, 6, 7, 7, 3))
})

test_that("Add tile covering entire matrix to matrix with columnwise tiles", {
    Tres <- add_tile(tiling, t4)
    expect_equal(unique(Tres$tiles[, "permutation"]), 4)
})

test_that("Add single-element tile to columnwise tiling", {
    Tres <- add_tile(tiling, t5)
    expect_equal(Tres$tiles[, "permutation"], c(1, 1, 1, 1, 1, 2, 2, 4, 2, 2, 3, 3, 3, 3, 3))
})

test_that("Add single-element tile to big tile covering entire matrix", {
    Tres <- add_tile(add_tile(tiling, t4), t5)
    expect_equal(Tres$tiles[, "permutation"], c(4, 4, 5, 4, 4, 4, 4, 5, 4, 4, 4, 4, 5, 4, 4))
})


test_that("Test a few special cases", {
    nr <- 7
    nc <- 11

    tiling <- init_tiling(c(nr, nc), columnwise_tiles = FALSE)

    t1 <- create_tile(rowset = c(2, 3, 4), colset = c(2, 3, 4), dims = c(nr, nc))
    t2 <- create_tile(rowset = c(3, 4, 5), colset = c(4, 5, 6), dims = c(nr, nc))
    t3 <- create_tile(rowset = c(3, 4), colset = c(6, 7, 8, 9), dims = c(nr, nc))
    t4 <- create_tile(rowset = c(6, 7), colset = c(8, 9, 10, 11), dims = c(nr, nc))

    Tres <- add_tile(tiling, t1)
    Tres <- add_tile(Tres, t2)
    Tres <- add_tile(Tres, t3)
    Tres <- add_tile(Tres, t4)
    expect_equal(Tres$tiles[, "permutation"], c(NA, NA, NA, NA, NA, NA, NA, NA, 2, 5, 5, NA, NA, NA, NA, 2, 5, 5, NA, NA, NA, NA, 2, 5, 5, 3, NA, NA, NA, NA, 5, 5, 3, NA, NA, NA, NA, 5, 5, 3, NA, NA, NA, NA, 5, 5, NA, NA, NA, NA, NA, 5, 5, NA, 6, 6, NA, NA, 5, 5, NA, 6, 6, NA, NA, NA, NA, NA, 6, 6, NA, NA, NA, NA, NA, 6, 6))


    tiling_cw <- init_tiling(c(nr, nc), columnwise_tiles = TRUE)
    Tres <- add_tile(tiling_cw, t1)
    Tres <- add_tile(Tres, t2)
    Tres <- add_tile(Tres, t3)
    Tres <- add_tile(Tres, t4)

    expect_equal(Tres$tiles[, "permutation"], c(1, 1, 1, 1, 1, 1, 1, 2, 12, 15, 15, 2, 2, 2, 3, 12, 15, 15, 3, 3, 3, 4, 12, 15, 15, 14, 4, 4, 5, 5, 15, 15, 14, 5, 5, 6, 6, 15, 15, 14, 6, 6, 7, 7, 15, 15, 7, 7, 7, 8, 8, 15, 15, 8, 16, 16, 9, 9, 15, 15, 9, 16, 16, 10, 10, 10, 10, 10, 16, 16, 11, 11, 11, 11, 11, 16, 16))
    
})


test_that("Test addition of full tile to case where each row is a separate tile", {
    nr <- 7
    nc <- 11
 
    tiling <- init_tiling(c(nr, nc), columnwise_tiles = FALSE)

    ## Make each row a separate tile
    tiling$tiles[, "permutation"] <- tiling$tiles[, "rowindex"]
    
    t1 <- create_tile(rowset = c(2, 3, 4), colset = c(2, 3, 4), dims = c(nr, nc))
    t2 <- create_tile(rowset = c(3, 4, 5), colset = c(4, 5, 6), dims = c(nr, nc))
    t3 <- create_tile(rowset = c(3, 4), colset = c(6, 7, 8, 9), dims = c(nr, nc))
    t4 <- create_tile(rowset = c(6, 7), colset = c(8, 9, 10, 11), dims = c(nr, nc))

    Tres <- add_tile(tiling, t1)
    Tres <- add_tile(Tres, t2)
    Tres <- add_tile(Tres, t3)
    Tres <- add_tile(Tres, t4)
    
    ## The number of permutations should match the number of rows
    expect_equal(length(unique(Tres$tiles[, "permutation"])),  nr)

    ## There should just be one permutation per row
    tm <- matrix(tiling$tiles[, "permutation"], ncol = nc)
    
    expect_equal(all(apply(tm, 1, function(i) length(unique(i))) == 1) , TRUE)

})
