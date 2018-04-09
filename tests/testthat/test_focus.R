context("Focusing")

## Create data for test
nr <- 5
nc <- 3

## Define tiles
tiling    <- init_tiling(c(nr, nc), columnwise_tiles = FALSE)
tiling_cw <- init_tiling(c(nr, nc), columnwise_tiles = TRUE)

## user-tiles
t1 <- create_tile(rowset = c(1, 2), colset = c(2, 3), dims = c(nr, nc))
t2 <- create_tile(rowset = c(3, 4), colset = c(1, 2), dims = c(nr, nc))
t5 <- create_tile(rowset = c(3), colset = c(2), dims = c(nr, nc))

## focus-tiles
## (1) cover columns 1, 2, 3
f1 <- create_tile(rowset = c(1,2, 3, 4, 5), colset = c(1, 2, 3), dims = c(nr, nc))

## (2) cover columns 1 and 2
f2 <- create_tile(rowset = c(1,2, 3, 4, 5), colset = c(1, 2), dims = c(nr, nc))

## (3) cover columns 1 and 3
f3 <- create_tile(rowset = c(1,2, 3, 4, 5), colset = c(1, 3), dims = c(nr, nc))

## (4) cover upper left corner
f4 <- create_tile(rowset = c(1,2, 3), colset = c(1, 2), dims = c(nr, nc))

## (5) cover lower right corner
f5 <- create_tile(rowset = c(3, 4, 5), colset = c(2, 3), dims = c(nr, nc))

## (6) cover rows (1, 3, 5) and columns (1, 3)
f6 <- create_tile(rowset = c(1,3, 5), colset = c(1, 3), dims = c(nr, nc))



test_that("Focus 1", {
    Tres  <- add_tile(tiling, t1)
    Tres  <- add_tile(Tres, f1, target = "tile_list_focus")
    res   <- c(4, 4, 3, 3, 3, 4, 4, 3, 3, 3, 4, 4, 3, 3, 3)

    expect_equal(Tres$tiles[, "permutation"], res)

    Tres_cw  <- add_tile(tiling_cw, t1)
    Tres_cw  <- add_tile(Tres_cw, f1, target = "tile_list_focus")
    res_cw <- c(6, 6, 5, 5, 5, 6, 6, 5, 5, 5, 6, 6, 5, 5, 5)

    expect_equal(Tres_cw$tiles[, "permutation"], res_cw)
})



test_that("Focus 2", {
    Tres <- add_tile(tiling, t1)
    Tres <- add_tile(Tres, f2, target = "tile_list_focus")
    res  <- c(4, 4, 3, 3, 3, 4, 4, 3, 3, 3, 4, 4, NA, NA, NA)

    expect_equal(Tres$tiles[, "permutation"], res)

    Tres_cw  <- add_tile(tiling_cw, t1)
    Tres_cw  <- add_tile(Tres_cw, f2, target = "tile_list_focus")
    res_cw   <- c(6, 6, 5, 5, 5, 6, 6, 5, 5, 5, 6, 6, 3, 3, 3)

    expect_equal(Tres_cw$tiles[, "permutation"], res_cw)
})

test_that("Focus 3", {
    Tres <- add_tile(tiling, t1)
    Tres <- add_tile(Tres, f3, target = "tile_list_focus")
    res  <- c(4, 4, 3, 3, 3, 4, 4, NA, NA, NA, 4, 4, 3, 3, 3)

    expect_equal(Tres$tiles[, "permutation"], res)

    Tres_cw  <- add_tile(tiling_cw, t1)
    Tres_cw  <- add_tile(Tres_cw, f3, target = "tile_list_focus")
    res_cw   <- c(6, 6, 5, 5, 5, 6, 6, 2, 2, 2, 6, 6, 5, 5, 5)

    expect_equal(Tres_cw$tiles[, "permutation"], res_cw)
})

test_that("Focus 4", {
    Tres <- add_tile(tiling, t1)
    Tres <- add_tile(Tres, f4, target = "tile_list_focus")
    res  <- c(4, 4, 3, NA, NA, 4, 4, 3, NA, NA, 4, 4, NA, NA, NA)

    expect_equal(Tres$tiles[, "permutation"], res)

    Tres_cw  <- add_tile(tiling_cw, t1)
    Tres_cw  <- add_tile(Tres_cw, f4, target = "tile_list_focus")
    res_cw   <- c(6, 6, 5, 1, 1, 6, 6, 5, 2, 2, 6, 6, 3, 3, 3)

    expect_equal(Tres_cw$tiles[, "permutation"], res_cw)
})

test_that("Focus 5", {
    Tres <- add_tile(tiling, t1)
    Tres <- add_tile(Tres, f5, target = "tile_list_focus")
    res  <- c(NA, NA, NA, NA, NA, 2, 2, 3, 3, 3, 2, 2, 3, 3, 3)

    expect_equal(Tres$tiles[, "permutation"], res)

    Tres_cw  <- add_tile(tiling_cw, t1)
    Tres_cw  <- add_tile(Tres_cw, f5, target = "tile_list_focus")
    res_cw   <- c(1, 1, 1, 1, 1, 4, 4, 5, 5, 5, 4, 4, 5, 5, 5)

    expect_equal(Tres_cw$tiles[, "permutation"], res_cw)
})

test_that("Focus 6", {
    Tres <- add_tile(tiling, t1)
    Tres <- add_tile(Tres, f6, target = "tile_list_focus")
    res  <- c(4, NA, 3, NA, 3, 4, 2, NA, NA, NA, 4, 2, 3, NA, 3)

    expect_equal(Tres$tiles[, "permutation"], res)

    Tres_cw  <- add_tile(tiling_cw, t1)
    Tres_cw  <- add_tile(Tres_cw, f6, target = "tile_list_focus")
    res_cw   <- c(6, 1, 5, 1, 5, 6, 4, 2, 2, 2, 6, 4, 5, 3, 5)

    expect_equal(Tres_cw$tiles[, "permutation"], res_cw)
})


