context("Initialisation of tiling")

## Create data for test

nr <- 5
nc <- 3

test_that("No columnwise tiles", {
    tiling <- init_tiling(c(nr, nc), columnwise_tiles = FALSE)

    expect_equal(all(is.na(tiling$tiles[, "permutation"])), TRUE)
})

test_that("Columnwise tiles", {
    tiling <- init_tiling(c(nr, nc), columnwise_tiles = TRUE)

    expect_equal(tiling$tiles[, "permutation"], c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3))
})

