context("Removing and rebuiling")

## Create data for test
nr <- 5
nc <- 3


## Define tiles
tiling    <- init_tiling(c(nr, nc), columnwise_tiles = FALSE)
tiling_cw <- init_tiling(c(nr, nc), columnwise_tiles = TRUE)

t1 <- create_tile(rowset = c(1, 2), colset = c(2, 3), dims = c(nr, nc))
t2 <- create_tile(rowset = c(3, 4), colset = c(1, 2), dims = c(nr, nc))
t3 <- create_tile(rowset = c(2, 3, 4), colset = c(1, 2, 3), dims = c(nr, nc))
t4 <- create_tile(rowset = c(1,2, 3, 4, 5), colset = c(1, 2, 3), dims = c(nr, nc))
t5 <- create_tile(rowset = c(3), colset = c(2), dims = c(nr, nc))


test_that("Add one tile - then remove it", {
    Tres  <- add_tile(tiling, t1)
    Tres  <- remove_tiles(Tres, 1, target = "tile_list_user")

    expect_equal(all(is.na(Tres$tiles[, "permutation"])), TRUE)

    Tres_cw <- add_tile(tiling_cw, t1)
    Tres_cw <- remove_tiles(Tres_cw, 1, target = "tile_list_user")

    res_cw <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)
    expect_equal(Tres_cw$tiles[, "permutation"], res_cw)

})


test_that("Add three  tiles - then remove 2nd tile", {
    Tres  <- add_tile(add_tile(add_tile(tiling, t1), t2), t5)
    Tres  <- remove_tiles(Tres, 2, target = "tile_list_user")

    res <- c(NA, NA, NA, NA, NA, 2, 2, 3, NA, NA, 2, 2, NA, NA, NA)
    expect_equal(Tres$tiles[, "permutation"], res)
    
    Tres_cw  <- add_tile(add_tile(add_tile(tiling_cw, t1), t2), t5)
    Tres_cw  <- remove_tiles(Tres_cw, 2, target = "tile_list_user")

    res_cw <- c(1, 1, 1, 1, 1, 4, 4, 5, 2, 2, 4, 4, 3, 3, 3)
    expect_equal(Tres_cw$tiles[, "permutation"], res_cw)

})

test_that("Add three  tiles to focus tiles- then remove 2nd tile", {
    tiling    <- init_tiling(c(nr, nc), columnwise_tiles = FALSE)
    tiling_cw <- init_tiling(c(nr, nc), columnwise_tiles = TRUE)

    Tres  <- add_tile(add_tile(add_tile(tiling, t1, target = "tile_list_focus"), t2, target = "tile_list_focus"), t5, target = "tile_list_focus")
    Tres  <- remove_tiles(Tres, 2, target = "tile_list_focus")

    res <- c(NA, NA, NA, NA, NA, 2, 2, 3, NA, NA, 2, 2, NA, NA, NA)
    expect_equal(Tres$tiles[, "permutation"], res)
    
    Tres_cw  <- add_tile(add_tile(add_tile(tiling_cw, t1, target = "tile_list_focus"), t2, target = "tile_list_focus"), t5, target = "tile_list_focus")
    Tres_cw  <- remove_tiles(Tres_cw, 2, target = "tile_list_focus")

    res_cw <- c(1, 1, 1, 1, 1, 4, 4, 5, 2, 2, 4, 4, 3, 3, 3)
    expect_equal(Tres_cw$tiles[, "permutation"], res_cw)

})

