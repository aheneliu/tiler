TILER
=========

The `tiler` R-package provides functions for constrained randomisation
of matrices using tiles. The package also includes an interactive
software for data exploration.


Installation
-------------
To install this R-package, proceed as follows.

First install the `devtools`-package and load it in R:
```R
install.packages("devtools")
library(devtools)
```

Then install the `tiler` package

```R
install_github("aheneliu/tiler")
```

Loading
-------------
After installation, start R and load the package using
```R
library(tiler)
```

Usage
-------------
Here is a brief example of how to use the R-package.

```R
library(tiler)

## Create a data matrix with a special structure
nr <- 11
nc <- 13
x <- apply(matrix(rep(seq.int(nc), nr), nrow = nc, ncol = nr), 1, function(i) (i-1)*10 ) + matrix(rep(seq.int(nr), nc), nr, nc)


## Initialise a tiling where each column is a separate tile
tiling <- init_tiling(dim(x), columnwise_tiles = TRUE)

## Create some tiles
t1 <- create_tile(rowset = c(1, 2, 3, 4), colset = c(5, 6, 7), dims = dim(x))
t2 <- create_tile(rowset = c(4, 5, 6, 7), colset = c(1, 2, 3,  8, 9), dims = dim(x))
t3 <- create_tile(rowset = c(3, 4, 5), colset = c(2, 3, 4, 5, 6), dims = dim(x))
t4 <- create_tile(rowset = c(2, 3, 4, 7, 8, 9), colset = c(2, 3, 4, 8, 9, 10), dims = dim(x))
t5 <- create_tile(rowset = c(5, 6,  7, 8, 9, 11), colset = c(11, 12, 13), dims = dim(x))

## Add the tiles to the tiling
tiling <- add_tile(tiling, t1)
tiling <- add_tile(tiling, t2)
tiling <- add_tile(tiling, t3)
tiling <- add_tile(tiling, t4)
tiling <- add_tile(tiling, t5)


## View the tiling
print_tiling(tiling)

## Obtain a permuted version of the data, constrained by the tiling
x2 <- permute_data(x, tiling)
```

Interactive application for data exploration
---------------------------------------------
The package also includes an interactive application based on shiny.
To start the application, proceed as follows in R.

```R
library(tiler)
tiler::run_tiler()
```

This should also automatically launch a web browser with the application.
Otherwise you can open a web browser and point it to `http://127.0.0.1:7000/`.

License
-------
The `tiler` R-package is licensed under the [MIT-license](http://opensource.org/licenses/MIT).
