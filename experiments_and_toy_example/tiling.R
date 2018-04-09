## Copyright 2018 Kai Puolamaki <kai.puolamaki@aalto.fi>
## Copyright 2018 Aalto University, Espoo, Finland
##
## Permission is hereby granted, free of charge, to any person obtaining
## a copy of this software and associated documentation files (the
## "Software"), to deal in the Software without restriction, including
## without limitation the rights to use, copy, modify, merge, publish,
## distribute, sublicense, and/or sell copies of the Software, and to
## permit persons to whom the Software is furnished to do so, subject to
## the following conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
## LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
## OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
## WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


require(digest)

tiling <- function(n,m,Tm=NULL,Tl=NULL,count=NULL) {
  ## Initial tiling
  ## Tiling matrix as in Alg. 1
  if(is.null(Tm)) Tm <- matrix(as.character(rep(1:m,each=n)),nrow=n,ncol=m)
  ## Additionally, we maintain a hash table of tiles (R,C).
  if(is.null(Tl)) {
    Tl <- new.env(hash=TRUE)
    for(j in 1:m) Tl[[as.character(j)]] <- list(R=1:n,C=j)
  }
  if(is.null(count)) count <- m
  
  newid <- function() {
    count <<- count+1
    as.character(count)
  }
  
  addtile <- function(R,C) {
    S <- new.env(hash=TRUE)
    ids <- NULL
    for(i in R) {
      raw <- Tm[i,C]
      K <- digest(raw) # hash of permutation ids
      if(exists(K,envir=S)) {
        if(any(raw!=S[[K]]$raw)) stop("addtile: hash collision.") # This is probably over-cautious...
        S[[K]]$rows <- c(S[[K]]$rows,i)
      } else {
        id <- unique(raw) 
        ids <- union(ids,id)
        S[[K]] <- list(rows=i,id=id,raw=raw) 
      } 
    }
    for(K in ls(S)) {
      Cp <- NULL
      for(id in S[[K]]$id) Cp <- union(Cp,Tl[[id]]$C)
      id <- newid()
      Tm[S[[K]]$rows,Cp] <<- id
      Tl[[id]] <- list(R=S[[K]]$rows,C=Cp)
    }
    for(id in ids) { # remove overlapping rows from existing tiles
      Rnew <- setdiff(Tl[[id]]$R,R)
      if(length(Rnew)>0)
        Tl[[id]]$R <- Rnew
      else 
        rm(list=id,envir=Tl)
    }
  }
  
  copy <- function() {
    newTl <- new.env(hash=TRUE)
    for(x in ls(Tl,all.names=TRUE)) assign(x,get(x,Tl),newTl)
    tiling(n,m,Tm=Tm,Tl=newTl,count=count)
  }
  
  permute <- function() {
    p <- matrix(1:(n*m),nrow=n,ncol=m)
    for(key in ls(Tl)) {
      R <- Tl[[key]]$R
      C <- Tl[[key]]$C
      if(length(R)>1) p[R,C] <- p[sample(R),C]
    }
    c(p)
  }
  
  permutedata <- function(x) {
    matrix(x[permute()],nrow=n,ncol=m,dimnames=dimnames(x))
  }
  
  list(newid=newid,
       addtile=addtile,
       copy=copy,
       permute=permute,
       permutedata=permutedata,
       status=function() { list(Tm=Tm,Tl=Tl,count=count) })
}
