#' @title Binds a fake.data.frame to another fake.data.frame or a data.frame.
#'
#' @description Binds a fake.data.frame to another fake.data.frame or a \code{data.frame}.
#'
#' @param x a fake.data.frame
#' @method cbind fake.data.frame
#' @export
cbind.fake.data.frame <- function(x, ...) {
  # do the subset like it's a data.frame
  res <- NextMethod("cbind")
  # copy over all of the attributes
  atrs <- names(attributes(x))
  # but don't copy these attributes over
  atrs <- atrs[!atrs %in% c("names", "row.names", "class")]
  lapply(atrs, function(z){
    # if the scope of an lapply were like a for loop, this is all that would be in the body of this function
    #attr(res, z) <- attr(x,z)
    dat <- get("res")
    attr(dat, z) <- attr(get("x"),z)
    res <<- dat
  })
  if(inherits(res, "data.frame")) {
    class(res) <- class(x)
  }
  res
}

#setMethod("cbind",
#          c(x="fake.data.frame"),
#          cbind.fake.data.frame)
