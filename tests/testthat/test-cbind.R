library(testthat)
library(cbindtest)


context("cbind function operates properly")
test_that("cbind function operates properly w/ NextMethod()",{
  x <- data.frame(a=c(1:10),b=c(2:11))
	y <- data.frame(c=c(3:12))
  class(x) <- c("fake.data.frame", "data.frame")
  class(y) <- c("fake.data.frame", "data.frame")
	xy <- cbind(x,y)
	xy2 <- data.frame(a=c(1:10),b=c(2:11),c=c(3:12))
	class(xy) <- c("fake.data.frame")
	class(xy2) <- c("fake.data.frame")
	expect_equal(attributes(xy)$names,attributes(xy2)$names)
 	expect_equal(xy,xy2) # test everything
 })



