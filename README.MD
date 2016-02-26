This motivation for this package is to ascertain the correct way of testing variations of S3 methods in devtools. When assigning different 

## Script to replicate the error:
```{r, message = FALSE}
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
library(devtools)
install_github("mdlee12/cbindtest")
library(cbindtest)
setwd(system.file(package = "cbindtest"))
devtools::test()
```

This error message pops up:
#### Error message:
```{r, message = FALSE}
wrong value for .Method
1: cbind(x, y) at <C:\package_location\cbindtest/tests/testthat/test-cbind.R:11>
2: cbind(deparse.level, ...)
3: NextMethod("cbind") at <C:\package_location\cbindtest/R/cbind.fake.data.frame.R:6>
4: cbind.fake.data.frame(deparse.level, ...)
5: NextMethod("cbind") at <C:\package_location\cbindtest/R/cbind.fake.data.frame.R:6>
```

### Other testing approaches that work:
Running the tests using R CMD check doesn't return errors:
```{r, message = FALSE}
test_check("cbindtest")
testthat results ================================================================
OK: 2 SKIPPED: 0 FAILED: 0
```

How can the tests be run using devtools? Is there something missing in the cbind.fake.data.frame function or documentation?

Thanks!