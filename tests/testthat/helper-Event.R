load_example()

## Waldo::compare method does not use the all.equal method.
expect_eq <- function(object, expected,...) {
  act <- testthat::quasi_label(rlang::enquo(object),arg="object")
  res <- all.equal(object,expected,...)
  if (!isTRUE(res)) {
    testthat::fail(paste(res,collapse="\n"))
  }
  testthat::succeed()
  invisible(act$val)
}






