test_that("Finding function arguments works", {

  some_fun <-
    function(x = 1,
             y = 2,
             z = "whatever",
             a = matrix(1:20, ncol = 5)) {

    }

  test_res <- find_args(some_fun)

  expect_equal(sapply(test_res, class),
               c(
                 x = "numeric",
                 y = "numeric",
                 z = "character",
                 a = "call"
               ))
})

test_that("Running a function iteratively works", {

  some_fun <-
    function(x = 1,
             y = 2) {
      y <- paste0("a",y)
      x <- x+y
      return(x)
    }

  result <- run_iterativelly(some_fun, list( x = 1, y = 2 ))

  expect_equal( length(result), 4 )
  expect_equal( names(result), c("succesful",
                                 "last_line",
                                 "last_line_number",
                                 "objects_in_scope") )
  expect_equal( result[["succesful"]], FALSE )
  expect_equal( result[["last_line"]], "x <- x + y")
  expect_equal( result[["last_line_number"]], 3)
  expect_equal( result[["objects_in_scope"]], list( x = 1, y = "a2"))
})
