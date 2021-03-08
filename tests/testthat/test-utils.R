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
