test_that("Partial function tests work", {


  dummy_fun <- function( x = 2, y = "death", z = 5 )
  {
    x <- x + 5
    z <- 12
    x <- y + 5
    x <- list("something")
    z <- "something_else"
    z <- "ensure_that_you_never_go_over_the_end"
    return(x)
  }

  object_comparison <- partial_test( dummy_fun,
                                     args = list(y = 2),
                                     eval.point = 8,
                                     compare.object = list("something") )
  expect_true(object_comparison)

  object_comparison_list <- partial_test( dummy_fun,
                                     args = list(y = 2),
                                     eval.point = 5,
                                     compare.object = list("something") )
  expect_true(object_comparison_list)




  object_comparison_string <- partial_test( dummy_fun,
                                     args = list(y = 2),
                                     eval.point = 6,
                                     compare.object = "something_else" )
  expect_true(object_comparison_string)



})
