test_that("Test that running a function for partial returns works", {

    dummy_fun <- function( x = 2, y = "death", z = 5 )
    {
      x <- x + 5
      z <- 12
      x <- y + 5
      return(x)
    }

    test_res <- partial(dummy_fun, args = list(x = 2, y = "death", z = 5), eval_point = 2,
                        full_scope = TRUE )

    expect_equal(test_res$y,"death")
    expect_equal(test_res$z, 5)
    expect_equal(test_res$x, 2)



    test_no_args <- partial(dummy_fun, eval_point = 2,
                            full_scope = TRUE)
    expect_equal(test_no_args$x, test_res$x)
    expect_equal(test_no_args$y, test_res$y)
    expect_equal(test_no_args$z, test_res$z)

    test_res <- partial(dummy_fun, args = list(x = 2, y = "death", z = 5),
                        eval_point = "x <- x + 5",
                        full_scope = TRUE,
                        fix_pattern = TRUE )

    expect_equal(test_res$z, 5)
    expect_equal(test_res$y, "death")
    expect_equal(test_res$x, 2)



})

test_that( "Failures work as expected",{

  dummy_fun <- function( x = 2, y = "death", z = 5 )
  {
    x <- x + 5
    z <- 12
    x <- y + 5
    return(x)
  }

  expect_error( partial(dummy_fun),
                "Please supply an evaluation point for me to return.",
                fixed = TRUE)
  expect_error( partial(dummy_fun, eval_point = -2),
                "Not a valid evaluation point, stopping. Please supply a positive integer.",
                fixed = TRUE)

  expect_error(partial(dummy_fun, eval_point = matrix(NA, 10, 10)),
               "Please supply a valid evaluation point -
         a number indicating the line in the body of the function,
         or a string which can be parsed and matched against the
         body of the function.",
               fixed = TRUE )


  expect_error( partial( dummy_fun, eval_point = "x <- ",
                         fix_pattern = FALSE ),
                "Multiple evaluation matches - please select an unambiguous one.",
                fixed = TRUE )





})





