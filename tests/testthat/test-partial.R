test_that("Test that running a function for partial returns works", {

    dummy_fun <- function( x = 2, y = "death", z = 5 )
    {
      x <- x + 5
      z <- 12
      x <- y + 5
      return(x)
    }

    test_res <- partial(dummy_fun, args = list(x = 2, y = "death", z = 5), eval.point = 2,
                        full.scope = TRUE )

    expect_equal(test_res$y,"death")
    expect_equal(test_res$z, 5)
    expect_equal(test_res$x, 2)



    test_no_args <- partial(dummy_fun, eval.point = 2,
                            full.scope = TRUE)
    expect_equal(test_no_args$y, test_res$y)
    expect_equal(test_no_args$z, test_res$z)
    expect_equal(test_no_args$x, list(c(" x = 2", " y = \"death\"", " z = 5 ")))



})





