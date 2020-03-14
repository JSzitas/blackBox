
test_that("Recovering from a function works", {
  dummy_fun <- function( x = 2, y = "death", z = 5 )
  {
    x <- x + 5
    z <- 12
    x <- y + 5
    return(x)
  }

  test_res <- recover(dummy_fun)

  expect_equal(names(test_res), c("Failing line", "Objects in scope"))
  # It correctly fails here due to addition of "death" + 5 resulting in an error
  expect_equal(as.character(test_res$'Failing line'), c("<-", "x", "y + 5"))
  # This gets updated - correctly
  expect_equal(test_res$`Objects in scope`$x, 7)
  expect_equal(test_res$`Objects in scope`$y, "death")
  # This gets updated correctly too
  expect_equal(test_res$`Objects in scope`$z, 12)

  test_res <- recover( dummy_fun,
                       args = list( x = 5,
                                    y ="fly",
                                    z = 2 ),
                       return.all = FALSE )
  expect_equal(test_res, 1)


})

test_that( "If the function runs the return is correct",{
  dummy_fun <- function( x = 2, y = 2, z = 5 )
  {
    x <- x + 5
    z <- 12
    x <- y + 5
    return(x)
  }

  test_res <- recover( dummy_fun,
                       return.all = FALSE )
  expect_equal(test_res, 0)

})


test_that( "This works even in parallel",{

  skip_on_cran()
  skip_on_travis()
  suppressWarnings( library(doFuture))
  registerDoFuture()
  plan(multiprocess)

  parallel_fun <- function(x,y,z, length.out ){

    magical <- function(x,y,z){
      x <- x + 5
      z <- 12
      x <- y + z
      return(x)
    }
    res <-  foreach(i = 1:length.out ) %dopar%
      {
        # if you dont assign here the test fails
        weirdness <- magical(x,y,z)
      }
    return(res)
  }

  test_res <- recover( parallel_fun,
                       args = list( x = 5,
                                    y ="fly",
                                    z = 2,
                                    length.out = 10 ))

  expect_equal(names(test_res), c("Failing line", "Objects in scope"))

  # returns the wrong line, ie
  expect_equal(as.character(test_res$'Failing line'),
               c( "<-",
                  "res",
        "foreach(i = 1:length.out) %dopar% {\n    weirdness <- magical(x, y, z)\n}"))
  expect_equal(test_res$'Objects in scope'$x, 5)
  expect_equal(test_res$'Objects in scope'$y, "fly")
  expect_equal(test_res$'Objects in scope'$z , 2)
  expect_equal(test_res$'Objects in scope'$length.out, 10)

  test_res <- recover( parallel_fun,
                       args = list( x = 5,
                                    y ="fly",
                                    z = 2,
                                    length.out = 10 ),
                       return.all = FALSE )
  expect_equal(test_res, 1)


})

test_that("Recovering from a conditional inside a function works", {
  dummy_fun <- function( x = 2, y = "death", z = 5, some_condition = TRUE )
  {
    if(some_condition){
      x <- x + 5
      z <- 12
      x <- y + 5
    }
    return(x)
  }

  test_res <- recover(dummy_fun)

  expect_equal(names(test_res), c("Failing line", "Objects in scope"))
  # It correctly fails here due to addition of "death" + 5 resulting in an error
  expect_equal(as.character(test_res$'Failing line'), c("<-", "x", "y + 5"))
  # This gets updated - correctly
  expect_equal(test_res$`Objects in scope`$x, 7)
  expect_equal(test_res$`Objects in scope`$y, "death")
  # This gets updated correctly too
  expect_equal(test_res$`Objects in scope`$z, 12)

  test_res <- recover( dummy_fun,
                       args = list( x = 5,
                                    y ="fly",
                                    z = 2 ),
                       return.all = FALSE )
  expect_equal(test_res, 1)

})


