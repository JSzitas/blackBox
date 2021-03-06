
test_that("Tracing failures works with drop_unchanged_args == TRUE", {

  traceable_fun <- function( x = 1, y = 2, z = "death", a_123 = 3, a12 = 4)
  {
    x <- x + 2
    y <- y + 3; x <- x*2
    z <- ((z -2)*y + x)^x +a_123 + a12

    return(z)

  }

  result <- trace_failures(traceable_fun, drop_unchanged_args = TRUE)

  expect_equal(length(result), 2)
  expect_true(is.list(result[[1]]))
  expect_equal(names(result), c("y","x"))
  expect_true(result[["y"]]$`Line 3` == "y <- y + 3" )
  expect_true(result[["x"]]$`Line 2` == "x <- x + 2" )
  expect_true(result[["x"]]$`Line 4` == "x <- x * 2" )
})

test_that("Tracing failures works with drop_unchanged_args == FALSE", {

  traceable_fun <- function( x = 1, y = 2, z = "death", a_123 = 3, a12 = 4)
  {
    x <- x + 2
    y <- y + 3; x <- x*2
    z <- ((z -2)*y + x)^x +a_123 + a12

    return(z)
  }

  result <- trace_failures(traceable_fun)

  expect_equal(length(result), 5)
  expect_true(is.character(result[[1]]))
  expect_true(is.list(result[[3]]))
  expect_equal(names(result), c("z","y","x","a_123","a12"))
  expect_true(result[["y"]]$`Line 3` == "y <- y + 3" )
  expect_true(result[["x"]]$`Line 2` == "x <- x + 2" )
  expect_true(result[["x"]]$`Line 4` == "x <- x * 2" )
  expect_equal(result[[1]], "Was not changed during the run of function")
  expect_equal(result[[4]], "Was not changed during the run of function")
  expect_equal(result[[5]], "Was not changed during the run of function")
})

test_that("A single conditional is not a problem",{

  traceable_fun_2 <- function( x = 1, y = 2, z = "death", a_123 = 3, a12 = 4, conditonal = TRUE )
  {
    x <- x + 2
    y <- y + 3; x <- x*2
    if(conditional){
      z <- ((z -2)*y + x)^x +a_123 + a12
    }
    return(z)
  }

  result <- trace_failures(traceable_fun_2)

  expect_equal(length(result), 5)
  expect_true(is.character(result[[1]]))
  expect_true(is.list(result[[3]]))
  expect_equal(names(result), c("z","y","x","a_123","a12"))
  expect_true(result[["y"]]$`Line 3` == "y <- y + 3" )
  expect_true(result[["x"]]$`Line 2` == "x <- x + 2" )
  expect_true(result[["x"]]$`Line 4` == "x <- x * 2" )
  expect_equal(result[[1]], "Was not changed during the run of function")
  expect_equal(result[[4]], "Was not changed during the run of function")
  expect_equal(result[[5]], "Was not changed during the run of function")
})

test_that("Tracing successes works",{

  traceable_fun <- function( x = 1, y = 2, z = 2, a_123 = 3, a12 = 4)
  {
    x <- x + 2
    y <- y + 3; x <- x*2
    z <- ((z -2)*y + x)^x +a_123 + a12

    return(z)

  }

  result <- trace_failures(traceable_fun)
  expect_equal(result, "The function ran succesfully!")
})
