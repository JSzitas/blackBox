#' Partial test of a function
#'
#' @description Partially evaluates a function, returning only the objects which exist
#'  after the last line of partial evaluation and compares them to expected output
#'
#' @param fun A function to partially evaluate.
#' @param args A list of the arguments necessary for the function to execute. See details.
#' @param eval.point The function line from which to return the result. A line number in the
#' body of the function, or a character string quoting a part of the function. See details.
#' @param compare.fun A predicate function to compare to, defaults to NULL.
#' @param compare.object An object to compare to, defaults to NULL.
#'
#' @details Parameter **args** can be safely ignored for
#' functions which take no arguments explicitly, or for functions that have all their arguments
#' set. **eval.point** stands for the line in the function body to be replaced with a return -
#' this line is temporarily overwritten so a return can be made from it.
#' The object to return is then compared, either using any form of predicate function,
#' or using an object to compare to.
#' If both function and object are used, the object is compared to the result of
#' the partial evaluation using the same function. See examples for usage.
#' @note If regex matching fails and your expression does not evaluate to anything valid,
#' please try shortening it or supplying a different part of it. (Or specifying the correct
#' line number.)
#'
#' @export
#' @examples
#'
#' # create a dummy function
#' dummy_function <- function( x,     y = 2,    z,
#'                             a = 5, b = TRUE, c = 10 )
#' {
#'   x_2 <- x + y - z
#'   TRUTHFULLY <- b
#'   negative <- (c-a) > 0
#'   return(y)
#'   }
#'
#' # works with function body line number
#' partial_test( fun = dummy_function,
#'               args = list(x = 10,z = FALSE, b = FALSE),
#'               eval.point = 1,
#'               compare.obj =  NA)
#'
#' partial_test( fun = dummy_function,
#'               args = list(x = 10,z = FALSE, b = FALSE),
#'               eval.point = 3,
#'               compare.object = FALSE)
#'
#'
#' # works with partial string matching
#' partial_test( fun = dummy_function,
#'               args = list(x = 10,z = FALSE, b = FALSE),
#'               eval.point = "negat",
#'               compare.object = TRUE )
#'
#' # and semi-full string matching
#' partial_test( fun = dummy_function,
#'               args = list(x = 10,z = FALSE, b = FALSE),
#'               eval.point = "negative <- ",
#'               compare.object = TRUE )
#'



partial_test <- function( fun,
                          args,
                          eval.point,
                          compare.fun = NULL,
                          compare.object = NULL )
{
  if(is.null(compare.fun) && is.null(compare.object)){
    stop("Please supply a predicate function, or an object to compare to.")
  }
  result <- partial(fun, args, eval.point, full.scope = FALSE)

  if(!is.null(compare.fun) && is.null(compare.object)){
    result <- do.call( what = compare.fun, args = list(result) )
  }
  else{
    result <- identical( result, compare.object)
  }

  return(result)
}
