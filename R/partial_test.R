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
#' @param partial.matching Allow partial matching of comparison object or predicate function,
#' using attributes.
#' @details Parameter **args** can be safely ignored for
#' functions which take no arguments explicitly, or for functions that have all their arguments
#' set. **eval.point** stands for the line in the function body to be replaced with a return -
#' this line is temporarily overwritten so a return can be made from it.
#' The object to return is then compared, either using any form of predicate function,
#' or using an object to compare to.
#' If both function and object are used, the object is compared to the result of
#' the partial evaluation using the same function.
#' If **partial.matching** is enabled, the result of partial evaluation is compared
#' to the comparison objects, and all partial matches (using **attributes**) are
#' returned instead of a **TRUE/FALSE**.  See examples for usage.
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
#'  x_2 <- x + y - z
#'  TRUTHFULLY <- b
#'  negative <- (c-a) > 0
#'
#' return(y)
#' }
#'
#' # works with function body line number
#' partial( fun = dummy_function,
#'          args = list(x = 10,z = FALSE, b = FALSE),
#'          eval.point = 1 )
#' partial( fun = dummy_function,
#'          args = list(x = 10,z = FALSE, b = FALSE),
#'          eval.point = 3 )
#'
#' # works with partial string matching
#' partial( fun = dummy_function,
#'          args = list(x = 10,z = FALSE, b = FALSE),
#'          eval.point = "negat" )
#' # and semi-full string matching
#' partial( fun = dummy_function,
#'          args = list(x = 10,z = FALSE, b = FALSE),
#'          eval.point = "negative <- " )
#'



partial_test <- function( fun,
                          args,
                          eval.point,
                          compare.fun = NULL,
                          compare.object = NULL,
                          partial.matching = FALSE )
{
  if(is.null(compare.fun) && is.null(compare.object)){
    stop("Please supply a predicate function, or an object to compare to.")
  }
  result <- partial(fun, args, eval.point, full.scope = FALSE)

  if(!is.null(compare.fun) && is.null(compare.object)){
    result <- do.call( what = compare.fun, args = list(result) )
  }
  else if(!is.null(compare.fun) && !is.null(compare.object)){
    result <- do.call( what = compare.fun, args = list(result) )
    result <- identical( result, compare.object)
  }
  else{
    result <- identical( result, compare.object)
  }
  if( partial.matching ){

    result <- lapply(attributes(result), function(i){
      lapply(attributes(compare.object), function(j){
        identical(i,j)
      })
    })
  }

  return(result)
}
