#' Partially evaluation of a function
#'
#' @description Partially evaluates a function, returning only the objects which exist
#'  after the last line of partial evaluation.
#'
#' @param fun A function to partially evaluate.
#' @param args A list of the arguments necessary for the function to execute. See details.
#' @param eval.point The function line from which to return the result. A line number in the
#' body of the function, or a character string quoting a part of the function. See details.
#' @details Parameter **args** can be safely ignored for
#' functions which take no arguments explicitly, or for functions that have all their arguments
#' set. **eval.point** stands for the line in the function body to be replaced with a return -
#' this line is temporarily overwritten so a return can be made from it. See examples for usage.
#' @note If regex matching fails and your expression does not evaluate to anything valid,
#' please try shortening it or supplying a different part of it.
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



partial <- function( fun,
                          args,
                          eval.point )
{
  if(missing(args)){
    fill_args <- gsub(x = head(fun)[[1]], pattern = "function|\\(|\\)", replacement = "")
    fill_args <- strsplit( fill_args, split = ",")

    args <- list(fill_args)
  }
  if(is.null(eval.point))
  {
    stop("Please supply an evaluation point for me to return.")
  }
  if(is.numeric(eval.point) && eval.point != 0)
  {
    # use the line in the body of the function as the evaluation point
    if(length(eval.point) == 0){
      stop("Not a valid evaluation point, stopping.")
    }
    new_return <- eval.point

  }
  else if(is.character(eval.point))
  {
    # fix the brackets (which would normally fail due to regex matching)
    eval.point <- gsub(pattern = "\\(", replacement = "\\\\(", x = eval.point)
    eval.point <- gsub(pattern = "\\)", replacement = "\\\\)", x = eval.point)
#    eval.point <- gsub(pattern = "\\-", replacement = "\\\\-", x = eval.point)
  #  eval.point <- gsub(pattern = "\\)", replacement = "\\\\)", x = eval.point)


    # defined as a character string inside the body of the function
    new_return <- as.numeric(grep( pattern = eval.point, x = as.character( body( fun ) ) ))
    if(length(new_return) == 0){
      stop("Not a valid evaluation point, stopping.")
    }
    if(length(new_return) > 1){
      stop("Multiple evaluation matches - please select an unambiguous one.")
    }


  }
  else
  {
    stop("Please supply a valid evaluation point -
         a number indicating the line in the body of the function,
         or a string which can be parsed and matched against the
         body of the function.")
  }


  body(fun)[[new_return]] <- substitute( return(as.list(environment())))
  fun_abbr <- fun

   result <- do.call(fun_abbr, args)
  return(result)
}




