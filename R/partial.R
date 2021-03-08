#' Partial evaluation of a function
#'
#' @description Partially evaluates a function, returning only the objects which exist
#'  after the last line of partial evaluation.
#'
#' @param fun A function to partially evaluate.
#' @param args A list of the arguments necessary for the function to execute. See details.
#' @param eval_point The function line from which to return the result. A line number in the
#' body of the function, or a character string quoting a part of the function. See details.
#' @param full_scope Whether to return everything that was in scope at the partial evaluation
#' point, defaults to **FALSE**.
#' @param fix_pattern Whether to used **fixed** for regex matching on **eval_point**, defaults to **FALSE**.
#' @details Parameter **args** can be safely ignored for
#' functions which take no arguments explicitly, or for functions that have all their arguments
#' set. **eval_point** stands for the line in the function body to be replaced with a return -
#' this line is temporarily overwritten so a return can be made from it. See examples for usage.
#' @note If regex matching fails and your expression does not evaluate to anything valid,
#' please try shortening it or supplying a different part of it.
#' @return A result of partial evaluations - the full environment containing every object
#' in scope at that evaluation if **full_scope** is **TRUE**, the last **call** otherwise.
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
#'          eval_point = 1 )
#' partial( fun = dummy_function,
#'          args = list(x = 10,z = FALSE, b = FALSE),
#'          eval_point = 3 )
#'
#' # works with partial string matching
#' partial( fun = dummy_function,
#'          args = list(x = 10,z = FALSE, b = FALSE),
#'          eval_point = "negat" )
#' # and semi-full string matching
#' partial( fun = dummy_function,
#'          args = list(x = 10,z = FALSE, b = FALSE),
#'          eval_point = "negative <- " )
#'
partial <- function(fun,
                    args,
                    eval_point = NULL,
                    full_scope = FALSE,
                    fix_pattern = FALSE)
{
  fun <- char_to_fun( fun )
  if (missing(args)) args <- find_args(fun)
  # if only a list was supplied, fill in the argument names
  args <- fix_argnames(fun, args)

  if (is.null(eval_point))
  {
    stop("Please supply an evaluation point for me to return.")
  }
  if (is.numeric(eval_point) && eval_point != 0)
  {
    # use the line in the body of the function as the evaluation point
    if (eval_point < 0) {
      stop("Not a valid evaluation point, stopping. Please supply a positive integer.")
    }
    new_return <- eval_point

  }
  else if (is.character(eval_point))
  {
    # fix the brackets (which would normally fail due to regex matching)
    eval_point <-
      gsub(pattern = "\\(",
           replacement = "\\\\(",
           x = eval_point)
    eval_point <-
      gsub(pattern = "\\)",
           replacement = "\\\\)",
           x = eval_point)

    # defined as a character string inside the body of the function
    new_return <- as.numeric(grep(
      pattern = eval_point,
      x = as.character(body(fun)),
      fixed = fix_pattern
    ))
    if (length(new_return) == 0) {
      stop("Not a valid evaluation point, stopping.")
    }
    if (length(new_return) > 1) {
      stop("Multiple evaluation matches - please select an unambiguous one.")
    }
  }
  else
  {
    stop(
      "Please supply a valid evaluation point -
         a number indicating the line in the body of the function,
         or a string which can be parsed and matched against the
         body of the function."
    )
  }

  if (full_scope) {
    body(fun)[[new_return]] <-
      substitute(return(as.list(environment())))
    fun_abbr <- fun

    result <- do.call(fun_abbr, args)
  }
  else{
    last_obj <- as.character(body(fun)[[new_return]])[2]
    body(fun)[[new_return + 1]] <-
      substitute(return(eval(parse(text = last_obj))))

    fun_abbr <- fun
    result <- do.call(fun_abbr, args)
  }

  return(result)
}
