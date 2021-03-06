#' Recover the types of objects in scope when a function that dies.
#'
#' @description Allows error recovery with specific lines and functions.
#'
#' @param fun A function to test. Please supply as either an unquoted name, or
#' as a character string.
#' @param args A list of optional function arguments to evaluate the function with.
#'
#' @details This function exists due to difficulties in dealing with composed functions.
#' A composed function routinely returns an error message without listing the function
#' within that caused the error, leading to uninformative error messages. Rather
#' than try to rewrite everything that already exists (and works rather well), this
#' function allows you to evaluate everything that happened until the function crashed
#' and recover the types of objects in the environment when it crashed. This is a
#' less verbose and heavy version of recovery
#' @return A list with the line caused the function to crash, and the list of types of
#' objects in scope when the crash happened. Alternatively 0 if the function ran succesfully.
#' @importFrom utils head
#' @export
#' @examples
#'
#'  dummy_fun <- function( x = 2,
#'                         y = "this_crashes",
#'                         z = 2 )
#'  {
#'  # these will run
#'  x = x + 2
#'  z = x + 3
#'  # this will crash due to y being type character
#'  x = y + z
#'  return(x)
#'  }
#'
#'  recover_types(fun = dummy_fun, args = list(x = 5))
#'  # this also works with arguments unspecified
#'  recover_types(fun = dummy_fun)
#'  # and it will try to be helpful, though it is recommended to specify args where possible
#'
recover_types <- function(fun, args) {
  if (is.character(fun))
  {
    fun <- eval(as.name(fun))
  }
  if (missing(args)) {
    args <- find_args(fun)
  }
  # get the line on which it fails
  # return(args)
  res <- as.numeric(tryCatch(
    for (i in 1:length(body(fun)))
    {
      partial(fun, args, eval_point = i)
      iter_death <- i
    },
    error = function(e) {
      return(iter_death + 1)
    }
  ))

  if (length(res) == 0) {
    result <- "The function ran succesfully!"
    cat(result)
    return(0)
  }

  get_objects <-
    partial(fun, args, eval_point = res, full_scope = TRUE)
  classes_get <- lapply(get_objects, FUN = class)
  names(classes_get) <- names(get_objects)
  result <- list(body(fun)[[res]], classes_get)
  names(result) <- c("Failing line", "Types")

  return(result)
}
