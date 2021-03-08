#' Recover the scope of a function that dies.
#'
#' @description Allows error recovery with specific lines and functions.
#'
#' @param fun A function to test. Please supply as either an unquoted name, or
#' as a character string.
#' @param args A list of optional function arguments to evaluate the function with.
#' @param return_all Whether to return all of the objects in the environment, or to only
#' print the last function called (which failed).
#'
#' @details This function exists due to difficulties in dealing with composed functions.
#' A composed function routinely returns an error message without listing the function
#' within that caused the error, leading to uninformative error messages. Rather
#' than try to rewrite everything that already exists (and works rather well), this
#' function allows you to evaluate everything that happened until the function crashed
#' and recover all of the objects in the environment when it crashed. You can thus reconstruct
#' the whole scenario and find the error message faster.
#' @return A list with the line caused the function to crash, and optionally the list of all
#' objects in scope when the crash happened. Alternatively print the failing line and return 1
#'  if **return_all** is set to **FALSE**. Return 0 if the function ran
#' succesfully (and print a message).
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
#'  recover(fun = dummy_fun, args = list(x = 5))
#'  # this also works with arguments unspecified (but they must have set defaults)
#'  recover(fun = dummy_fun)
#'  # and it will try to be helpful, though it is recommended to specify args where possible
#'
#'  # finally, recover can also return functions
#'
#' dummy_fun <- function( x = 2 )
#'  {
#'  x = x + 2
#'  z = x + 3
#'  lister <- function() ls()
#'  stop()
#'  print("Hello, github!")
#'  return(x)
#'  }
#'
#'  recover(fun = dummy_fun)
#'
recover <- function(fun,
                    args,
                    return_all = TRUE)
{
  # if character, convert to a name and look it up
  if (is.character(fun))
  {
    fun <- eval(as.name(fun))
  }
  # if args were not specified, fetch them manually
  if (missing(args)) {
    args <- find_args(fun)
  }
  res <- run_iterativelly(fun, args)
  # if we have no result, we return a happy, cheerful message
  if ( res[["succesful"]] ) {
    return("The function ran succesfully!")
  }
  result <- list("Failing line" =  res[["last_line"]] )

  if ( grepl(x = res[["last_line"]], pattern = "if")) {
    # we do have to rewrite the function a bit, though
    # so we create a helper function
    helper_fun <- function(){}
    body(helper_fun) <-
      parse(text = gsub(
        pattern = "if\\s+\\(.+\\)",
        replacement = "",
        x =  res[["last_line"]]
      ))

    # we get the arguments of the helper function via a call to partial
    get_args <- partial(fun, args, eval_point = res[["last_line_number"]]-1,
                        full_scope = TRUE)
    # and set those as the formals
    formals(helper_fun) <- get_args
    # next we figure out where we should put the ending bracket
    where_fix_brackets <- length(head(helper_fun))
    to_fix <- head(helper_fun)
    to_fix[where_fix_brackets] <- "}"
    # and we convert this thing to an actual function again.
    helper_fun <- eval(parse(text = to_fix))
    # and run recovery over this new function.
    return(recover(helper_fun, return_all = return_all))
  }

  if (return_all == TRUE)
  {
    # if we are returning everything, not just the failing line, we
    # return also a list of all of the available objects
    result <- c(result, list(res[["objects_in_scope"]]) )
    names(result)[2] <- c("Objects in scope")
  }
  # otherwise just return the offending line

  return(result)
}

