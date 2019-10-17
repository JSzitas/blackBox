#' Recover the black box of a function that dies.
#'
#' @description Allows error recovery with specific lines and functions.
#'
#' @param fun A function to test. Please supply as either an unquoted name, or
#' as a character string.
#' @param args A list of optional function arguments to evaluate the function with.
#' @param return.all Whether to return all of the objects in the environment, or to only
#' return the last function called.
#'
#' @details This function exists due to difficulties in dealing with composed functions.
#' A composed function routinely returns an error message without listing the function
#' within that caused the error, leading to uninformative error messages. Rather
#' than try to rewrite everything that already exists (and works rather well), this
#' function allows you to evaluate everything that happened until the function crashed
#' and recover all of the objects in the environment when it crashed. You can thus reconstruct
#' the whole scenario and find the error message faster.
#' @return A list with the line caused the function to crash, and optionally the list of all
#' objects in scope when the crash happened. Alternatively 0 if the function ran succesfully.
#' @importFrom utils head
#' @export
#' @examples
#'
#'  dummy_fun <- function( x,
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
#'  # this also works with arguments unspecified
#'  recover(fun = dummy_fun)
#'  # and it will try to be helpful, though it is recommended to specify args where possible
#'
#'  # finally, recover can also return functions
#'
#' dummy_fun <- function( x )
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
#'






recover <- function( fun,
                      args,
                      return.all = TRUE )
{
  if(is.character(fun))
  {
    fun <- eval(as.name(fun))
  }
  if(missing(args)){
    fill_args <- gsub(x = head(fun)[[1]], pattern = "function|\\(|\\)", replacement = "")
    fill_args <- strsplit( fill_args, split = ",")

    args <- list(fill_args)
  }
# get the line on which it fails
res <- as.numeric(
    tryCatch(
      for (i in 1:length(body(fun)))
      {
        partial( fun, args, eval.point = i)
        iter_death <- i
      },
      error = function(e){ return(iter_death)}
    ))

if(length(res) == 0){
  result <- "The function ran succesfully!"
  cat(result)
  return(0)
}

if(return.all == TRUE)
{
  result <- list(body(fun)[[res]], partial(fun, args, eval.point = iter_death))
  names(result) <- c("Failing line", "Objects in scope")

}
else
{
  result <- list(body(fun)[[res]])
  names(result) <- c("Failing line")
}

return(result)
}

