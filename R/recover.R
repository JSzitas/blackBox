#' Recover the scope of a function that dies.
#'
#' @description Allows error recovery with specific lines and functions.
#'
#' @param fun A function to test. Please supply as either an unquoted name, or
#' as a character string.
#' @param args A list of optional function arguments to evaluate the function with.
#' @param return.all Whether to return all of the objects in the environment, or to only
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
#'  if **return.all** is set to **FALSE**. Return 0 if the function ran
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

    split_names_values <- lapply(fill_args, function(i){
      strsplit(i, split = "=", fixed = TRUE)
    })
    RHS <- lapply(split_names_values[[1]], function(i){
      eval(parse(text = i[[2]]))
    })

    args <- RHS
  }
# get the line on which it fails
res <- as.numeric(
    tryCatch(
      for (i in 1:length(body(fun)))
      {
        partial( fun, args, eval.point = i)
        iter_death <- i
      },
      error = function(e){ return(iter_death+1)}
    ))

if(length(res) == 0){
  result <- "The function ran succesfully!"
  cat(result)
  return(0)
}

if(return.all == TRUE)
{
  result <- list(body(fun)[[res]], partial( fun, args, eval.point = res,
                                            full.scope = TRUE))
  names(result) <- c("Failing line", "Objects in scope")

}
else
{
  print(body(fun)[[res]])
  return(1)
}

return(result)
}

