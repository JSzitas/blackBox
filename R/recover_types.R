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
#'  recover_types(fun = dummy_fun, args = list(x = 5))
#'  # this also works with arguments unspecified
#'  recover_types(fun = dummy_fun)
#'  # and it will try to be helpful, though it is recommended to specify args where possible
#'






recover_types <- function( fun, args ){
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


    get_objects <- partial(fun, args, eval.point = iter_death, full.scope = TRUE)
    classes_get <- lapply(get_objects, FUN = class)
    names(classes_get) <- names(get_objects)
    result <- list(body(fun)[[res]], classes_get)
    names(result) <- c("Failing line", "Types")

  return(result)
}
