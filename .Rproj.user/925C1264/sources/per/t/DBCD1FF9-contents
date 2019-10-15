#'
#'
#' @description
#'
#' @param fun
#' @param args
#' @param return.all
#'
#' @details
#' @return A list with the line caused the function to crash, and optionally the list of all
#' objects in scope when the crash happened. Alternatively 0 if the function ran succesfully.
#' @importFrom utils head
#' @export
#' @examples
#'






blackbox <- function( fun,
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
        partial_eval( fun, args, eval.point = i)
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
    result <- list(body(fun)[[res]], partial_eval(fun, args, eval.point = iter_death))
    names(result) <- c("Failing line", "Objects in scope")

  }
  else
  {
    result <- list(body(fun)[[res]])
    names(result) <- c("Failing line")
  }

  return(result)
}
