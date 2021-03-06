
find_args <- function( fun )
{
  # substitute over the head (head being the function signature, basically)
  fill_args <-
    gsub(x = head(fun)[[1]],
         pattern = "function|\\(|\\)",
         replacement = "")
  args <- list()
  #test if the function actually has any arguments
  if( !(gsub(x = fill_args, pattern = " ",replacement="") == ""))
  {
    # finally, split by commas
    fill_args <- strsplit(fill_args, split = ",")
    # and in case they have any assigned values, split those out
    split_names_values <- lapply(fill_args, function(i) {
      strsplit(i, split = "=", fixed = TRUE)
    })
    # grab only the argument names
    RHS <- lapply(split_names_values[[1]], function(i) {
      eval(parse(text = i[[2]]))
    })
    args <- RHS
  }
  return(args)
}
