
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

    LHS <- sapply(split_names_values[[1]], function(i) i[[1]] )
    LHS<- gsub(pattern = " ",
               replacement = "",
               x = LHS)
    args <- RHS
    names(args) <- LHS
  }
  return(args)
}

# dummy_fun <- function(x = 2,
#                       y = "this_crashes",
#                       z = 2)
# {
#   # these will run
#   x = x + 2
#   z = x + 3
#   if( TRUE )
#   {
#     x <- x+1
#   }
#   # this will crash due to y being type character
#   x = y + z
#   return(x)
# }

run_iterativelly <- function( fun, args  )
{
  # create environment with
  custom_env <- new.env()
  names_args <- gsub( x = names(args), pattern = " ", replacement = "")

  # assign objects to this environment, for instance
  for (i in 1:length(args))
  {
    assign(x = names_args[i],
           value = args[i][[1]],
           envir = custom_env)
  }

  to_compare <- as.character(body(fun))
  # remove whitespace from function
  fun_body <- to_compare
  fun_body <- gsub(pattern = " ",
                   replacement = "",
                   x = fun_body[2:length(fun_body)])
  if( grepl( x = fun_body[length(fun_body)], pattern = "return\\(.*"))
  {
    fun_body <- fun_body[1:(length(fun_body)-1)]
  }
  fun_body <-
    fun_body[grep(pattern = "^$",
                  x = fun_body,
                  invert = TRUE)]
  # run function line by line, slowly updating objects
  # figure out how to handle conditionals - it probably makes sense to just
  # replace them as soon as they are known
  res <- tryCatch({
    iter_death <- 1
    for (i in fun_body)
    {
      eval(parse(text = i), envir = custom_env)
      iter_death <- iter_death + 1
    }
    TRUE
  },
  error = function(e) {
    return(iter_death + 1)
  })
  # I will admit this comparison is a bit confusing- comparing a number to
  # TRUE yiels FALSE, so if res is TRUE, we succeeded, otherwise we failed
  success <- ifelse(res == TRUE, TRUE, FALSE )
  last_line <- ifelse( res == TRUE, length(body(fun)), res   )

  return(list(
    "succesful" = success,
    "last_line" = as.character(body(fun))[last_line],
    "last_line_number" = ifelse( res == TRUE, res, length(body(fun))),
    "objects_in_scope" = as.list.environment(custom_env)
  ))
}
