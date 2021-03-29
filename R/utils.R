
find_args <- function( fun )
{
  # as list converts from a pairlist object
  return(as.list(formals(fun)))
}

run_iterativelly <- function(fun, args)
{
  # create environment with
  custom_env <- new.env()
  names_args <- names(args)
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
  if (grepl(x = fun_body[length(fun_body)], pattern = "return\\(.*"))
  {
    fun_body <- fun_body[1:(length(fun_body) - 1)]
  }
  fun_body <-
    fun_body[grep(pattern = "^$",
                  x = fun_body,
                  invert = TRUE)]
  # run function line by line, slowly updating objects
  # figure out how to handle conditionals - it probably makes sense to just
  # replace them as soon as they are known
  res <- tryCatch({
    # for lines in the function body
    for (i in seq_len(length(fun_body)))
    {
      # parse and evaluate line
      eval(parse(text = fun_body[i]), envir = custom_env)
    }
    # if we get at the end, i is the last line, and the last line succeeded
    list( last_line = i, success = TRUE)
  },error = function(e){
    # if we fail, we need to increment
    return(list( last_line = i+1,
                 success = FALSE))
  })

  success <- res[["success"]]
  last_line <- res[["last_line"]]

  return(
    list(
      "succesful" = success,
      "last_line" = as.character(body(fun))[last_line],
      "last_line_number" = last_line,
      "objects_in_scope" = as.list.environment(custom_env)
    )
  )
}
