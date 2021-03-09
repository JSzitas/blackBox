#' Trace the failing objects of a function
#'
#' @description Trace back the manipulations of object inputs to a function
#'
#' @param fun The function to trace.
#' @param args An optional list of argument names to trace - by default, all arguments
#' are traced throughout the function.
#' @param drop_unchanged_args Whether arguments that stay constant should be returned.
#' defaults to **FALSE**.
#' @return A list of all the objects that existed within the function environment,
#' including the line numbers denoting the lines where the changes happened,
#' or a full trace of all of the changes to arguments throughout the function
#' (if drop_unchanged_args is set to **TRUE**).
#' @export
trace_failures <- function(fun, args, drop_unchanged_args = FALSE)
{
  if (is.character(fun))
  {
    fun <- eval(as.name(fun))
  }
  if (missing(args)) {
    args <- find_args(fun)
  }
  res <- run_iterativelly(fun, args)
  # if we have no result, we return a happy, cheerful message
  if ( res[["succesful"]] ) {
    return("The function ran succesfully!")
  }
  res_line <- res[["last_line_number"]]-1

  if ( grepl(x = res[["last_line"]], pattern = "if")) {
      helper_fun <- as.function(list(body(fun)[[res_line]][3]))

      get_args <- partial(fun, args, eval_point = res_line,
                          full_scope = TRUE)

      formals(helper_fun) <- get_args

      where_fix_brackets <- length(head(helper_fun))
      to_fix <- head(helper_fun)
      to_fix[where_fix_brackets] <- "}"

      helper_fun <- eval(parse(text = to_fix))

      body_of_call <- body(helper_fun)[[2]][[3]]
  }
  else{
    body_of_call <- body(fun)[[res_line]][[3]]
  }

  split_body <- unlist(strsplit(split = "\\s|[:punct:]",
                                x = as.character(body_of_call)))
  replace_punct <-
    gsub(pattern = "\\+|\\-|\\(|\\)|\\^|\\*|\\/",
         replacement = " ",
         x = split_body)

  isolated_objects <- unlist(strsplit(replace_punct, split = " "))

  identifiers <- grep(pattern = "[a-z].?|[A-Z].?",
                      x = isolated_objects,
                      value = TRUE)

  are_functions <- unlist(lapply(
    identifiers,
    FUN = function(i) {
      is.function(parse(text = i))
    }
  ))

  identifiers <- identifiers[!are_functions]

  unique_identifiers <- unique(identifiers)

  where_changes_to_objects_were_made <- lapply(
    unique_identifiers,
    FUN = function(j) {
      # recall that res is where the function dies - we want the changes before that
      unlist(lapply(
        1:(res_line-1),
        FUN = function(i) {
          current_line <- body(fun)[[i]]
          # skip first line if it is formatted correctly - ie no one is writing oneliners
          if (current_line == "{") {
            return()
          }
          else if (as.character(current_line[[2]]) == j) {
            return(current_line)
          }
        }
      ))
    }
  )

  line_numbers <- lapply(
    unique_identifiers,
    FUN = function(j) {
      unlist(lapply(
        1:(res_line-1),
        FUN = function(i) {
          current_line <- body(fun)[[i]]
          if (current_line == "{") {
            return()
          }
          else if (as.character(current_line[[2]]) == j) {
            return(i)
          }
        }
      ))
    }
  )

  names(where_changes_to_objects_were_made) <- unique_identifiers

  result <- where_changes_to_objects_were_made
  # we need the side effects, so do a loop like this.
  if (drop_unchanged_args) {
    result <- Filter(
      f = function(i) {
        !is.null(i)
      },
      result
    )
    line_numbers <-
      Filter(
        f = function(i) {
          !is.null(i)
        },
        line_numbers
      )
  }
  for (i in 1:length(result)) {
    if (is.null(result[[i]])) {
      result[[i]] <- "Was not changed during the run of function"
    }
    else{
      names(result[[i]]) <- paste("Line", line_numbers[[i]])
    }
  }

  return(result)
}
