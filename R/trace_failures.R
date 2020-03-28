#' Trace the failing objects of a function
#'
#' @description Trace back the manipulations of object inputs to a function
#'
#' @param
#' @param
#'
#'
#'


trace_failures <- function( fun, args, drop.unchanged.args = FALSE )
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

  if( substr(body(fun)[[res]],start = 1, stop = 10)[1] == "if"){

    helper_fun <- as.function(list( body(fun)[[res]][3]))

    get_args <- partial( fun, args, eval.point = res,
                         full.scope = TRUE)

    formals(helper_fun) <- get_args

    where_fix_brackets <- length(head(helper_fun))
    to_fix <- head(helper_fun)
    to_fix[where_fix_brackets] <- "}"

    helper_fun <- eval( parse(text = to_fix))

    body_of_call <- body(helper_fun)[[2]][[3]]
  }
  else{
    body_of_call <- body(fun)[[res]][[3]]
  }

  split_body <- unlist( strsplit( split = "\\s|[:punct:]",
                                  x = as.character(body_of_call)))
  replace_punct <- gsub(pattern = "\\+|\\-|\\(|\\)|\\^|\\*|\\/", replacement = " ", x = split_body)

  isolated_objects <- unlist(strsplit(replace_punct, split = " "))

  identifiers <- grep( pattern = "[a-z].?|[A-Z].?",
                       x = isolated_objects,
                       value = TRUE)

  are_functions <- unlist( lapply(identifiers, FUN = function(i){
    is.function(parse(text = i))
  }))

  identifiers <- identifiers[!are_functions]

  unique_identifiers <- unique(identifiers)

  where_changes_to_objects_were_made <- lapply(unique_identifiers,
     FUN = function(j){
  # recall that res is where the function dies - we want the changes before that
  unlist(lapply(1:(res - 1), FUN = function(i){
    current_line <- body(fun)[[i]]
    # skip first line if it is formatted correctly - ie no one is writing oneliners
      if( current_line == "{"){
        return()
      }
      else if(as.character(current_line[[2]]) == j){
        return(current_line)
      }
    }))
  })

  line_numbers <- lapply(unique_identifiers,
     FUN = function(j){
     unlist(lapply(1:(res - 1), FUN = function(i){
           current_line <- body(fun)[[i]]
   if( current_line == "{"){ return() }
   else if(as.character(current_line[[2]]) == j){ return(i) }
      }))
    })

  names(where_changes_to_objects_were_made) <- unique_identifiers

  result <- where_changes_to_objects_were_made
# we need the side effects, so do a loop like this.
  if(drop.unchanged.args){
    result <- Filter(f = function(i){!is.null(i)}, result)
    line_numbers <- Filter(f = function(i){!is.null(i)},line_numbers)
  }
  for(i in 1:length(result)){
    if(is.null(result[[i]])){
      result[[i]] <- "Was not changed during the run of function"
    }
    else{
      names(result[[i]]) <- paste("Line", line_numbers[[i]])
    }
  }

  return(result)
}



traceable_fun <- function( x = 1, y = 2, z = "death", a_123 = 3, a12 = 4)
{
  x <- x + 2
  y <- y + 3; x <- x*2
  z <- ((z -2)*y + x)^x +a_123 + a12

  return(z)

}




