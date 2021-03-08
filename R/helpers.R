

# if a function is given as a string, take it as a name and convert it back to
# a function
char_to_fun <- function(fun)
{
  if (is.character(fun))
  {
    fun <- eval(as.name(fun))
  }
  stopifnot(is.function(fun))
  return(fun)
}

# if arguments were given to a function, but without names,
# find the names of the function arguments
fix_argnames <- function( fun, args )
{
  if( is.null(names(args)))
  {
    names(args) <- names(find_args(fun))
  }
  return(args)
}
