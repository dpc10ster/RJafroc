#
# AddArguments1 <- function(f,n){
#   # adds n arguments to a function f; returns that new function 
#   t <- paste("arg <- alist(", paste(names(formals(f)), collapse="=, "), "=, ",
#              paste(sapply(1:n, function(i) paste("zeta", i, "=", sep = "")), collapse = ", "),
#              ")", sep= "")
#   formals(f) <- eval(parse(text = t))
#   return(f)
# }

AddArguments2 <- function(f,n){
  # adds n arguments to a function f; returns that new function 
  t <- paste("arg <- alist(", paste(names(formals(f)), collapse="=, "), "=, ",
             paste(sapply(1:n, function(i) paste("zeta", i, "=", sep = "")), collapse = ", "),
             ")", sep= "")
  formals(f) <- eval(parse(text = t))
  return(f)
}


# from following website
# https://stackoverflow.com/questions/7572260/creating-function-arguments-from-a-named-list-with-an-application-to-stats4ml
AddArguments <- function(f,n){
  # adds n arguments to a function f; returns that new function 
  t <- paste("arg <- alist(", paste(names(formals(f)), collapse="=, "), "=, ",
            paste(sapply(1:n, function(i) paste("zetaFwd", i, "=", sep = "")), collapse = ", "),
            ")", sep= "")
  formals(f) <- eval(parse(text = t))
  return(f)
}

AddZetaXFwd <- function(f,n){
  # adds n arguments to a function f; returns that new function 
  t <- paste("arg <- alist(", paste(names(formals(f)), collapse="=, "), "=, ",
             paste(sapply(1:n, function(i) paste("zetaXFwd", i, "=", sep = "")), collapse = ", "),
             ")", sep= "")
  formals(f) <- eval(parse(text = t))
  return(f)
}

AddZetaYFwd <- function(f,n){
  # adds n arguments to a function f; returns that new function 
  t <- paste("arg <- alist(", paste(names(formals(f)), collapse="=, "), "=, ",
             paste(sapply(1:n, function(i) paste("zetaYFwd", i, "=", sep = "")), collapse = ", "),
             ")", sep= "")
  formals(f) <- eval(parse(text = t))
  return(f)
}

AddZetaX <- function(f,n){
  # adds n arguments to a function f; returns that new function 
  t <- paste("arg <- alist(", paste(names(formals(f)), collapse="=, "), "=, ",
             paste(sapply(1:n, function(i) paste("zetaX", i, "=", sep = "")), collapse = ", "),
             ")", sep= "")
  formals(f) <- eval(parse(text = t))
  return(f)
}

AddZetaY <- function(f,n){
  # adds n arguments to a function f; returns that new function 
  t <- paste("arg <- alist(", paste(names(formals(f)), collapse="=, "), "=, ",
             paste(sapply(1:n, function(i) paste("zetaY", i, "=", sep = "")), collapse = ", "),
             ")", sep= "")
  formals(f) <- eval(parse(text = t))
  return(f)
}