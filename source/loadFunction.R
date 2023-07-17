LoadFunction <- function(file,...) {
  
  dots <- match.call(expand.dots = FALSE)$...
  dots <- sapply(dots, as.character)
  
  output <- lapply(dots, function(x,file){eval(parse(text=paste(x," <- function(x) {0}",sep="")),envir = .GlobalEnv)
    suppressMessages(insertSource(file, functions=x))
    eval(parse(text=paste(x," <- ",x,"@.Data",sep="")),envir = .GlobalEnv) },file=file)
  
}

UnloadFunction <- function(...) {
  
  dots <- match.call(expand.dots = FALSE)$...
  dots <- sapply(dots, as.character)
  
  output <- lapply(dots, function(x,file){eval(parse(text=paste("rm(",x,",envir = .GlobalEnv)",sep="")))},file=file)
  
}