#!Rscript

################################
# Wrappers for common operations
################################

Rpipe.group <- function(d, key, expr) {
  expr <- substitute(expr)
  key <- eval(substitute(key), d)
  do.call(rbind, 
          by(d, key, function(x) Rpipe.eval(x, expr)))
}

Rpipe.aggregate <- function(d, key, ...) {
  key.name <- deparse(substitute(key))
  key <- eval(substitute(key), d)
  expr <- substitute(list(...))
  do.call(rbind,
          by(cbind(d, .key = key), key, function(x) {
             do.call(data.frame,
                     c(structure(list(x[1, ".key"]),
                                 names = key.name),
                            eval(expr, x)))
          }))
}

Rpipe.sort <- function(d, ..., decreasing = FALSE) {
  expr <- substitute(list(...))
  d[do.call("order", c(eval(expr, d), decreasing = decreasing)),]
}

################################
# Core operations
################################

Rpipe.prefix <- function(e1) {
  UseMethod("Rpipe.prefix")
}

Rpipe.prefix.default <- function(e1) {
  c("Rpipe.", "")
}

Rpipe.eval <- function(e1, expr) {
  if (as.character(expr[1]) == "(") {
    do.call(`%|%`, list(e1 = e1, e2 = expr[[2]]))
  } else if (as.character(expr[1]) == "%|%") {
    left <- do.call(`%|%`, list(e1 = e1, e2 = expr[[2]]))
    do.call(`%|%`, list(e1 = left, e2 = expr[[3]]))
  } else if (as.character(expr[1]) == "%&&%") {
    structure(list(e1 = do.call(`%|%`, list(e1 = e1, e2 = expr[[2]])),
                   e2 = do.call(`%|%`, list(e1 = e1, e2 = expr[[3]]))),
              class = "Rpipe.tuple")
  } else {
    prefixes <- Rpipe.prefix(e1)
    func.names <- paste(prefixes, expr[1], sep = "")
    lookups <- mget(func.names, 
                    envir = as.environment(-1),
                    ifnotfound = list(NULL),
                    inherits = TRUE)
    func.names <- which(sapply(lookups, function(x) !is.null(x)))
    if (length(func.names) == 0) {
      stop(paste("Could not find pipe function", expr[1]))
    }
    do.call(names(func.names)[1],
            c(list(e1), as.list(expr[-1])))
  }
}

`%|%` <- function(e1, e2) {
  expr <- substitute(e2)
  Rpipe.eval(e1, expr)
}

