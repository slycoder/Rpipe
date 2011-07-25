################################
# Initial Rpipe interface for jjplot
################################

  ## Need to do checks on e1 = jjplot.state?
  ## both jjplot.stat.foo and jjplot.geom.foo

Rpipe.prefix.jjplot.state <- function(e1) {
  c("jjplot.stat",
    "jjplog.geom",
    "Rpipe",
    "")
}

jjplot.data <- function(data, ...) {
  structure(list(data = transform(data, ...)),
            class = "jjplot.state")
}

jjplot.range <- function(state, ...) {
  list(x = range(state$data$x),
       y = range(state$data$y))
}

jjplot.geom <- function(plot.function, 
                        range.function = jjplot.range) {
  ## Add extra info here!
  function(state, ...) {
    structure(list(plot.function = plot.function,
                   range = range.function(state, ...),
                   state = state),
              class = "jjplot.goem")
  }
}

## Logic for scales:
## Two aspects
##  1.) ticks
##  2.) range
##  range should be union of numeric ranges
##  ticks depend on type
##   numeric - pretty
##   factor - at ticks
##  cast numeric -> factor
##  check if factors equal.

jjplot.data(iris) %|% 
  aggregate(Species, Mean.Sepal.Width = mean(Sepal.Width)) %|%
  transform(x = Species, y = Mean.Sepal.Width)
