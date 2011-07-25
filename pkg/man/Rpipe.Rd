\name{Rpipe}
\alias{Rpipe}
\title{
  Manipulate a data frame through a series of pipe operations.
}
\description{
  This provides an alternative syntax for calling a series of operations successively on data frames.  Rather than nesting/composing function calls, this implements a pipe operator similar to that in command shells.  This allows users to generate data frames by applying stages of data frame operations one after the other. 
}
\usage{
df %|% operation()
}
\arguments{
  \item{df}{
    A data frame to operate on.  Note that this can be the output of another \code{Rpipe} stage.}
  \item{operation}{
  Any function call which takes a data frame and returns a data frame.  Note that in addition to typical operations such as subset, this package also makes available/changes the semantics of \code{group}, \code{aggregate}, and \code{sort}.
}
}
\value{
  A data frame resulting from calling \code{operation} with \code{df} inserted as the first argument.
}
\author{
  Jonathan Chang <jonchang@fb.com>
}

\examples{
iris \%|\% 
  subset(Sepal.Width > 3.0) \%|\% 
  sort(Sepal.Length) \%|\% 
  group(Species, head()) \%|\% 
  aggregate(Species, Mean.Petal.Width = mean(Petal.Width))
}
