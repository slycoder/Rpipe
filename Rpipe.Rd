\name{Rpipe}
\alias{Rpipe}
\title{
  Manipulate a data frame through a series of pipe operations.
}
\description{
  
}
\usage{
df %|% operation()
}
\arguments{
  \item{df}{
    A data frame to operate on.  Note that this can be the output of another \code{Rpipe} stage.}
  \item{e2}{
%%     ~~Describe \code{e2} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
  Jonathan Chang <jonchang@fb.com>
}

\examples{
iris %|%
  subset(Sepal.Width > 3.0) %|%
    sort(Sepal.Length) %|%
      group(Species, head()) %|%
        aggregate(Species, Mean.Petal.Width = mean(Petal.Width))
}
