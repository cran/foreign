\name{read.spss}
\alias{read.spss}
\title{Read an SPSS data file}
\description{
  \code{read.spss} reads a file stored by the SPSS \code{save} and
  \code{export} commands and returns a list.
}
\usage{
read.spss(file, use.value.labels=TRUE, to.data.frame=FALSE, max.value.labels=Inf)
}
\arguments{
  \item{file}{ character variable with the name of the file to read.}
  \item{use.value.labels}{ Convert variables with value labels into \R
    factors with those levels?}
  \item{to.data.frame}{return a data frame?}
  \item{max.value.labels}{Only variables with at most this many unique
    values will be converted to factors}
}
\value{
  A list (or data frame) with one component for each variable in the saved data set.
}
\details{
  This uses modified code from the PSPP project for reading the SPSS
  formats.

  Occasionally in SPSS value labels will be added to some values of a
  continuous variable (eg to distinguish different types of missing
  data), and you will not want these variables converted to factors.
  By setting \code{max.val.labels} you can specify that variables
  with a large number of distinct values are not converted to factors
  even if they have value labels. The value labels are then returned in
  the \code{"value.labels"} attribute of the variable.
  
}
\author{Saikat DebRoy}
\examples{
\dontrun{
read.spss("datafile")
## don't convert value labels to factor levels
read.spss("datafile",use.value.labels=FALSE)
## convert value labels to factors for variables with at most
## ten distinct values.
read.spss("datafile",max.val.labels=10)
}
}
\keyword{file}
