\name{read.spss}
\alias{read.spss}
\title{Read an SPSS data file}
\description{
  \code{read.spss} reads a file stored by the SPSS \code{save} and
  \code{export} commands and returns a list.
}
\usage{
read.spss(file, use.value.labels=TRUE, to.data.frame=FALSE)
}
\arguments{
  \item{file}{ character variable with the name of the file to read.}
  \item{use.value.labels}{ Convert variables with value labels into \R
    factors with those levels?}
  \item{to.data.frame}{return a data frame?}
}
\value{
  A list (or data frame) with one component for each variable in the saved data set.
}
\details{
  This uses modified code from the PSPP project for reading the SPSS
  formats.
}
\author{Saikat DebRoy}
\examples{
\dontrun{
read.spss("datafile")
read.spss("datafile",use.value.labels=FALSE)
}
}
\keyword{file}
