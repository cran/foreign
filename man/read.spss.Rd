\name{read.spss}
\alias{read.spss}
\title{Read an SPSS data file}
\description{
  \code{read.spss} reads a file stored by the SPSS \code{save} and
  \code{export} commands and returns a list.
}
\usage{
read.spss(file)
}
\arguments{
  \item{file}{ character variable with the name of the file to read.}
}
\value{
  A list with one component for each variable in the saved data set.
}
\details{
  This uses modified code from the PSPP project for reading the SPSS
  formats.
}
\author{Saikat DebRoy}
\examples{
\dontrun{
read.spss("datafile")
}
}
\keyword{file}
