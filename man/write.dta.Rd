\name{write.dta}
\alias{write.dta}
\title{Write files in Stata binary format}
\usage{
write.dta(dataframe, file, version = 6)
}
\arguments{
  \item{dataframe}{a data frame.}
  \item{file}{character string giving filename.}
  \item{version}{Stata version: 6 and 7 are supported.}
} 
\description{
  Writes the data frame to file in the Stata version 6.0 or 7.0 binary
  format.  Does not write matrix variables.
}
\details{
  The columns in the data frame become variables in the Stata data set.
  Missing values are correctly handled.  Nothing is done with factor
  levels, which should end up as variable labels.  The major difference
  between versions is that 7.0 allows 32-character variable names.
}
\value{
  \code{NULL}
}
\references{
  Stata 6.0 Users Manual, Stata 7.0 Programming manual describe the file
  formats.
} 
\author{Thomas Lumley}
\seealso{
  \code{\link{read.dta}},
  \code{\link{attributes}}
}
\examples{
data(swiss)
write.dta(swiss,swissfile<-tempfile())
read.dta(swissfile)
}
\keyword{file}
