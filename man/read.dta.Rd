\name{read.dta}
\alias{read.dta}
\title{Read Stata binary files}
\description{
  Reads a file in Stata version 5-7 binary format into a dataframe. 
}
\usage{
read.dta(file)
}
\arguments{
  \item{file}{a filename as a character string.}
}
\details{
  The variables in the Stata data set become the columns of the data
  frame.  Missing values are correctly handled.  The data label,
  variable labels, and timestamp are stored as attributes of the data
  frame.  Nothing is done with variable characteristics, print formats,
  or value labels.
}
\value{
  a data frame
}
\references{
  Stata Users Manual (versions 5 & 6) and Programming manual (version 7)
  describe the format of the files
}
\author{Thomas Lumley}
\seealso{
  \code{\link{write.dta}},
  \code{\link{attributes}}
}
\examples{
data(swiss)
write.dta(swiss,swissfile<-tempfile())
read.dta(swissfile)
}
\keyword{file}
