\name{read.dta}
\alias{read.dta}
\title{Read Stata binary files}
\description{
  Reads a file in Stata version 5-7 or 7/SE binary format into a dataframe. 
}
\usage{
read.dta(file, convert.dates=TRUE,tz="GMT",convert.factors=TRUE)
}
\arguments{
  \item{file}{a filename as a character string.}
  \item{convert.dates}{Convert Stata dates to \code{POSIXct} class?}
  \item{tz}{timezone for date conversion}
  \item{convert.factors}{Use Stata value labels to create factors?
    (version 6.0 or later)}
}
\details{
  The variables in the Stata data set become the columns of the data
  frame.  Missing values are correctly handled.  The data label,
  variable labels, and timestamp are stored as attributes of the data
  frame.  Nothing is done with variable characteristics.

  Optionally, Stata dates (\%d formats) are converted to \R's
  \code{POSIXct} class and variables with Stata value labels are
  converted to factors. In any case the value label and format
  information is stored as attributes on the returned data frame. 
  
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
  \code{\link{DateTimeClasses}}
  \code{\link{factor}}
}
\examples{
data(swiss)
write.dta(swiss,swissfile<-tempfile())
read.dta(swissfile)
}
\keyword{file}
