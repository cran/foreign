\name{write.dta}
\alias{write.dta}
\title{Write files in Stata binary format}
\usage{
write.dta(dataframe, file, version = 6,convert.dates=TRUE, tz="GMT",
 convert.factors=c("labels","string","numeric","codes"))
}
\arguments{
  \item{dataframe}{a data frame.}
  \item{file}{character string giving filename.}
  \item{version}{Stata version: 6 and 7 are supported.}
  \item{convert.dates}{Convert \code{POSIXt} objects to Stata dates?}
  \item{tz}{timezone for date conversion}
  \item{convert.factors}{how to handle factors}
  } 
\description{
  Writes the data frame to file in the Stata version 6.0 or 7.0 binary
  format.  Does not write matrix variables.
}
\details{
 
 The major difference between Stata versions is that 7.0 allows 32-character
 variable names. The \code{abbreviate} function is used to trim long
 variables to the permitted length. A warning is given if this is needed and
 it is an error for the abbreviated names not to be unique.

  The columns in the data frame become variables in the Stata data set.
  Missing values are correctly handled.  Optionally, R date/time objects
 (\code{POSIXt} classes) are converted into the Stata format. This loses
 information -- Stata dates are in days since 1960-1-1.  \code{POSIXct}
 objects can be written without conversion but will not be understood as
 dates by Stata;  \code{POSIXlt} objects cannot be written without
 conversion.

  There are four options for handling factors. The default is to use
  Stata \code{value labels} for the factor levels.
  With \code{convert.factors="string"}, the factor levels are written as
  strings. With \code{convert.factors="numeric"} the numeric values
  of the levels are written, or \code{NA} if they cannot be coerced to
  numeric. Finally, \code{convert.factors="codes"} writes the underlying
  integer codes of the factors. This last used to be the only available
  method and is provided largely for backwards compatibility.
  
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
  \code{\link{attributes}},
  \code{\link{DateTimeClasses}},
  \code{\link{abbreviate}}
}
\examples{
data(swiss)
write.dta(swiss,swissfile<-tempfile())
read.dta(swissfile)
}
\keyword{file}
