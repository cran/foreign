\name{read.dta}
\alias{read.dta}
\title{Read Stata binary files}
\description{
  Reads a file in Stata version 5-8 or 7/SE binary format into a data frame. 
}
\usage{
read.dta(file, convert.dates = TRUE, tz = NULL,
         convert.factors = TRUE, missing.type = FALSE,
         convert.underscore=TRUE, warn.missing.labels=TRUE)
}
\arguments{
  \item{file}{a filename as a character string.}
  \item{convert.dates}{Convert Stata dates to \code{Date} class?}
  \item{tz}{Previously used to specify time zone. Now deprecated.}
  \item{convert.factors}{Use Stata value labels to create factors?
    (version 6.0 or later).}
  \item{missing.type}{For version 8 only, store information about
    different types of missing data?}
  \item{convert.underscore}{Convert \code{"_"} in Stata variable names
    to \code{"."} in R names?}
  \item{warn.missing.labels}{Warn if a variable is specified with value
    labels and those value labels are not present in the file.}
}
\details{
  The variables in the Stata data set become the columns of the data
  frame.  Missing values are correctly handled.  The data label,
  variable labels, and timestamp are stored as attributes of the data
  frame.  Nothing is done with variable characteristics.

  Optionally, Stata dates (\%d formats) are converted to \R's
  \code{Date} class and variables with Stata value labels are
  converted to factors. Ordinarily, \code{read.dta} will not convert
  a variable to a factor unless a label is present for every level. Use
  \code{convert.factors=NA} to override this. In any case the value
  label and format information is stored as attributes on the returned data frame. 

  Stata 8.0 has 27 different missing data values. If \code{missing.type}
  is \code{TRUE} a separate list is created with the same variable
  names as the loaded data. For string variables the list value is
  \code{NULL}. For other variables the value is \code{NA} where the
  observation is not missing and 0-26 when the observation is
  missing. This is attached as the code{"missing"} attribute of the
  returned value.
  
  The option to allow underscores in variable names may become the
  default in future versions now that R supports their use.
}
\value{
  a data frame
}
\references{
  Stata Users Manual (versions 5 & 6), Programming manual (version 7),
  or online help (version 8) describe the format of the files.
}

\author{Thomas Lumley}
\seealso{
  \code{\link{write.dta}},
  \code{\link{attributes}}
  \code{\link{Date}}
  \code{\link{factor}}
}
\examples{
data(swiss)
write.dta(swiss,swissfile<-tempfile())
read.dta(swissfile)
}
\keyword{file}
