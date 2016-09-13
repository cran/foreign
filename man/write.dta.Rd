% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

\name{write.dta}
\alias{write.dta}
\title{Write Files in Stata Binary Format}
\usage{
write.dta(dataframe, file, version = 7L,
          convert.dates = TRUE, tz = "GMT",
          convert.factors = c("labels", "string", "numeric", "codes"))
}
\arguments{
  \item{dataframe}{a data frame.}
  \item{file}{character string giving filename.}
  \item{version}{integer: Stata version: 6, 7, 8 and 10 are supported,
    and 9 is mapped to 8, 11 to 10.}
  \item{convert.dates}{logical: convert \code{Date} and \code{POSIXct}
    objects: see section \sQuote{Dates}.}
  \item{tz}{timezone for date conversion.}
  \item{convert.factors}{how to handle factors.}
  } 
\description{
  Writes the data frame to file in the Stata binary
  format.  Does not write array variables unless they can be
  \code{\link{drop}}-ed to a vector.

  Frozen: will not support Stata formats after 10 (also used by Stata 11).
}
\details{
  The major difference between supported file formats in Stata versions
  is that version 7.0 and later allow 32-character variable names (5 and
  6 were restricted to 8-character names).  The \code{abbreviate}
  function is used to trim variable names to the permitted length.  A
  warning is given if this is needed and it is an error for the
  abbreviated names not to be unique.  Each version of Stata is claimed
  to be able to read all earlier formats.

  The columns in the data frame become variables in the Stata data set.
  Missing values are handled correctly.

  There are four options for handling factors.  The default is to use
  Stata \sQuote{value labels} for the factor levels.  With
  \code{convert.factors = "string"}, the factor levels are written as
  strings (the name of the value label is taken from the 
  \code{"val.labels"} attribute if it exists or the variable name 
  otherwise).  With \code{convert.factors = "numeric"} the numeric values
  of the levels are written, or \code{NA} if they cannot be coerced to
  numeric.  Finally, \code{convert.factors = "codes"} writes the
  underlying integer codes of the factors.  This last used to be the
  only available method and is provided largely for backwards
  compatibility.

  If the \code{"label.table"} attribute contains value labels with
  names not already attached to a variable (not the variable name or
  name from \code{"val.labels"}) then these will be written out as well.

  If the \code{"datalabel"} attribute contains a string, it is written out
  as the dataset label otherwise the dataset label is \code{"Written by R."}.

  If the \code{"expansion.table"} attribute exists expansion fields are
  written. This attribute should contain a list where each element is 
  string vector of length three. The first vector element contains the 
  name of a variable or "_dta" (meaning the dataset). The second element
  contains the characeristic name. The third contains the associated data.

  If the \code{"val.labels"} attribute contains a string vector with a
  string label for each variable then this is written as the variable
  labels.  Otherwise the variable names are repeated as variable labels.

  If the \code{"var.labels"} attribute contains a string vector with a
  string label for each variable then this is written as the variable
  labels.  Otherwise the variable names are repeated as variable labels.

  For Stata 8 or later use the default \code{version = 7} -- the only
  advantage of Stata 8 format over 7 is that it can represent multiple
  different missing value types, and \R doesn't have them.  Stata 10/11
  allows longer format lists, but \R does not make use of them.

  Note that the Stata formats are documented to use ASCII strings --
  \R does not enforce this, but use of non-ASCII character strings will
  not be portable as the encoding is not recorded.  Up to 244 bytes are
  allowed in character data, and longer strings will be truncated with a
  warning.

  Stata uses some large numerical values to represent missing
  values.  This function does not currently check, and hence integers
  greater than \code{2147483620} and doubles greater than
  \code{8.988e+307} may be misinterpreted by Stata.
}

\section{Dates}{
  Unless disabled by argument \code{convert.dates = FALSE}, \R date and
  date-time objects (\code{POSIXt} classes) are converted into the Stata
  date format, the number of days since 1960-01-01.  (For date-time
  objects this may lose information.)  Stata can be told that these are
  dates by \preformatted{format xdate \%td;}

  It is possible to pass objects of class \code{POSIXct} to Stata to be
  treated as one of its versions of date-times.  Stata uses the number
  of milliseconds since 1960-01-01, either excluding (format
  \code{\%tc}) or counting (format \code{\%tC}) leap seconds.  So
  either an object of class \code{POSICct} can be passed to Stata with
  \code{convert.dates = FALSE} and converted in Stata, or
  \code{315619200} should be added and then multiplied by \code{1000}
  before passing to \code{write.dta} and assigning format \code{\%tc}.
  Stata's comments on the first route are at
  \url{http://www.stata.com/manuals13/ddatetime.pdf}, but at the time of
  writing were wrong: \R uses POSIX conventions and hence does not count
  leap seconds.
}
\value{
  \code{NULL}
}
\references{
  Stata 6.0 Users Manual, Stata 7.0 Programming manual, Stata online
  help (version 8 and later, also \url{http://www.stata.com/help.cgi?dta_114}
  and \url{http://www.stata.com/help.cgi?dta_113}) describe the file formats.
} 
\author{
  Thomas Lumley and R-core members: support for value labels by
  Brian Quistorff.
}
\seealso{
  \code{\link{read.dta}},
  \code{\link{attributes}},
  \code{\link{DateTimeClasses}},
  \code{\link{abbreviate}}
}
\examples{
write.dta(swiss, swissfile <- tempfile())
read.dta(swissfile)
}
\keyword{file}
