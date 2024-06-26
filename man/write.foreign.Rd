% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

\name{write.foreign}
\alias{write.foreign}
\title{Write Text Files and Code to Read Them}
\description{
This function exports simple data frames to other statistical packages by
writing the data as free-format text and writing a separate file of
instructions for the other package to read the data. 
}
\usage{
write.foreign(df, datafile, codefile,
              package = c("SPSS", "Stata", "SAS"), ...)
}
\arguments{
  \item{df}{A data frame}
  \item{datafile}{Name of file for data output}
  \item{codefile}{Name of file for code output}
  \item{package}{Name of package}
  \item{\dots}{Other arguments for the individual \code{writeForeign}
    functions}
}
\details{
The work for this function is done by
\code{foreign:::writeForeignStata}, \code{foreign:::writeForeignSAS} and
\code{foreign:::writeForeignSPSS}.  To add support for another package,
eg Systat, create a function \code{writeForeignSystat} with the same first
three arguments as \code{write.foreign}. This will be called from
\code{write.foreign} when \code{package="Systat"}.

Numeric variables and factors are supported for all packages: dates and
times (\code{Date}, \code{dates}, \code{date}, and \code{POSIXt}
classes) and logical vectors are also supported for SAS and characters
are supported for SPSS.

For \code{package="SAS"} there are optional arguments
\code{dataname = "rdata"} taking a string that will be the SAS data set
name, \code{validvarname} taking either \code{"V6"} or \code{"V7"},
and \code{libpath = NULL} taking a string that will be the directory where
the target SAS datset will be written when the generated SAS code been
run.


For \code{package="SPSS"} there is an optional argument \code{maxchars = 32L} 
taking an integer that causes the variable names (not variable labels) 
to be abbreviated to not more than \code{maxchars} chars.
For compatibility with SPSS version 12 and before, change this to \code{maxchars = 8L}.
In single byte locales with SPSS versions 13 or later, this can be set to \code{maxchars = 64L}.

For \code{package="SPSS"}, as a side effect, the decimal indicator is always set by 
\code{SET DECIMAL=DOT.} which may override user settings of the indicator or its default 
derived from the current locale.
}
\value{
  Invisible \code{NULL}.
}

\author{
  Thomas Lumley and Stephen Weigand
}
\examples{\dontrun{
datafile <- tempfile()
codefile <- tempfile()
write.foreign(esoph, datafile, codefile, package="SPSS")
file.show(datafile)
file.show(codefile)
unlink(datafile)
unlink(codefile)
}}
\keyword{file}

