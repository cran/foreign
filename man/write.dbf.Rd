\name{write.dbf}
\alias{write.dbf}
\title{Write a DBF File}
\description{
  The function tries to write a data frame to a DBF file.
}
\usage{
write.dbf(dataframe, file, factor2char = TRUE)
}
\arguments{
  \item{dataframe}{a data frame object.}
  \item{file}{a file name to be written to.}
  \item{factor2char}{logical, default \code{TRUE}, convert factor columns to
    character: otherwise they are written as the internal integer codes.}
}
\details{
  Dots in column names are replaced by underlines in the DBF file, and
  names are truncated to 11 characters.

  Only vector columns of classes \code{"logical"}, \code{"numeric"},
  \code{"integer"}, \code{"character"}, \code{"factor"} and
  \code{"Date"} can be written.  Other columns should be converted to
  one of these.
  
  Maximum precision (number of digits including minus sign and decimal
  sign) for numeric is 19 - scale (digits after the decimal sign) which is
  calculated internally based on the number of digits before the decimal
  sign.
}
\value{
  Invisible \code{NULL}.
}
\references{
  \url{http://shapelib.maptools.org/}
}
\note{
  Other applications have varying abilities to read the data types used
  here.  Microsoft Access read \code{"numeric"}, \code{"integer"},
  \code{"character"} and \code{"Date"} fields, including recognizing
  missing values, but not \code{"logical"} (read as \code{0,-1}).
  Microsoft Excel understood all possible types but did not interpret missing
  values in character fields correctly (showing them as character nuls).
}

\author{
  Nicholas J. Lewin-Koh, modified by Roger Bivand and Brian Ripley;
  shapelib by Frank Warmerdam.
}

\seealso{
  \code{\link{read.dbf}}
}
\examples{
str(warpbreaks)
try1 <- paste(tempfile(), ".dbf", sep = "")
write.dbf(warpbreaks, try1, factor2char = FALSE)
in1 <- read.dbf(try1)
str(in1)
try2 <- paste(tempfile(), ".dbf", sep = "")
write.dbf(warpbreaks, try2, factor2char = TRUE)
in2 <- read.dbf(try2)
str(in2)
unlink(c(try1, try2))
\testonly{
DF <- data.frame(a=c(1:3, NA), b=c(NA, rep(pi, 3)),
                 c=c(TRUE,NA, FALSE, TRUE), d=c("aa", "bb", NA, "dd"),
                 e=I(c("a1", NA, NA, "a4")))
DF$f <- as.Date(c("2001-01-01", NA, NA, "2004-10-26"))
str(DF)
write.dbf(DF, try2)
in2 <- read.dbf(try2)
str(in2)
unlink(try2)
}}
\keyword{file}
