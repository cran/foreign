\name{read.dbf}
\alias{read.dbf}
\title{Read a DBF File}
\description{
  The function reads a DBF file into a data frame, converting character
  fields to factors, and trying to respect NULL fields.
}
\usage{
read.dbf(file, as.is = FALSE)
}
\arguments{
  \item{file}{name of input file}
  \item{as.is}{should character vectors not be converted to factors?}
}
\details{
  DBF is the extension used for files written for the \sQuote{XBASE}
  family of database languages, \sQuote{covering the dBase, Clipper,
    FoxPro, and their Windows equivalents Visual dBase, Visual Objects,
    and Visual FoxPro, plus some older products}
  (\url{http://www.e-bachmann.dk/docs/xbase.htm}).  Most of these follow
  the file structure used by Ashton-Tate's dBase II, III or 4 (later
  owned by Borland).

  \code{read.dbf} is based on C code from
  \url{http://shapelib.maptools.org/} which implements the
  \sQuote{XBASE} specification.  It can convert fields of type
  \code{"L"} (logical), \code{"N"} and \code{"F"} (numeric and float)
  and \code{"D"} (dates): all other field types are read as-is as
  character vectors.  A numeric field are read as an \R integer vector if
  it is encoded to have no decimals, otherwise as a numeric vector.
}

\value{
  A data frame of data from the DBF file; note that the field names are
  adjusted to use in R using \code{\link{make.names}(unique=TRUE)}.

  There is an attribute \code{"data_type"} giving the single-character
  dBase types for each field.
}

\references{
  \url{http://shapelib.maptools.org/}.

  The Borland file specification \emph{via}
  \url{http://www.wotsit.org/search.asp?s=database}.
}

\author{
  Nicholas Lewin-Koh and Roger Bivand; shapelib by Frank Warmerdam
}

\seealso{
  \code{\link{write.dbf}}
}
\examples{
x <- read.dbf(system.file("files/sids.dbf", package="foreign")[1])
str(x)
summary(x)
}
\keyword{file}
