% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

% Original by Nicholas Lewin-Koh and Roger Bivand,
% Changes for the foreign package (C) 2004-15 R Core Team

\name{read.dbf}
\alias{read.dbf}
\title{Read a DBF File}
\description{
  The function reads a DBF file into a data frame, converting character
  fields to factors, and trying to respect NULL fields.

  The DBF format is documented but not much adhered to.  There is is no
  guarantee this will read all DBF files.
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
  (\url{http://www.clicketyclick.dk/databases/xbase/format/}).
  Most of these follow the file structure used by Ashton-Tate's dBase
  II, III or 4 (later owned by Borland).

  \code{read.dbf} is based on C code from
  \url{http://shapelib.maptools.org/} which implements the
  \sQuote{XBASE} specification.  It can convert fields of type
  \code{"L"} (logical), \code{"N"} and \code{"F"} (numeric and float)
  and \code{"D"} (dates): all other field types are read as-is as
  character vectors.  A numeric field is read as an \R integer vector if
  it is encoded to have no decimals, otherwise as a numeric vector.  However,
  if the numbers are too large to fit into an integer vector, it is
  changed to numeric.  Note that is possible to read integers that cannot be
  represented exactly even as doubles: this sometimes occurs if IDs are
  incorrectly coded as numeric.
}

\value{
  A data frame of data from the DBF file; note that the field names are
  adjusted to use in R using \code{\link{make.names}(unique=TRUE)}.

  There is an attribute \code{"data_type"} giving the single-character
  dBase types for each field.
}

\references{
  \url{http://shapelib.maptools.org/}.
  
% site is 'unavailable' in Aug 2015.
%  The Borland file specification \emph{via} \url{http://www.wotsit.org},
%  currently at \url{http://www.wotsit.org/list.asp?fc=6}.
}

\author{
  Nicholas Lewin-Koh and Roger Bivand; shapelib by Frank Warmerdam
}

\note{
  Not to be able to read a particular \sQuote{DBF} file is not a bug:
  this is a convenience function especially for shapefiles.
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
