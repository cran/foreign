% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

\name{read.arff}
\alias{read.arff}
\title{Read Data from ARFF Files}
\description{
  Reads data from Weka Attribute-Relation File Format (ARFF) files.
}
\usage{
read.arff(file)
}
\arguments{
  \item{file}{a character string with the name of the ARFF file to read
    from, or a \code{\link{connection}} which will be opened if
    necessary, and if so closed at the end of the function call.}
}
\value{
  A data frame containing the data from the ARFF file.
}
\references{
  Attribute-Relation File Format
  \url{https://waikato.github.io/weka-wiki/formats_and_processing/arff/}.
}
\seealso{
  \code{\link{write.arff}};
  functions \code{write.arff} and \code{read.arff} in package
  \pkg{RWeka} which provide some support for logicals via conversion to
  or from factors.
}
\keyword{file}
\keyword{connection}
