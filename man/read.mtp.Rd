\name{read.mtp}
\alias{read.mtp}
\title{Read a Minitab Portable Worksheet}
\description{
  Return a list with the data stored in a file as a Minitab Portable
  Worksheet.
}
\usage{
read.mtp(fname)
}
\arguments{
  \item{fname}{character variable with the name of the file to read.
    The file must be in Minitab Portable Worksheet format.}
}
\value{
  A list with one component for each column, matrix, or constant
  stored in the Minitab worksheet.
}
\references{
  FIXME: put references to the literature/web site here}
\author{Douglas M. Bates}
\seealso{\code{\link{read.stata}}}
\examples{
library(foreign)
\dontrun{
read.mtp("ex1-10.mtp")
}
}
\keyword{data}%-- one or more ...
