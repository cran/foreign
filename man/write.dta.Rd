\name{write.dta}
\alias{write.dta}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Write files in Stata binary format}
\usage{
write.dta(dataframe, filename)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{dataframe}{a data frame }
  \item{filename}{character string giving filename }
} 
\description{
  Writes the data frame to file in the Stata v6.0 binary format. Does
  not write matrix variables.
}
\details{
  The columns in the data frame become variables in the Stata data
  set. Missing values are correctly handled. Nothing is done with factor
  levels, which should end up as variable labels.
}
\value{ \code{NULL} }
\references{Stata v6.0 Users Manual describes the file format}
\author{Thomas Lumley}
\seealso{\code{\link{read.dta}},\code{\link{attributes}}}
\examples{
data(swiss)
write.dta(swiss,swissfile<-tempfile())
read.dta(swissfile)
}
\keyword{file}%-- one or more ...
