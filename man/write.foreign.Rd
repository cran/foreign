\name{write.foreign}
\alias{write.foreign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Write text files and code to read them.}
\description{
This function exports data frames to other statistical packages by
writing the data as free-format text and writing a separate file of
instructions for the other package to read the data. 
}
\usage{
write.foreign(df, datafile, codefile, package = c("SPSS","Stata"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{A data frame}
  \item{datafile}{Name of file for data output}
  \item{codefile}{Name of file for code output}
  \item{package}{Name of package}
  \item{\dots}{Other arguments for the individual \code{writeForeign} functions}
}
\details{
The work for this function is done by \code{foreign:::writeForeignStata} and
\code{foreign:::writeForeignSPSS}. To add support for another package,
eg SAS, create a function \code{writeForeignSAS} with the same first
three arguments as \code{write.foreign}. This will be called from
\code{write.foreign} when \code{package="SAS"}. 
}
\value{
  None
}

\author{Thomas Lumley}
\examples{
\dontrun{
datafile<-tempfile()
codefile<-tempfile()
write.foreign(esoph,datafile,codefile,package="SPSS")
file.show(datafile)
file.show(codefile)
unlink(datafile)
unlink(codefile)
}
}
\keyword{file}% at least one, from doc/KEYWORDS

