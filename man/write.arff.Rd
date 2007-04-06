\name{write.arff}
\alias{write.arff}
\title{Write Data into ARFF Files}
\description{
  Writes data into Weka Attribute-Relation File Format (ARFF) files.
}
\usage{
write.arff(x, file, eol = "\n")
}
\arguments{
  \item{x}{the data to be written, preferably a matrix or data frame.
    If not, coercion to a data frame is attempted.}
  \item{file}{either a character string naming a file, or a connection.
    \code{""} indicates output to the standard output connection.}
  \item{eol}{the character(s) to print at the end of each line (row).}
}
\references{
  Attribute-Relation File Format
  \url{http://www.cs.waikato.ac.nz/~ml/weka/arff.html}
}  
\seealso{
  \code{\link{read.arff}}
}
\examples{
write.arff(iris, file = "")
}
\keyword{print}
\keyword{file}
