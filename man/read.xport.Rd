\name{read.xport}
\alias{read.xport}
\title{Read a SAS XPORT format library}
\description{
  Reads a file as a SAS XPORT format library and returns a list of
  data.frames.
}
\usage{
read.xport(file)
}
\arguments{
 \item{file}{character variable with the name of the file to read.  The
   file must be in SAS XPORT format.}
}
\value{
  A list with one component for each dataset in the XPORT format library.
}
\references{
  SAS Technical Support document TS-140:
  ``The Record Layout of a Data Set in SAS Transport (XPORT) Format''
  available as
  \url{http://ftp.sas.com/techsup/download/technote/ts140.html}.
}
\author{Saikat DebRoy}
\seealso{
  \code{\link{read.xport}}
}
\examples{
\dontrun{
read.xport("transport")
}
}
\keyword{file}
