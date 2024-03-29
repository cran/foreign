% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

\name{lookup.xport}
\alias{lookup.xport}
\title{Lookup Information on a SAS XPORT Format Library}
\description{
  Scans a file as a SAS XPORT format library and returns a list
  containing information about the SAS library.
}
\usage{
lookup.xport(file)
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
  \dQuote{The Record Layout of a Data Set in SAS Transport (XPORT) Format}
  available as
  \url{https://support.sas.com/content/dam/SAS/support/en/technical-papers/record-layout-of-a-sas-version-5-or-6-data-set-in-sas-transport-xport-format.pdf}
} 
\author{Saikat DebRoy}
\seealso{
  \code{\link{read.xport}}
}
\examples{
\dontrun{## no XPORT file is installed.
lookup.xport("test.xpt")
}
}
\keyword{file}
