\name{read.ssd}
\alias{read.ssd}
\title{ obtain a data frame from a SAS permanent dataset, via read.xport }
\description{
  generates a SAS program to convert the ssd contents to SAS transport format
  and then uses read.xport to obtain a dataframe
}
\usage{
read.ssd(libname, sectionnames, 
   tmpXport=tempfile(), tmpProgLoc=tempfile(), sascmd="sas")
}
\arguments{
  \item{libname}{ string defining the SAS library (usually a directory reference)}
  \item{sectionnames}{ vector of strings giving member names (usually has .ssd0x
    or .sas7bdat (?!) as suffix}
  \item{tmpXport}{ string: location where temporary xport format archive should reside --
    defaults to a randomly named file in /tmp, which will be removed}
  \item{tmpProgLoc}{ string: location where temporary conversion program should reside --
    defaults to a randomly named file in /tmp, which will be removed}
  \item{sascmd}{ string: defaults to "sas"}
}
\details{
  creates a SAS program and runs it
}
\value{
  a data frame if all goes well, or NULL with warnings and some
  enduring side effects (log file for auditing)
}
\author{VJ Carey \email{<stvjc@channing.harvard.edu>}}
\note{ error handling is primitive }
\seealso{\code{\link{read.xport}}}
\examples{
# hard to do in any generality.  if there are ssd files on the
# web we could pull one down to demonstrate in real time, or stick
# a set of platform-specific ssds in 'data' of the package
}
\keyword{file}
