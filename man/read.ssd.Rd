\name{read.ssd}
\alias{read.ssd}
\title{ obtain a data frame from a SAS permanent dataset, via read.xport }
\description{
  generates a SAS program to convert the ssd contents to SAS transport format
  and then uses \code{read.xport} to obtain a dataframe
}
\usage{
read.ssd(libname, sectionnames, 
   tmpXport=tempfile(), tmpProgLoc=tempfile(), sascmd="sas")
}
\arguments{
  \item{libname}{ string defining the SAS library (usually a directory reference)}
  \item{sectionnames}{ vector of strings giving member names. These are
files in the `libname' directory. They will usually have a `.ssd0x' or
`.sas7bdat' extension, which should be omitted.}
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
## if there were some files on the web we could get a real
## runnable example
\dontrun{
R> list.files("trialdata")
 [1] "baseline.sas7bdat" "form11.sas7bdat"   "form12.sas7bdat"  
 [4] "form13.sas7bdat"   "form22.sas7bdat"   "form23.sas7bdat"  
 [7] "form3.sas7bdat"    "form4.sas7bdat"    "form48.sas7bdat"  
[10] "form50.sas7bdat"   "form51.sas7bdat"   "form71.sas7bdat"  
[13] "form72.sas7bdat"   "form8.sas7bdat"    "form9.sas7bdat"   
[16] "form90.sas7bdat"   "form91.sas7bdat"  
R> baseline<-read.ssd("trialdata","baseline")
R> form90<-read.ssd("trialdata","form90")
}
}
\keyword{file}
