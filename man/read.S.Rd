\name{S3 read functions}
\alias{data.restore}
\alias{read.S}
% \alias{SModeNames}
\title{Read an S3 Binary File}
\description{
  Reads binary data files or \code{data.dump} files that were produced
  in S version 3.
}
\usage{
  data.restore(file, print = FALSE, verbose = FALSE, env = .GlobalEnv)
  read.S(file)
}
\arguments{
  \item{file}{the filename of the S-PLUS \code{data.dump} or binary
    file.}
  \item{print}{whether to print the name of each object as read from the
    file.}
  \item{verbose}{whether to print the name of every subitem within each
    object.}
  \item{env}{environment within which to create the restored object.}
}
\value{
  an R version of the S3 object.
}
\details{
  \code{read.S} can read the binary files produced in most recent
  versions of S-PLUS on either Windows (versions 3.x, 4.x, 2000) or Unix
  (version 3.x with 4 byte integers).  It automatically detects whether
  the file was produced on a big- or little-endian machine and adapts
  itself accordingly.\cr
  \code{data.restore} can read a similar range of files produced by
  \code{data.dump}, for newer versions of S-PLUS, those from
  \code{data.dump(....., oldStyle=TRUE)}.

  Not all S3 objects can be handled in the current version.  The most
  frequently encountered exceptions are functions and expressions; you
  will also have trouble with objects that contain model formulas.  In
  particular, comments will be lost from function bodies, and the
  argument lists of functions will often be changed.

%  The \code{SModeNames} variable contains a list of the modes of
%  variables as used in S3.
}
\author{
  Duncan Murdoch
}
\examples{
\dontrun{read.S(file.path("_Data", "myobj"))
data.restore("dumpdata", print = TRUE)
}}
\keyword{data}
\keyword{file}
