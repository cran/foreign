% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

\name{read.spss}
\alias{read.spss}
\title{Read an SPSS Data File}
\description{
  \code{read.spss} reads a file stored by the SPSS \code{save} or
  \code{export} commands.

  This was orignally written in 2000 and has limited support for changes
  in SPSS formats since (which have not been many).
}
\usage{
read.spss(file, use.value.labels = TRUE, to.data.frame = FALSE,
          max.value.labels = Inf, trim.factor.names = FALSE,
          trim_values = TRUE, reencode = NA, use.missings = to.data.frame, 
          sub = ".", add.undeclared.levels = c("sort", "append", "no"),
          duplicated.value.labels = c("append", "condense"),
          duplicated.value.labels.infix = "_duplicated_", ...)
}
\arguments{
  \item{file}{character string: the name of the file or URL to read.}
  \item{use.value.labels}{logical: convert variables with value labels
    into \R factors with those levels?  This is only done if there are
    at least as many labels as values of the variable (when values
    without a matching label are returned as \code{NA}).}
  \item{to.data.frame}{logical: return a data frame?}
  \item{max.value.labels}{logical: only variables with value labels and
    at most this many unique values will be converted to factors if
    \code{TRUE}.}
  \item{trim.factor.names}{logical: trim trailing spaces from factor levels?}
  \item{trim_values}{logical: should values and value labels have
    trailing spaces ignored when matching for \code{use.value.labels = TRUE}?}
  \item{reencode}{logical: should character strings be re-encoded to the
    current locale.  The default, \code{NA}, means to do so in UTF-8 or latin-1
    locales, only.  Alternatively a character string specifying an encoding to
    assume for the file.}
  \item{use.missings}{logical: should information on user-defined
    missing values be used to set the corresponding values to \code{NA}?}
  \item{sub}{character string: If not \code{NA} it is used by \code{\link{iconv}} 
    to replace any non-convertible bytes in character/factor input. 
    Default is \code{"."}. For back compatibility with \pkg{foreign} 
    versions <= 0.8-68 use \code{sub=NA}.}
  \item{add.undeclared.levels}{character: 
    specify how to handle variables with at least one value label and further 
    non-missing values that have no value label (like a factor levels in R).
    For \code{"sort"} (the default) it adds undeclared factor levels to the 
    already declared levels (and labels) and sort them according to level,
    for \code{"append"} it appends undeclared factor levels to declared levels 
    (and labels) without sorting, and
    for \code{"no"} this does not convert to factor in case of numeric SPSS levels 
    (not labels), and still converts to factor if the SPSS levels are characters 
    and \code{to.data.frame=TRUE}.
    For back compatibility with \pkg{foreign} versions <= 0.8-68 use 
    \code{add.undeclared.levels="no"} (not recommended as this may convert some 
    values with missing corresponding value labels to \code{NA}).}
  \item{duplicated.value.labels}{character: what to do with duplicated value 
    labels for different levels.
    For \code{"append"} (the default), the first original value label is kept 
    while further duplicated labels are renamed to 
    \code{paste0(label, duplicated.value.labels.infix, level)},
    for \code{"condense"}, all levels with identical labels are condensed into 
    exactly the first of these levels in R.
    Back compatibility with \pkg{foreign} versions <= 0.8-68 is not given as 
    R versions >= 3.4.0 no longer support duplicated factor labels.
    }
  \item{duplicated.value.labels.infix}{character: the infix used for labels of 
    factor levels with duplicated value labels in SPSS (default \code{"_duplicated_"}) 
    if \code{duplicated.value.labels="append"}.}
  \item{...}{passed to \code{\link{as.data.frame}} if \code{to.data.frame = TRUE}.}}
\value{
  A list (or optionally a data frame) with one component for each
  variable in the saved data set.

  If what looks like a Windows codepage was recorded in the SPSS file,
  it is attached (as a number) as attribute \code{"codepage"} to the
  result.

  There may be attributes \code{"label.table"} and
  \code{"variable.labels"}.  Attribute \code{"label.table"} is a named
  list of value labels with one element per variable, either \code{NULL}
  or a named character vector.  Attribute \code{"variable.labels"} is a
  named character vector with names the short variable names and
  elements the long names.
  
  If there are user-defined missing values, there will be a attribute
  \code{"Missings"}.  This is a named list with one list element per
  variable.  Each element has an element \code{type}, a length-one
  character vector giving the type of missingness, and may also have an
  element \code{value} with the values corresponding to missingness.
  This is a complex subject (where the \R and C source code for
  \code{read.spss} is the main documentation), but the simplest cases
  are types \code{"one"}, \code{"two"} and \code{"three"} with a
  corresponding number of (real or string) values whose labels can be
  found from the \code{"label.table"} attribute.  Other possibilities are
  a finite or semi-infinite range, possibly plus a single value.
  See also \url{http://www.gnu.org/software/pspp/manual/html_node/Missing-Observations.html#Missing-Observations}.
}
\details{
  This uses modified code from the PSPP project
  (\url{http://www.gnu.org/software/pspp/} for reading the SPSS formats.

  If the filename appears to be a URL (of schemes \samp{http:},
  \samp{ftp:} or \samp{https:}) the URL is first downloaded to a
  temporary file and then read.  (\samp{https:} is supported where
  supported by \code{\link{download.file}} with its current default
  \code{method}.)
  
  Occasionally in SPSS, value labels will be added to some values of a
  continuous variable (e.g. to distinguish different types of missing
  data), and you will not want these variables converted to factors.  By
  setting \code{max.value.labels} you can specify that variables with a
  large number of distinct values are not converted to factors even if
  they have value labels.  
  
  If SPSS variable labels are present, they are returned as the
  \code{"variable.labels"} attribute of the answer.

  Fixed length strings (including value labels) are padded on the right
  with spaces by SPSS, and so are read that way by \R.  The default
  argument \code{trim_values=TRUE} causes trailing spaces to be ignored
  when matching to value labels, as examples have been seen where the
  strings and the value labels had different amounts of padding.  See
  the examples for \code{\link{sub}} for ways to remove trailing spaces
  in character data.
  
  URL \url{http://msdn.microsoft.com/en-us/library/ms776446(VS.85).aspx}
  provides a list of translations from Windows codepage numbers to
  encoding names that \code{\link{iconv}} is likely to know about and so
  suitable values for \code{reencode}.  Automatic re-encoding is
  attempted for apparent codepages of 200 or more in a UTF-8 or latin-1 locale:
  some other high-numbered codepages can be re-encoded on most systems,
  but the encoding names are platform-dependent (see
  \code{\link{iconvlist}}).
}
\note{
  If SPSS value labels are converted to factors the underlying numerical
  codes will not in general be the same as the SPSS numerical
  values, since the numerical codes in R are always \eqn{1,2,3,\dots}.

  You may see warnings about the file encoding for SPSS \code{save}
  files: it is possible such files contain non-ASCII character data
  which need re-encoding.  The most common occurrence is Windows codepage
  1252, a superset of Latin-1.  The encoding is recorded (as an integer)
  in attribute \code{"codepage"} of the result if it looks like a
  Windows codepage.  Automatic re-encoding is done only in UTF-8 and latin-1
  locales: see argument \code{reencode}.
}
\author{Saikat DebRoy and the R-core team}
\seealso{
  A different interface also based on the PSPP codebase is available in
  package \pkg{memisc}: see its help for \code{spss.system.file}.
}
\examples{
(sav <- system.file("files", "electric.sav", package = "foreign"))
dat <- read.spss(file=sav) 
str(dat)   # list structure with attributes

dat <- read.spss(file=sav, to.data.frame=TRUE) 
str(dat)   # now a data.frame


### Now we use an example file that is not very well structured and 
### hence may need some special treatment with appropriate argument settings.
### Expect lots of warnings as value labels (corresponding to R factor labels) are uncomplete, 
### and an unsupported long string variable is present in the data
(sav <- system.file("files", "testdata.sav", package = "foreign"))

### Examples for add.undeclared.levels:
## add.undeclared.levels = "sort" (default):
x.sort <- read.spss(file=sav, to.data.frame = TRUE)
## add.undeclared.levels = "append":
x.append <- read.spss(file=sav, to.data.frame = TRUE, 
    add.undeclared.levels = "append")
## add.undeclared.levels = "no":
x.no <- read.spss(file=sav, to.data.frame = TRUE, 
    add.undeclared.levels = "no")

levels(x.sort$factor_n_undeclared)
levels(x.append$factor_n_undeclared)
str(x.no$factor_n_undeclared)


### Examples for duplicated.value.labels:
## duplicated.value.labels = "append" (default)
x.append <- read.spss(file=sav, to.data.frame=TRUE)
## duplicated.value.labels = "condense"
x.condense <- read.spss(file=sav, to.data.frame=TRUE, 
    duplicated.value.labels = "condense")

levels(x.append$factor_n_duplicated)
levels(x.condense$factor_n_duplicated)

as.numeric(x.append$factor_n_duplicated)
as.numeric(x.condense$factor_n_duplicated)

    
## Long Strings (>255 chars) are imported in consecutive separate variables 
## (see warning about subtype 14):
x <- read.spss(file=sav, to.data.frame=TRUE, stringsAsFactors=FALSE)

cat.long.string <- function(x, w=70) cat(paste(strwrap(x, width=w), "\n"))

## first part: x$string_500:
cat.long.string(x$string_500)
## second part: x$STRIN0:
cat.long.string(x$STRIN0)
## complete long string:
long.string <- apply(x[,c("string_500", "STRIN0")], 1, paste, collapse="")
cat.long.string(long.string)
}
\keyword{file}
