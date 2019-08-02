### This file is part of the 'foreign' package for R.
###
###     Read SPSS system data files
###
### Copyright 2000-2002 Saikat DebRoy <saikat$stat.wisc.edu>
###         Douglas M. Bates <bates$stat.wisc.edu>,
###         Thomas Lumley
### Copyright 2007-2017 R Core Development Team
### Patched 2013-01-02 following PR#15073 by Peggy Overcashier

### This file is part of the `foreign' package for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, a copy is available at
### http://www.r-project.org/Licenses/

read.spss <- function(file, use.value.labels = TRUE, to.data.frame = FALSE,
                      max.value.labels = Inf, trim.factor.names = FALSE,
                      trim_values = TRUE, reencode = NA,
                      use.missings = to.data.frame, sub = ".",
                      add.undeclared.levels = c("sort", "append", "no"),
                      duplicated.value.labels = c("append", "condense"),
                      duplicated.value.labels.infix = "_duplicated_", ...)
{

    add.undeclared.levels <- match.arg(add.undeclared.levels)
    duplicated.value.labels <- match.arg(duplicated.value.labels)
    trim <- function(strings, trim=TRUE)
        if (trim && is.character(strings)) sub(" +$", "", strings) else strings

    ## mappings taken from win-iconv
    knownCP <- c("UCS-2LE" = 1200, "UCS-2BE" = 1201,
                 "macroman" = 10000, " UCS-4LE" = 12000, "UCS-4BE" = 12001,
                 "koi8-r" = 20866, "koi8-u" = 21866,
                 "latin1" = 28591, "latin2" = 28592, "latin3" = 28593,
                 ## latin-9 seems most portable, but only on Windows
                 ## R >= 2.10.0.  libiconv doesn't know latin9.
                 "latin4" = 28594, "latin-9" = 28605,
                 "ISO-2022-JP" = 50221, "euc-jp" = 51932,
                 "UTF-8" = 65001,
                 "ASCII" = 20127,
                 ## pages known to glibc and libiconv
                 "CP1250" = 1250,
                 "CP1251" = 1251,
                 "CP1252" = 1252,
                 "CP1253" = 1253,
                 "CP1254" = 1254,
                 "CP1255" = 1255,
                 "CP1256" = 1256,
                 "CP1257" = 1257,
                 "CP1258" = 1258,
                 "CP874" = 874,
                 "CP936" = 936)

    if(length(grep("^(http|ftp|https)://", file))) {
        tmp <- tempfile()
        download.file(file, tmp, quiet = TRUE, mode = "wb")
        file <- tmp
        on.exit(unlink(file))
    }
    rval <- .Call(do_read_SPSS, file)
    codepage <- attr(rval, "codepage")
    if(is.null(codepage)) codepage <- 2 # .por files
    if(!capabilities("iconv")) reencode <- FALSE
    if(!identical(reencode, FALSE)) {
        cp <- "unknown"
        if(is.character(reencode)) {
            cp <- reencode
            reencode <- TRUE
        } else if(codepage == 20127) {
            reencode <- FALSE # ASCII
        } else if(m <- match(codepage, knownCP, 0L)) {
            cp <- names(knownCP)[m]
        } else if (codepage < 200) {
            ## small numbers are not codepages, and real codepages are large
            attr(rval, "codepage") <- NULL
            reencode <- FALSE
        } else cp <- paste("CP", codepage, sep="")
        if(is.na(reencode)){
            l10ni <- l10n_info()
            ## Do not reencode from UTF-8 in a UTF-8 locale and 
            ## not from latin1 in a latin1 locale
            reencode <- (l10ni[["UTF-8"]] && (codepage != 65001)) || 
                        (l10ni[["Latin-1"]] && (codepage != 28591))
        }

        if(reencode) {
            message(gettextf("re-encoding from %s", cp), domain = NA)
            names(rval) <- iconv(names(rval), cp, "", sub=sub)
            vl <- attr(rval, "variable.labels")
            nm <- names(vl)
            vl <- iconv(vl, cp, "", sub=sub)
            names(vl) <- iconv(nm, cp, "", sub=sub)
            attr(rval, "variable.labels") <- vl
            for(i in seq_along(rval)) {
                xi <- rval[[i]]
                if(is.character(xi)) rval[[i]] <- iconv(xi, cp, "", sub=sub)
            }
        }
    }

    miss <- attr(rval, "missings")
    vl <- attr(rval,"label.table")
    if(!is.null(miss)) {
        if(reencode) {
            nm <- names(miss)
            names(miss) <- iconv(nm, cp, "", sub=sub)
            for(i in seq_along(miss))
                if(is.character(miss[[i]]$value))
                   miss[[i]]$value <- iconv(miss[[i]]$value, cp, "", sub=sub)
            attr(rval, "missings") <- miss
        }
        if(use.missings)
            for(v in names(rval)) {
                tp <- miss[[v]]$type
                xi <- rval[[v]]
                z <- miss[[v]]$value

                ## Convert data (xi) to NA for values that either match a
                ## specified discrete missing value code or fall within
                ## the specified missing value range, if applicable.
                ##
                ## Added Oct. 2012: Retain value labels (vl[[v]]) only for
                ## codes that haven't been converted to NA in the data.

                if(tp %in% "none") next
                if(tp %in% c("one", "two", "three")) {
                    other <- miss[[v]]$value
                    ## FIXME: do we need to worry about padding for string vals?
                    xi[ xi %in% other ] <- NA
                    vl[[v]] <- vl[[v]][ !(vl[[v]] %in% other) ]
                } else if(tp == "low" || tp == "low+1") {
                    xi[ xi <= z[1L] ] <- NA
                    vl[[v]] <- vl[[v]][ as.numeric(vl[[v]]) > z[1L] ]
                    if(tp == "low+1"){
                        xi[ xi == z[2L] ] <- NA
                        vl[[v]] <- vl[[v]][ as.numeric(vl[[v]]) != z[2L] ]
                    }
                } else if(tp == "high" || tp == "high+1") {
                    xi[ xi >= z[1L] ] <- NA
                    vl[[v]] <- vl[[v]][ as.numeric(vl[[v]]) < z[1L] ]
                    if(tp == "high+1"){
                        xi[ xi == z[2L] ] <- NA
                        vl[[v]] <- vl[[v]][ as.numeric(vl[[v]]) != z[2L] ]
                    }
                } else if(tp == "range" || tp == "range+1") {
                    xi[ xi >= z[1L] & xi <= z[2L] ] <- NA
                    vl[[v]] <- vl[[v]][ as.numeric(vl[[v]]) < z[1L] | as.numeric(vl[[v]]) > z[2L] ]
                    if(tp == "range+1"){
                        xi[ xi == z[3L] ] <- NA
                        vl[[v]] <- vl[[v]][ as.numeric(vl[[v]]) != z[3L] ]
                    }
                } else
                    warning(gettextf("missingness type %s is not handled", tp),
                            domain = NA)
                rval[[v]] <- xi
        }
    } else use.missings <- FALSE

    if(reencode) names(vl) <- iconv(names(vl), cp, "", sub=sub)
    has.vl <- which(!vapply(vl, is.null, NA))
    for(v in has.vl) {
        nm <- names(vl)[[v]]
        vlv <- vl[[v]]
        nlabels <- length(vlv)
        if(reencode && nlabels) {
            nm2 <- names(vlv)
            vl[[v]] <- vlv <- iconv(vlv, cp, "", sub=sub)
            names(vl[[v]]) <- names(vlv) <- iconv(nm2, cp, "", sub=sub)
        }
        
        newlevels <- rev(trim(vlv, trim_values))
        newrval <- trim(rval[[nm]], trim_values)

        uniquevalues <- na.omit(unique(newrval))
        nvalues <- length(uniquevalues)

        ## We may have nlabels == nvalues but they do not always match,
        ## e.g. in case 2 labels are duplicated, hence be careful:
        if (use.value.labels && (add.undeclared.levels != "no" || all(uniquevalues %in% c(newlevels, ""))) &&
            (!is.finite(max.value.labels) || nvalues <= max.value.labels)) {

            newlabels <- rev(trim(names(vlv), trim.factor.names))
            if(add.undeclared.levels != "no" && !all(uniquevalues %in% c(newlevels, ""))){
                addlabels <- addlevels <- sort(uniquevalues[!(uniquevalues %in% c(newlevels, ""))])
                newlevels <- c(newlevels, addlevels)
                newlabels <- c(newlabels, addlabels)  
                if(add.undeclared.levels == "sort"){              
                    o <- order(newlevels)
                    newlevels <- newlevels[o]
                    newlabels <- newlabels[o]
                }
                warning("Undeclared level(s) ", paste(addlevels, collapse = ", "), " added in variable: ", nm)
            }
            dupnewlabels <- duplicated(newlabels)
            ## duplicated factor labels are no longer possible for R >= 3.4.0,
            ## hence adding two ways around
            ## - append: appends infix plus original level (that is unique)
            ## - condense: removes additional levels with identical labels and 
            ##   condenses to the first of all duplicated levels
            if(any(dupnewlabels)) {
              warning("Duplicated levels in factor ", nm, ": ", 
                      paste(newlabels[dupnewlabels], collapse=", "))
              if(duplicated.value.labels == "append"){
                newlabels[dupnewlabels] <- 
                    paste(newlabels[dupnewlabels], newlevels[dupnewlabels], 
                          sep = duplicated.value.labels.infix)
              }
              if(duplicated.value.labels == "condense"){
                for(d in unique(newlabels[dupnewlabels])){
                    dups <- newlabels %in% d
                    newrval[newrval %in% newlevels[dups]] <- newlevels[dups][1]
                }
                newlabels <- newlabels[!dupnewlabels]
                newlevels <- newlevels[!dupnewlabels]
              }
            }
            rval[[nm]] <- factor(newrval,
                                 levels = newlevels,
                                 labels = newlabels)
        } else
            attr(rval[[nm]], "value.labels") <- vl[[v]]
    }
    if(reencode) attr(rval, "label.table") <- vl

    if (to.data.frame) {
        varlab <- attr(rval, "variable.labels")
        rval <- as.data.frame(rval, ...)
        attr(rval, "variable.labels") <- varlab
        if(codepage > 500) attr(rval, "codepage") <- codepage
    }
    rval
}
