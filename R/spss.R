### This file is part of the 'foreign' package for R.

###
###		Read SPSS system data files
###
### Copyright 2000-2002 Saikat DebRoy <saikat$stat.wisc.edu>
###			Douglas M. Bates <bates$stat.wisc.edu>,
###			Thomas Lumley
### Copyright 2007-8 R Core Development Team
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
                      use.missings = to.data.frame)
{

    trim <- function(strings, trim=TRUE)
	if (trim) sub(" +$","",strings) else strings

    rval <- .Call(do_read_SPSS, file)
    codepage <- attr(rval, "codepage")
    if(is.null(codepage)) codepage <- 2 # .por files
    if(!capabilities("iconv")) reencode <- FALSE
    if(is.character(reencode)) {
        cp <- reencode
        reencode <- TRUE
    } else if(codepage <= 500 || codepage >= 2000) {
        attr(rval, "codepage") <- NULL
        reencode <- FALSE
        # http://msdn.microsoft.com/en-us/library/ms776446(VS.85).aspx
    } else if(codepage == 65001) cp <- "UTF-8"
    else cp <- paste("CP", codepage, sep="")
    if(is.na(reencode)) reencode <- l10n_info()[["UTF-8"]]

    if(reencode) {
        names(rval) <- iconv(names(rval), cp, "")
        vl <- attr(rval, "variable.labels")
        nm <- names(vl)
        vl <- iconv(vl, cp, "")
        names(vl) <- iconv(nm, cp, "")
        attr(rval, "variable.labels") <- vl
        for(i in seq_along(rval)) {
            xi <- rval[[i]]
            if(is.character(xi)) rval[[i]] <- iconv(xi, cp, "")
        }
    }

    miss <- attr(rval, "missings")
    if(!is.null(miss)) {
        if(reencode) {
            nm <- names(miss)
            nm <- iconv(nm, cp, "")
            names(miss) <- iconv(nm, cp, "")
            for(i in seq_along(miss))
                if(is.character(miss[[i]]$value))
                   miss[[i]]$value <- iconv(miss[[i]]$value, cp, "")
            attr(rval, "missings") <- miss
        }
        if(use.missings)
            for(v in names(rval)) {
                tp <- miss[[v]]$type
                if(tp %in% "none") next
                if(tp %in% c("one", "two", "three")) {
                    xi <- rval[[v]]
                    other <- miss[[v]]$value
                    ## FIXME: do we need to worry about padding for string vals?
                    xi[xi %in% other] <- NA
                    rval[[v]] <- xi
                    ## NB: not much tested from here down
                } else if(tp == "low" || tp == "low+1") {
                    xi <- rval[[v]]
                    z <- miss[[v]]$value
                    if(tp == "low+1") xi[ xi <= z[1L] | xi == z[2L] ] <- NA
                    else  xi[xi <= z[1L]] <- NA
                    rval[[v]] <- xi
                } else if(tp == "high" || tp == "high+1") {
                    xi <- rval[[v]]
                    z <- miss[[v]]$value
                    if(tp == "high+1") xi[ xi >= z[1L] | xi == z[2L] ] <- NA
                    else xi[ xi >= z[1L] ] <- NA
                    rval[[v]] <- xi
                } else if(tp == "range" || tp == "range+1") {
                    xi <- rval[[v]]
                    z <- miss[[v]]$value
                    if(tp == "range+1")
                        xi[ xi >= z[1L] | xi <= z[2L] | xi[xi == z[3L]] ] <- NA
                    else
                        xi[ xi >= z[1L] | xi <= z[2L] ] <- NA
                    rval[[v]] <- xi
                } else
                    warning(gettextf("missingness type %s is not handled", tp),
                            domain = NA)
        }
    } else use.missings <- FALSE

    vl <- attr(rval,"label.table")
    if(reencode) names(vl) <- iconv(names(vl), cp, "")
    has.vl <- which(!sapply(vl, is.null))
    for(v in has.vl) {
	nm <- names(vl)[[v]]
	nvalues <- length(na.omit(unique(rval[[nm]])))
	nlabels <- length(vl[[v]])
        if(reencode && nlabels) {
            nm2 <- names(vl[[v]])
            vl[[v]] <- iconv(vl[[v]], cp, "")
            names(vl[[v]]) <- iconv(nm2, cp, "")
        }
        if(use.missings && !is.null(mv <- miss[[v]]$value))
            vl[[v]] <- vl[[v]][! vl[[v]] %in% mv]
	if (use.value.labels &&
	    (!is.finite(max.value.labels) || nvalues <= max.value.labels) &&
	    nlabels >= nvalues) {
	    rval[[nm]] <- factor(trim(rval[[nm]], trim_values),
                                 levels = rev(trim(vl[[v]], trim_values)),
				 labels = rev(trim(names(vl[[v]]), trim.factor.names)))
        } else
	    attr(rval[[nm]], "value.labels") <- vl[[v]]
    }
    if(reencode) attr(rval, "label.table") <- vl

    if (to.data.frame) {
	varlab <- attr(rval, "variable.labels")
	rval <- as.data.frame(rval)
	attr(rval, "variable.labels") <- varlab
        if(codepage > 500) attr(rval, "codepage") <- codepage
    }
    rval
}
