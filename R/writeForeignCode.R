### This file is part of the 'foreign' package for R.

#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

write.foreign <-
    function(df, datafile, codefile, package = c("SPSS","Stata","SAS"), ...)
{
    do.call(paste("writeForeign", package, sep = ""),
            c(list(df = df, datafile = datafile, codefile = codefile), ...))
    invisible(NULL)
}

## we want ASCII quotes, not UTF-8 quotes here
adQuote <- function(x) paste("\"", x, "\"", sep = "")



## FIXME: 
# - Missing values
# - Date/Time
# - character variables > 255 chars
# - wrapper to write .sav directly

writeForeignSPSS <- function(df, datafile, codefile, varnames = NULL, maxchars = 32L)
{
    ## FIXME: re-write this to hold a connection open
    dfn <- lapply(df, function(x) if (is.factor(x)) as.numeric(x) else x)
    write.table(dfn, file = datafile, row.names = FALSE, col.names = FALSE,
                sep = ",", quote = FALSE, na = "", eol = ",\n")

    varlabels <- names(df)
    if (is.null(varnames)) {
        varnames <- abbreviate(names(df), maxchars)
        if (any(vapply(varnames, nchar, 0L) > maxchars))
            stop("I cannot abbreviate the variable names to 'maxchars' or fewer chars")
        if (any(varnames != varlabels))
            warning("some variable names were abbreviated")
    }

    varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", varnames)

    dl.varnames <- varnames
    chv <- vapply(df, is.character, NA)
    fav <- vapply(df, is.factor, NA)
    if (any(chv)) {
        lengths <- vapply(df[chv],function(v) max(c(nchar(v), 8L), na.rm=TRUE), 0L)                         
        lengths <- paste0("(A", lengths, ")")
        dl.varnames[chv] <- paste(dl.varnames[chv], lengths)
    }
    if (any(fav)) {
        dl.varnames[fav] <- paste(dl.varnames[fav], "(F8.0)")  # Factor-Format    
    }    
    if (any(chv) || any(fav)) {
        ## actually the rule is: prepend a star if a variable with type/size declaration
        ## follows on a variable without declaration; no star for first variable or variables 
        ## following other variables with declarations
        star <- ifelse(c(FALSE, diff(chv | fav) == 1)[chv | fav], " *", " ")
        dl.varnames[chv | fav] <- paste(star,  dl.varnames[chv | fav])
    }
  
    cat("SET DECIMAL=DOT.\n\n", file = codefile) # required if SPSS runs in a locale with DECIMAL=comma
    cat("DATA LIST FILE=", adQuote(datafile), " free (\",\")\n",
        file = codefile, append = TRUE)
    cat('ENCODING="Locale"\n', file = codefile, append = TRUE)

    ## No line longer than 251 chars:
    cat("/", paste(strwrap(paste(dl.varnames, collapse=" "), width=70), "\n"), " .\n\n", 
        file = codefile, append = TRUE)
    cat("VARIABLE LABELS\n", file = codefile, append = TRUE)
    cat(paste(varnames, adQuote(varlabels),"\n"), ".\n",
        file = codefile, append = TRUE)
    if (any(fav)) {
        cat("\nVALUE LABELS\n", file = codefile, append = TRUE)
        for(v in which(fav)){
            cat("/\n", file = codefile, append = TRUE)
            cat(varnames[v]," \n", file = codefile, append = TRUE, sep = "")
            levs <- levels(df[[v]])
            cat(paste(seq_along(levs), adQuote(levs), "\n", sep = " "),
                file = codefile, append = TRUE)
        }
        cat(".\n", file = codefile, append = TRUE)
    }

    ord <- vapply(df, is.ordered, NA)
    if(any(ord)) 
      cat("VARIABLE LEVEL", 
        paste(strwrap(paste(varnames[ord], collapse = ", "), width=70), "\n"), 
        "(ordinal).\n", file = codefile, append = TRUE)
    
    num <- vapply(df, is.numeric, NA)
    if(any(num)) 
      cat("VARIABLE LEVEL", 
        paste(strwrap(paste(varnames[num], collapse = ", "), width=70), "\n"), 
        "(scale).\n", file = codefile, append = TRUE)
    
    cat("\nEXECUTE.\n", file = codefile, append = TRUE)
}


writeForeignStata <- function(df, datafile, codefile)
{
    write.table(df, file = datafile, row.names = FALSE, col.names = FALSE,
                sep = ",", quote = FALSE, na = ".")
    nms <- names(df)
    factors <- vapply(df, is.factor, NA) | vapply(df, is.character, NA)
    formats <- paste(nms, "fmt", sep = "_")
    nms <- ifelse(factors, paste(nms,formats, sep = ":"), nms)

    cat("infile", nms, " using ", datafile,", automatic\n", file = codefile)
}
