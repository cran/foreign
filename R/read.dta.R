read.dta <- function(file){
    .External("do_readStata", file, PACKAGE = "foreign")
}

write.dta <- function(dataframe, file, version = 6) {
    if (any(sapply(dataframe, function(x) !is.null(dim(x)))))
        stop("Can't handle multicolumn columns")
    invisible(.External("do_writeStata", file, dataframe, version,
                        PACKAGE = "foreign"))
}
