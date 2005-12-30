# Copyright 2004 by Roger Bivand
#

read.systat <- function(file, to.data.frame=TRUE)
{
    if (length(file) != 1) stop("only one file")
    if (!is.character(file)) stop("file must be character")
    res <- .Call(readSystat, as.character(file))
    if (to.data.frame) {
        comment <- NULL
        if (!is.null(attr(res, "comment")) &&
            (nchar(attr(res, "comment")) > 0))
            comment <- attr(res, "comment")
        res <- as.data.frame(res)
        if (!is.null(comment)) comment(res) <- comment
    }
    res
}
