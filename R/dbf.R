### Copyright 2000-2001 (c) Nicholas Lewin-Koh
### Changes for foreign package (C) 2004 R Development Core Team
read.dbf <- function(file, as.is = FALSE)
{
    df <- .Call("Rdbfread", as.character(path.expand(file)), PACKAGE="foreign")
    onames <- names(df)
    inames <- make.names(onames, unique = TRUE)
    names(df) <- inames
    if (!(identical(onames, inames))) {
        for (i in 1:length(onames))
            if (!(identical(onames[i], inames[i])))
                cat("Field name: ", onames[i], " changed to: ",
                    inames[i], "\n")
    }
    data_types <- attr(df, "data_types")
    for(i in seq(along=onames))
        if(data_types[i] == "D") df[[i]] <- as.Date(df[[i]], format="%Y%m%d")
    if(!as.is) {
        df <- data.frame(lapply(df, function(x) {
            if(is.character(x)) {factor(x)} else x
        }))
       attr(df, "data_types") <-  data_types
    }
    df
}


write.dbf <- function(dataframe, file, factor2char = TRUE)
{
### need to check precision
    allowed_classes <- c("logical", "integer", "numeric", "character",
                         "factor", "Date")

    if (!is.data.frame(dataframe)) dataframe <- as.data.frame(dataframe)
    if (any(sapply(dataframe, function(x) !is.null(dim(x)))))
        stop("cannot handle matrix/array columns")
    cl <- sapply(dataframe, function(x) class(x[1]))
    asis <- cl == "AsIs"
    cl[asis & sapply(dataframe, mode) == "character"] <- "character"
    if(length(cl0 <- setdiff(cl, allowed_classes)))
        stop("data frame contains columns of unsupported class(es) ",
             paste(cl0, collapse = ","))
    m <- ncol(dataframe)
    DataTypes <- c(logical="L", integer="N", numeric="F", character="C",
                   factor=if(factor2char) "C" else "N", Date="D")[cl]
    for(i in seq(length = m)) {
        x <- dataframe[[i]]
        if(is.factor(x))
            dataframe[[i]] <-
                if(factor2char) as.character(x) else as.integer(x)
        else if (inherits(x, "Date"))
            dataframe[[i]] <- format(x, "%Y%m%d")
    }
    precision <- integer(m)
    scale <- integer(m)
    dfnames <- names(dataframe)
    for (i in seq(length = m)) {
        nlen <- nchar(dfnames[i])
        x <- dataframe[, i]
        if (is.logical(x)) {
            precision[i] <- 1
            scale[i] <- 0
        } else if (is.integer(x)) {
            rx <- range(x, na.rm = TRUE)
            mrx <- as.integer(max(ceiling(log10(abs(rx))))+3)
            precision[i] <- min(max(nlen, mrx), 19)
            scale[i] <- 0
        } else if (is.double(x)) {
            precision[i] <- 19
            rx <- range(x, na.rm = TRUE)
            mrx <- max(ceiling(log10(abs(rx))))
            scale[i] <- min(precision[i] - ifelse(mrx > 0, mrx+3, 3), 15)
        } else if (is.character(x)) {
            mf <- max(nchar(x[!is.na(x)]))
            precision[i] <- min(max(nlen, mf), 254)
            scale[i] <- 0
        } else stop("unknown column type in data frame")
    }
    invisible( .Call("DoWritedbf", as.character(file),
                     dataframe, as.integer(precision), as.integer(scale),
                     as.character(DataTypes),
                     PACKAGE = "foreign"))
}
