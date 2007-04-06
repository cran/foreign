### Functions for reading and writing files in Weka ARFF format.

### <NOTE>
### String and evaluation types are enclosed by single quotes upon
### writing and enclosing single quotes are removed upon reading.
### Escaped single quotes inside single quotes may also occur.
### </NOTE>

read.arff <-
function(file)
{
    ## See read.table().
    if(is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if(!inherits(file, "connection")) 
        stop("Argument 'file' must be a character string or connection.")
    if(!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }

    ## Get header.
    col_names <- NULL
    col_types <- NULL
    col_dfmts <- character()
    line <- readLines(file, n = 1)
    while(length(line) > 0 && 
          (regexpr('^[[:space:]]*@(?i)data', line,
                   perl = TRUE) == -1)) {
        if(regexpr('^[[:space:]]*@(?i)attribute', line,
                   perl = TRUE) > 0) {
            con <- textConnection(line)
            line <- scan(con, character(), quiet = TRUE)
            close(con)
            if(length(line) < 3) 
                stop("Invalid attribute specification.")
            col_names <- c(col_names, line[2])
            if((type <- tolower(line[3])) == "date") {
                col_types <- c(col_types, "character")
                col_dfmts <- c(col_dfmts,
                               if(length(line) > 3)
                               ISO_8601_to_POSIX_datetime_format(line[4])
                               else "%Y-%m-%d %H:%M:%S")
            }
            else if(type == "relational")
                stop("Type 'relational' currently not implemented.")
            else {
                type <- sub("\\{.*", "factor", type)
                ## (Could try to preserve factor levels ...)
                type <- sub("string", "character", type)
                type <- sub("real", "numeric", type)
                col_types <- c(col_types, type)
                col_dfmts <- c(col_dfmts, NA)
            }
        }
        line <- readLines(file, n = 1)
    }

    ## Test header.
    if(length(line) == 0)
        stop("Missing data section.")
    if(is.null(col_names))
        stop("Missing attribute section.")
    if(length(col_names) !=
       length(grep('factor|numeric|character', col_types)))
        stop("Invalid type specification.")
    
    ## Get data.
    data <- read.table(file, sep = ",", na.strings = "?",
                       colClasses = col_types, comment.char = '%')
    if(any(ind <- which(!is.na(col_dfmts))))
        for(i in ind)
            data[i] <- as.data.frame(strptime(data[[i]], col_dfmts[i]))
    ## Remove left over escapes.
    for (i in seq_len(length(data)))
        if (is.factor(data[[i]]))
           levels(data[[i]]) <- gsub("\\\\", "", levels(data[[i]]))
    names(data) <- col_names
    data
}

write.arff <-
function(x, file, eol = "\n")
{
    ## See write.table().
    if(file == "") 
        file <- stdout()
    else if(is.character(file)) {
        file <- file(file, 'w')
        on.exit(close(file))
    }
    if(!inherits(file, "connection")) 
        stop("Argument 'file' must be a character string or connection.")

    if (!is.data.frame(x) && !is.matrix(x))
        x <- data.frame(x)

    ## We need to quote ourselves, as write.table() escapes the quote
    ## char but not the backslash.  Weka seems to prefer backslash
    ## escapes inside single quotes, so we provide that ...
    squote <- function(s) {
        ## Don't quote NAs.
        ifelse(is.na(s), s,
               sprintf("'%s'", gsub("(['\\])", "\\\\\\1", s)))
    }

    ## Write header.
    text <- paste('@relation', deparse(substitute(x)))
    writeLines(text, file, sep = eol)
    for(name in names(x)) {
        text <- paste('@attribute', name)
        if(is.factor(x[[name]])) {
            lev <- squote(levels(x[[name]]))
            levels(x[[name]]) <- lev
            text <- paste(text, " {",
                          paste(lev, collapse = ","), "}",
                          sep = "")
        } else if(is.character(x[[name]])) {
            text <- paste(text, "string")
            x[[name]] <- squote((x[[name]]))
        } else if(inherits(x[[name]], "POSIXt")) {
            text <- paste(text, "date \"yyyy-MM-dd hh:mm:ss\"")
            x[[name]] <- squote(format(x[[name]]))
        } else {
            text <- paste(text, "numeric")
        }
        writeLines(text, file, sep = eol)
    }

    ## Write data.
    writeLines("@data", file)
    write.table(x, file = file, na = "?", sep = ",",
                eol = eol, quote = FALSE, row.names = FALSE,
                col.names = FALSE)
}


ISO_8601_to_POSIX_datetime_format <-
function(x)
{
    ## First, Weka thinks that 'yyyy' is ISO 8601 ...
    x <- sub("yyyy", "%Y", x, ignore.case = TRUE)
    ## And it's 'DD' and not 'dd' ...
    x <- sub("dd", "%d", x)
    ## And it's 'hh' and not 'HH' ...
    x <- sub("HH", "%H", x)
    
    ## Now the real stuff.
    ## Is there a POSIX format string for the century component of year?
    x <- sub("CCYY", "%Y", x)
    x <- sub("YY", "%y", x)
    x <- sub("MM", "%m", x)
    x <- sub("DD", "%d", x)
    x <- sub("DDD", "%j", x)
    x <- sub("ww", "%U", x)
    x <- sub("D", "%w", x)
    x <- sub("hh", "%H", x)
    x <- sub("mm", "%M", x)
    x <- sub("ss", "%S", x)
    ## Is there a POSIX format string for fractions of seconds?

    x
}
