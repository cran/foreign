SModeNames <-
c("name", "string", "literal", "compiled", "(", ")", "[", "]",
"{", "}", ",", "=", "!", ":", "addop", "*/", "<dummy>", "^",
"-", "$", "logop", "&|", "<-", "->", "sp.op", " ", "repeat",
"if", "else", "break", ";", "next", "while", "for", "in", "recursive.return",
"return", "argument", "system", "end.of.file", "expression",
"system.function", "missing", "call", "function", "?", "unbalanced",
"[[", "unknown", "]]", "quit", "continue", "comment.expression",
"vector", "call(...)", "<<-", "graphics", "arg.lvalue", "internal",
"S.call", "S.data", "comment", "comment(leftover)",
"evaluation.frame", "destination")

nam.or.i <- function(nam, i) if (nam != "") nam else i

read.S <- function (file)
{
    endian <- .Platform$endian
    s <- file(file, open = "rb")
    on.exit(close(s))

    readheader <- function(s)
    {
	head <- readBin(s, "int", 8, 1)
	all(head == c(0, 83, 32, 100, 97, 116, 97, 1))
    }

    ReadSObj <- function (code, len)
    {
	if (code == 1)
	    result <- as.logical(readBin(s, "int", len, endian = endian))
	else if (code == 2)
	    result <- readBin(s, "int", len, endian = endian)
	else if (code == 3)
	    result <- readBin(s, "numeric", len, size=4, endian = endian)
	else if (code == 4)
	    result <- readBin(s, "numeric", len, endian = endian)
	else if (code == 5) {
	    charsize <- readBin(s, "int", endian = endian)
	    newpos <- charsize + seek(s, NA)
	    result <- readBin(s, "character", len)
	    seek(s, newpos)
	}
	else if (code == 6) {
	    result <- list()
	    if (len > 0) {
		names <- ReadSObj(5, len)
		codes <- ReadSObj(2, len)
		lens <- ReadSObj(2, len)
		offsets <- ReadSObj(2, len)
		for (i in 1:len) {
		    seek(s, offsets[i])
                    temp <-
                        if (codes[i] > 0)
                            ReadSObj(codes[i], lens[i]) else as.name(names[i])
                    result[[nam.or.i(names[i], i)]] <- temp
		}
	    }
	}
	else if (code == 7)
	    result <- readBin(s, "complex", len, endian = endian)
	else if (code == 21) {
	    temp <- ReadSObj(6, len)
	    result <- temp[[".Data"]]
	    attributes(result) <-
		temp[-match(c(".Data", ".Dim", ".Dimnames", ".Label"),
			    names(temp), nomatch = 0)]
	    dim(result) <- temp[[".Dim"]]
	    names(result) <- names(temp[[".Data"]])
	    if (!is.null(temp[[".Label"]]))
		levels(result) <- temp[[".Label"]]
	    if (!is.null(temp[[".Dimnames"]]))
		dimnames(result) <- temp[[".Dimnames"]]
	}
	else if (code %in% 257:321) {
	    code <- SModeNames[code - 256]
	    if (code %in% c("name", "missing"))
		result <- ReadSObj(5, len)
	    else
		result <- ReadSObj(6, len)
	    if (code == "function")
		try(result <- as.function(result, env=.GlobalEnv))
	    else if (code %in% c("break", "if", "for", "return", "S.call",
				 "while", "<-", "<<-", "(", "{"))
		result <- as.call(c(as.name(code),result))
	    else if (code == "call(...)")# these aren't special in R
		result <- result[[1]]
	    else if (code == "comment") # ignore comments
		result <- NULL
	    else if (code == "comment.expression")# just keep the expression, not the comment
		result <- result[unlist(lapply(result,function(y) !is.null(y)))][[1]]
	    else if (code == "internal")
		result <- as.call(list(as.name(".Internal"), result[[1]]))
	    else if (code == "missing")
		result <- call("stop", "Argument is missing")
	    else try(mode(result) <- code)
	}
	else {
	    return(paste("Unrecognized S mode", code, "not supported"))
	}
	result
    }
    if(readheader(s)) {
	code <- readBin(s, "int", endian = endian)
	if (code < 0 | code > 65535) {
	    endian <- switch(endian, big = "little", little = "big")
	    seek(s,  seek(s, NA) -4)
	    code <- readBin(s, "int", endian = endian)
	    if (code < 0 | code > 65535)
		stop("internal error - illegal S code value")
	}
	len <- readBin(s, "int", endian = endian)
	return(ReadSObj(code, len))
    }
    else stop("not an S object")
}

data.restore <-
    function (file, print = FALSE, verbose = FALSE, env = .GlobalEnv)
{
    dump <- file(file, open="rt")
    on.exit(close(dump))

    ReadSdump <- function(top = FALSE, prefix) {
	name <- readLines(dump, 1)
	if(length(name) == 0) return(NULL)
	code <- readLines(dump, 1)
	len <- as.integer(readLines(dump, 1))
	if (top && print)
	    cat("\"", name, "\": ", code, "\n", sep="")
	if (verbose)
	    cat(prefix, summary(dump)$position, name, code, len, "\n")
        ## first decide between atomic and "the rest"
	if (code %in% c("logical", "numeric","integer","single","double",
			"character", "name", "missing", "complex")) {
	    value <- readLines(dump, len)
	    value[value == "N"] <- as.character(NA)
	    if (code != "character")
		value <-
		    if (code == "logical")# "0", "1" and <NA> :
			as.logical(as.integer(value))
		    else if (code %in% c("integer", "double", "name", "complex"))
			methods::as(value, code)
		    else if (code %in% c("numeric","single"))
			as.numeric(value)
		    else if (code == "missing")
			## Workaround:	should be value <- as.name("")
			call("stop",
			     paste("Argument ", sQuote(name), " is missing",
                                   sep=""))
	}
	else if (code %in% c("list", "structure", "NULL", SModeNames)) {
	    value <- list()
	    for (i in seq(length = len)) {
		temp <- ReadSdump(FALSE, c(prefix, " "))
		value[[nam.or.i(temp$name,i)]] <- temp$value
	    }
	    if (code == "structure") {
		thelist <- value
		value <- thelist[[".Data"]]
		attributes(value) <-
		    thelist[-match(c(".Data", ".Dim", ".Dimnames", ".Label"),
				   names(thelist), nomatch = 0)]
		dim(value) <- thelist[[".Dim"]]
		names(value) <- names(thelist[[".Data"]])
		if (!is.null(thelist[[".Label"]]))
		    levels(value) <- thelist[[".Label"]]
		if (!is.null(thelist[[".Dimnames"]]))
		    try(dimnames(value) <- thelist[[".Dimnames"]])
		if (!is.null(tsp <- thelist[[".Tsp"]]))# valid R ts()
		    try(value <- stats::ts(c(value),tsp[1],tsp[2],tsp[3]))
	    }
	    else if (code == "function")
		try(value <- as.function(value,env=env))
	    else if (code %in% c("break", "if", "for", "return", "S.call",
				 "while", "<-", "<<-", "(", "{"))
		value <- as.call(c(as.name(code), value))
	    else if (code == "NULL") {
                value <- if (name != "") as.name(name)
            }
	    else if (code == "call(...)")# these aren't special in R
		value <- value[[1]]
	    else if (code == "comment") # ignore comments
		value <- NULL
	    else if (code == "comment.expression")# just keep the expression, not the comment
		value <- value[unlist(lapply(value,function(y) !is.null(y)))][[1]]
	    else if (code == "internal")
		value <- as.call(list(as.name(".Internal"),value[[1]]))

	    else try(mode(value) <- code)
	}
	else {
	    stop("S mode ", sQuote(code), " (near byte offset ",
                 seek(dump), ") not supported")
	}
	list(name = name, value = value)
    }
    repeat {
	temp <- ReadSdump(TRUE, " ")
	if(is.null(temp)) break
	assign(temp$name, temp$value, env = env)
    }
    file
}
