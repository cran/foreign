read.ssd <- function(libname, sectionnames, tmpXport=tempfile(),
                     tmpProgLoc=tempfile(), sascmd="sas")
{
    ##
    ## copyright 2002 VJ Carey <stvjc@channing.harvard.edu>
    ##           2004 R Development Core Team
    ##
    ## read.ssd -- 'read' a SAS v6 ssd format file by converting
    ## the data to sas xport format and then using R foreign:read.xport
    ## march 22 2002 -- works fine if the desired sas lib/section exist
    ## but cannot detect when sas 'fails' owing to nonexistence
    ##
    ## tries to clean up interim results
    ##
    ## works for sas v8
    ##
    ## require(foreign)
    on.exit(unlink(tmpXport))
    logGuess <- function (x)
    {
        ## guess the name of the log file by stripping all
        ## path to the sas program (log will lie in executing dir)
        expl <- strsplit(x, "")[[1]]
        rex <- rev(expl)
        br <- match("/", rex)[1]
        if (is.na(br))
            return(x)
        return(paste(rev(rex[1:(br - 1)]), sep = "", collapse = ""))
    }
    st1 <- paste("libname src2rd '",libname,"';\n",sep="")
    st2 <- paste("libname rd xport '", tmpXport, "';\n", sep="")
    st3 <- paste("proc copy in=src2rd out=rd;\n")
    st4 <- paste("select", sectionnames, ";\n", sep=" ")
    tmpProg <- paste(tmpProgLoc, ".sas", sep="")
    tmpProgLogBase <- logGuess(tmpProgLoc)
    tmpProgLog <- paste(tmpProgLogBase, ".log", sep="")
    cat(st1, file=tmpProg)
    cat(st2, file=tmpProg, append=TRUE)
    cat(st3, file=tmpProg, append=TRUE)
    cat(st4, file=tmpProg, append=TRUE)
    if(.Platform$OS.type == "windows")
        sascmd <- paste(dQuote(sascmd), "-sysin")
    sasrun <- try(sysret <- system( paste( sascmd, tmpProg ) ))
    if (!inherits(sasrun, "try-error") & sysret == 0)
    {
        unlink( tmpProg )
        unlink( tmpProgLog )
        return( read.xport( tmpXport ) )
    }
    else
    {
        cat("SAS failed.  SAS program at", tmpProg,"\n")
        if(.Platform$OS.type == "unix") {
            cat("a log and other error products should be in the vicinity\n")
            system(paste("ls -l ", tmpProgLog))
        } else {
            cat("The log file will be ",
                paste(basename(tmpProgLoc), ".log", sep=""),
                " in the current directory\n", sep="")
        }
        warning("SAS return code was ", sysret)
        return(NULL)
    }
}
