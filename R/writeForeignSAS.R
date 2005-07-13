writeForeignSAS<-function(df,datafile,codefile,dataname="rdata"){
  
  factors <- sapply(df, is.factor)
  strings <- sapply(df, is.character)

  varlabels <- names(df)
  varnames <- abbreviate(names(df), 8)
  if (any(sapply(varnames, nchar) > 8)) 
    stop("Cannot abbreviate the variable names to eight or fewer letters")
  if (any(abbreviated <- (varnames != varlabels))) 
    message("Some variable names were abbreviated.")
  
  
  dfn<-df
  if (any(factors))
    dfn[factors]<-lapply(dfn[factors], as.numeric)     
  write.table(dfn, file = datafile, row = FALSE, col = FALSE, 
              sep = ",", quote = FALSE, na = ".")
  lrecl<-max(sapply(readLines(datafile),nchar))+4

  cat("* Written by R;\n", file=codefile)
  cat("* ",deparse(sys.call(-2))[1],";\n\n",file=codefile,append=TRUE)
  if (any(factors)){
    cat("PROC FORMAT;\n",file=codefile,append=TRUE)
    for(v in 1:ncol(df)){
      if (factors[v]){
        cat("value ",varnames[v],"\n",file=codefile,append=TRUE)
        values<-levels(df[[v]])
        for(i in 1:length(values)){
          cat("    ",i,"=",adQuote(values[i]),"\n",file=codefile,append=TRUE)
        }
        cat(";\n\n",file=codefile,append=TRUE)
      }
      }
  }

  cat("DATA ",dataname,";\n",file=codefile,append=TRUE)
  cat("INFILE ",adQuote(datafile),
      "\n     DELIMITER=','",
      "\n     LRECL=",lrecl,";\n",
      file=codefile,append=TRUE)

  cat("INPUT",file=codefile,append=TRUE)
  for(v in 1:ncol(df)){
    cat("\n",varnames[v],file=codefile,append=TRUE)
    if(strings[v])
      cat(" $ ",file=codefile,append=TRUE)
  }
  cat("\n;\n",file=codefile,append=TRUE)

  for(v in 1:ncol(df)){
    if (abbreviated[v])
      cat("LABEL ",varnames[v],"=",adQuote(varlabels[v]),";\n",
          file=codefile,append=TRUE)
  } 

  for(v in 1:ncol(df)){
    if(factors[v])
      cat("FORMAT ",varnames[v],paste(varnames[v],".",sep=""),";\n",
          file=codefile,append=TRUE)
  }

  cat("RUN;\n",file=codefile,append=TRUE)
} 
