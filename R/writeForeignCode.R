
write.foreign<-function(df, datafile, codefile, package=c("SPSS","Stata"),...){

  do.call(paste("writeForeign",package,sep=""),
          list(df=df,datafile=datafile,codefile=codefile),...)

}

writeForeignSPSS<-function(df,datafile,codefile,varnames=NULL){
  
  dfn<-lapply(df, function(x) if (is.factor(x)) as.numeric(x) else x)
  write.table(dfn, file=datafile,row=FALSE,col=FALSE)

  varlabels<-names(df)
  if (is.null(varnames)){
    varnames<-abbreviate(names(df),8)
    if (any(sapply(varnames,nchar)>8))
      stop("I can't abbreviate the variable names to eight or fewer letters")
    if (any(varnames!=varlabels))
      warning("Some variable names were abbreviated")
  }
  
  cat("DATA LIST FILE=",datafile," free\n",file=codefile)
  cat("/", varnames,"\n\n",file=codefile,append=TRUE)
  cat("VARIABLE LABELS\n",file=codefile,append=TRUE)
  cat(paste(varnames, dQuote(varlabels),"\n"),file=codefile,append=TRUE)
  factors<-sapply(df,is.factor)
  if (any(factors)){
    cat("\nVALUE LABELS\n",file=codefile,append=TRUE)
    for(v in which(factors)){
      cat("/\n",file=codefile,append=TRUE)
      cat(varnames[v]," \n",file=codefile,append=TRUE)
      levs<-levels(df[[v]])
      cat(paste(1:length(levs),dQuote(levs),"\n",sep=" "),file=codefile,append=TRUE)
    }
  }
  cat("\nEXECUTE.\n",file=codefile,append=TRUE)
}


writeForeignStata<-function(df,datafile,codefile){
  
  write.table(df, file=datafile, row=FALSE, col=FALSE, sep=",", quote=FALSE, na=".")

  nms<-names(df)
  factors<-sapply(df,is.factor) | sapply(df, is.character)
  formats<-paste(nms,"fmt",sep="_")
  nms<-ifelse(factors,paste(nms,formats,sep=":"),nms)
  
  cat("infile",nms," using ",datafile,", automatic\n", file=codefile)
  
}