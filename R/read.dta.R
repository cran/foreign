.First.lib<-function(libname,pkgname){
  library.dynam("stataread",pkgname,libname)
}
read.dta<-function(filename){
    .External("do_readStata",filename)
  }

write.dta<-function(dataframe,filename){
    if (any(sapply(dataframe,function(x) !is.null(dim(x)))))
      stop("Can't handle multicolumn columns")
    invisible( .External("do_writeStata",filename,dataframe))
  }
