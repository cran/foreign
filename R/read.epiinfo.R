
##format of EpiInfo 6 files is at http://www.cdc.gov/epiinfo/epi6man/manchp37.htm

read.epiinfo<-function(file,read.deleted=FALSE,guess.broken.dates=FALSE,thisyear=NULL){
    ## allow file, url, connection
    if (is.character(file)) {
        file <- file(file, "rt")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "rt")
        on.exit(close(file))
    }

    line<-readLines(file,1, ok=TRUE)
    headerlength<-na.omit(sapply(strsplit(line," ")[[1]],as.numeric))[1]
    if (headerlength<=0) stop(paste("file has zero or fewer variables"))

    headerlines<-readLines(file,n=headerlength)
    pushBack(headerlines,file)
    comments<-sapply(headerlines,function(s) substring(s,46,46+80))
  
    header<-scan(file,nlines=headerlength,what=list(name="", x=0,y=0, color=0,x1=0,y1=0,type=0,len=0,color=0),flush=TRUE,quiet=TRUE)
    header<-as.data.frame(lapply(header,I))
 
    header$start<-cumsum(c(1,header$len))[1:headerlength]
    header$stop<-cumsum(header$len)
    multiline<-ceiling(max(header$stop)/78)
    
    really.variables<-header$len!=0
    header<-header[really.variables,]

    entrychar<-substr(header$name,1,1)
    if (all(entrychar %in% c("#","_")))
          header$name<-substr(header$name,2,12)
    
    comments<-comments[really.variables]

    numbers<-(header$len>0) & ( (header$type %in% c(0,6)) | (header$type>12))
    
    datalines<-scan(file,what="",sep="\n",quote="",quiet=TRUE,blank.lines.skip=TRUE,comment="\032")
    if (length(datalines) %% multiline)
        warning("wrong number of records")

    datalines<-matrix(datalines,nrow=multiline)
    if (multiline>1)
        datalines[-multiline,]<-substr(datalines[-multiline,],1,78)
    datalines<-apply(datalines,2,paste,collapse="")

    deleted<-substr(datalines,nchar(datalines),nchar(datalines))=="?"

    nvars<-NROW(header)
    data<-as.data.frame(lapply(1:nvars, function(i) I(substring(datalines,header$start[i],header$stop[i]))))
    names(data)<-header$name
    names(comments)<-header$name
    
    if (is.na(read.deleted))
        data[deleted,]<-NA
    else if(!read.deleted)
        data<-data[!deleted,]
    

    if (guess.broken.dates && is.null(thisyear)) thisyear<-format(Sys.time(),format="%Y")

    for(i in 1:nvars){
        if (numbers[i])
            data[[i]]<-as.numeric(data[[i]])
        else if (header$type[i]==5) ## Yes/No
            data[[i]]<-ifelse(data[[i]] %in% c("Y","N"),data[[i]]=="Y",NA)
        else if (header$type[i]==11 && header$len[i]==5 && guess.broken.dates) ##dd/mm
            data[[i]]<-as.POSIXct(strptime(paste(data[[i]],thisyear,sep="/"),format="%d/%m/%Y"))
        else if (header$type[i]==11 && header$len[i]==8 && guess.broken.dates) ##dd/mm/yy
            data[[i]]<-as.POSIXct(strptime(data[[i]],format="%d/%m/%y"))
        else if (header$type[i]==11 && header$len[i]==10)                       ##dd/mm/yyyy
            data[[i]]<-as.POSIXct(strptime(data[[i]],format="%d/%m/%Y")) 
        else if (header$type[i]==2 && header$len[i]==5 && guess.broken.dates)  ##mm/dd
            data[[i]]<-as.POSIXct(strptime(paste(data[[i]],thisyear,sep="/"),format="%m/%d/%Y"))
        else if (header$type[i]==2 && header$len[i]==8 && guess.broken.dates)  ##mm/dd/yy
            data[[i]]<-as.POSIXct(strptime(data[[i]],format="%d/%m/%y"))
        else if (header$type[i]==2 && header$len[i]==10)                        ##mm/dd/yyyy
            data[[i]]<-as.POSIXct(strptime(data[[i]],format="%d/%m/%Y")) 
        else {
            blanks<-grep("^[ \\t]*$",data[[i]])
            data[[i]][blanks]<-NA
        }
    }

    if (!is.na(read.deleted && read.deleted)) attr(data,"deleted")<-deleted
    attr(data,"prompts")<-comments
    data
}
