 read.dta <- function(file, convert.dates=TRUE,tz="GMT",convert.factors=TRUE){
    rval<-.External("do_readStata", file, PACKAGE = "foreign")
    if (convert.dates){
        ff<-attr(rval,"formats")
        dates<-grep("%-*d",ff)
        for(v in dates)
            rval[[v]]<-ISOdate(1960,1,1,tz=tz)+24*60*60*rval[[v]]
    }
    if (convert.factors){
        ll<-attr(rval,"val.labels")
        tt<-attr(rval,"label.table")
        factors<-which(ll!="")
        for(v in factors)
            rval[[v]]<-factor(rval[[v]],levels=tt[[ll[v]]],labels=names(tt[[ll[v]]]))
    }
        
    rval
}

write.dta <- function(dataframe, file, version = 6,convert.dates=TRUE,tz="GMT",
                      convert.factors=c("labels","string","numeric","codes"))
{
    namelength<-if (version==6) 8 else 31
    nn<-abbreviate(names(dataframe),namelength )
    if (any(nchar(nn)>namelength))
        stop("Can't uniquely abbreviate variable names")
    if (any(nchar(names(dataframe))>namelength))
        warning("Abbreviating variable names")
    names(dataframe)<-nn
    
    if (convert.dates){
        dates<-which(sapply(dataframe,function(x) inherits(x,"POSIXt")))
        for( v in dates)
            dataframe[[v]]<-as.vector(round(julian(dataframe[[v]],ISOdate(1960,1,1,tz=tz))))
    }
    convert.factors<-match.arg(convert.factors)
    factors<-which(sapply(dataframe,is.factor))
    if(convert.factors=="string"){
        for(v in factors)
            dataframe[[v]]<-I(as.character(dataframe[[v]]))
    } else if (convert.factors=="numeric"){
         for(v in factors)
             dataframe[[v]]<-as.numeric(as.character(dataframe[[v]]))
     } else if (convert.factors=="codes"){
         for (v in factors)
             dataframe[[v]]<-as.numeric(dataframe[[v]])
     }

    shortlevels<-function(f){
        ll<-levels(f)
        if (is.null(ll))
            return(NULL)
        abbreviate(ll,80)}


    leveltable<-lapply(dataframe,shortlevels)
     
    if (any(sapply(dataframe, function(x) !is.null(dim(x)))))
        stop("Can't handle multicolumn columns")
    invisible(.External("do_writeStata", file, dataframe, version, leveltable,
                        PACKAGE = "foreign"))
}
