
read.dta <- function(file, convert.dates=TRUE,tz="GMT",
                      convert.factors=TRUE,missing.type=FALSE,
                     convert.underscore=TRUE){
    rval<-.External("do_readStata", file,  PACKAGE = "foreign")

    if(convert.underscore)
      names(rval)<-gsub("_",".",names(rval))

    types<-attr(rval,"types")    
    stata.na<-data.frame(type=251:255,
                         min=c(101, 32741, 2147483621, 2^127, 2^1023),
                         inc=c(1,1,1,2^115,2^1011)
                         )

         
    if(!missing.type){
        if (abs(attr(rval,"version"))==8){
            for(v in which(types>250)){
                this.type<-types[v]-250
                rval[,v][rval[,v]>=stata.na$min[this.type]]<-NA
            }
        }
    } else {
        if (abs(attr(rval,"version"))==8){
            missings<-vector("list",NCOL(rval))
            names(missings)<-names(rval)
            for(v in which(types>250)){
                this.type<-types[v]-250
                nas<-is.na(rval[,v]) |  rval[,v]>=stata.na$min[this.type]
                natype<-(rval[nas,v]-stata.na$min[this.type])/stata.na$inc[this.type]
                natype[is.na(natype)]<-0
                missings[[v]]<-rep(NA,NROW(rval))
                missings[[v]][nas]<-natype
                rval[nas,v]<-NA
            }
            attr(rval,"missing")<-missings
        } else {
            warning("`missing.type' only applicable to version 8 files")
        }
        
    }

    if (convert.dates){
        ff<-attr(rval,"formats")
        dates<-grep("%-*d",ff)
        for(v in dates)
            rval[[v]]<-ISOdate(1960,1,1,tz=tz)+24*60*60*rval[[v]]
    }
    if (convert.factors){
        if (attr(rval, "version")==5)
          warning("Can't read factor labels from Stata 5 files.")
        else {
          ll<-attr(rval,"val.labels")
          tt<-attr(rval,"label.table") 
          factors<-which(ll!="")
          for(v in factors){
            labels<-tt[[ll[v]]]
            if (is.null(labels)){
              warning("Value labels (",ll[v],") for ",names(rval)[v]," are missing")
              next
            }
            rval[[v]]<-factor(rval[[v]],levels=tt[[ll[v]]],labels=names(tt[[ll[v]]]))
          }
        }
      }
    

    rval
    
  }

write.dta <- function(dataframe, file, version = 6,convert.dates=TRUE,tz="GMT",
                      convert.factors=c("labels","string","numeric","codes"))
{

    if (version<6) stop("Version must be 6-8")
    if (version>8) {
        warning("Version must be 6-8: using 7")
        version<-7
    }
        

    
    namelength<-if (version==6) 8 else 31
    oldn<-names(dataframe)
    nn<-abbreviate(oldn,namelength )
    if (any(nchar(nn)>namelength))
        stop("Can't uniquely abbreviate variable names")
    if (any(nchar(oldn)>namelength))
        warning("Abbreviating variable names")
    names(dataframe)<-nn
    attr(dataframe,"orig.names")<-oldn
    
    if (convert.dates){
        dates<-which(isdate<-sapply(dataframe,function(x) inherits(x,"POSIXt")))
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
