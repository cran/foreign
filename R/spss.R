### $Id: spss.R,v 1.3 2002/01/12 02:10:26 tlumley Exp $
###
###             Read SPSS system data files
###
### Copyright 2000-2002 Saikat DebRoy <saikat$stat.wisc.edu>
###                     Douglas M. Bates <bates$stat.wisc.edu>,
###                     Thomas Lumley
### This file is part of the `foreign' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA

read.spss <- function(file,use.value.labels=TRUE,to.data.frame=FALSE) {
    rval<-.Call("do_read_SPSS", file, PACKAGE = "foreign")
    
    if (use.value.labels){
      vl<-attr(rval,"label.table")
      has.vl<-which(!sapply(vl,is.null))
      for(v in has.vl){
        nm<-names(vl)[[v]]
        rval[[nm]]<-factor(rval[[nm]],levels=vl[[v]],labels=names(vl[[v]]))
      }
    }
    
    if (to.data.frame)
      as.data.frame(rval)
    else
      rval
    
}
