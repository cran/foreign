### $Id: xport.R,v 1.2 2001/06/09 14:40:20 hornik Exp $
###
###             Read SAS xport format libraries
###
### Copyright 1999-1999 Douglas M. Bates <bates$stat.wisc.edu>,
###                     Saikat DebRoy <saikat$stat.wisc.edu>
###
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

lookup.xport <- function(file) {
    .Call("xport_info", file, PACKAGE = "foreign")
}

read.xport <- function(file) {
    data.info <- lookup.xport(file)
    ans <- .Call("xport_read", file, data.info, PACKAGE = "foreign")
    if (length(ans) == 1)
        as.data.frame(ans[[1]])
    else lapply(ans, as.data.frame)
}
