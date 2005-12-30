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
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA

lookup.xport <- function(file) .Call(xport_info, file)


read.xport <- function(file) {
    data.info <- lookup.xport(file)
    ans <- .Call(xport_read, file, data.info)
    if (length(ans) == 1) as.data.frame(ans[[1]])
    else lapply(ans, as.data.frame)
}
