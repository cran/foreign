### $Id: minitab.R,v 1.2 2001/06/09 14:40:20 hornik Exp $
###
###             Read stored Minitab worksheets
###
### Copyright 1999-2000 Douglas M. Bates <bates$stat.wisc.edu>,
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

## Files in the Minitab portable worksheet format represent numbers in a
## fixed format (written in Fortran as 5e15.9 or something like that) but
## the data values are only stored in Minitab in single precision.  We use
## signif to remove the inaccuracies in the representation in the files.

"read.mtp" <-
function (file) {
    clean <- function(x) if(is.numeric(x)) signif(x, 6) else x
    val <- .Call("read_mtp", file, PACKAGE = "foreign")
    lapply(val, clean)
}
