### $Id: spss.R,v 1.2 2001/06/09 14:40:20 hornik Exp $
###
###             Read SPSS system data files
###
### Copyright 2000-2000 Saikat DebRoy <saikat$stat.wisc.edu>
###                     Douglas M. Bates <bates$stat.wisc.edu>,
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

read.spss <- function(file) {
    .Call("do_read_SPSS", file, PACKAGE = "foreign")
}
