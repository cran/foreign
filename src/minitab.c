/*
 *  $Id: minitab.c,v 1.2 1999/12/16 00:20:18 bates Exp $ 
 *
 *  Read Minitab portable data set format
 *
 *  Copyright 1999-1999 Douglas M. Bates <bates@stat.wisc.edu>,
 *                      Saikat DebRoy <saikat@stat.wisc.edu>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be
 *  useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *  PURPOSE.  See the GNU General Public License for more
 *  details.
 *
 *  You should have received a copy of the GNU General Public
 *  License along with this program; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 *  MA 02111-1307, USA
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "Rdefines.h"
#include "Rinternals.h"

#define MTP_BUF_SIZE 85
#define MTB_INITIAL_ENTRIES 10

typedef struct {
    int    type;		/* 3 = column, m = matrix, k = constant */
    int    cnum;		/* column number in the Minitab worksheet */
    int    len;			/* length of column */
    int    dtype;		/* data type: 0 = numeric */
    union {
	double *ndat;
	char   **cdat;
    } dat;
    char   name[9];
} MTBDATC, *MTB;

#define Column 3
#define Matrix 4
#define Constant 2

static				/* trim white space from end of string */
char *strtrim(char *str)
{
    int i;
    for (i = strlen(str) - 1; i >= 0 && isspace(str[i]); i--) str[i] = '\0';
    return str;
}

static
SEXP MTB2SEXP(MTB mtb[], int len) /* Create a list from a vector of
				     MTB's and Free the MTB storage */
{
    SEXP ans = PROTECT(NEW_LIST(len)),
         names = PROTECT(NEW_CHARACTER(len));
    int i, nprot = 2;
    
    
    for (i = 0; i < len; i++) {
	MTB this = mtb[i];

	STRING(names)[i] = PROTECT(COPY_TO_USER_STRING(this->name)); nprot++;
	switch(mtb[i]->dtype) {
	case 0:			/* numeric data */
	    VECTOR(ans)[i] = PROTECT(NEW_NUMERIC(mtb[i]->len)); nprot++;
	    Memcpy(NUMERIC_POINTER(VECTOR(ans)[i]), mtb[i]->dat.ndat,
		   mtb[i]->len);
	    Free(mtb[i]->dat.ndat);
	    break;
	default:
	    PROBLEM "Non-numeric data types are not yet implemented" ERROR;
	}
	Free(mtb[i]);
    }
    Free(mtb);
    setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(nprot);
    return(ans);
}
    
SEXP
read_mtp(SEXP fname)
{
    FILE *f;
    char buf[MTP_BUF_SIZE], blank[1];
    MTB  *mtb, this;
    int i, j, nMTB = MTB_INITIAL_ENTRIES;
    
    PROTECT(fname = AS_CHARACTER(fname));
    if ((f = fopen(CHAR(STRING(fname)[0]), "r")) == NULL) {
	PROBLEM "Unable to open file %s for reading", 
	    CHAR(STRING(fname)[0])
	    ERROR;
    }
    if ((fgets(buf, MTP_BUF_SIZE, f) == NULL) ||
	strncmp(buf, "Minitab Portable Worksheet ", 27) != 0) {
	PROBLEM "File %s is not in Minitab Portable Worksheet format",
	    CHAR(STRING(fname)[0])
	    ERROR;
    }
    fgets(buf, MTP_BUF_SIZE, f);
    UNPROTECT(1);
    
    mtb = Calloc(nMTB, MTB);
    for (i = 0; !feof(f); i++) {
	if (i >= nMTB) {
	    nMTB *= 2;
	    mtb = Realloc(mtb, nMTB, MTB);
	}
	this = mtb[i] = Calloc(1, MTBDATC);
	if (sscanf(buf, "%%%7d%7d%7d%7d%c%8c", &(this->type), &(this->cnum),
		   &(this->len), &(this->dtype), blank, this->name) != 6) {
	    PROBLEM "First record for entry %d is corrupt", i+1 ERROR;
	}
	this->name[8] = '\0';
	strtrim(this->name);	/* trim trailing white space on name */
	switch (this->dtype) {
	case 0:		/* numeric data */
	    this->dat.ndat = Calloc(this->len, double);
	    for (j = 0; j < this->len; j++) {
		fscanf(f, "%lg", this->dat.ndat + j);
	    }
	    break;
	default:
	    PROBLEM "Non-numeric data types are not yet implemented" ERROR;
	} 
	fgets(buf, MTP_BUF_SIZE, f); /* clear rest of current line */
	fgets(buf, MTP_BUF_SIZE, f); /* load next line */
    }
    return MTB2SEXP(mtb, i);
}
