/* Based on code in the shapelib.maptools.org library.
 *
 * First version for R's maptools package appears to be
 * Copyright 2000-2001 (c) Nicholas Lewin-Koh
 *
 * Changes for the foreign package Copyright (C) 2004 the R Code
 * Development Team, including adding support for logical fields.
 */

#include <stdlib.h>
#include <string.h>
#include "shapefil.h"
#include <R.h>
#include <Rinternals.h>

SEXP Rdbfread(SEXP dbfnm)
{
    DBFHandle hDBF;
    int i, iRecord, nflds, nrecs, nRvar, pc=0;
    char labelbuff[81], *pszFilename = NULL;
    int nWidth, nDecimals, val;
    char szTitle[12], buf[2];
    const char *p;
    DBFFieldType eType;
    SEXP df, tmp, varlabels, row_names, DataTypes;
    short *types;

/* -------------------------------------------------------------------- */
/*      Handle arguments.                                               */
/* -------------------------------------------------------------------- */

    pszFilename = CHAR(STRING_ELT(dbfnm, 0));


/* -------------------------------------------------------------------- */
/*      Open the file.                                                  */
/* -------------------------------------------------------------------- */
    hDBF = DBFOpen(pszFilename, "rb" );
    if( hDBF == NULL ) error("unable to open DBF file");

/* -------------------------------------------------------------------- */
/*	If there is no data in this file let the user know.		*/
/* -------------------------------------------------------------------- */
    if( DBFGetFieldCount(hDBF) == 0 )
    {
    	DBFClose( hDBF );
	error("No fields in DBF table");
    }

    nRvar = 0;
    nflds = DBFGetFieldCount(hDBF);
    nrecs = DBFGetRecordCount(hDBF);
    types = (short *) R_alloc(nflds, sizeof(short));
    PROTECT(DataTypes = allocVector(STRSXP, nflds)); pc++;
    for( i = 0; i < nflds; i++ ) {
	eType = DBFGetFieldInfo( hDBF, i, szTitle, &nWidth, &nDecimals );
	switch(eType) {
	case FTString:
	    types[i] = 1;
	    nRvar++;
	    break;
	case FTInteger:
	    types[i] = 2;
	    nRvar++;
	    break;
	case FTDouble:
	    types[i] = 3;
	    nRvar++;
	    break;
	case FTLogical:
	    types[i] = 4;
	    nRvar++;
	    break;
	default: /* doesn't seem to be possible */
	    types[i] = 0;
	}
	buf[0] = hDBF->pachFieldType[i]; buf[1] = '\0';
	SET_STRING_ELT(DataTypes, i, mkChar(buf));
    }

    PROTECT(df = allocVector(VECSXP, nRvar)); pc++;
    PROTECT(varlabels = allocVector(STRSXP, nRvar)); pc++;
    for(i = 0, nRvar = 0; i < nflds; i++)
    {
	eType = DBFGetFieldInfo( hDBF, i, szTitle, &nWidth, &nDecimals );
	switch(types[i]) {
	case 1:
	    SET_VECTOR_ELT(df, nRvar, allocVector(STRSXP,nrecs));
	    break;
	case 2:
	    SET_VECTOR_ELT(df, nRvar, allocVector(INTSXP,nrecs));
	    break;
	case 3:
	    SET_VECTOR_ELT(df, nRvar, allocVector(REALSXP,nrecs));
	    break;
	case 4:
	    SET_VECTOR_ELT(df, nRvar, allocVector(LGLSXP,nrecs));
	    break;
	default:
	    continue;
	}
	SET_STRING_ELT(varlabels, nRvar, mkChar(szTitle));
	nRvar++;
    }

    for(iRecord = 0; iRecord < nrecs; iRecord++)
    {
	nRvar = 0;
	for(i = 0; i < nflds; i++)
	    switch(types[i]) {
	    case 1:
		if( DBFIsAttributeNULL( hDBF, iRecord, i ))
		    SET_STRING_ELT(VECTOR_ELT(df, nRvar), iRecord, NA_STRING);
		else
		    SET_STRING_ELT(VECTOR_ELT(df, nRvar), iRecord, 
				   mkChar(DBFReadStringAttribute( hDBF, iRecord, i)));
		nRvar++;
		break;

	    case 2:
		if( DBFIsAttributeNULL( hDBF, iRecord, i ))
		    INTEGER(VECTOR_ELT(df, nRvar))[iRecord] = NA_INTEGER;
		else
		    INTEGER(VECTOR_ELT(df, nRvar))[iRecord] =
			DBFReadIntegerAttribute( hDBF, iRecord, i );
		nRvar++;
		break;

	    case 3:
		if( DBFIsAttributeNULL( hDBF, iRecord, i ))
		    REAL(VECTOR_ELT(df, nRvar))[iRecord] = NA_REAL;
		else
		    REAL(VECTOR_ELT(df, nRvar))[iRecord] =
			DBFReadDoubleAttribute( hDBF, iRecord, i );
		nRvar++;
		break;

	    case 4:
		if( DBFIsAttributeNULL( hDBF, iRecord, i ))
		    LOGICAL(VECTOR_ELT(df, nRvar))[iRecord] = NA_LOGICAL;
		else {
		    p = DBFReadStringAttribute( hDBF, iRecord, i );
		    switch(*p){
		    case 'f':
		    case 'F':
		    case 'n':
		    case 'N':
			val = 0;
			break;
		    case 't':
		    case 'T':
		    case 'y':
		    case 'Y':
			val = 1;
			break;
		    case '?':
			val = NA_LOGICAL;
			break;
		    default:
			warning("value |%d| found in logical field", *p);
			val = NA_LOGICAL;
			break;
		    }
		    LOGICAL(VECTOR_ELT(df, nRvar))[iRecord] = val;
		}
		nRvar++;
		break;
	    default:
		break;
	    }
    }
    DBFClose( hDBF );
    PROTECT(tmp = mkString("data.frame")); pc++;
    setAttrib(df, R_ClassSymbol, tmp);
    setAttrib(df, R_NamesSymbol, varlabels);
    setAttrib(df, install("data_types"), DataTypes);
    PROTECT(row_names = allocVector(STRSXP, nrecs)); pc++;
    for (i = 0; i < nrecs; i++) {
        sprintf(labelbuff, "%d", i+1);
        SET_STRING_ELT(row_names, i, mkChar(labelbuff));
    }
    setAttrib(df, R_RowNamesSymbol, row_names);

    UNPROTECT(pc);
    return(df);
}
