/*
 *  $Id: SASxport.c,v 1.3 1999/12/16 21:18:17 saikat Exp $
 *
 *  Read SAS transport data set format
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

#include <stdio.h>
#include <string.h>
#include "S.h"
#include "Rinternals.h"
#include "SASxport.h"

#define HEADER_BEG "HEADER RECORD*******"
#define HEADER_TYPE_LIBRARY "LIBRARY "
#define HEADER_TYPE_MEMBER "MEMBER  "
#define HEADER_TYPE_DSCRPTR "DSCRPTR "
#define HEADER_TYPE_NAMESTR "NAMESTR "
#define HEADER_TYPE_OBS "OBS     "
#define HEADER_END "HEADER RECORD!!!!!!!000000000000000000000000000000  "

#define LIB_HEADER HEADER_BEG HEADER_TYPE_LIBRARY HEADER_END
#define MEM_HEADER HEADER_BEG HEADER_TYPE_MEMBER \
                   "HEADER RECORD!!!!!!!000000000000000001600000000"
#define DSC_HEADER HEADER_BEG HEADER_TYPE_DSCRPTR HEADER_END
#define NAM_HEADER HEADER_BEG HEADER_TYPE_NAMESTR \
                   "HEADER RECORD!!!!!!!000000"
#define OBS_HEADER HEADER_BEG HEADER_TYPE_OBS HEADER_END
#define BLANK24 "                        "

#define GET_RECORD(rec, fp, len) \
          fread((rec), (size_t) 1, (size_t) (len), (fp))

#define IS_SASNA_CHAR(c) ((c) == 0x5f || (c) == 0x2e || (0x4l <= (c) \
							 && (c) <= 0x5a))

#ifndef NULL
#define NULL ((void *) 0)
#endif

static double
get_ieee64(char *record)
{
  union {
    double d;
    char c[8];
  } u;
  cnxptiee(record, CN_TYPE_XPORT, u.c, CN_TYPE_NATIVE);
  return u.d;
}

static short
get_int16(const char *record)
{
  short sval = 0;
#ifdef WORDS_BIGENDIAN
  ((char *)&sval)[sizeof(short) - 2] = record[0];
  ((char *)&sval)[sizeof(short) - 1] = record[1];
#else
  ((char *)&sval)[1] = record[0];
  ((char *)&sval)[0] = record[1];
#endif
  return (short) sval;
}

static long
get_int32(const char *record)
{
  long lval = 0;
#ifdef WORDS_BIGENDIAN
  ((char *)&lval)[sizeof(long) - 4] = record[0];
  ((char *)&lval)[sizeof(long) - 3] = record[1];
  ((char *)&lval)[sizeof(long) - 2] = record[2];
  ((char *)&lval)[sizeof(long) - 1] = record[3];
#else
  ((char *)&lval)[3] = record[0];
  ((char *)&lval)[2] = record[1];
  ((char *)&lval)[1] = record[2];
  ((char *)&lval)[0] = record[3];
#endif
  return lval;
}

static int
get_nam_header(FILE *fp, struct SAS_XPORT_namestr *namestr, int length)
{
  char record[141];
  int n;

  record[length] = '\0';
  n = GET_RECORD(record, fp, length);
  if(n != length)
    return 0;

  namestr->ntype = get_int16(record);
  namestr->nhfun = get_int16(record + 2);
  namestr->nlng  = get_int16(record + 4);
  namestr->nvar0 = get_int16(record + 6);
  Memcpy(namestr->nname, record + 8, 8);
  Memcpy(namestr->nlabel, record + 16, 40);
  Memcpy(namestr->nform, record + 56, 8);
  namestr->nfl   = get_int16(record + 64);
  namestr->nfd   = get_int16(record + 66);
  namestr->nfj   = get_int16(record + 68);
  Memcpy(namestr->nfill, record + 70, 2);
  Memcpy(namestr->niform, record + 72, 8);
  namestr->nifl  = get_int16(record + 80);
  namestr->nifd  = get_int16(record + 82);
  namestr->npos  = get_int32(record + 84);
  return 1;
}

static int
get_lib_header(FILE *fp, struct SAS_XPORT_header *head)
{
  char record[81];
  int n;

  n = GET_RECORD(record, fp, 80);
  if(n == 80 && strncmp(LIB_HEADER, record, 80) != 0)
    error("File not in SAS transfer format");
  
  n = GET_RECORD(record, fp, 80);
  if(n != 80)
    return 0;
  record[80] = '\0';
  Memcpy(head->sas_symbol[0], record, 8);
  Memcpy(head->sas_symbol[1], record+8, 8);
  Memcpy(head->saslib, record+16, 8);
  Memcpy(head->sasver, record+24, 8);
  Memcpy(head->sas_os, record+32, 8);
  if((strrchr(record+40, ' ') - record) != 63)
    return 0;
  Memcpy(head->sas_create, record+64, 16);

  n = GET_RECORD(record, fp, 80);
  if(n != 80)
    return 0;
  record[80] = '\0';
  Memcpy(head->sas_mod, record, 16);
  if((strrchr(record+16, ' ') - record) != 79)
    return 0;
  return 1;
}
  
static int
get_mem_header(FILE *fp, struct SAS_XPORT_member *member)
{
  char record[81];
  int n;

  n = GET_RECORD(record, fp, 80);
  if(n != 80 || strncmp(DSC_HEADER, record, 80) != 0)
    error("File not in SAS transfer format");
  
  n = GET_RECORD(record, fp, 80);
  if(n != 80)
    return 0;
  record[80] = '\0';
  Memcpy(member->sas_symbol, record, 8);
  Memcpy(member->sas_dsname, record+8, 8);
  Memcpy(member->sasdata, record+16, 8);
  Memcpy(member->sasver, record+24, 8);
  Memcpy(member->sas_osname, record+32, 8);
  if((strrchr(record+40, ' ') - record) != 63)
    return 0;
  Memcpy(member->sas_create, record+64, 16);

  n = GET_RECORD(record, fp, 80);
  if(n != 80)
    return 0;
  Memcpy(member->sas_mod, record, 16);
  if((strrchr(record+16, ' ') - record) != 79)
    return 0;
  return 1;
}

static int
init_xport_info(FILE *fp)
{
  char record[81];
  int n;
  int namestr_length;

  struct SAS_XPORT_header *lib_head;

  lib_head = Calloc(1, struct SAS_XPORT_header);

  if(!get_lib_header(fp, lib_head))
    error("SAS transfer file has incorrect library header");

  n = GET_RECORD(record, fp, 80);
  if(n != 80 || strncmp(MEM_HEADER, record, 75) != 0 ||
     strncmp("  ", record+78, 2) != 0)
    error("File not in SAS transfer format");
  record[78] = '\0';
  sscanf(record+75, "%d", &namestr_length);

  Free(lib_head);

  return namestr_length;
}

static int
init_mem_info(FILE *fp, char *name)
{
  int length, n;
  char record[81];
  char *tmp;
  struct SAS_XPORT_member  *mem_head;

  mem_head = Calloc(1, struct SAS_XPORT_member);
  if(!get_mem_header(fp, mem_head))
    error("SAS transfer file has incorrect member header");

  n = GET_RECORD(record, fp, 80);
  record[80] = '\0';
  if(n != 80 || strncmp(NAM_HEADER, record, 54) != 0 ||
     (strrchr(record+58, ' ') - record) != 79)
    error("File not in SAS transfer format");
  record[58] = '\0';
  sscanf(record+54, "%d", &length);

  tmp = strchr(mem_head->sas_dsname, ' ');
  n = tmp - mem_head->sas_dsname;
  if(n > 0) {
    strncpy(name, mem_head->sas_dsname, n);
    name[n] = '\0';
  } else name[0] = '\0';

  Free(mem_head);

  return length;
}

static int
next_xport_info(FILE *fp, int namestr_length, int nvars, int *headpad,
		int *tailpad, int *length, int ntype[], int nlng[],
		int nvar0[], char *nname[], int npos[])
{
  char *tmp;
  char record[81];
  int i, n, nbytes, totwidth;
  int *nname_len;
  struct SAS_XPORT_namestr *nam_head;

  nam_head = Calloc(nvars, struct SAS_XPORT_namestr);

  for(i = 0; i < nvars; i++) {
    if(!get_nam_header(fp, nam_head+i, namestr_length))
      error("SAS transfer file has incorrect library header");
  }

  *headpad = 480 + nvars * namestr_length;
  i = *headpad % 80;
  if(i > 0) {
    i = 80 - i;
    fseek(fp, i, SEEK_CUR);
    (*headpad) += i;
  }
    
  n = GET_RECORD(record, fp, 80);
  if(n != 80 || strncmp(OBS_HEADER, record, 80) != 0)
    error("File not in SAS transfer format");
  
  nname_len = Calloc(nvars, int);

  for(i = 0; i < nvars; i++) {
    ntype[i] = nam_head[i].ntype;
    nlng[i]  = nam_head[i].nlng;
    nvar0[i] = nam_head[i].nvar0;
    npos[i]  = nam_head[i].npos;
    tmp = strchr(nam_head[i].nname, ' ');
    nname_len[i] = tmp - nam_head[i].nname;
  }

  for(i = 0; i < nvars; i++) {
    strncpy(nname[i], nam_head[i].nname, nname_len[i]);
    nname[i][nname_len[i]] = '\0';
  }

  Free(nname_len);
  Free(nam_head);

  nbytes = 0;

  while(!feof(fp)) {
    n = GET_RECORD(record, fp, 80);
    if(n == 80 && strncmp(MEM_HEADER, record, 75) == 0 &&
       strncmp("  ", record+78, 2) == 0) {
      record[78] = '\0';
      sscanf(record+75, "%d", &namestr_length);
      fseek(fp, -160, SEEK_CUR);
      n = GET_RECORD(record, fp, 80);
      fseek(fp, 80, SEEK_CUR);
      break;
    }
    nbytes += n;
  }

  totwidth = 0;
  for(i = 0; i < nvars; i++)
    totwidth += nlng[i];

  i = n - 1;
  while(i >= (n - nbytes)%totwidth) {
    if(record[i--] != ' ')
      break;
  }

  *length = ((nbytes - n) + i + totwidth)/totwidth;
  *tailpad = nbytes - *length * totwidth;

  return (feof(fp)?-1:namestr_length);
}

/*
 * get the list element named str.
 */

static SEXP
getListElement(SEXP list, char *str) {
  SEXP names;
  SEXP elmt = (SEXP) NULL;
  char *tempChar;
  int i;

  names = getAttrib(list, R_NamesSymbol);

  for (i = 0; i < LENGTH(list); i++) {
    tempChar = CHAR(STRING(names)[i]);
    if( strcmp(tempChar,str) == 0) {
      elmt = VECTOR(list)[i];
      break;
    }
  }
  return elmt;
}

#define VAR_INFO_LENGTH 9

const char *cVarInfoNames[] = {
  "headpad",
  "type",
  "width",
  "index",
  "position",
  "name",
  "sexptype",
  "tailpad",
  "length"
};

#define XPORT_VAR_HEADPAD(varinfo)   VECTOR(varinfo)[0]
#define XPORT_VAR_TYPE(varinfo)      VECTOR(varinfo)[1]
#define XPORT_VAR_WIDTH(varinfo)     VECTOR(varinfo)[2]
#define XPORT_VAR_INDEX(varinfo)     VECTOR(varinfo)[3]
#define XPORT_VAR_POSITION(varinfo)  VECTOR(varinfo)[4]
#define XPORT_VAR_NAME(varinfo)      VECTOR(varinfo)[5]
#define XPORT_VAR_SEXPTYPE(varinfo)  VECTOR(varinfo)[6]
#define XPORT_VAR_TAILPAD(varinfo)   VECTOR(varinfo)[7]
#define XPORT_VAR_LENGTH(varinfo)    VECTOR(varinfo)[8]

SEXP
xport_info(SEXP xportFile)
{
  FILE *fp;
  int i, namestrLength, memLength, ansLength;
  int *ntype;
  char *tmpchar, *dsname;
  char **nnames;
  SEXP ans, ansNames, newAns, newAnsNames, varInfoNames, varInfo;
  SEXP char_numeric, char_character;

  PROTECT(varInfoNames = allocVector(STRSXP, VAR_INFO_LENGTH));
  for(i = 0; i < VAR_INFO_LENGTH; i++)
    STRING(varInfoNames)[i] = mkChar(cVarInfoNames[i]);

  PROTECT(char_numeric   = mkChar("numeric"));
  PROTECT(char_character = mkChar("character"));

  dsname = Calloc(9, char);
  fp = fopen(CHAR(STRING(xportFile)[0]), "rb");
  namestrLength = init_xport_info(fp);

  ansLength = 0;
  PROTECT(ans = allocVector(VECSXP, 0));
  PROTECT(ansNames = allocVector(STRSXP, 0));

  while(namestrLength > 0 && (memLength = init_mem_info(fp, dsname)) > 0) {

    PROTECT(varInfo = allocVector(VECSXP, VAR_INFO_LENGTH));
    setAttrib(varInfo, R_NamesSymbol, varInfoNames);

    XPORT_VAR_TYPE(varInfo)     = allocVector(STRSXP, memLength);
    XPORT_VAR_WIDTH(varInfo)    = allocVector(INTSXP, memLength);
    XPORT_VAR_INDEX(varInfo)    = allocVector(INTSXP, memLength);
    XPORT_VAR_POSITION(varInfo) = allocVector(INTSXP, memLength);
    XPORT_VAR_NAME(varInfo)     = allocVector(STRSXP, memLength);
    XPORT_VAR_SEXPTYPE(varInfo) = allocVector(INTSXP, memLength);
    XPORT_VAR_HEADPAD(varInfo)  = allocVector(INTSXP, 1);
    XPORT_VAR_TAILPAD(varInfo)  = allocVector(INTSXP, 1);
    XPORT_VAR_LENGTH(varInfo)   = allocVector(INTSXP, 1);

    ntype     = Calloc(memLength, int);
    nnames    = Calloc(memLength, char *);
    tmpchar = Calloc(memLength * 9, char);
    for(i = 0; i < memLength; i++, tmpchar += 9)
      nnames[i] = tmpchar;

    namestrLength =
      next_xport_info(fp, namestrLength, memLength,
		      INTEGER(XPORT_VAR_HEADPAD(varInfo)),
		      INTEGER(XPORT_VAR_TAILPAD(varInfo)),
		      INTEGER(XPORT_VAR_LENGTH(varInfo)), ntype,
		      INTEGER(XPORT_VAR_WIDTH(varInfo)),
		      INTEGER(XPORT_VAR_INDEX(varInfo)), nnames,
		      INTEGER(XPORT_VAR_POSITION(varInfo)));

    for(i = 0; i < memLength; i++) {
      STRING(XPORT_VAR_TYPE(varInfo))[i] =
	(ntype[i] == 1) ? char_numeric : char_character;
      INTEGER(XPORT_VAR_SEXPTYPE(varInfo))[i] =
	(int) ((ntype[i] == 1) ? REALSXP : STRSXP);
      STRING(XPORT_VAR_NAME(varInfo))[i] = mkChar(nnames[i]);
    }
    PROTECT(newAns = allocVector(VECSXP, ansLength+1));
    PROTECT(newAnsNames = allocVector(STRSXP, ansLength+1));

    for(i = 0; i < ansLength; i++) {
      VECTOR(newAns)[i] = VECTOR(ans)[i];
      STRING(newAnsNames)[i] = STRING(ansNames)[i];
    }
    ans = newAns;
    ansNames = newAnsNames;

    STRING(ansNames)[ansLength] = mkChar(dsname);
    VECTOR(ans)[ansLength] = varInfo;
    ansLength++;
 
    Free(ntype);
    Free(nnames[0]);
    Free(nnames);
    UNPROTECT(5);
    PROTECT(ans);
    PROTECT(ansNames);
  }

  setAttrib(ans, R_NamesSymbol, ansNames);  
  UNPROTECT(5);
  Free(dsname);
  fclose(fp);
  return ans;
}

SEXP
xport_read(SEXP xportFile, SEXP xportInfo)
{
  int i, j, k, n;
  int nvar, nbyte;
  int ansLength, dataLength, totalWidth;
  int dataHeadPad, dataTailPad;
  int *dataWidth;
  int *dataPosition;
  SEXPTYPE *dataType;
  char *record, *tmpchar, *c;
  FILE *fp;
  SEXP ans, names, data, dataInfo, dataName;

  ansLength = LENGTH(xportInfo);
  PROTECT(ans = allocVector(VECSXP, ansLength));
  names = getAttrib(xportInfo, R_NamesSymbol);
  setAttrib(ans, R_NamesSymbol, names);

  fp = fopen(CHAR(STRING(xportFile)[0]), "rb");
  fseek(fp, 240, SEEK_SET);

  for(i = 0; i < ansLength; i++) {
    dataInfo = VECTOR(xportInfo)[i];
    dataName = getListElement(dataInfo, "name");
    nvar = LENGTH(dataName);
    dataLength = asInteger(getListElement(dataInfo, "length"));
    data = VECTOR(ans)[i] = allocVector(VECSXP, nvar);
    setAttrib(data, R_NamesSymbol, dataName);
    dataType = (SEXPTYPE *) INTEGER(getListElement(dataInfo, "sexptype"));
    for(j = 0; j < nvar; j++)
      VECTOR(data)[j] = allocVector(dataType[j], dataLength);

    dataWidth = INTEGER(getListElement(dataInfo, "width"));
    dataPosition = INTEGER(getListElement(dataInfo, "position"));

    totalWidth = 0;
    for(j = 0; j < nvar; j++)
      totalWidth += dataWidth[j];
    record = Calloc(totalWidth + 1, char);

    dataHeadPad = asInteger(getListElement(dataInfo, "headpad"));
    dataTailPad = asInteger(getListElement(dataInfo, "tailpad"));
    fseek(fp, dataHeadPad, SEEK_CUR);

    for(j = 0; j < dataLength; j++) {
      n = GET_RECORD(record, fp, totalWidth);
      if(n != totalWidth)
	error("Problem reading SAS transport file");

      for(k = nvar-1; k >= 0; k--) {
	tmpchar = record + dataPosition[k];
	if(dataType[k] == REALSXP) {
	  REAL(VECTOR(data)[k])[j] = get_ieee64(tmpchar);
	} else {
	  tmpchar[dataWidth[k]] = '\0';
	  if(strlen(tmpchar) == 1 && IS_SASNA_CHAR(tmpchar[0])) {
	    STRING(VECTOR(data)[k])[j] = R_NaString;
	  } else {
	    c = strchr(tmpchar, ' ');
	    *c = '\0';
	    STRING(VECTOR(data)[k])[j] = 
	      (c == tmpchar) ? R_BlankString : mkChar(tmpchar);
	  }
	}
      }
    }

    fseek(fp, dataTailPad, SEEK_CUR);

    Free(record);
  }
  UNPROTECT(1);
  fclose(fp);
  return ans;
}
