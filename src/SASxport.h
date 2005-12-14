/*
 *  $Id: SASxport.h,v 1.1 2001/03/23 16:15:26 bates Exp $
 *
 *  This file is derived from code in the SAS Technical Support
 *  document TS-140 "The Record Layout of a Data Set in SAS Transport
 *  (XPORT) Format" available as 
 *       http://ftp.sas.com/techsup/download/technote/ts140.html
 */

#ifndef SASEXPORT_H
#define SASEXPORT_H

#include <string.h>		/* for memcpy and memset */
#include "foreign.h"
#include "swap_bytes.h"

/* double cnxptiee(double from, int fromtype, int totype); */



struct SAS_XPORT_header { 
  char sas_symbol[2][8];	/* should be "SAS     " */
  char saslib[8];		/* should be "SASLIB  " */
  char sasver[8];
  char sas_os[8];
  char sas_create[16];
  char sas_mod[16];
};

struct SAS_XPORT_member { 
  char sas_symbol[8];
  char sas_dsname[8];
  char sasdata[8];
  char sasver[8]; 
  char sas_osname[8];
  char sas_create[16];
  char sas_mod[16];
};

struct SAS_XPORT_namestr {
    short   ntype;              /* VARIABLE TYPE: 1=NUMERIC, 2=CHAR    */
    short   nhfun;              /* HASH OF NNAME (always 0)            */
    short   nlng;               /* LENGTH OF VARIABLE IN OBSERVATION   */
    short   nvar0;              /* VARNUM                              */
    char    nname[8];		/* NAME OF VARIABLE                    */
    char    nlabel[40];		/* LABEL OF VARIABLE                   */
    char    nform[8];		/* NAME OF FORMAT                      */
    short   nfl;                /* FORMAT FIELD LENGTH OR 0            */
    short   nfd;                /* FORMAT NUMBER OF DECIMALS           */
    short   nfj;                /* 0=LEFT JUSTIFICATION, 1=RIGHT JUST  */
    char    nfill[2];           /* (UNUSED, FOR ALIGNMENT AND FUTURE)  */
    char    niform[8];		/* NAME OF INPUT FORMAT                */
    short   nifl;               /* INFORMAT LENGTH ATTRIBUTE           */
    short   nifd;               /* INFORMAT NUMBER OF DECIMALS         */
    int     npos;               /* POSITION OF VALUE IN OBSERVATION    */
    char    rest[52];           /* remaining fields are irrelevant     */
};

#ifdef WORDS_BIGENDIAN

#define char_to_short(from, to)			\
    do { (to) = *((short *)(from)); } while (0)

#define char_to_int(from, to)			\
    do { (to) = *((int *)(from)); } while (0)

#define char_to_uint(from, to)			\
    do { (to) = *((unsigned int *)(from)); } while (0)

#else

#define char_to_short(from, to)			\
do {						\
    swap_bytes_short(*((short *)(from)), to);	\
} while (0)

#define char_to_int(from, to)			\
do {						\
    swap_bytes_int(*((int *)(from)), to);	\
} while (0)

#define char_to_uint(from, to)			\
do {						\
    swap_bytes_uint(*((unsigned int *)(from)), to);	\
} while (0)

#endif /* WORDS_BIGENDIAN */

#endif /* SASEXPORT_H */
