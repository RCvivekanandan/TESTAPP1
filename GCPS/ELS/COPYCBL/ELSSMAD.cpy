******************************************************************      00010000
*                                                                *      00020000
*    COPYBOOK:   ELSSMAC                                         *      00030000
*    DATE:       17-JUN-1986                                     *      00040000
*    AUTHOR:     EDWARD G. LISS                                  *      00050000
*    FUNCTION:   STORAGE MANAGEMENT AREA FOR ELS                 *      00060000
*                                                                *      00070000
*                CONTAINS A TABLE LISTING ALL THE VALID ELS      *      00080000
*                AREAS TO ENFORCE COMMON LOCATION OF STORAGE     *      00090000
*                TABLES.                                         *      00100000
*                                                                *      00110000
******************************************************************      00120000
*                                                                *      00130000
*                      MAINTENANCE HISTORY                       *      00140000
*                                                                *      00150000
*  MOD     DATE     BY  DRPT                ACTION               *      00160000
* ----- ----------- --- ----- ---------------------------------- *      00170000
* 01.00 17-JUN-1988 EGL       CREATED                            *      00180000
*                                                                *      00190000
******************************************************************      00200000
SMA      DSECT                                                          00210000
SMACOUNT DS    XL2         SMA-NUMBER-OF-ENTRIES                        00220000
SMAITEM1 DS    XL20        SMA FIRST ITEM                               00230002
SMATABLE DSECT             TABLE WHICH OCCURS SMA-NUMBER-OF-ENTRIES     00240002
SMADDN   DS    CL8         SMA-DDNAME                                   00250000
SMALEN   DS    XL2         SMA-LEN                                      00260000
SMATYP   DS    CL2         SMA-TYP                                      00270000
SMAPTR   DS    XL4         SMA-PTR                                      00280000
SMAMVO   DS    XL2         SMA-MVO                                      00290000
         DS    XL2         FILLER                                       00300000
SMASIZE  EQU   *-SMADDN                                                 00310000
