         TITLE 'ELS STORAGE MANAGEMENT AREA POINTER SAVE PGM'           00001   
*                                                                       00002   
*      PROGRAM-ID.         ELUSAVAD.                                    00003   
*                                                                       00004   
*      AUTHOR.             EDWARD G LISS                                00005   
*                                                                       00006   
*      INSTALLATION.       HEALTH CARE SERVICE CORPORATION              00007   
*                          A MUTUAL LEGAL RESERVE COMPANY               00008   
*                          BLUE CROSS/BLUE SHIELD OF ILLINOIS           00009   
*                          233 N. MICHIGAN AVE                          00010   
*                          CHICAGO, ILLINOIS 60601                      00011   
*                                                                       00012   
*      DATE-WRITTEN.       20-JUN-1988.                                 00013   
*                                                                       00014   
*      SECURITY.           COPYRIGHT 1988,                              00015   
*                          HEALTH CARE SERVICE CORPORATION              00016   
*                                                                       00017   
******************************************************************      00018   
*      DESCRIPTION   -  THIS MODULE WILL HANDLE POINTER MANAGEMENT.     00019   
*                       THESE ROUTINES WILL PROVIDE A FLEXABLE          00020   
*                       WAY OF RETRIEVING POINTERS FROM A STORAGE       00021   
*                       MANAGEMENT TABLE.  THE SCHEME IMPLEMENTED       00022   
*                       BY THESE MODULES WILL AVOID THE MASS COMPILE    00023   
*                       BLUES CREATED BY THE OLD ELS STORAGE MANAGE-    00024   
*                       SCHEME.                                         00025   
*                                                                       00026   
*                       EACH ROUTINE WILL BE DOCUMENTED ON HOW TO       00027   
*                       CALL BEFORE IT BEGINS.                          00028   
*                                                                       00029   
*                                                                       00030   
         SPACE 3                                                        00031   
******************************************************************      00032   
**AKK 12/06/05 REGEN FOR TEST                                    *      00033   
*                      MAINTENANCE HISTORY                       *      00034   
*                                                                *      00035   
*  MOD     DATE     BY  DRPT                ACTION               *      00036   
* ----- ----------- --- ----- ---------------------------------- *      00037   
* 01.00 20-JUN-1988 EGL       CREATED                            *      00038   
* 01.01 03-APR-1989 EGL       CHANGED SERIAL SEARCH TO BINARY    *      00039   
*                             SEARCH                             *      00040   
*                                                                *      00041   
* 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *      00042   
*                                                                *      00043   
* 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *      00044   
*                                                                *      00045   
* 02.01 09-JAN-2004 AKK INTERTEST S0C7                           *      00046   
*                                                                *      00047   
* 02.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *      00048   
*                                                                *      00049   
******************************************************************      00050   
         EJECT                                                          00051   
*                                                                       00052   
*                                                                       00053   
*    REGISTER EQUS                                                      00054   
*                                                                       00055   
R0       EQU   0                                                        00056   
R1       EQU   1                                                        00057   
R2       EQU   2                                                        00058   
R3       EQU   3                                                        00059   
R4       EQU   4                                                        00060   
R5       EQU   5                                                        00061   
R6       EQU   6                                                        00062   
R7       EQU   7                                                        00063   
R8       EQU   8                                                        00064   
R9       EQU   9                                                        00065   
R10      EQU   10                                                       00066   
R11      EQU   11                                                       00067   
R12      EQU   12                                                       00068   
R13      EQU   13                                                       00069   
R14      EQU   14                                                       00070   
R15      EQU   15                                                       00071   
         EJECT                                                          00072   
******************************************************************      00073   
*                                                                       00074   
*      ELUSAVAD                                                         00075   
*      DESCRIPTION   -  THIS MODULE WILL LOOKUP THE REQUESTED           00076   
*                       DDNAME IN THE SMA TABLE.  THE ADDRESS OF        00077   
*                       THE AREA WILL BE STORED.  IF THE DDNAME         00078   
*                       IS NOT FOUND, A RETURN CODE (IN CIA) WILL       00079   
*                       BE RETURNED.                                    00080   
*                                                                       00081   
*      CALL          -  SET CIA-DDNAME-RECORD TO TRUE.                  00082   
*                       CALL 'ELUSAVAD' USING DFHCOMMAREA               00083   
*                          ADDRESS OF DDNAME-RECORD.                    00084   
*                                                                       00085   
*                       THE ADDRESS OF THE DDNAME-RECORD WILL BE        00086   
*                       SAVED.                                          00087   
*                                                                       00088   
         USING ELUSAVAD,R12                                             00089   
ELUSAVAD CSECT                                                          00090   
         B     12(0,R15)               BYPASS TITLE                     00091   
         DC    CL8'ELUSAVAD'           CSECT NAME                       00092   
         STM   R14,R12,12(R13)         SAVE REGISTERS IN CALLERS S/A    00093   
         LR    R12,R15                 ESTABLISH BASE REGISTER          00094   
         L     R2,0(0,R1)              GET ADDRESS OF COMMAREA          00095   
**       L     R2,0(0,R2)              GET ADDRESS OF SMA AREA          00096   
**       LA    R4,2(0,R2)              SET ADDRESS OF FIRST ITEM        00097   
**       USING SMA,R4                  ESTABLISH SMA ENTRY              00098   
**       L     R3,SMAPTR               CIA POINTER ALWAYS AT FIRST      00099   
**       USING CIA,R3                  ESTABLISH CIA AREA               00100   
**   TRANSPARENCY PATCH                                                 00101   
         L     R3,0(0,R2)              SET ADDRESS OF CIA               00102   
         USING CIA,R3                  ESTABLISH CIA AREA               00103   
         LA    R2,CIASMA               SET ADDRESS OF SMA               00104   
         USING SMA,R2                  ESTABLISH SMA AREA               00105   
**   END OF PATCH                                                       00106   
         LA    R4,SMAITEM1             SET ADDRESS OF SMA TABLE         00107   
         USING SMATABLE,R4                                              00108   
         LA    R11,CIADDN             LOAD ADDRESS OF DDNAME TO SEARCH  00109   
         L     R15,=A(SEARCH)         LOAD ADDRESS OF SEARCH ROUTINE    00110   
         BALR  R14,R15                CALL SEARCH ROUTINE               00111   
         LTR   R4,R4                  TEST FOR NULL POINTER             00112   
         BZ    SAVNODDN               BRANCH IS DDNAME NOT FOUND        00113   
         L     R5,4(0,R1)             GET 2ND PARM ADDRESS              00114   
         L     R5,0(0,R5)             GET 2ND PARM                      00115   
         ST    R5,SMAPTR              SAVE POINTER                      00116   
         MVC   CIARTNCD,=XL2'0000'    SET RETURN CODE DDNAME OK         00117   
         B     SAVEXIT                                                  00118   
SAVNODDN EQU   *                                                        00119   
         MVC   CIARTNCD,=XL2'0001'    SET RETURN CODE DDNAME NOT FND    00120   
SAVEXIT  EQU   *                                                        00121   
         LM    R14,R12,12(R13)         RESTORE THE REGISTERS            00122   
         BR    R14                     RETURN TO CALLER                 00123   
         LTORG                                                          00124   
         DROP  R12,R2,R3,R4                                             00125   
         EJECT                                                          00126   
*                                                                       00127   
*   SEARCH ROUTINE                                                      00128   
*    UPON CALL, THIS ROUTINE EXPECTS:                                   00129   
*      R2 TO POINT AT THE SMA MAX OCCURS                                00130   
*      R4  POINTS TO THE 1ST SMA TABLE ENTRY.                           00131   
*      R11 TO POINT AT THE DDNAME TO SEARCH FOR                         00132   
*      R14 TO BE THE RETURN ADDRESS                                     00133   
*      R15 TO BE THE ENTRY POINT FOR THIS ROUTINE                       00134   
*                                                                       00135   
*    UPON EXIT, THIS ROUTINE SET REGISTERS AS FOLLOWS:                  00136   
*      R4  POINTS TO THE SMA TABLE ENTRY.                               00137   
*      R4  CONTAINS ZERO IF THE DDNAME IS NOT FOUND                     00138   
*      R5  IS UNDEFINED                                                 00139   
*      R6  IS UNDEFINED                                                 00140   
*      R7  IS UNDEFINED                                                 00141   
*      R8  IS UNDEFINED                                                 00142   
*      R10 IS UNDEFINED                                                 00143   
*                                                                       00144   
         USING SEARCH,R15                                               00145   
         USING SMA,R2                                                   00146   
         USING SMATABLE,R4                                              00147   
SEARCH   EQU   *                                                        00148   
         CLC   SMADDN,0(R11)      COMPARE 1ST ENTRY AND PARM            00149   
         BH    SEARCHFL           BRANCH IF PARM < 1ST ENTRY            00150   
         BE    SEARCHDN           BRANCH IF PARM = 1ST ENTRY            00151   
*                                                                       00152   
         LA    R6,1               ESTABLISH SEARCH LOW BOUND            00153   
         LH    R7,SMACOUNT        GET NUMBER OF ITEMS IN TABLE          00154   
*                                 AS SEARCH HIGH BOUND                  00155   
SEARCHLP EQU   *                                                        00156   
         LA    R8,1(R6,R7)        COMPUTE MID POINT                     00157   
         SRA   R8,1                  OF SEARCH BOUNDS                   00158   
         LA    R4,SMASIZE         GET LENGTH OF EACH TABLE ENTRY        00159   
         LR    R5,R8                                                    00160   
         BCTR  R5,0                                                     00161   
         MR    R4,R4              COMPUTE OFFSET INTO TABLE             00162   
         LA    R4,SMAITEM1(R5)    COMPUTE ADDRESS OF DDNAME IN TABLE    00163   
         CLC   SMADDN,0(R11)      COMPARE DDNAMES                       00164   
         BE    SEARCHDN           IF =, BRANCH                          00165   
         BH    RESETHI                                                  00166   
         CR    R6,R8                                                    00167   
         BE    SEARCHFL           IF NOT =, CONTINUE                    00168   
         LR    R6,R8                                                    00169   
         B     SEARCHLP                                                 00170   
RESETHI  EQU   *                                                        00171   
         CR    R7,R8                                                    00172   
         BE    SEARCHFL           IF NOT =, CONTINUE                    00173   
         LR    R7,R8                                                    00174   
         B     SEARCHLP                                                 00175   
SEARCHFL EQU   *                                                        00176   
         LA    R4,0               SET R4 TO NULL                        00177   
SEARCHDN EQU   *                                                        00178   
         BR    R14                                                      00179   
         LTORG                                                          00180   
         DROP  R15,R2,R4                                                00181   
         EJECT                                                          00182   
         COPY  ELSCIA2D                                                 00183   
         SPACE 3                                                        00184   
         COPY  ELSSMAD                                                  00185   
         END                                                            00186   
