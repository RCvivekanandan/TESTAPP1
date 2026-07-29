         TITLE 'ELS STORAGE MANAGEMENT AREA ADDRESS RETRIEVE PGM'       00001   
*                                                                       00002   
*      PROGRAM-ID.         ELUSETAD.                                    00003   
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
*                                                                       00015   
*      SECURITY.           COPYRIGHT 1988,                              00016   
*                          HEALTH CARE SERVICE CORPORATION              00017   
**AKK 12/06/05 REGEN FOR TEST                                           00018   
******************************************************************      00019   
*      DESCRIPTION   -  THIS MODULE WILL HANDLE POINTER MANAGEMENT.     00020   
*                       THESE ROUTINES WILL PROVIDE A FLEXABLE          00021   
*                       WAY OF RETRIEVING POINTERS FROM A STORAGE       00022   
*                       MANAGEMENT TABLE.  THE SCHEME IMPLEMENTED       00023   
*                       BY THESE MODULES WILL AVOID THE MASS COMPILE    00024   
*                       BLUES CREATED BY THE OLD ELS STORAGE MANAGE-    00025   
*                       SCHEME.                                         00026   
*                                                                       00027   
*                       EACH ROUTINE WILL BE DOCUMENTED ON HOW TO       00028   
*                       CALL BEFORE IT BEGINS.                          00029   
*                                                                       00030   
         SPACE 3                                                        00031   
******************************************************************      00032   
*                      MAINTENANCE HISTORY                       *      00033   
*                                                                *      00034   
*  MOD     DATE     BY  DRPT                ACTION               *      00035   
* ----- ----------- --- ----- ---------------------------------- *      00036   
* 01.00 20-JUN-1988 EGL       CREATED                            *      00037   
* 01.01 03-APR-1989 EGL       CHANGED SERIAL SEARCH TO BINARY    *      00038   
*                             SEARCH                             *      00039   
*                                                                *      00040   
* 01.02 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *      00041   
*                                                                *      00042   
* 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *      00043   
*                                                                *      00044   
* 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *      00045   
*                                                                *      00046   
* 02.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *      00047   
*                                                                *      00048   
******************************************************************      00049   
         EJECT                                                          00050   
*                                                                       00051   
*    REGISTER EQUS                                                      00052   
*                                                                       00053   
R0       EQU   0                                                        00054   
R1       EQU   1                                                        00055   
R2       EQU   2                                                        00056   
R3       EQU   3                                                        00057   
R4       EQU   4                                                        00058   
R5       EQU   5                                                        00059   
R6       EQU   6                                                        00060   
R7       EQU   7                                                        00061   
R8       EQU   8                                                        00062   
R9       EQU   9                                                        00063   
R10      EQU   10                                                       00064   
R11      EQU   11                                                       00065   
R12      EQU   12                                                       00066   
R13      EQU   13                                                       00067   
R14      EQU   14                                                       00068   
R15      EQU   15                                                       00069   
         EJECT                                                          00070   
******************************************************************      00071   
*                                                                       00072   
*      ELUSETAD                                                         00073   
*      DESCRIPTION   -  THIS MODULE WILL LOOKUP THE REQUESTED           00074   
*                       DDNAME IN THE SMA TABLE.  THE ADDRESS OF        00075   
*                       THE AREA WILL BE RETURNED.  IF THE DDNAME       00076   
*                       IS NOT FOUND, A RETURN CODE (IN CIA) WILL       00077   
*                       BE RETURNED.                                    00078   
*                                                                       00079   
*      CALL          -  SET CIA-DDNAME-RECORD TO TRUE.                  00080   
*                       CALL 'ELUSETAD' USING DFHCOMMAREA               00081   
*                          ADDRESS OF DDNAME-RECORD.                    00082   
*                                                                       00083   
*                       THE ADDRESS OF THE DDNAME-RECORD WILL BE        00084   
*                       ESTABLISHED.  IF THE ADDRESS IS NULL,           00085   
*                       A RETURN CODE WILL BE SET AND THE NULL          00086   
*                       ADDRESS RETURNED.                               00087   
*                                                                       00088   
         USING ELUSETAD,R12                                             00089   
ELUSETAD CSECT                                                          00090   
         B     12(0,R15)               BYPASS TITLE                     00091   
         DC    CL8'ELUSETAD'           CSECT NAME                       00092   
         STM   R14,R12,12(R13)         SAVE REGISTERS IN CALLERS S/A    00093   
         LR    R12,R15                 ESTABLISH BASE REGISTER          00094   
         L     R2,0(0,R1)              GET ADDRESS OF COMMAREA          00095   
**       L     R2,0(0,R2)              GET ADDRESS OF SMA AREA          00096   
**       LA    R4,2(0,R2)              SET ADDRESS OF FIRST ITEM        00097   
**       USING SMA,R4                  ESTABLISH SMA ENTRY              00098   
**       L     R3,SMAPTR               CIA POINTER ALWAYS AT FIRST      00099   
**       USING CIA,R3                  ESTABLISH CIA AREA               00100   
**  TRANSPARENCY PATCH                                                  00101   
         L     R3,0(0,R2)              GET ADDRESS OF CIA               00102   
         USING CIA,R3                  ESTABLISH CIA ENTRY              00103   
         LA    R2,CIASMA               GET ADDRESS OF SMA AREA          00104   
         USING SMA,R2                  ESTABLISH SMA AREA               00105   
**  END TRANSPARENCY PATCH                                              00106   
         LA    R4,SMAITEM1             GET ADDRESS OF SMA TABLE         00107   
         USING SMATABLE,R4             ESTABLISH SMA TABLE              00108   
         LA    R11,CIADDN             LOAD ADDRESS OF DDNAME TO SEARCH  00109   
         L     R15,=A(SEARCH)         LOAD ADDRESS OF SEARCH ROUTINE    00110   
         BALR  R14,R15                CALL SEARCH ROUTINE               00111   
         LTR   R15,R4                 TEST FOR NULL POINTER             00112   
         BZ    SETNODDN               BRANCH IS DDNAME NOT FOUND        00113   
         LH    R6,SMALEN              RETURN THE AREA LENGTH            00114   
         ST    R6,CIALEN                                                00115   
*********MVC   CIATYPE,SMATYP         RETURN THE AREA TYPE              00116   
         MVC   CIAMVO,SMAMVO          RETURN THE MAX OFFSETS            00117   
         L     R15,SMAPTR             GET AREA ADDRESS                  00118   
         LTR   R15,R15                TEST FOR NULL POINTER             00119   
         BZ    SETNULL                BRANCH IS AREA NOT ALLOCATED      00120   
         MVC   CIARTNCD,=XL2'0000'    SET RETURN CODE OK                00121   
         B     SETEXIT                                                  00122   
SETNODDN EQU   *                                                        00123   
         MVC   CIARTNCD,=XL2'0001'    SET RETURN CODE DDNAME NOT FND    00124   
         B     SETEXIT                                                  00125   
SETNULL  EQU   *                                                        00126   
         MVC   CIARTNCD,=XL2'0002'    SET RETURN CODE NOT ALLOCATED     00127   
SETEXIT  EQU   *                                                        00128   
         L     R5,4(0,R1)             GET 2ND PARM ADDRESS              00129   
         ST    R15,0(0,R5)            SAVE ADDRESS IN 2ND PARM          00130   
*                                                                       00131   
         LM    R14,R12,12(R13)         RESTORE THE REGISTERS            00132   
         BR    R14                     RETURN TO CALLER                 00133   
         LTORG                                                          00134   
         DROP  R12,R2,R3,R4                                             00135   
         EJECT                                                          00136   
*                                                                       00137   
*   SEARCH ROUTINE                                                      00138   
*    UPON CALL, THIS ROUTINE EXPECTS:                                   00139   
*      R2 TO POINT AT THE SMA MAX OCCURS                                00140   
*      R4 TO  POINT TO THE 1ST SMA TABLE ENTRY                          00141   
*      R11 TO POINT AT THE DDNAME TO SEARCH FOR                         00142   
*      R14 TO BE THE RETURN ADDRESS                                     00143   
*      R15 TO BE THE ENTRY POINT FOR THIS ROUTINE                       00144   
*                                                                       00145   
*    UPON EXIT, THIS ROUTINE SET REGISTERS AS FOLLOWS:                  00146   
*      R4  POINTS TO THE SMA TABLE ENTRY.                               00147   
*      R4  CONTAINS ZERO IF THE DDNAME IS NOT FOUND                     00148   
*      R5  IS UNDEFINED                                                 00149   
*      R6  IS UNDEFINED                                                 00150   
*      R7  IS UNDEFINED                                                 00151   
*      R8  IS UNDEFINED                                                 00152   
*      R10 IS UNDEFINED                                                 00153   
*                                                                       00154   
         USING SEARCH,R15                                               00155   
         USING SMA,R2                                                   00156   
         USING SMATABLE,R4                                              00157   
SEARCH   EQU   *                                                        00158   
         CLC   SMADDN,0(R11)      COMPARE 1ST ENTRY AND PARM            00159   
         BH    SEARCHFL           BRANCH IF PARM < 1ST ENTRY            00160   
         BE    SEARCHDN           BRANCH IF PARM = 1ST ENTRY            00161   
*                                                                       00162   
         LA    R6,1               ESTABLISH SEARCH LOW BOUND            00163   
         LH    R7,SMACOUNT        GET NUMBER OF ITEMS IN TABLE          00164   
*                                 AS SEARCH HIGH BOUND                  00165   
SEARCHLP EQU   *                                                        00166   
         LA    R8,1(R6,R7)        COMPUTE MID POINT                     00167   
         SRA   R8,1                  OF SEARCH BOUNDS                   00168   
         LA    R4,SMASIZE         GET LENGTH OF EACH TABLE ENTRY        00169   
         LR    R5,R8                                                    00170   
         BCTR  R5,0                                                     00171   
         MR    R4,R4              COMPUTE OFFSET INTO TABLE             00172   
         LA    R4,SMAITEM1(R5)    COMPUTE ADDRESS OF DDNAME IN TABLE    00173   
         CLC   SMADDN,0(R11)      COMPARE DDNAMES                       00174   
         BE    SEARCHDN           IF =, BRANCH                          00175   
         BH    RESETHI                                                  00176   
         CR    R6,R8                                                    00177   
         BE    SEARCHFL           IF NOT =, CONTINUE                    00178   
         LR    R6,R8                                                    00179   
         B     SEARCHLP                                                 00180   
RESETHI  EQU   *                                                        00181   
         CR    R7,R8                                                    00182   
         BE    SEARCHFL           IF NOT =, CONTINUE                    00183   
         LR    R7,R8                                                    00184   
         B     SEARCHLP                                                 00185   
SEARCHFL EQU   *                                                        00186   
         LA    R4,0               SET R4 TO NULL                        00187   
SEARCHDN EQU   *                                                        00188   
         BR    R14                                                      00189   
         LTORG                                                          00190   
         DROP  R15,R2,R4                                                00191   
         EJECT                                                          00192   
         COPY ELSCIA2D                                                  00193   
         SPACE 3                                                        00194   
         COPY ELSSMAD                                                   00195   
         END                                                            00196   
