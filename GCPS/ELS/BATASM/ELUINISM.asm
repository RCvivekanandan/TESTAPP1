         TITLE 'ELS STORAGE MANAGEMENT AREA INITIALIZATION'             00001   
*                                                                       00002   
*      PROGRAM-ID.         ELUINISM.                                    00003   
*                                                                       00004   
*      AUTHOR.             EDWARD G LISS                                00005   
*                                                                       00006   
*                                                                       00007   
*      INSTALLATION.       HEALTH CARE SERVICE CORPORATION              00008   
*                          A MUTUAL LEGAL RESERVE COMPANY               00009   
*                          BLUE CROSS/BLUE SHIELD OF ILLINOIS           00010   
*                          233 N. MICHIGAN AVE                          00011   
*                          CHICAGO, ILLINOIS 60601                      00012   
*                                                                       00013   
*      DATE-WRITTEN.       20-JUN-1988.                                 00014   
*                                                                       00015   
*      SECURITY.           COPYRIGHT 1988,                              00016   
*                          HEALTH CARE SERVICE CORPORATION              00017   
*                                                                       00018   
*                                                                       00019   
******************************************************************      00020   
*      DESCRIPTION   -  THIS MODULE WILL HANDLE POINTER MANAGEMENT.     00021   
*                       THESE ROUTINES WILL PROVIDE A FLEXABLE          00022   
*                       WAY OF RETRIEVING POINTERS FROM A STORAGE       00023   
*                       MANAGEMENT TABLE.  THE SCHEME IMPLEMENTED       00024   
*                       BY THESE MODULES WILL AVOID THE MASS COMPILE    00025   
*                       BLUES CREATED BY THE OLD ELS STORAGE MANAGE-    00026   
*                       SCHEME.                                         00027   
*                                                                       00028   
         SPACE 3                                                        00029   
******************************************************************      00030   
**AKK 12/06/05 REGEN FOR TEST                                    *      00031   
*                      MAINTENANCE HISTORY                       *      00032   
*                                                                *      00033   
*  MOD     DATE     BY  DRPT                ACTION               *      00034   
* ----- ----------- --- ----- ---------------------------------- *      00035   
* 01.00 20-JUN-1988 EGL       CREATED                            *      00036   
*                                                                *      00037   
* 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *      00038   
*                                                                *      00039   
* 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *      00040   
*                                                                *      00041   
* 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *      00042   
*                                                                *      00043   
* 02.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *      00044   
*                                                                *      00045   
******************************************************************      00046   
         EJECT                                                          00047   
*                                                                       00048   
*    REGISTER EQUS                                                      00049   
*                                                                       00050   
R0       EQU   0                                                        00051   
R1       EQU   1                                                        00052   
R2       EQU   2                                                        00053   
R3       EQU   3                                                        00054   
R4       EQU   4                                                        00055   
R5       EQU   5                                                        00056   
R6       EQU   6                                                        00057   
R7       EQU   7                                                        00058   
R8       EQU   8                                                        00059   
R9       EQU   9                                                        00060   
R10      EQU   10                                                       00061   
R11      EQU   11                                                       00062   
R12      EQU   12                                                       00063   
R13      EQU   13                                                       00064   
R14      EQU   14                                                       00065   
R15      EQU   15                                                       00066   
         EJECT                                                          00067   
******************************************************************      00068   
*                                                                       00069   
*      ELUINISM                                                         00070   
*      DESCRIPTION   -  THIS MODULE WILL INITIALIZE THE STORAGE         00071   
*                       MANAGEMENT TABLE POINTERS.  EACH PROGRAM        00072   
*                       WHICH WILL CALL ELUSETAD OR ELUSAVAD MUST       00073   
*                       INITIALIZE BY CALLING ELUINISM AT LEAST         00074   
*                       ONE TIME.                                       00075   
*                                                                       00076   
*      CALL          -  CALL 'ELUINISM' USING DFHCOMMAREA               00077   
*                          ADDRESS OF ELS-CIA-COMMON-INTERFACE-AREA.    00078   
*                                                                       00079   
*                       THE ADDRESS OF THE COMMAREA WILL BE             00080   
*                       ESTABLISHED.  IF THE ADDRESS IS NULL,           00081   
*                       SOMETHING IS WRONG.                             00082   
*                                                                       00083   
         USING ELUINISM,R12                                             00084   
ELUINISM CSECT                                                          00085   
         B     12(0,R15)               BYPASS TITLE                     00086   
         DC    CL8'ELUINISM'           CSECT NAME                       00087   
         STM   R14,R12,12(R13)         SAVE REGISTERS IN CALLERS S/A    00088   
         LR    R12,R15                 ESTABLISH BASE REGISTER          00089   
         L     R2,0(0,R1)              GET ADDRESS OF COMMAREA          00090   
**       L     R2,0(0,R2)              GET ADDRESS OF SMA AREA          00091   
**       L     R3,SMAPTR               CIA POINTER ALWAYS AT FIRST      00092   
**       USING CIA,R3                  ESTABLISH CIA AREA               00093   
**  TRANSPARENCY PATCH                                                  00094   
         L     R3,0(0,R2)              GET CIA ADDRESS                  00095   
         USING CIA,R3                  ESTABLISH CIA AREA               00096   
**  END OF TRANSPARENCY PATCH                                           00097   
         L     R5,4(0,R1)             GET 2ND PARM                      00098   
         ST    R3,0(0,R5)             SAVE CIA POINTER TO PASS BACK     00099   
*                                     TO CALLER                         00100   
         MVC   CIARTNCD,=XL2'0000'    SET RETURN CODE OK                00101   
*                                                                       00102   
         LM    R14,R12,12(R13)         RESTORE THE REGISTERS            00103   
         BR    R14                     RETURN TO CALLER                 00104   
         LTORG                                                          00105   
         DROP  R12,R3                                                   00106   
         EJECT                                                          00107   
         COPY  ELSCIA2D                                                 00108   
         END                                                            00109   
