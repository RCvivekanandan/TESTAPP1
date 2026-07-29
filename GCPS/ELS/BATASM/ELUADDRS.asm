         TITLE 'ENGLISH LANGUAGE INQUIRY - ELUADDRS ROUTINE'            00001   
*                                                                       00002   
*      PROGRAM-ID.         ELUADDRS.                                    00003   
*                                                                       00004   
*      AUTHOR.             EDWARD G LISS                                00005   
*                                                                       00006   
*      INSTALLATION.       HEALTH CARE SERVICE CORPORATION              00007   
*                          A MUTUAL LEGAL RESERVE COMPANY               00008   
*                          BLUE CROSS/BLUE SHIELD OF ILLINOIS           00009   
*                          233 N. MICHIGAN AVE                          00010   
*                          CHICAGO, ILLINOIS 60601                      00011   
*                                                                       00012   
*      DATE-WRITTEN.       1-SEP-1987.                                  00013   
*                                                                       00014   
*      SECURITY.           COPYRIGHT 1987,                              00015   
*                          HEALTH CARE SERVICE CORPORATION              00016   
*                                                                       00017   
******************************************************************      00018   
*                                                                       00019   
*                                                                       00020   
*      DESCRIPTION   -  THIS MODULE WILL SET THE ADDRESS OF A           00021   
*                       LINKAGE SECTION ITEM TO A WORKING               00022   
*                       STORAGE ITEM.  COBOL II WILL NOT ALLOW          00023   
*                       THIS, THEREFORE THIS MODULE WAS NEEDED.         00024   
*                                                                       00025   
*                       I.E.   SET ADDRESS OF LS-ITEM TO                00026   
*                                  ADDRESS OF WS-ITEM                   00027   
*                       WILL NOT COMPILE.  TO DO THIS, CODE             00028   
*                              CALL 'ELUADDRS' USING                    00029   
*                                        WS-ITEM                        00030   
*                                        ADDRESS OF LS-ITEM.            00031   
*                                                                       00032   
         SPACE 3                                                        00033   
******************************************************************      00034   
*                                                                *      00035   
*                      MAINTENANCE HISTORY                       *      00036   
*                                                                *      00037   
*  MOD     DATE     BY  DRPT                ACTION               *      00038   
* ----- ----------- --- ----- ---------------------------------- *      00039   
* 01.00 01-SEP-1987 EGL       TRANSLATED FROM COBOL II           *      00040   
*                                                                *      00041   
* 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *      00042   
*                                                                *      00043   
* 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *      00044   
*                                                                *      00045   
* 02.01 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *      00046   
*                                                                *      00047   
******************************************************************      00048   
         EJECT                                                          00049   
ELUADDRS START 0                                                        00050   
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
*                                                                       00070   
         STM   R14,R12,12(R13)         SAVE REGISTERS IN CALLERS S/A    00071   
         LM    R2,R3,0(R1)             LOAD PARAMETER ADDRESSES         00072   
*                                      INTO REGISTERS                   00073   
         ST    R2,0(0,R3)              SAVE THE WS ADDRESS INTO THE     00074   
*                                      BLL CELL.                        00075   
         LM    R14,R12,12(R13)         RESTORE THE REGISTERS            00076   
         BR    R14                     RETURN TO CALLER                 00077   
         END   ELUADDRS                                                 00078   
