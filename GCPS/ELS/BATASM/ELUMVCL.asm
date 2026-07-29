         TITLE 'ENGLISH LANGUAGE INQUIRY - MVCL ROUTINE'                00001   
*                                                                       00002   
*                                                                       00003   
*      PROGRAM-ID.         ELUMVCL.                                     00004   
*                                                                       00005   
*      AUTHOR.             EDWARD G LISS                                00006   
*                                                                       00007   
*      INSTALLATION.       HEALTH CARE SERVICE CORPORATION              00008   
*                          A MUTUAL LEGAL RESERVE COMPANY               00009   
*                          BLUE CROSS/BLUE SHIELD OF ILLINOIS           00010   
*                          233 N. MICHIGAN AVE                          00011   
*                          CHICAGO, ILLINOIS 60601                      00012   
*                                                                       00013   
*                                                                       00014   
*      DATE-WRITTEN.       29-SEP-1986.                                 00015   
*                                                                       00016   
*      SECURITY.           COPYRIGHT 1986,                              00017   
*                          HEALTH CARE SERVICE CORPORATION              00018   
         SPACE 3                                                        00019   
******************************************************************      00020   
*                      MAINTENANCE HISTORY                       *      00021   
*                      MAINTENANCE HISTORY                       *      00022   
*                                                                *      00023   
*  MOD     DATE     BY  DRPT                ACTION               *      00024   
* ----- ----------- --- ----- ---------------------------------- *      00025   
* 01.00 29-SEP-1986 EGL       CREATED                            *      00026   
*                                                                *      00027   
* 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *      00028   
*                                                                *      00029   
* 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *      00030   
*                                                                *      00031   
* 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *      00032   
*                                                                *      00033   
* 02.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *      00034   
**                                                                *     00035   
** *AKK 12/06/05 REGEN FOR TEST                                   *     00036   
******************************************************************      00037   
         EJECT                                                          00038   
ELUMVCL  START 0                                                        00039   
*                                                                       00040   
*    REGISTER EQUS                                                      00041   
*                                                                       00042   
R0       EQU   0                                                        00043   
R1       EQU   1                                                        00044   
R2       EQU   2                                                        00045   
R3       EQU   3                                                        00046   
R4       EQU   4                                                        00047   
R5       EQU   5                                                        00048   
R6       EQU   6                                                        00049   
R7       EQU   7                                                        00050   
R8       EQU   8                                                        00051   
R9       EQU   9                                                        00052   
R10      EQU   10                                                       00053   
R11      EQU   11                                                       00054   
R12      EQU   12                                                       00055   
R13      EQU   13                                                       00056   
R14      EQU   14                                                       00057   
R15      EQU   15                                                       00058   
*                                                                       00059   
         STM   R14,R12,12(R13)         SAVE REGISTERS IN CALLERS S/A    00060   
         LM    R2,R6,0(R1)             LOAD PARAMETER ADDRESSES         00061   
*                                      INTO REGISTERS                   00062   
         L     R3,0(,R3)               LOAD SENDING LENGTH IN REG 3     00063   
         L     R5,0(,R5)               LOAD RECEIVING LENGTH IN REG 5   00064   
         ICM   R3,8,0(R6)              INSERT THE FILL CHARACTER        00065   
         MVCL  R4,R2                   MOVE THE DATA                    00066   
         LM    R14,R12,12(R13)         RESTORE THE REGISTERS            00067   
         BR    R14                     RETURN TO CALLER                 00068   
         END   ELUMVCL                                                  00069   
