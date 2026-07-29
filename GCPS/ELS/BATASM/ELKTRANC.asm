*                                                                       00001   
*      MODULE-ID.          ELKTRANC.                                    00002   
*                                                                       00003   
*      AUTHOR.             EDWARD G LISS                                00004   
*                                                                       00005   
*      INSTALLATION.       HEALTH CARE SERVICE CORPORATION              00006   
*                          A MUTUAL LEGAL RESERVE COMPANY               00007   
*                          BLUE CROSS/BLUE SHIELD OF ILLINOIS           00008   
*                          233 N. MICHIGAN AVE                          00009   
*                          CHICAGO, ILLINOIS 60601                      00010   
*                                                                       00011   
*      DATE-WRITTEN.       31-JAN-1990.                                 00012   
*                                                                       00013   
*      SECURITY.           COPYRIGHT 1990,                              00014   
*                          HEALTH CARE SERVICE CORPORATION              00015   
*                                                                       00016   
*      REMARKS.            THIS MODULE WLL TRANSLATE LOWER CASE         00017   
*                          CHARACTERS TO UPPER CASE CHARACTERS          00018   
*                          AS WELL AS SIFT OUT JUNK CHARACTERS.         00019   
*                                                                       00020   
*      CALL SEQUENCE       CALL 'ELKTRANC' USING THE-LENGTH             00021   
*                                                THE-AREA.              00022   
*              WHERE       THE-LENGTH IS S9(4) COMP.                    00023   
*                          THE-AREA IS THE STUFF TO TRANSLATE           00024   
*                                                                       00025   
         SPACE 3                                                        00026   
******************************************************************      00027   
*                                                                *      00028   
*                      MAINTENANCE HISTORY                       *      00029   
*                                                                *      00030   
*  MOD     DATE     BY  DRPT                ACTION               *      00031   
* ----- ----------- --- ----- ---------------------------------- *      00032   
* 01.00 31-JAN-1990 EGL       CREATED                            *      00033   
*                                                                *      00034   
******************************************************************      00035   
         TITLE 'TRANSLATE LOWER TO UPPER CASE'                          00036   
*                                                                       00037   
*    REGISTER EQUS                                                      00038   
*                                                                       00039   
R0       EQU   0                                                        00040   
R1       EQU   1                                                        00041   
R2       EQU   2                                                        00042   
R3       EQU   3                                                        00043   
R4       EQU   4                                                        00044   
R5       EQU   5                                                        00045   
R6       EQU   6                                                        00046   
R7       EQU   7                                                        00047   
R8       EQU   8                                                        00048   
R9       EQU   9                                                        00049   
R10      EQU   10                                                       00050   
R11      EQU   11                                                       00051   
R12      EQU   12                                                       00052   
R13      EQU   13                                                       00053   
R14      EQU   14                                                       00054   
R15      EQU   15                                                       00055   
         USING ELKTRANC,R15                                             00056   
ELKTRANC CSECT 0                                                        00057   
         STM   R14,R12,12(R13)         SAVE REGISTERS IN CALLERS S/A    00058   
         LM    R2,R3,0(R1)             LOAD LENGTH AND ADDRESS OF       00059   
         LH    R2,0(,R2)                   DATA AREA.                   00060   
*                                                                       00061   
*    TRANSLATE ALL NON DISPLAY CHARACTERS TO SPACES                     00062   
*    AS WELL AS TRANSLATING LOWER TO UPPER CASE                         00063   
*                                                                       00064   
         CH    R2,=H'256'              IS LENGTH <= 256?                00065   
         BNH   SHORT                   YES, BRANCH                      00066   
TRLOOP   EQU   *                                                        00067   
         TR    0(256,R3),TRDISTB       TRANSLATE THE CHARACTERS         00068   
         LA    R3,256(,R3)             INCREMENT STARTING POINT         00069   
         SH    R2,=H'256'              DECREMENT LENGTH TO PROCESS      00070   
         CH    R2,=H'256'              IS LENGTH NOT <= 256?            00071   
         BH    TRLOOP                  NO, BRANCH                       00072   
SHORT    EQU   *                                                        00073   
         BCTR  R2,0                    ADJUST FOR EX INSTRUCTION        00074   
         EX    R2,TRDISPL              VARIABLE LENGTH TRANSLATE        00075   
         LM    R14,R12,12(R13)                                          00076   
         BR    R14                                                      00077   
*                                                                       00078   
*    EXECUTED INSTRUCTIONS                                              00079   
*                                                                       00080   
TRDISPL  TR    0(0,R3),TRDISTB         TRANSLATE NON DISPLAY CHARS      00081   
*                                      TO SPACES                        00082   
         EJECT                                                          00083   
**********************************************************************  00084   
*                                                                    *  00085   
*                 DATA AREAS FOR ELKTRANC                            *  00086   
*                                                                    *  00087   
**********************************************************************  00088   
         SPACE 3                                                        00089   
TRDISTB  DS    0XL256                 INVALID CHARACTER TRANS. TABLE    00090   
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        00091   
*               *                                *                      00092   
         DC    X'40404040404040404040404040404040'  X'00'-X'0F'         00093   
*               *                                *                      00094   
         DC    X'40404040404040404040404040404040'  X'10'-X'1F'         00095   
*               *                                *                      00096   
         DC    X'40404040404040404040404040404040'  X'20'-X'2F'         00097   
*               *                                *                      00098   
         DC    X'40404040404040404040404040404040'  X'30'-X'3F'         00099   
*               *                    Â¢ . < ( + | *                      00100   
         DC    X'404040404040404040404A4B4C4D4E4F'  X'40'-X'4F'         00101   
*               *&                   ! $ * ) ; ¬ *                      00102   
         DC    X'504040404040404040405A5B5C5D5E5F'  X'50'-X'5F'         00103   
*               *- /                 Â¦ , % _ > ? *                      00104   
         DC    X'606140404040404040406A6B6C6D6E6F'  X'60'-X'6F'         00105   
*               *                  ` : # @ ' = \
         DC    X'404140404040404040797A7B7C7D7E7F'  X'70'-X'7F'         00107   
*               *  A B C D E F G H I             *  <== LOWER CASE      00108   
         DC    X'40C1C2C3C4C5C6C7C8C9404040404040'  X'80'-X'8F'         00109   
*               *  J K L M N O P Q R             *  <== LOWER CASE      00110   
         DC    X'40D1D2D3D4D5D6D7D8D9404040404040'  X'90'-X'9F'         00111   
*               *  ~ S T U V W X Y Z             *  <== LOWER CASE      00112   
         DC    X'40A1E2E3E4E5E6E7E8E9404040404040'  X'A0'-X'AF'         00113   
*               *                                *                      00114   
         DC    X'40404040404040404040404040404040'  X'B0'-X'BF'         00115   
*               *{ A B C D E F G H I             *                      00116   
         DC    X'C0C1C2C3C4C5C6C7C8C9404040404040'  X'C0'-X'CF'         00117   
*               *} J K L M N O P Q R             *                      00118   
         DC    X'D0D1D2D3D4D5D6D7D8D9404040404040'  X'D0'-X'DF'         00119   
*               *\   S T U V W X Y Z             *                      00120   
         DC    X'E040E2E3E4E5E6E7E8E9404040404040'  X'E0'-X'EF'         00121   
*               *0 1 2 3 4 5 6 7 8 9             *                      00122   
         DC    X'F0F1F2F3F4F5F6F7F8F9404040404040'  X'F0'-X'FF'         00123   
         LTORG                                                          00124   
         END                                                            00125   
