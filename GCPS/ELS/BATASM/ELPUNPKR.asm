         TITLE 'ENGLISH LANGUAGE INQUIRY - UNPACK ROUTINE'              00001   
*                                                                       00002   
*      PROGRAM-ID.         ELPUNPKR.                                    00003   
*                                                                       00004   
*      AUTHOR.             EDWARD G LISS                                00005   
*                                                                       00006   
*      INSTALLATION.       HEALTH CARE SERVICE CORPORATION              00007   
*                          A MUTUAL LEGAL RESERVE COMPANY               00008   
*                          BLUE CROSS/BLUE SHIELD OF ILLINOIS           00009   
*                          233 N. MICHIGAN AVE                          00010   
*                          CHICAGO, ILLINOIS 60601                      00011   
*                                                                       00012   
*      DATE-WRITTEN.       05-JUN-1989.                                 00013   
*                                                                       00014   
*      SECURITY.           COPYRIGHT 1989,                              00015   
*                          HEALTH CARE SERVICE CORPORATION              00016   
         SPACE 3                                                        00017   
******************************************************************      00018   
*                                                                *      00019   
*                      MAINTENANCE HISTORY                       *      00020   
*                                                                *      00021   
*  MOD     DATE     BY  DRPT                ACTION               *      00022   
* ----- ----------- --- ----- ---------------------------------- *      00023   
* 01.00 05-JUN-1989 EGL       CREATED                            *      00024   
*                                                                *      00025   
******************************************************************      00026   
*                                                                       00027   
*    THIS SUBPROGRAM ACCEPTS UNPACKS A GIVEN AREA INTO THE              00028   
*    DOUBLE DIGIT HEX FORMAT.                                           00029   
*                                                                       00030   
*    CALLING SEQUENCE:                                                  00031   
*                                                                       00032   
*        CALL 'ELPUNPKR' USING ELPUNPKR-PARM                            00033   
*                              RECORD-NAME.                             00034   
         EJECT                                                          00035   
ELPUNPKR START 0                                                        00036   
R0       EQU   0                                                        00037   
R1       EQU   1                                                        00038   
R2       EQU   2                                                        00039   
R3       EQU   3                                                        00040   
R4       EQU   4                                                        00041   
R5       EQU   5                                                        00042   
R6       EQU   6                                                        00043   
R7       EQU   7                                                        00044   
R8       EQU   8                                                        00045   
R9       EQU   9                                                        00046   
R10      EQU   10                                                       00047   
R11      EQU   11                                                       00048   
R12      EQU   12                                                       00049   
R13      EQU   13                                                       00050   
R14      EQU   14                                                       00051   
R15      EQU   15                                                       00052   
         SPACE 3                                                        00053   
START    EQU   *                                                        00054   
         USING *,R12                                                    00055   
         B     12(R15)                                                  00056   
         DC    CL8'ELPUNPKR'                                            00057   
         STM   R14,R12,12(R13)         SAVE REGISTERS                   00058   
         LR    R12,R15                 ESTABLISH BASE REGISTER          00059   
         L     R3,0(0,R1)              GET ADDRESS OF PARM BLOCK        00060   
         USING PARM,R3                                                  00061   
         L     R2,4(0,R1)              GET ADDRESS OF PACKED DATA       00062   
         A     R2,POFFSET              AND ADDRESS OF FIRST BYTE TO     00063   
*                                      DUMP.                            00064   
         USING PACK,R2                                                  00065   
*                                                                       00066   
         MVI   PLINE1,C' '                                              00067   
         MVC   PLINE1+1(L'PLINE1-1),PLINE1                              00068   
         MVC   PLINE2,PLINE1                                            00069   
         MVC   PLINE3,PLINE2                                            00070   
*                                                                       00071   
*    UNPACK THE CICS ADDRESS INTO THE FIRST LINE                        00072   
*                                                                       00073   
         L     R4,PCICSPTR             GET ADDRESS OF BLOCK AT TIME     00074   
         A     R4,POFFSET              OF ABEND (FROM CICS)             00075   
         ST    R4,PLINE1+12                                             00076   
         UNPK  PLINE1+1(9),PLINE1+12(5)                                 00077   
         TR    PLINE1+1(8),TRTAB-X'F0' TRANSLATE TO PRINTABLE CHAR.     00078   
         MVI   PLINE1+9,C' '                                            00079   
         MVC   PLINE1+12(4),=CL4' '    CLEAR WORK AREA                  00080   
*                                                                       00081   
*    UNPACK THE OFFSET INTO THE SECOND LINE                             00082   
*                                                                       00083   
         L     R4,POFFSET              GET OFFSET WITHIN BLOCK          00084   
         ST    R4,PLINE2+12                                             00085   
         UNPK  PLINE2(9),PLINE2+12(5)                                   00086   
         TR    PLINE2(8),TRTAB-X'F0'   TRANSLATE TO PRINTABLE CHAR.     00087   
         MVI   PLINE2+8,C')'                                            00088   
         MVC   PLINE2(4),=C'  (+'                                       00089   
         MVC   PLINE2+12(4),=CL4' '    CLEAR WORK AREA                  00090   
*                                                                       00091   
*    PROCESS DATA TO DUMP                                               00092   
*                                                                       00093   
         LH    R4,PLEN                 GET LENGTH                       00094   
         LTR   R4,R4                   SEE IF ZERO                      00095   
         BZ    DEFLEN                  USE DEFAULT IF ZERO              00096   
         CH    R4,=H'121'              SEE IF TOO LARGE                 00097   
         BL    LENOK                   BRANCH IF LEN OK                 00098   
DEFLEN   EQU   *                                                        00099   
         LA    R4,120                                                   00100   
LENOK    EQU   *                                                        00101   
         BCTR  R4,0                    ADJUST FOR EXECUTE COMMAND       00102   
         EX    R4,MOVE1                MOVE THE PACKED DATA             00103   
         EX    R4,MOVE2                     TO THE PRINT LINES          00104   
         EX    R4,MOVE3                                                 00105   
         EX    R4,TRTAB1               TRANSLATE THE DATA FOR           00106   
         EX    R4,TRTAB2                    PRINTING                    00107   
         EX    R4,TRTAB3                                                00108   
*                                                                       00109   
         LM    R14,R12,12(R13)         RESTORE REGISTERS                00110   
         SR    R15,R15                 SET RETURN CODE                  00111   
         BR    R14                     RETURN                           00112   
*                                                                       00113   
MOVE1    MVC   PLINE1+11(0),PACKED                                      00114   
MOVE2    MVC   PLINE2+11(0),PACKED                                      00115   
MOVE3    MVC   PLINE3+11(0),PACKED                                      00116   
TRTAB1   TR    PLINE1+11(0),PRTAB1     TRANSLATE EBCDIC BYTES.          00117   
TRTAB2   TR    PLINE2+11(0),PRTAB2     TRANSLATE HIGH ORDER NIBBLES.    00118   
TRTAB3   TR    PLINE3+11(0),PRTAB3     TRANSLATE LOW ORDER NIBBLES.     00119   
*                                      NOTE - A NIBBLE IS A HALF BYTE   00120   
PRTAB1   DC    CL16'................'  THIS TABLE TRANSLATES ALL NON-   00121   
         DC    CL16'................'  PRINTABLE CHARACTERS TO '.'      00122   
         DC    CL16'................'                                   00123   
         DC    CL16'................'                                   00124   
         DC    CL16' ...............'                                   00125   
         DC    CL16'................'                                   00126   
         DC    CL16'................'                                   00127   
         DC    CL16'................'                                   00128   
         DC    CL16'................'                                   00129   
         DC    CL16'................'                                   00130   
         DC    CL16'................'                                   00131   
         DC    CL16'................'                                   00132   
         DC    CL16'.ABCDEFGHI......'                                   00133   
         DC    CL16'.JKLMNOPQR......'                                   00134   
         DC    CL16'..STUVWXYZ......'                                   00135   
         DC    CL16'0123456789......'                                   00136   
PRTAB2   DC    16CL1'0'                THIS TABLE TRANSLATES THE HIGH   00137   
         DC    16CL1'1'                ORDER NIBBLES TO 0-F             00138   
         DC    16CL1'2'                                                 00139   
         DC    16CL1'3'                                                 00140   
         DC    16CL1'4'                                                 00141   
         DC    16CL1'5'                                                 00142   
         DC    16CL1'6'                                                 00143   
         DC    16CL1'7'                                                 00144   
         DC    16CL1'8'                                                 00145   
         DC    16CL1'9'                                                 00146   
         DC    16CL1'A'                                                 00147   
         DC    16CL1'B'                                                 00148   
         DC    16CL1'C'                                                 00149   
         DC    16CL1'D'                                                 00150   
         DC    16CL1'E'                                                 00151   
         DC    16CL1'F'                                                 00152   
PRTAB3   DC    16CL16'0123456789ABCEDF' TRANSLATES LOW-ORDER NIBBLES    00153   
TRTAB    DC    CL16'0123456789ABCDEF'                                   00154   
         LTORG                                                          00155   
PACK     DSECT                                                          00156   
PACKED   DS    CL120   SYMBOLIC PACKED AREA                             00157   
PARM     DSECT                                                          00158   
POFFSET  DS    F       OFFSET INTO RECORD AREA TO START DUMP AT         00159   
PCICSPTR DS    F       POINTER TO RECORD IN MEMORY AT ABEND TIME        00160   
PLEN     DS    H       LENGTH OF AREA TO UNPACK                         00161   
PLINE1   DS    CL132                                                    00162   
PLINE2   DS    CL132                                                    00163   
PLINE3   DS    CL132                                                    00164   
         END                                                            00165   
