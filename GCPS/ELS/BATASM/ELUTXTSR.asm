*                                                                       00001   
*      MODULE-ID.          TEXT PROCESSING SUBROUTINES                  00002   
*                                                                       00003   
*      AUTHOR.             EDWARD G LISS                                00004   
*                                                                       00005   
*      INSTALLATION.       HEALTH CARE SERVICE CORPORATION              00006   
*                          A MUTUAL LEGAL RESERVE COMPANY               00007   
*                          BLUE CROSS/BLUE SHIELD OF ILLINOIS           00008   
*                          233 N. MICHIGAN AVE                          00009   
*                          CHICAGO, ILLINOIS 60601                      00010   
*                                                                       00011   
*      DATE-WRITTEN.       18-FEB-1987.                                 00012   
*                                                                       00013   
*      SECURITY.           COPYRIGHT 1987,                              00014   
*                          HEALTH CARE SERVICE CORPORATION              00015   
         SPACE 3                                                        00016   
******************************************************************      00017   
*                                                                *      00018   
*                      MAINTENANCE HISTORY                       *      00019   
*                                                                *      00020   
*  MOD     DATE     BY  DRPT                ACTION               *      00021   
* ----- ----------- --- ----- ---------------------------------- *      00022   
* 01.00 18-FEB-1987 EGL       CREATED                            *      00023   
* 01.01 25-JAN-1989 EGL       CORRECTED ROUTINE WHICH CALCULATES *      00024   
*                             TEXT LENGTH IN THE UNSTRING        *      00025   
*                             ROUTINE.                           *      00026   
*       14-AUG-2003 AKK     REGEND IN TESTCONV                  *       00027   
******************************************************************      00028   
         EJECT                                                          00029   
*                                                                       00030   
*     THIS MODULE CONTAINS A NUMBER OF TEXT PROCESSING SUBROUTINES.     00031   
*     DUE TO THIS UNIQUE SETUP, EACH SUBROUTINE WILL HAVE A PREFIX      00032   
*     ON ALL IT'S LABEL.  THE PREFIX IS A SINGLE CHARACTER, A-Z.        00033   
*     BELOW IS A USE OF EACH PREFIX.  FOR EXAMPLE, ACOMP WHOULD         00034   
*     BE A LABEL BELONGING TO ELUTCOMP.                                 00035   
*                                                                       00036   
*     PREFIX     ROUTINE                                                00037   
*     ------     ---------                                              00038   
*        A       ELUTCOMP                                               00039   
*        B       ELUTUNST                                               00040   
*        E       ENTRY POINTS                                           00041   
*        T       PARAMETERS                                             00042   
         SPACE 3                                                        00043   
*                                                                       00044   
*    REGISTER EQUS                                                      00045   
*                                                                       00046   
R0       EQU   0                                                        00047   
R1       EQU   1                                                        00048   
R2       EQU   2                                                        00049   
R3       EQU   3                                                        00050   
R4       EQU   4                                                        00051   
R5       EQU   5                                                        00052   
R6       EQU   6                                                        00053   
R7       EQU   7                                                        00054   
R8       EQU   8                                                        00055   
R9       EQU   9                                                        00056   
R10      EQU   10                                                       00057   
R11      EQU   11                                                       00058   
R12      EQU   12                                                       00059   
R13      EQU   13                                                       00060   
R14      EQU   14                                                       00061   
R15      EQU   15                                                       00062   
         TITLE 'ENGLISH LANGUAGE INQUIRY - TEXT COMPRESSION'            00063   
         USING ELUTCOMP,R15                                             00064   
         USING TCARAREA,R1,R2                                           00065   
ELUTCOMP CSECT 0                                                        00066   
         STM   R14,R12,12(R13)         SAVE REGISTERS IN CALLERS S/A    00067   
         L     R1,0(R1)                LOAD ADDRESS OF PARM AREA AND    00068   
         L     R2,=F'4096'                 ESTABLISH BOTH BASE REGS.    00069   
         LA    R2,0(R1,R2)                                              00070   
         SPACE 3                                                        00071   
*                                                                       00072   
*    VERIFY INPUT TEXT LENGTH                                           00073   
*                                                                       00074   
         LH    R4,TAREALEN             GET LENGTH OF THE RAW TEXT       00075   
         LTR   R4,R4                   IS THE LENGTH ZERO?              00076   
         BL    ADEFAULT                BRANCH IF NEGATIVE.              00077   
         BE    ADEFAULT                BRANCH IF ZERO.                  00078   
         LA    R5,L'TFROMTXT+1         GET MAX RAW TEXT LENGTH          00079   
         CR    R4,R5                   PASSED LENGTH TOO LONG?          00080   
         BL    ALENPASS                NO, ACCEPT IT.                   00081   
ADEFAULT EQU   *                                                        00082   
         LA    R4,L'TFROMTXT           USE THE DEFAULT LENGTH           00083   
         STH   R4,TAREALEN               AND SAVE IT                    00084   
ALENPASS EQU   *                                                        00085   
         SPACE 3                                                        00086   
*                                                                       00087   
*    TRANSLATE ALL NON DISPLAY CHARACTERS TO SPACES                     00088   
*                                                                       00089   
         LA    R3,TFROMTXT             GET THE ADDRESS OF THE RAW TEXT  00090   
         CH    R4,=H'256'              IS LENGTH <= 256?                00091   
         BNH   ASHORT                  YES, BRANCH                      00092   
ATRLOOP  EQU   *                                                        00093   
         TR    0(256,R3),ATRDISTB      TRANSLATE THE CHARACTERS         00094   
         LA    R3,256(,R3)             INCREMENT STARTING POINT         00095   
         SH    R4,=H'256'              DECREMENT LENGTH TO PROCESS      00096   
         CH    R4,=H'256'              IS LENGTH NOT <= 256?            00097   
         BH    ATRLOOP                 NO, BRANCH                       00098   
ASHORT   EQU   *                                                        00099   
         BCTR  R4,0                    ADJUST FOR EX INSTRUCTION        00100   
         EX    R4,ATRDISPL             VARIABLE LENGTH TRANSLATE        00101   
         EJECT                                                          00102   
*                                                                       00103   
*   DETERMINE LENGTH OF TEXT     (MINUS TRAILING SPACES)                00104   
*                                                                       00105   
         LH    R4,TAREALEN             GET LENGTH OF PASSED TEXT        00106   
ASEARCH  EQU   *                                                        00107   
         LA    R3,TFROMTXT-1(R4)       GET ADDRESS OF LAST CHARACTER    00108   
*                                      OF THE RAW TEXT                  00109   
         CLI   0(R3),C' '              IS THE CURRENT CHAR A SPACE?     00110   
         BNE   ANONBLK                 NO, BRANCH                       00111   
         BCT   R4,ASEARCH              SUBTRACT 1 FROM THE LENGTH       00112   
*                                      AND BRANCH IF R4 > 0             00113   
ANONBLK  EQU   *                                                        00114   
         STH   R4,TFROMLEN             STORE THE INPUT TEXT LENGTH      00115   
*                                      LESS TRAILING SPACE              00116   
         SPACE 3                                                        00117   
*                                                                       00118   
*    CLEAR OUTPUT AREA TO SPACES                                        00119   
*                                                                       00120   
         LA    R4,TTOTXT               SET UP SENDER FOR MVCL           00121   
         ICM   R5,15,=X'40000000'      SET LENGTH TO ZERO AND           00122   
*                                      PAD CHARACTER TO SPACE           00123   
         LA    R6,TTOTXT               SET UP RECEIVER FOR MVCL         00124   
         LA    R7,L'TTOTXT                                              00125   
         MVCL  R6,R4                                                    00126   
         EJECT                                                          00127   
*                                                                       00128   
*    SQUEEZE OUT ALL EXTRA SPACES                                       00129   
*                                                                       00130   
*    REGISTER USAGE   R3 = INPUT POINTER   R4 = INPUT LENGTH            00131   
*                     R5 = OUTPUT POINTER  R6 = OUTPUT LENGTH           00132   
*                     R7 = BLANK COUNTER   R8 = NON BLANK COUNTER       00133   
*                                                                       00134   
         LA    R3,TFROMTXT             GET ADDRESS OF RAW TEXT          00135   
         LH    R4,TFROMLEN             GET RAW LENGTH                   00136   
         LA    R5,TTOTXT               GET DESTINATION ADDRESS          00137   
         LA    R6,0                    SET OUTPUT LENGTH TO ZERO        00138   
         LTR   R4,R4                   IF THE RAW LENGTH IS ZERO        00139   
         BZ    ARETURN                 RETURN - THE INPUT AREA IS       00140   
*                                      ALL SPACES.                      00141   
*                                                                       00142   
*    FIND THE NEXT NON BLANK CHARACTER                                  00143   
*                                                                       00144   
AMOVLUP  EQU   *                                                        00145   
         LA    R7,0                    BLANK COUNTER                    00146   
ABLNKLUP EQU   *                                                        00147   
         LA    R9,0(R7,R3)             GET TEST ADDRESS                 00148   
         CLI   0(R9),C' '              IS IT BLANK?                     00149   
         BNE   ANBLKFND                NO, NON BLANK FOUND - BRANCH     00150   
         LA    R7,1(,R7)               INCREMENT BLANK COUNT            00151   
         BCT   R4,ABLNKLUP             SUBTRACT 1 FROM R4 AND BRANCH    00152   
*                                      IF NOT ZERO                      00153   
         B     AFINISH                 BRANCH TO END OF TEXT            00154   
ANBLKFND EQU   *                                                        00155   
         LA    R3,0(R7,R3)             ADJUST POINTER TO NON-BLANK CHAR 00156   
         LA    R8,0                    NON-BLANK COUNTER                00157   
*                                                                       00158   
*    PROCESS PUNTUATION                                                 00159   
*     IF THE FIRST NON BLANK IS A PUNCTUATION,                          00160   
*                                                                       00161   
         CLI   0(R3),C'.'              IS IT A PERIOD?                  00162   
         BE    ADOPUNCT                YES, DO PUNCTUATION RTN          00163   
         CLI   0(R3),C','              IS IT A COMMA?                   00164   
         BE    ADOPUNCT                YES, DO PUNCTUATION RTN          00165   
         CLI   0(R3),C';'              IS IT A SEMI COLON?              00166   
         BE    ADOPUNCT                YES, DO PUNCTUATION RTN          00167   
         CLI   0(R3),C':'              IS IT A COLON?                   00168   
         BNE   ANBLKLUP                NO, BRANCH                       00169   
ADOPUNCT EQU   *                                                        00170   
         LA    R8,1                    SET CORRECT LENGTH FOR           00171   
*                                      PUNCTUATION CHARACTER.           00172   
         LTR   R6,R6                   TEST FOR FIRST CHAR OUT          00173   
         BZ    AFIRSTCH                YES, BRANCH                      00174   
         BCTR  R6,0                    SUBTRACT 1 FROM OUT LEN          00175   
         S     R5,=F'1'                SUBTRACT 1 FROM DEST ADDR.       00176   
AFIRSTCH EQU   *                                                        00177   
         B     ABLNKFND                BRANCH TO MOVE PUCTUATION        00178   
*                                                                       00179   
*      OTHERWISE, FIND THE NEXT BLANK CHARACTER                         00180   
*                                                                       00181   
ANBLKLUP EQU   *                                                        00182   
         LA    R9,0(R8,R3)             GET TEST ADDRESS                 00183   
         CLI   0(R9),C' '              IS IT BLANK?                     00184   
         BE    ABLNKFND                YES, BLANK FOUND                 00185   
         LA    R8,1(,R8)               INCREMENT NON BLANK COUNT        00186   
         BCT   R4,ANBLKLUP             SUBTRACT 1 FROM THE INPUT LENGTH 00187   
*                                      AND BRANCH IF > 0.               00188   
         SPACE 3                                                        00189   
ABLNKFND EQU   *                                                        00190   
*                                                                       00191   
*    MOVE THE BLOCK OF CHARACTERS                                       00192   
*                                                                       00193   
         BCTR  R8,0                   SUBTRACT 1 FROM LENGTH            00194   
         EX    R8,AMVWORD             MOVE THE WORD                     00195   
         LA    R6,2(R8,R6)            ADJUST THE LENGTH AND             00196   
         LA    R5,2(R8,R5)            THE OUTPUT POINTER BY             00197   
*                                     THE LENGTH OF THE WORD JUST       00198   
*                                     MOVED PLUS 1 TO COMPENSATE        00199   
*                                     FOR THE BCTR PLUS 1 FOR THE       00200   
*                                     INSERTED SPACE.                   00201   
         SPACE 1                                                        00202   
         LA    R3,1(R8,R3)            ADJUST THE INPUT POINTER BY       00203   
*                                     THEN LENGTH OF THE WORD JUST      00204   
*                                     MOVED PLUS 1 TO COMPENSATE        00205   
*                                     FOR THE BCTR.                     00206   
         LTR   R4,R4                  ALL INPUT PROCESSED?              00207   
         BNZ   AMOVLUP                NO, IF NOT ZERO.                  00208   
         EJECT                                                          00209   
AFINISH  EQU   *                                                        00210   
*******  THESE STATEMENTS COMMENTED OUT FOR COMPATABILITY               00211   
*******  LTR   R6,R6                  TEST FOR NO OUTPUT                00212   
*******  BZ    ARETURN                IF SO, BRANCH                     00213   
*******  BCTR  R6,0                   SUBTRACT 1 FROM TEXT LENGTH       00214   
ARETURN  EQU   *                                                        00215   
         STH   R6,TCARL               SAVE THE LENGTH OF THE COMPRESSED 00216   
         STH   R6,TTOSUB              TEXT                              00217   
         MVC   TAREALEN,=H'0'         CLEAR THE INPUT AREA LENGTH       00218   
         LM    R14,R12,12(R13)                                          00219   
         BR    R14                                                      00220   
*                                                                       00221   
*    EXECUTED INSTRUCTIONS                                              00222   
*                                                                       00223   
ATRDISPL TR    0(0,R3),ATRDISTB        TRANSLATE NON DISPLAY CHARS      00224   
AMVWORD  MVC   0(0,R5),0(R3)                                            00225   
*                                      TO SPACES                        00226   
         EJECT                                                          00227   
**********************************************************************  00228   
*                                                                    *  00229   
*                 DATA AREAS FOR ELUTCOMP                            *  00230   
*                                                                    *  00231   
**********************************************************************  00232   
         SPACE 3                                                        00233   
ATRDISTB DS    0XL256                 INVALID CHARACTER TRANS. TABLE    00234   
*                0 1 2 3 4 5 6 7 8 9 A B C D E F                        00235   
*               *                                *                      00236   
         DC    X'40404040404040404040404040404040'  X'00'-X'0F'         00237   
*               *                                *                      00238   
         DC    X'40404040404040404040404040404040'  X'10'-X'1F'         00239   
*               *                                *                      00240   
         DC    X'40404040404040404040404040404040'  X'20'-X'2F'         00241   
*               *                                *                      00242   
         DC    X'40404040404040404040404040404040'  X'30'-X'3F'         00243   
*               *                    Â¢ . < ( + | *                      00244   
         DC    X'404040404040404040404A4B4C4D4E4F'  X'40'-X'4F'         00245   
*               *&                   ! $ * ) ; ¬ *                      00246   
         DC    X'504040404040404040405A5B5C5D5E5F'  X'50'-X'5F'         00247   
*               *- /                 Â¦ , % _ > ? *                      00248   
         DC    X'606140404040404040406A6B6C6D6E6F'  X'60'-X'6F'         00249   
*               *                  ` : # @ ' = \
         DC    X'404140404040404040797A7B7C7D7E7F'  X'70'-X'7F'         00251   
*               *  A B C D E F G H I             *  <== LOWER CASE      00252   
         DC    X'40818283848586878889404040404040'  X'80'-X'8F'         00253   
*               *  J K L M N O P Q R             *  <== LOWER CASE      00254   
         DC    X'40919293949596979899404040404040'  X'90'-X'9F'         00255   
*               *  ~ S T U V W X Y Z             *  <== LOWER CASE      00256   
         DC    X'40A1A2A3A4A5A6A7A8A9404040404040'  X'A0'-X'AF'         00257   
*               *                                *                      00258   
         DC    X'40404040404040404040404040404040'  X'B0'-X'BF'         00259   
*               *{ A B C D E F G H I             *                      00260   
         DC    X'C0C1C2C3C4C5C6C7C8C9404040404040'  X'C0'-X'CF'         00261   
*               *} J K L M N O P Q R             *                      00262   
         DC    X'D0D1D2D3D4D5D6D7D8D9404040404040'  X'D0'-X'DF'         00263   
*               *\   S T U V W X Y Z             *                      00264   
         DC    X'E040E2E3E4E5E6E7E8E9404040404040'  X'E0'-X'EF'         00265   
*               *0 1 2 3 4 5 6 7 8 9             *                      00266   
         DC    X'F0F1F2F3F4F5F6F7F8F9404040404040'  X'F0'-X'FF'         00267   
         LTORG                                                          00268   
         DROP                                                           00269   
         TITLE 'ENGLISH LANGUAGE INQUIRY - TEXT UNSTRING'               00270   
         USING ELUTUNST,R15                                             00271   
         USING TCARAREA,R1,R2                                           00272   
         USING TOPFTBL,R12                                              00273   
ELUTUNST CSECT 0                                                        00274   
         STM   R14,R12,12(R13)         SAVE REGISTERS IN CALLERS S/A    00275   
         L     R1,0(R1)                LOAD ADDRESS OF PARM AREA AND    00276   
         L     R2,=F'4096'                 ESTABLISH BOTH BASE REGS.    00277   
         LA    R2,0(R1,R2)                                              00278   
         SPACE 3                                                        00279   
*                                                                       00280   
*     INITIALIZATION                                                    00281   
*                                                                       00282   
         LA    R4,TOPFAREA             SET UP TO CLEAR FORMATTED AREA   00283   
         ICM   R5,15,=X'40000000'      SET LENGTH TO ZERO AND           00284   
*                                      PAD CHARACTER TO SPACE           00285   
         LA    R6,TOPFAREA                                              00286   
         LA    R7,L'TOPFAREA             TO SPACES                      00287   
         MVCL  R6,R4                                                    00288   
         SPACE 3                                                        00289   
         LA    R11,TOUTLEN             GET ADDRESS OF LENGTH TABLE      00290   
         LA    R12,TOPFAREA            GET ADDRESS OF OUTPUT AREA       00291   
         LH    R2,TFLDCNT              GET MAXIMUM NUMBER OF LINES      00292   
         LA    R3,0                    CLEAR USED FIELD COUNT           00293   
         LA    R4,TTOTXT               GET SENDING AREA ADDRESS         00294   
         LH    R5,TCARL                LENGTH OF SENDING AREA           00295   
         SPACE 2                                                        00296   
         LTR   R5,R5                   RETURN IF INPUT AREA IS EMPTY    00297   
         BZ    BMOVEFIN                                                 00298   
         BCTR  R5,0                    ADJUST AREA LENGTH BY 1          00299   
         LTR   R2,R2                   IF FIELD COUNT IS OUT OF RANGE   00300   
         BNH   BDEFCNT                   USE THE DEFAULT VALUE OF 1.    00301   
         CH    R2,=H'21'                                                00302   
         BL    BMVLUP                                                   00303   
BDEFCNT  EQU   *                                                        00304   
         LA    R2,1                                                     00305   
         SPACE 3                                                        00306   
*                                                                       00307   
*     MOVE THE TEXT LINES                                               00308   
*                                                                       00309   
BMVLUP   EQU   *                                                        00310   
         LH    R6,0(,R11)              MAX LENGTH OF CURRENT LINE       00311   
         LTR   R6,R6                   USE DEFAULT LENGTH IF LINE       00312   
         BNH   BDEFAULT                LENGTH IS INVALID                00313   
         CH    R6,=H'80'                                                00314   
         BL    BCHKLEN                                                  00315   
BDEFAULT EQU   *                                                        00316   
         LA    R6,79                   LOAD DEFAULT LENGTH              00317   
BCHKLEN  EQU  *                                                         00318   
         CR   R5,R6                    COMPARE THE MAXIMUM LENGTH       00319   
*                                      WITH THE REMAINING LENGTH        00320   
         BNL  BSCANSP                  BRANCH IF THERE IS MORE TEXT     00321   
*                                      THAN WILL FIT IN THE LINE        00322   
         LR   R6,R5                    OTHERWISE, FORCE THE MAX LINE    00323   
*                                      LENGTH TO THE REMAINING TEXT LEN 00324   
BSCANSP  EQU   *                                                        00325   
         LA    R7,0(R6,R4)             COMPUTE THE ADDRESS OF THE       00326   
*                                      LAST BYTE THAT COULD FIT IN      00327   
*                                      THE CURRENT LINE.                00328   
         CLI   0(R7),C' '              IF BLANK                         00329   
         BE    BMOVELN                   THEN MOVE THE LINE             00330   
         BCT   R6,BSCANSP              SCAN FOR A BLANK                 00331   
BMOVELN  EQU   *                                                        00332   
         STH   R6,TOPFLEN              SAVE THE LINE LENGTH             00333   
         BCTR  R6,0                    ADJUST LINE LENGTH FOR EX        00334   
         EX    R6,BMVCLN               MOVE THE LINE                    00335   
         LA    R3,1(,R3)               ADD 1 TO LINES USED COUNT        00336   
         LA    R4,2(R6,R4)             MOVE THE SOURCE POINTER UP       00337   
*                                      TO THE NEXT PLACE                00338   
         SR    R5,R6                   SUBTRACT THE MOVED LINE LENGTH   00339   
         S     R5,=F'2'                PLUS 1 FROM THE LENGTH OF THE    00340   
*                                      REMAINING TEXT                   00341   
         BNH   BMOVEFIN                ALL DONE IF <= 0                 00342   
         LA    R11,2(,R11)             NEXT LINE                        00343   
         LA    R12,TOPFSIZE(,R12)                                       00344   
         BCT   R2,BMVLUP               CONTINUE TILL MAX LINES MOVED    00345   
         SPACE 3                                                        00346   
BMOVEFIN EQU   *                                                        00347   
         STH   R3,TFLDUSED             SAVE LINES USED COUNT.           00348   
         SPACE 3                                                        00349   
         LM    R14,R12,12(R13)                                          00350   
         BR    R14                                                      00351   
*                                                                       00352   
*    EXECUTED INSTRUCTION                                               00353   
*                                                                       00354   
BMVCLN   MVC   TOPFDATA(0),0(R4)       MOVE THE LINE                    00355   
         LTORG                                                          00356   
         DROP                                                           00357   
         EJECT                                                          00358   
**********************************************************************  00359   
*                                                                    *  00360   
*                  PARAMETER AREA DEFINITION                         *  00361   
*                                                                    *  00362   
*     THIS AREA IS DESCRIBES THE PARAMETER LIST PASSED BY THE        *  00363   
*     CALLING PROGRAMS.  THIS AREA MUST MATCH THE EQUIVALENT         *  00364   
*     COBOL AREA, WHICH IS IN COPYBOOK ELSTCWAC.                     *  00365   
*                                                                    *  00366   
**********************************************************************  00367   
TCARAREA DSECT                         COBOL NAME                       00368   
TCAR     DS    0CL3170                 TCAR-TEXT-COMPRESSION-AREA       00369   
         DS    H                       TCAR-FROM-SUB                    00370   
         DS    H                       TCAR-FROM-SUB-PLUS-1             00371   
TTOSUB   DS    H                       TCAR-TO-SUB                      00372   
TFROMLEN DS    H                       TCAR-FROM-LENGTH                 00373   
TAREALEN DS    H                       TCAR-AREA-LENGTH                 00374   
TFROMTXT DS    CL1580                  TCAR-FROM-AREA                   00375   
TTOTXT   DS    CL1580                  TCAR-TO-AREA                     00376   
         DS    0CL1698                 TCAR-TEXT-UNSTRING-AREA          00377   
TFLDCNT  DS    H                       TCAR-OUTPUT-FIELD-COUNT          00378   
TFLDUSED DS    H                       TCAR-OUTPUT-FIELDS-USED          00379   
TOUTLEN  DS    20H                     TCAR-OUTPUT-FIELD-#-LEN          00380   
TCARL    DS    H                       TCAR-L                           00381   
         DS    H                       TCAR-A                           00382   
         DS    H                       TCAR-B                           00383   
         DS    H                       TCAR-C                           00384   
         DS    H                       TCAR-OA                          00385   
         DS    H                       TCAR-OR                          00386   
         DS    H                       TCAR-X                           00387   
TOPFAREA DS    CL1640                  TCAR-OUTPUT-FIELD-TABLE          00388   
TOPFTBL  DSECT                                                          00389   
TOPFLEN  DS    H                       TCAR-OPF-LENGTH                  00390   
TOPFDATA DS    CL80                    TCAR-OPF-DATA                    00391   
TOPFSIZE EQU   *-TOPFLEN                                                00392   
         END                                                            00393   
