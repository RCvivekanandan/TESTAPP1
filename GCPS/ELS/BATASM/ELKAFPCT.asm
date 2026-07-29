*                                                                       00001   
*      PROGRAM-ID.         ELKAFPCT.                                    00002   
*                                                                       00003   
*      AUTHOR.             EDWARD G LISS                                00004   
*                                                                       00005   
*      INSTALLATION.       HEALTH CARE SERVICE CORPORATION              00006   
*                          A MUTUAL LEGAL RESERVE COMPANY               00007   
*                          BLUE CROSS/BLUE SHIELD OF ILLINOIS           00008   
*                          233 N. MICHIGAN AVE                          00009   
*                          CHICAGO, ILLINOIS 60601                      00010   
*                                                                       00011   
*      DATE-WRITTEN.       08-JAN-1990.                                 00012   
*                                                                       00013   
*      SECURITY.           COPYRIGHT 1988,                              00014   
*                          HEALTH CARE SERVICE CORPORATION              00015   
*                                                                       00016   
******************************************************************      00017   
*                                                                       00018   
*      DESCRIPTION   -  THIS MODULE WILL GENERATE THE ADVANCED          00019   
*                       FUNCTION PRINTING (AFP) DATA STREAM TO          00020   
*                       DRAW LINE VIA THE TEXT CONTROL SEQUENCES        00021   
*                       (CTX) COMMAND OF AFP.  SINCE THESE COMMANDS     00022   
*                       ARE A COMBINATION OF BIT STRINGS, IT IS         00023   
*                       EASIER TO GENERATE THE STREAM FROM AN           00024   
*                       ASSEMBLER PROGRAM.                              00025   
*                                                                       00026   
         SPACE 3                                                        00027   
******************************************************************      00028   
*                                                                *      00029   
*                      MAINTENANCE HISTORY                       *      00030   
*                                                                *      00031   
*  MOD     DATE     BY  DRPT                ACTION               *      00032   
* ----- ----------- --- ----- ---------------------------------- *      00033   
* 01.00 08-JAN-1990 EGL       CREATED                            *      00034   
*                                                                *      00035   
******************************************************************      00036   
         TITLE 'ELS AFP CTX INTERFACE MODULE'                           00037   
*                                                                       00038   
*      ELKAFPCT                                                         00039   
*                                                                       00040   
*      CALL  -  CALL 'ELKAFPCT' USING                                   00041   
*                        PRINT LINE                  X(133)             00042   
*                        COMMAND CODE                X(3)               00043   
*                        BASE LINE                   S9(4) COMP         00044   
*                        IN LINE                     S9(4) COMP         00045   
*                        LENGTH                      S9(4) COMP         00046   
*                        WIDTH                       S9(4) COMP         00047   
*                                                                       00048   
*      WHERE                                                            00049   
*            - PRINT LINE IS AREA WHERE THE CTX IS GENERATED            00050   
*            - COMMAND CODE IS WHAT FUNCTION YOU WANT.  VALID           00051   
*                 FUNCTIONS ARE DBR AND DIR                             00052   
*            - BASE LINE IS THE VERTICAL POSITION ON THE PAGE           00053   
*                 IN PELS.                                              00054   
*            - IN LINE IS THE HORIZONTAL POSITION ON THE PAGE           00055   
*                 IN PELS.                                              00056   
*            - LENGTH IS THE LENGTH OF THE LINE IN PELS                 00057   
*            - WIDTH IS WIDTH OF THE LINE IN PELS.                      00058   
*                                                                       00059   
*     NOTE - ANY NUMBER OF COMMAND CODE/WIDTH SEQUENCES MAY BE          00060   
*            USED IN THE CALL AS LONG AS THE 133 CHARACTER LIMIT OF     00061   
*            PRINT LINE IS NOT VIOLATED.  EACH DIR OR DRB TAKES 16      00062   
*            BYTES.                                                     00063   
*                                                                       00064   
         EJECT                                                          00065   
*                                                                       00066   
*    REGISTER EQUS            GLOBAL USAGE                              00067   
*                                                                       00068   
R0       EQU   0                                                        00069   
R1       EQU   1              PARAMETER LIST BASE REGISTER              00070   
R2       EQU   2              PRINT LINE PARM BASE REGISTER             00071   
R3       EQU   3              ADDRESS OF CURRENT COMMAND IN PARM LIST   00072   
R4       EQU   4              ADDRESS WITHIN PRINT LINE TO USE          00073   
R5       EQU   5              LENGTH OF PRINT LINE                      00074   
R6       EQU   6              ADDRESS OF COMMAND CODE                   00075   
R7       EQU   7                GENERAL                                 00076   
R8       EQU   8                  PURPOSE                               00077   
R9       EQU   9                    WORK                                00078   
R10      EQU   10                     REGISTERS                         00079   
R11      EQU   11                                                       00080   
R12      EQU   12             PROGRAM BASE REGISTER                     00081   
R13      EQU   13                                                       00082   
R14      EQU   14                                                       00083   
R15      EQU   15                                                       00084   
         MACRO                                                          00085   
&LAB     APPEND &FROM,&LEN                                              00086   
&LAB     LA    R5,&LEN.(R5)            VERIFY THAT DATA WILL FIT        00087   
         C     R5,=F'132'               IN THE PRINT LINE               00088   
         BH    BADPARM                 IF NOT, INDICATE BAD PARM        00089   
         MVC   0(&LEN,R4),&FROM        APPEND THE PARM                  00090   
         LA    R4,&LEN.(R4)            ADJUST THE ADDRESS               00091   
         MEND                                                           00092   
         EJECT                                                          00093   
ELKAFPCT CSECT                                                          00094   
         USING *,R12                                                    00095   
         B     12(0,R15)               BYPASS TITLE                     00096   
         DC    CL8'ELKAFPCT'           CSECT NAME                       00097   
         STM   R14,R12,12(R13)         SAVE REGISTERS IN CALLERS S/A    00098   
         LR    R12,R15                 ESTABLISH BASE REGISTER          00099   
*                                                                       00100   
         USING PARMAREA,R1                                              00101   
         TM    PRTLINEA,X'80'          LAST PARM?                       00102   
         BO    BADPARM                 YES? BRANCH                      00103   
         L     R2,PRTLINEA                                              00104   
         USING PRTLINED,R2                                              00105   
         XC    PRTLINE,PRTLINE         CLEAR LINE TO ZEROS              00106   
         LA    R4,PRTLINE              ADDRESS OF PRINT LINE            00107   
         LA    R5,0                    LENGTH OF PRINT LINE             00108   
         APPEND AFPHDR,AFPHDRLN                                         00109   
         BCTR  R5,0                    DON'T COUNT THE X'5A' CHAR       00110   
*                                      IN THE PREFIX                    00111   
         LA    R3,PARMS                GET ADDRESS OF 1ST COMMAND       00112   
NEXTCMD  EQU   *                                                        00113   
         TM    0(R3),X'80'             LAST PARM?                       00114   
         BO    BADPARM                 YES BRANCH?                      00115   
         L     R6,0(R3)                GET ADDRESS OF PARM              00116   
         USING CMDESCD,R6                                               00117   
         CLC   CMD,=C'*BX'             DRAW BOX?                        00118   
         BE    DRAWBOXA                YES, BRANCH                      00119   
         CLC   CMD,=C'*HL'             DRAW HORIZONTAL LINE?            00120   
         BE    DRAWHORA                YES, BRANCH                      00121   
         CLC   CMD,=C'*VL'             DRAW HORIZONTAL LINE?            00122   
         BE    DRAWVERA                YES, BRANCH                      00123   
         B     BADPARM                                                  00124   
         EJECT                                                          00125   
**********************************************************************  00126   
*                                                                    *  00127   
*       DRAWBOXA MACRO COMMAND REQUIRES FIVE PARMS                   *  00128   
*              - BASE LINE NUMBER IN PELS (UPPER LEFT)               *  00129   
*              - IN LINE NUMBER (COLUMN) IN PELS (UPPER LEFT)        *  00130   
*              - BOX WIDTH (HORIZONTAL)                              *  00131   
*              - BOX DEPTH (VERITCAL)                                *  00132   
*              - LINE WIDTH IN PELS                                  *  00133   
*                                                                    *  00134   
**********************************************************************  00135   
DRAWBOXA EQU   *                                                        00136   
         TM    4(R3),X'80'             IS BASE LINE LAST PARM?          00137   
         BO    BADPARM                 YES BRANCH?                      00138   
         TM    8(R3),X'80'             IS IN LINE LAST PARM?            00139   
         BO    BADPARM                 YES BRANCH?                      00140   
         TM    12(R3),X'80'            IS BASE LINE LAST PARM?          00141   
         BO    BADPARM                 YES BRANCH?                      00142   
         TM    16(R3),X'80'            IS IN LINE LAST PARM?            00143   
         BO    BADPARM                 YES BRANCH?                      00144   
         LM    R7,R11,4(R3)            GET THE PARM ADDRESSES           00145   
*                                                                       00146   
*       STORE THE PARAMETERS IN THE MACRO DEFINITION                    00147   
*                                                                       00148   
         MVC   BOXBASA,0(R7)           MOVE THE START LINE TO           00149   
         MVC   BOXBAS4A,0(R7)            TOP LINE AND RIGHT SIDE        00150   
         MVC   BOXINLA,0(R8)           MOVE THE START COLUMN            00151   
         LH    R8,0(R8)                GET THE ACTUAL START COL         00152   
         MVC   BOXLNA,0(R9)            MOVE THE BOX WIDTH TO            00153   
         MVC   BOXLN3A,0(R9)              THE TOP AND BOTTOM            00154   
         AH    R8,0(R9)                ADD BOX WIDTH TO START COL       00155   
*                                        FOR THE RIGHT SIDE             00156   
         MVC   BOXLN2A,0(R10)          MOVE THE BOX DEPTH TO            00157   
         MVC   BOXLN4A,0(R10)             THE SIDES                     00158   
         MVC   BOXBAS3A,0(R10)                                          00159   
         MVC   BOXWIDA,0(R11)          MOVE THE LINE WIDTH TO           00160   
         MVC   BOXWID2A,0(R11)           ALL THE PLACES                 00161   
         MVC   BOXWID3A,0(R11)           IT BELONGS.                    00162   
         MVC   BOXWID4A,0(R11)                                          00163   
         SH    R8,0(R11)               SUBTRACT LINE WIDTH              00164   
         STH   R8,BOXINL4A             SAVE THE COMPUTED COLUMN         00165   
         APPEND BOXA,BOXALEN           MOVE THE MACRO                   00166   
         TM    20(R3),X'80'            IS LINE WIDTH LAST PARM?         00167   
         BO    LASTPARM                YES, BRANCH                      00168   
         LA    R3,24(0,R3)             ADVANCE TO NEXT PARM             00169   
         B     NEXTCMD                                                  00170   
         EJECT                                                          00171   
**********************************************************************  00172   
*                                                                    *  00173   
*       DRAWHORA MACRO COMMAND REQUIRES FOUR PARMS                   *  00174   
*              - BASE LINE NUMBER IN PELS                            *  00175   
*              - IN LINE NUMBER (COLUMN) IN PELS                     *  00176   
*              - LINE LENGTH IN PELS                                 *  00177   
*              - LINE WIDTH IN PELS                                  *  00178   
*                                                                    *  00179   
**********************************************************************  00180   
DRAWHORA EQU   *                                                        00181   
         TM    4(R3),X'80'             IS BASE LINE LAST PARM?          00182   
         BO    BADPARM                 YES BRANCH?                      00183   
         TM    8(R3),X'80'             IS IN LINE LAST PARM?            00184   
         BO    BADPARM                 YES BRANCH?                      00185   
         TM    12(R3),X'80'            IS LENGTH LAST PARM?             00186   
         BO    BADPARM                 YES BRANCH?                      00187   
         LM    R7,R10,4(R3)            GET THE PARM ADDRESSES           00188   
         MVC   DIRBASEL,0(R7)          STORE THE PARAMETERS IN          00189   
         MVC   DIRINLN,0(R8)             THE MACRO DEFINITION           00190   
         MVC   DIRLEN,0(R9)                                             00191   
         MVC   DIRWIDTH,0(R10)                                          00192   
         APPEND HLINE,HLINELEN         MOVE THE MACRO                   00193   
         TM    16(R3),X'80'            IS LINE WIDTH LAST PARM?         00194   
         BO    LASTPARM                YES, BRANCH                      00195   
         LA    R3,20(0,R3)             ADVANCE TO NEXT PARM             00196   
         B     NEXTCMD                                                  00197   
         EJECT                                                          00198   
**********************************************************************  00199   
*                                                                    *  00200   
*       DRAWVERA MACRO COMMAND REQUIRES FOUR PARMS                   *  00201   
*              - BASE LINE NUMBER IN PELS                            *  00202   
*              - IN LINE NUMBER (COLUMN) IN PELS                     *  00203   
*              - LINE LENGTH IN PELS                                 *  00204   
*              - LINE WIDTH IN PELS                                  *  00205   
*                                                                    *  00206   
**********************************************************************  00207   
DRAWVERA EQU   *                                                        00208   
         TM    4(R3),X'80'             IS BASE LINE LAST PARM?          00209   
         BO    BADPARM                 YES BRANCH?                      00210   
         TM    8(R3),X'80'             IS IN LINE LAST PARM?            00211   
         BO    BADPARM                 YES BRANCH?                      00212   
         TM    12(R3),X'80'            IS LENGTH LAST PARM?             00213   
         BO    BADPARM                 YES BRANCH?                      00214   
         LM    R7,R10,4(R3)            GET THE PARM ADDRESSES           00215   
         MVC   DBRBASEL,0(R7)          STORE THE PARAMETERS IN          00216   
         MVC   DBRINLN,0(R8)             THE MACRO DEFINITION           00217   
         MVC   DBRLEN,0(R9)                                             00218   
         MVC   DBRWIDTH,0(R10)                                          00219   
         APPEND VLINE,VLINELEN         MOVE THE MACRO                   00220   
         TM    16(R3),X'80'            IS LINE WIDTH LAST PARM?         00221   
         BO    LASTPARM                YES, BRANCH                      00222   
         LA    R3,20(0,R3)             ADVANCE TO NEXT PARM             00223   
         B     NEXTCMD                                                  00224   
*                                                                       00225   
BADPARM  EQU    *                                                       00226   
         MVC   PRTLINE+3(L'PARMERR),PARMERR                             00227   
*                                                                       00228   
LASTPARM EQU   *                                                        00229   
         STH   R5,PRTLINE+1            SAVE LENGTH IN AFP STREAM        00230   
         LM    R14,R12,12(R13)         RESTORE THE REGISTERS            00231   
         BR    R14                     RETURN TO CALLER                 00232   
         TITLE 'ELS AFP CTX INTERFACE MODULE WORKING STORAGE'           00233   
AFPHDR   EQU   *                                                        00234   
         DC    X'5A'                   AFP INDICATOR CHARACTER          00235   
AFPLEN   DC    XL2'0000'               AFP MSG LENGTH                   00236   
         DC    X'D3EE9B'               AFP CTX STRUCTURE FIELD ID       00237   
AFPFLAG  DC    X'00'                   AFP FLAG BYTE                    00238   
AFPSEQ   DC    X'0001'                 AFP SEQUENCE NUMBER              00239   
AFPHDRLN EQU   *-AFPHDR                AFP LENGTH OF HEADER             00240   
*                                                                       00241   
BOXA     EQU   *                       SKELETON FOR *BX MACRO           00242   
         DC    X'2BD3'                 ESCAPE SEQUENCE                  00243   
*                           TOP OF BOX                                  00244   
         DC    X'04D3'                 AMB COMMAND                      00245   
BOXBASA  DC    X'0000'                 BASE LINE COORDINATE             00246   
         DC    X'04C7'                 AMI COMMAND                      00247   
BOXINLA  DC    X'0000'                 IN LINE COORDINATE               00248   
         DC    X'07E5'                 DIR COMMAND                      00249   
BOXLNA   DC    X'0000'                 LINE LENGTH                      00250   
BOXWIDA  DC    X'0000'                 LINE WIDTH                       00251   
         DC    X'00'                   REQUIRED FILLER                  00252   
*                           LEFT SIDE OF BOX                            00253   
         DC    X'07E7'                 DBR COMMAND                      00254   
BOXLN2A  DC    X'0000'                 LINE LENGTH                      00255   
BOXWID2A DC    X'0000'                 LINE WIDTH                       00256   
         DC    X'00'                   REQUIRED FILLER                  00257   
*                           BOTTOM OF BOX                               00258   
         DC    X'04D5'                 RMB COMMAND                      00259   
BOXBAS3A DC    X'0000'                                                  00260   
         DC    X'07E5'                 DIR COMMAND                      00261   
BOXLN3A  DC    X'0000'                 LINE LENGTH                      00262   
BOXWID3A DC    X'0000'                 LINE WIDTH                       00263   
         DC    X'00'                   REQUIRED FILLER                  00264   
*                           RIGHT SIDE OF BOX                           00265   
         DC    X'04D3'                 AMB COMMAND                      00266   
BOXBAS4A DC    X'0000'                 BASE LINE COORDINATE             00267   
         DC    X'04C7'                 AMI COMMAND                      00268   
BOXINL4A DC    X'0000'                 IN LINE COORDINATE               00269   
         DC    X'07E6'                 DBR COMMAND                      00270   
BOXLN4A  DC    X'0000'                 LINE LENGTH                      00271   
BOXWID4A DC    X'0000'                 LINE WIDTH                       00272   
         DC    X'00'                   REQUIRED FILLER                  00273   
BOXALEN  EQU   *-BOXA                  MACRO LENGTH                     00274   
*                                                                       00275   
HLINE    EQU   *                       SKELETON FOR *HL MACRO           00276   
         DC    X'2BD3'                 ESCAPE SEQUENCE                  00277   
         DC    X'04D3'                 AMB COMMAND                      00278   
DIRBASEL DC    X'0000'                 BASE LINE COORDINATE             00279   
         DC    X'04C7'                 AMI COMMAND                      00280   
DIRINLN  DC    X'0000'                 IN LINE COORDINATE               00281   
         DC    X'07E4'                 DIR COMMAND                      00282   
DIRLEN   DC    X'0000'                 LINE LENGTH                      00283   
DIRWIDTH DC    X'0000'                 LINE WIDTH                       00284   
         DC    X'00'                   REQUIRED FILLER                  00285   
HLINELEN EQU   *-HLINE                 MACRO LENGTH                     00286   
*                                                                       00287   
VLINE    EQU   *                       SKELETON FOR *VL MACRO           00288   
         DC    X'2BD3'                 ESCAPE SEQUENCE                  00289   
         DC    X'04D3'                 AMB COMMAND                      00290   
DBRBASEL DC    X'0000'                 BASE LINE COORDINATE             00291   
         DC    X'04C7'                 AMI COMMAND                      00292   
DBRINLN  DC    X'0000'                 IN LINE COORDINATE               00293   
         DC    X'07E6'                 DBR COMMAND                      00294   
DBRLEN   DC    X'0000'                 LINE LENGTH                      00295   
DBRWIDTH DC    X'0000'                 LINE WIDTH                       00296   
         DC    X'00'                   REQUIRED FILLER                  00297   
VLINELEN EQU   *-VLINE                 MACRO LENGTH                     00298   
*                                                                       00299   
PARMERR  DC    C'0***ELKAFPCT PARAMETER ERROR***'                       00300   
         LTORG                                                          00301   
CMDESCD  DSECT                                                          00302   
CMD      DS    CL3                                                      00303   
PRTLINED DSECT                                                          00304   
PRTLINE  DS    CL133                                                    00305   
PARMAREA DSECT                                                          00306   
PRTLINEA DS    A                                                        00307   
PARMS    DS    A                                                        00308   
         END                                                            00309   
