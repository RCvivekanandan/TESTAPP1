*            LAST MAINTENANCE TIME: 11.32.13  DATE: 03/17/86            00001   
         TITLE 'MODULE ELAIOPGM  -  ENGLISH LANGUAGE GENERALIZED I/O MOX00002   
               DULE'                                                    00003   
ELAIOPGM DFHEIENT CODEREG=(3,4,5)                                       00004   
*********************************************************************** 00005   
*        M O D I F I C A T I O N S   L O G                              00006   
*********************************************************************** 00007   
*   MOD#    DATE     NAME        DESCRIPTION                            00008   
*   ------  -------- ----------- -----------------------------          00009   
*           99/99/99 D WILSON    INITIAL INSTALLATION                   00010   
*                                (JUST A SKELETON OF WHAT IT            00011   
*                                 IS NOW)                               00012   
*                                                                       00013   
*           99/99/99 B WOLOWEIC  FLESHED OUT DUANE'S INITIAL VERSION    00014   
*                              CHANGES BY BOB WOLOWIEC TO MAKE THIS A   00015   
*                              PRIME EXAMPLE OF WHAT CODING SKILL LEVEL 00016   
*                              WE SHOULD ALL STRIVE FOR IN OUR WORK.    00017   
*                                                                       00018   
*    XXXXX  02/27/86 D SECOR     REWORKED THE PROGRAM FOR               00019   
*                                ENGLISH LANGUAGE APPLICATION           00020   
*                                                                       00021   
*     0001  03/26/86 D SECOR     ADDED THE 'DLM' GENERIC DELETE         00022   
*                                FOR ALL ENGLISH LANGUAGE FILES BUT     00023   
*                                THE DATA ELEMENT FILE.                 00024   
*                                                                       00025   
*********************************************************************** 00026   
*********************************************************************** 00027   
*                                                                       00028   
*   PROGRAM:      ELAIOPGM                                              00029   
*   AUTHOR:       D. SECOR                                              00030   
*   DATE WRITTEN: 02/27/86                                              00031   
*                                                                       00032   
*   PURPOSE:   PROVIDE I/O SUPPORT FOR THE ENGLISH LANGUAGE             00033   
*              APPLICATION FILES.                                       00034   
*                                                                       00035   
*   FUNCTIONS: WHEN LINKED TO                                           00036   
*              THIS MODULE WILL PROVIDE ONE OF THE FOLLOWING            00037   
*              SERVICES:                                                00038   
*                       *. READ A RECORD (DIRECTLY USING KEY EQUAL OR   00039   
*                                        EQUAL/GREATER THAN OPTION)     00040   
*                       *. READ A RECORD FOR UPDATE                     00041   
*                       *. REWRITE A RECORD (AFTER A READ FOR           00042   
*                                             UPDATE)                   00043   
*                       *. DELETE A RECORD (AFTER A READ FOR            00044   
*                                        UPDATE ON SELECTED             00045   
*                                        FILES)                         00046   
*                       *. DELETE A RECORD (ONLY ON SELECTED            00047   
*                                        FILES)                         00048   
*                       *. UNLOCK A RECORD (AFTER A READ FOR            00049   
*                                             UPDATE)                   00050   
*                       *. WRITE A RECORD (ADD NEW RECORD)              00051   
*                                         IF DUP RECORD FOUND           00052   
*                                         THEN THIS MODULE              00053   
*                                         WILL ABEND THE TASK           00054   
*                                         TO BACK OUT OTHER             00055   
*                                         FILE UPDATES VIA DTB)         00056   
*                       *. WRITE A RECORD (ADD NEW RECORD BUT           00057   
*                                         IF DUP RECORD FOUND           00058   
*                                         THEN RETURN TO                00059   
*                                         APPLICATION TO                00060   
*                                         UPDATE RECORD FOUND)          00061   
*                       *. START A BROWSE AND READ THE FIRST            00062   
*                           RECORD (OPTIONS ARE KEY EQUAL OR            00063   
*                                   KEY EQUAL/GREATER THAN)             00064   
*                       *. START A GENERIC BROWSE AND READ              00065   
*                                  THE FIRST RECORD                     00066   
*                                  (OPTIONS ARE KEY EQUAL OR            00067   
*                                   KEY EQUAL/GREATER THAN)             00068   
*                       *. START A BROWSE AND READ THE PREVIOUS         00069   
*                           RECORD (OPTIONS ARE KEY EQUAL OR            00070   
*                                   KEY EQUAL/GREATER THAN)             00071   
*                       *. START A GENERIC BROWSE AND READ              00072   
*                                  THE PREVIOUS RECORD                  00073   
*                                  (OPTIONS ARE KEY EQUAL OR            00074   
*                                   KEY EQUAL/GREATER THAN)             00075   
*                       *. READ NEXT RECORD OF BROWSE                   00076   
*                       *. READ PREVIOUS RECORD OF BROWSE               00077   
*                       *. END  A BROWSE                                00078   
*                                                                       00079   
*                                                                       00080   
*              ALTERNATE INDEX PROCESSING PLAYS AN IMPORTANT PART IN    00081   
*              THIS PROGRAM AS IT OCCUPIES A LARGE PART OF THE CODE.    00082   
*              THE ACTIONS THAT CAN OCCUR ARE SHOWN BY TOPIC:           00083   
*                       *. READ A RECORD USING AN ALTERNATE INDEX       00084   
*                       *. READ A RECORD FOR UPDATE CAN BE DONE USING   00085   
*                                        AN ALTERNATE INDEX             00086   
*                       *. START A BROWSE AND READ THE NEXT SEQUENTIAL  00087   
*                           RECORD USING THE ALTERNATE INDEX (OPTIONS   00088   
*                           ARE KEY EQUAL OR GREATER/EQUAL, GREATER     00089   
*                           THAN, LESS/EQUAL, OR LESS THAN)             00090   
*                       *. START A BROWSE AND READ THE PREVIOUS         00091   
*                           SEQUENTIAL RECORD USING THE ALTERNATE INDEX 00092   
*                           (OPTIONS ARE KEY EQUAL OR GREATER/EQUAL,    00093   
*                           GREATER THAN, LESS/EQUAL, OR LESS THAN)     00094   
*                                                                       00095   
*                                                                       00096   
*              THIS MODULE CAN RETURN WITH THE FOLLOWING                00097   
*              RETURN CODES:                                            00098   
*                         00 - FUNCTION COMPLETED NORMALLY              00099   
*                         01 - RECORD NOT FOUND                         00100   
*                         02 - DUPLICATE KEY ON ADD                     00101   
*                              THIS MODULE WILL ABEND WITH              00102   
*                              AN ABEND CODE OF 'DKEY' ON               00103   
*                              AN ADD OF DUPKEY RECORD.                 00104   
*                         03 - END OF FILE                              00105   
*                         10 - FUNCTION COMPLETED NORMALLY              00106   
*                              HOWEVER VSAM AIX LOGIC                   00107   
*                              INDICATES ADDITIONAL RECORDS             00108   
*                              WITH THE SAME SECONDARY KEY              00109   
*                              ARE AVAILABLE...                         00110   
*                         12 - DUPLICATE KEY ON ADD BUT                 00111   
*                              APPLICATION LOGIC                        00112   
*                              WANTS THE RECORD FOUND                   00113   
*                              FOR UPDATE PROCESSING                    00114   
*                                                                       00115   
*                          ANY OTHER ERROR CONDITIONS ARE               00116   
*                          CONSIDERED UNRECOVERABLE AND ARE             00117   
*                          HANDLED VIA A LINK TO MODULE                 00118   
*                          HGFCPER FOR DISPOSITION.                     00119   
*                                                                       00120   
*                                                                       00121   
*********************************************************************** 00122   
         EJECT                                                          00123   
*********************************************************************** 00124   
*        R E G I S T E R     U S A G E                                  00125   
*********************************************************************** 00126   
R0       EQU   0    LINKAGE/WORK                                        00127   
R1       EQU   1    LINKAGE/WORK                                        00128   
R2       EQU   2    WORK                                                00129   
R3       EQU   3    1ST BASE REGISTER                                   00130   
R4       EQU   4    2ND BASE REGISTER                                   00131   
R5       EQU   5    3RD BASE REGISTER                                   00132   
R6       EQU   6    COMMON INTERFACE AREA FROM CALLER                   00133   
R7       EQU   7    COMMON I/O PARM AREA FROM CALLER                    00134   
R8       EQU   8    RECORD ADDRESS POINTER                              00135   
R9       EQU   9    ALTERNATE INDEX RECORD ADDRESS POINTER              00136   
R10      EQU   10   WORK                                                00137   
R11      EQU   11   EXEC INTERFACE POINTER                              00138   
R12      EQU   12   BAL REGISTER FOR SUBROUTINE PROCESSES               00139   
R13      EQU   13   DFHEISTG DYNAMIC STORAGE AREA                       00140   
R14      EQU   14   LINKAGE/WORK                                        00141   
R15      EQU   15   LINKAGE/WORK                                        00142   
         EJECT                                                          00143   
         COPY  ELADCIA                                                  00144   
         EJECT                                                          00145   
         COPY  ELADIOPM                                                 00146   
         EJECT                                                          00147   
ELPDED   DSECT                                                          00148   
         COPY  ELPDED               DATA ELEMENT FILE                   00149   
         EJECT                                                          00150   
ELALTIDX DSECT                                                          00151   
         COPY  ELPEND               ENGLISH NAME FILE                   00152   
         EJECT                                                          00153   
         ORG   ENENGLNM                                                 00154   
*                                                                       00155   
         COPY  ELPCND               SYSTEM NAME FILE                    00156   
*                                                                       00157   
         ORG   *                                                        00158   
         EJECT                                                          00159   
ELPCVD   DSECT                                                          00160   
         COPY  ELPCVD               CODE VALUE FILE                     00161   
         EJECT                                                          00162   
ELPRLD   DSECT                                                          00163   
         COPY  ELPRLD               RECORD LIST FILE                    00164   
         EJECT                                                          00165   
         COPY  HGNADCEI             HGAFCPER COMMAREA                   00166   
         EJECT                                                          00167   
*                                                                       00168   
*  DSECT FOR THE VARIABLE RECORD TABLE                                  00169   
*                                                                       00170   
VARRECDS DSECT                                                          00171   
VARRDDN  DS    CL8                                                      00172   
VARROCCT DS    H             DISPLACEMENT TO DEPENDING ON FIELD         00173   
VARRFLEN DS    H             FIXED AREA LENGTH                          00174   
VARRVLEN DS    H             VARIABLE SECTION LENGTH (INDIVIDUAL)       00175   
VARMAXOC DS    H             MAX NUMBER OF OCCURS FOR TOTAL RECORD.     00176   
VARRECLN EQU   *-VARRECDS                                               00177   
*                                                                       00178   
*                                                                       00179   
*     DSECT FOR THE NAME/LENGTH TABLE                                   00180   
*                                                                       00181   
DDNTABLE DSECT                                                          00182   
TABDDN   DS    CL8                                                      00183   
TABDDNLN DS    CL2                                                      00184   
         EJECT                                                          00185   
DFHEISTG DSECT                                                          00186   
PACK8    DS    PL8                PACKED WORK AREA                      00187   
PACK2    DS    PL2                                                      00188   
LENGTH   DS    H                                                        00189   
ABCODE   DS    CL4                ABEND CODE                            00190   
ENGLNAME DS    0CL83            ENGLISH NAME ALT INDEX KEY              00191   
ENMRECID DS    CL8                RECORD ID                             00192   
ENMENAME DS    CL75               ENGLISH NAME                          00193   
SYSTNAME DS    0CL38            SYSTEM NAME ALT INDEX KEY               00194   
SNMRECID DS    CL8                RECORD ID                             00195   
SNMENAME DS    CL30               SYSTEM NAME                           00196   
DTELNAME DS    0CL11            DATA ELEMENT FILE KEY                   00197   
DEMRECID DS    CL8                RECORD ID                             00198   
DEMELTNO DS    PL3                ELEMENT NUMBER                        00199   
CEIAREA  DS    CL(CEILEN)         COMMAREA PASSED TO HGAFCPER           00200   
SVNAMLN  DS    F                  NAME/LENGTH POINTER                   00201   
ALTGTMN  DS    F                  SAVED GETMAIN ADDRESS                 00202   
SAVE10   DS    F                  INTERNAL REGISTER SAVE AREA           00203   
HALFWK   DS    H                                                        00204   
RCNLEN   EQU   L'DECOBLNM                                               00205   
*                                                                       00206   
         EJECT                                                          00207   
*                                                                       00208   
ELAIOPGM CSECT                                                          00209   
         PROGDATE                                                       00210   
*                                                                       00211   
         CLC   EIBCALEN,=H'4'      EXPECTED COMMAREA LENGTH             00212   
         BNE   INVREQ99                                                 00213   
*                                                                       00214   
         L     R6,DFHEICAP         GET THE COMMAREA ADDRESS             00215   
         L     R6,0(R6)            GET THE COMMON INTERFACE AREA        00216   
         USING ELADCIA,R6           PASSED FROM CALLER.                 00217   
*                                                                       00218   
         L     R7,CIAIOINT         GET THE IO PARM AREA                 00219   
         USING ELADIOPM,R7          PASSED FROM CALLER.                 00220   
*                                                                       00221   
         L     R8,ELIORECA         AREA ADDRESS                         00222   
         USING ELPDED,R8          PRIMARY RECORD REFERENCE              00223   
         USING ELALTIDX,R9         SECONDARY REFERENCE                  00224   
*                                                                       00225   
         MVC   ELIORC,=C'00'       INITIALIZE THE RETURN CODE           00226   
*                                                                       00227   
*                                                                       00228   
*   SCAN THE LENGTH TABLE AND MATCH ON DDNAME FOR FOR                   00229   
*   VERIFICATION.   IT WILL ALSO GIVE US MAX RECORD LENGTH.             00230   
*                                                                       00231   
         LA    R10,ELRECLEN           POINT TO NAME/LENGTH TABLE        00232   
         USING DDNTABLE,R10                                             00233   
FILELOOP DS    0H                                                       00234   
         CLC   TABDDN,ELIODDNM              CHECK DDNAME                00235   
         BE    GOTDDN                                                   00236   
         AH    R10,=H'10'                    BUMP UP TABLE POINTER      00237   
         CLC   0(4,R10),=4X'FF'              IF END OF TABLE            00238   
         BE    INVREQDN                      GO TO INVALID REQUEST      00239   
         B     FILELOOP                                                 00240   
*                                                                       00241   
GOTDDN   DS    0H       S A V E   P O I N T E R   F O R   E N T R Y     00242   
*                               D D N A M E   I S   V A L I D           00243   
         ST    R10,SVNAMLN                                              00244   
*                                                                       00245   
*    IF THE ACCESS CODE IS A READ TYPE OPERATION THEN A GETMAIN         00246   
*    AREA MUST HAVE BEEN PASSED FROM THE CALLER OR THE                  00247   
*    I/O PROGRAM MUST OBTAIN ONE FOR THE RECORD READ.                   00248   
*                                                                       00249   
         CLC   ELIOACES,=C'RD '    READ?                                00250   
         BE    GETAREA                                                  00251   
         CLC   ELIOACES,=C'RU '    READ FOR UPDATE?                     00252   
         BE    GETAREA                                                  00253   
         CLC   ELIOACES,=C'SB '    STARTBR?                             00254   
         BE    GETAREA                                                  00255   
         CLC   ELIOACES,=C'SBP'    STARTBR PREVIOUS?                    00256   
         BE    GETAREA                                                  00257   
         CLC   ELIOACES,=C'RN '    READ NEXT                            00258   
         BE    GETAREA                                                  00259   
         CLC   ELIOACES,=C'RP '    READ PREVIOUS                        00260   
         BE    GETAREA                                                  00261   
         CLC   ELIOACES,=C'GB '    START GENERIC BROWSE?                00262   
         BE    GETAREA                                                  00263   
         CLC   ELIOACES,=C'GBP'    START GENERIC BROWSE PREVIOUS?       00264   
         BE    GETAREA                                                  00265   
         B     VERLEN      NOT A READ OPERATION                         00266   
         EJECT                                                          00267   
GETAREA  DS    0H                                                       00268   
*   WE HAVE A READ TYPE OPERATION.                                      00269   
*   CHECK THE MAXIMUM RECORD LENGTH INSURE IT EQUALS TABLE.             00270   
*   CHECK THE STORAGE MODE.  IF THE CALLER PASSED US                    00271   
*   A RECORD AREA CHECK THAT IT EXISTS.                                 00272   
*                                                                       00273   
         CLC   ELIOMXRL,TABDDNLN   MAX REC LEN = TABLE'S VALUE          00274   
         BNE   INVREQML            MAX REC LENGTH NOT CORRECT           00275   
*                                                                       00276   
         CLI   ELIOSTOR,ELIOGETM   GETMAIN BY THE IO PROGRAM ?          00277   
         BE    IOGTMAIN            YES THEN DO IT.                      00278   
         CLI   ELIOSTOR,ELIORCAV   GETMAIN PASSED FROM CALLER           00279   
         BNE   INVREQSM            NO   THEN ERROR                      00280   
*                                                                       00281   
*    CHECK THAT THE RECORD ADDRESS POINTER IS FILLED.                   00282   
         CLC   ELIORECA,=4X'00'                                         00283   
         BE    INVREQRA                                                 00284   
         B     HANDLE             ALL OK GO TO OPERATIONS               00285   
*                                                                       00286   
IOGTMAIN DS    0H                                                       00287   
*   STORAGE MODE = P.                                                   00288   
*   IO PROGRAM WILL DO THE GETMAIN FOR THE RECORD.  PLUG THE            00289   
*   MAX RECORD LENGTH IN THE MAX RECORD LENGTH FIELD.                   00290   
         MVC   ELIOMXRL,TABDDNLN                                        00291   
*                                                                       00292   
         EXEC CICS  GETMAIN   SET(R8)  LENGTH(ELIOMXRL)  INITIMG(X'00') 00293   
*                                                                       00294   
         ST    R8,ELIORECA         STORAGE ADDRESS IN I/O PARM          00295   
         MVI   ELIOSTOR,ELIORCAV   STORAGE MODE SWITCH TO 'M'           00296   
*                                                                       00297   
*   INDICATE THAT THE RECORD AREA WAS OBTAINED BY ELAIOPGM.             00298   
*                                                                       00299   
         OI    ELGTMIND,X'02'                                           00300   
*                                                                       00301   
*   THIS ONE BYTE INFO FIELD DESCRIBES HOW THE I/O PARM AREA            00302   
*   AND THE RECORD WERE OBTAINED.                                       00303   
*                                                                       00304   
*     BIT  0  I/O PARM AREA OBTAINED BY ELAIOGMN                        00305   
*          1                                                            00306   
*          2                                                            00307   
*          3                                                            00308   
*          4  RECORD AREA OBTAINED BY ELAIOGMN                          00309   
*          5                                                            00310   
*          6  RECORD AREA OBTAINED BY ELAIOPGM                          00311   
*          7                                                            00312   
*                                                                       00313   
         B     HANDLE                                                   00314   
         EJECT                                                          00315   
**********************************************************************  00316   
*  DETERMINE IF ACCESS CODE IS REQUESTING AN UPDATE                     00317   
*   OPERATION OR ADD.  VERIFY RECORD LENGTH IS                          00318   
*        REASONABLE LENGTH                                              00319   
**********************************************************************  00320   
VERLEN   DS    0H                                                       00321   
         CLC   ELIOACES,=C'WU '    WRITE FOR UPDATE(REWRITE)?           00322   
         BE    LENGHTC                                                  00323   
         CLC   ELIOACES,=C'WR '    WRITE(ADD NEW RECORD)?               00324   
         BE    LENGHTC                                                  00325   
         CLC   ELIOACES,=C'WDP'    WRITE(ADD NEW RECORD/DUPREC)?        00326   
         BNE   HANDLE                                                   00327   
*                                                                       00328   
LENGHTC  DS    0H                                                       00329   
*  ELIOMXRL IS SET.   ELIORL IS FROM THE CALLER FOR THE RECORD          00330   
*   TO BE WRITTEN.  ELIORL CANNOT BE MORE THAN MAX LENGTH OR            00331   
*   WE MAY WRITE A RECORD IN TOO SHORT AN AREA.                         00332   
*                                                                       00333   
*    IS THE MAX REC LENGTH IN I/O PARMS OK?                             00334   
         CLC   ELIOMXRL,TABDDNLN                                        00335   
         BNE   INVREQML       ERROR IF NOT EQUAL TO TABLE MAX           00336   
*                                                                       00337   
         CLC   ELIORL,ELIOMXRL     NEW LENGTH VERSUS MAXIMUM ???        00338   
         BNH   HANDLE              IF NOT HIGH THEN OK                  00339   
         B     INVREQNL            IF HIGH THEN BAD                     00340   
         EJECT                                                          00341   
***************************************************************         00342   
*     THE FOLLOWING COMMAND ROUTES EXECUTION TO THE           *         00343   
*     APPROPRIATE ROUTINE IF CERTAIN ERROR TYPES OCCUR        *         00344   
***************************************************************         00345   
*                                                                       00346   
*                                                                       00347   
HANDLE   EXEC CICS  HANDLE CONDITION   NOTFND(NOTFND)  DUPKEY(DUPKEY)  X00348   
                   ENDFILE(ENDFILE)  ERROR(ERROR)                       00349   
*                                                                       00350   
         EJECT                                                          00351   
**********************************************************************  00352   
*        V A L I D A T E   F U N C T I O N   C O D E   A N D            00353   
*           T A K E   A P P R O P R I A T E   A C T I O N               00354   
**********************************************************************  00355   
MAINLINE DS    0H                                                       00356   
         CLC   ELIOACES,=C'RD '    READ?                                00357   
         BE    READ                                                     00358   
         CLC   ELIOACES,=C'SB '    STARTBR?                             00359   
         BE    STARTBR                                                  00360   
         CLC   ELIOACES,=C'SBP'    STARTBR PREVIOUS?                    00361   
         BE    STARTBR                                                  00362   
         CLC   ELIOACES,=C'GB '    START GENERIC BROWSE?                00363   
         BE    GENBR                                                    00364   
         CLC   ELIOACES,=C'GBP'    START GENERIC BROWSE PREVIOUS?       00365   
         BE    GENBR                                                    00366   
         CLC   ELIOACES,=C'RN '    READ NEXT?                           00367   
         BE    READNEXT                                                 00368   
         CLC   ELIOACES,=C'RP '    READ PREVIOUS?                       00369   
         BE    READPREV                                                 00370   
         CLC   ELIOACES,=C'EB '    END BROWSE?                          00371   
         BE    ENDBR                                                    00372   
         CLC   ELIOACES,=C'RU '    READ FOR UPDATE?                     00373   
         BE    READUPDT                                                 00374   
         CLC   ELIOACES,=C'WU '    WRITE FOR UPDATE(REWRITE)?           00375   
         BE    REWRITE                                                  00376   
         CLC   ELIOACES,=C'DLU'    DELETE (AFTER READ FOR UPDATE)?      00377   
         BE    DELETEU                                                  00378   
         CLC   ELIOACES,=C'DL '    DELETE (WITHOUT READ FOR UPDATE)?    00379   
         BE    DELETE                                                   00380   
         CLC   ELIOACES,=C'DLM'    GENERIC DELETE (FOR PARTIAL KEY)?    00381   
         BE    MASSDELT                                                 00382   
         CLC   ELIOACES,=C'ULK'    UNLOCK (AFTER READ FOR UPDATE)?      00383   
         BE    UNLOCK                                                   00384   
         CLC   ELIOACES,=C'WR '    WRITE(ADD NEW RECORD)?               00385   
         BE    WRITE                                                    00386   
         CLC   ELIOACES,=C'WDP'    WRITE(ADD NEW RECORD BUT RETURN      00387   
         BE    WRITE                        ON DUPLECATE RECORD)        00388   
         B     INVREQ              HANDLE INVALID REQUEST.              00389   
*                                                                       00390   
**********************************************************************  00391   
*           R E T U R N   T O   C A L L E R                             00392   
**********************************************************************  00393   
*                                                                       00394   
RETURN   DS    0H                                                       00395   
*                                                                       00396   
         EXEC CICS  RETURN                                              00397   
         EJECT                                                          00398   
**********************************************************************  00399   
*     R E A D   A   R E C O R D   D I R E C T   F R O M                 00400   
*          T H E   S P E C I F I E D   F I L E                          00401   
**********************************************************************  00402   
*                                                                       00403   
READ     DS    0H                                                       00404   
*                                                                       00405   
         CLC   ELIOALDD,ELELPCN                                         00406   
         BNE   READSYNM                                                 00407   
         BAL   R2,REDSYSNM                                              00408   
READSYNM CLC   ELIOALDD,ELELPEN                                         00409   
         BNE   READNOAL                                                 00410   
         BAL   R2,REDENGNM                                              00411   
*    MOVE THE MAX RECORD LENGTH TO THE RECORD LENGTH SO IT              00412   
*    WILL BE UPDATED FROM THE READ.                                     00413   
READNOAL MVC   ELIORL,ELIOMXRL                                          00414   
         CLC   ELIOQUAL,=C'GTE'        READ 'GT' OR 'EQ' REQUEST        00415   
         BE    READGTEQ            ELSE                    T.R. 9/25/84 00416   
*                                                                       00417   
*                                                                       00418   
         EXEC CICS  READ   DATASET(ELIODDNM)    INTO(DEDATELM)         X00419   
                   LENGTH(ELIORL)  RIDFLD(ELIOKEY)                      00420   
*                                                                       00421   
         B     RETURN                  RETURN TO CALLER                 00422   
*                                                                       00423   
READGTEQ DS    0H                                                       00424   
         EXEC CICS  READ   DATASET(ELIODDNM)   GTEQ   INTO(DEDATELM)   X00425   
                   LENGTH(ELIORL)  RIDFLD(ELIOKEY)                      00426   
*                                                                       00427   
         MVC   ELIOQUAL,=C'   '        RESET QUALIFIER                  00428   
         B     RETURN                  RETURN TO CALLER                 00429   
         EJECT                                                          00430   
**********************************************************************  00431   
*         R E A D   R E C O R D   F O R   U P D A T E                   00432   
**********************************************************************  00433   
*                                                                       00434   
READUPDT DS    0H                                                       00435   
*                                                                       00436   
         CLC   ELIOALDD,ELELPCN                                         00437   
         BNE   RDUPSYNM                                                 00438   
         BAL   R2,REDSYSNM                                              00439   
RDUPSYNM CLC   ELIOALDD,ELELPEN                                         00440   
         BNE   RDUPNOAL                                                 00441   
         BAL   R2,REDENGNM                                              00442   
*    MOVE THE MAX RECORD LENGTH TO THE RECORD LENGTH SO IT              00443   
*    WILL BE UPDATED FROM THE READ.                                     00444   
RDUPNOAL MVC   ELIORL,ELIOMXRL                                          00445   
*                                                                       00446   
         EXEC CICS  READ   DATASET(ELIODDNM)  UPDATE   INTO(DEDATELM)  X00447   
                   LENGTH(ELIORL)  RIDFLD(ELIOKEY)                      00448   
*                                                                       00449   
         CLC   ELIODDNM,=C'ELPDE   '      WAS IT A CLAIM REC READ       00450   
         BNE   RETURN                     IF NOT RETURN                 00451   
         BAL   R2,PRCIAALT               IF SO PRIME THE CIA WITH       00452   
         B     RETURN              THE ENGLISH & SYSTEM NAME FILE KEYS  00453   
         EJECT                                                          00454   
**********************************************************************  00455   
*     W R I T E   R E C O R D   F O R   U P D A T E (REWRITE)           00456   
**********************************************************************  00457   
*                                                                       00458   
REWRITE  DS    0H                                                       00459   
*  SOME OF OUR FILES CONTAIN VARIABLE LENGTH RECORDS.                   00460   
*  FOR THOSE FILES WE MUST CALCULATE THE RECORD LENGTH TO BE USED       00461   
*  FOR THE REWRITE.                                                     00462   
*                                                                       00463   
         BAL   R2,VARRECCK                                              00464   
*                                                                       00465   
         EXEC CICS  REWRITE  DATASET(ELIODDNM)  LENGTH(ELIORL)         X00466   
                   FROM(DEDATELM)                                       00467   
*                                                                       00468   
*   SUCCESSFUL REWRITE,  CHECK IF THE ENGLISH OR SYSTEM NAME            00469   
*   HAS CHANGED.  IF SO WE MUST UPDATE THE ALTERNATE INDEX FILES.       00470   
         CLC   ELIODDNM,=CL8'ELPDE'                                     00471   
         BNE   RETURN                                                   00472   
         L     R8,ELIORECA          ADDRESSABILITY TO UPDATED REC       00473   
         MVC   ENMRECID,DERECPRF    RECORD ID                           00474   
         MVC   ENMENAME,DEELMTNM    ENGLISH NAME                        00475   
         MVC   SNMRECID,DERECPRF    RECORD ID                           00476   
         MVC   SNMENAME,DECOBLNM    SYSTEM NAME                         00477   
*                                                                       00478   
         CLC   ENGLNAME,CIAENNMK     ENGLISH NAME KEY  CHANGE           00479   
         BE    RWSYSCH1                 NO, GO CHECK SYS NAME CHANGE    00480   
         BAL   R2,ENGNMDEL       YES   1. DELETE OLD ALT ENTRY          00481   
         BAL   R2,ENGNMADD             2. ADD NEW ALT ENTRY             00482   
*                                                                       00483   
RWSYSCH1 CLC   SYSTNAME,CIASYNMK     SYSTEM NAME KEY  CHANGE            00484   
         BE    CLCIA1                   NO,  CLEAR CIA AND RETURN       00485   
         CLI   CIASYSNM,C' '         OLD SYSTEM NAME ALL BLANKS         00486   
         BNE   RWSYSCH2                 NO, GO CHECK FOR LOW-VALUES     00487   
         CLC   CIASYSNM+1(L'CIASYSNM-1),CIASYSNM                        00488   
         BE    RWSYSCH4                 YES THEN DON'T DELETE           00489   
RWSYSCH2 CLI   CIASYSNM,X'00'        OLD SYSTEM NAME ALL LOW-VALUES     00490   
         BNE   RWSYSCH3                 NO, GO DELETE THE ALTERNATE IDX 00491   
         CLC   CIASYSNM+1(L'CIASYSNM-1),CIASYSNM                        00492   
         BE    RWSYSCH4                 YES THEN DON'T DELETE           00493   
RWSYSCH3 BAL   R2,SYSNMDEL       YES   1.  DELETE OLD ALT ENTRY         00494   
RWSYSCH4 CLI   SNMENAME,C' '         NEW SYSTEM NAME ALL BLANKS         00495   
         BNE   RWSYSCH5                 NO, GO CHECK FOR LOW-VALUES     00496   
         CLC   SNMENAME+1(L'SNMENAME-1),SNMENAME                        00497   
         BE    CLCIA1                   YES THEN DON'T ADD NEW NAME     00498   
RWSYSCH5 CLI   SNMENAME,X'00'        NEW SYSTEM NAME ALL LOW-VALUES     00499   
         BNE   RWSYSCH6                 NO, THEN ADD NEW SYSTEM NAME    00500   
         CLC   SNMENAME+1(L'SNMENAME-1),SNMENAME                        00501   
         BE    CLCIA1                   YES THEN DON'T ADD NEW NAME     00502   
RWSYSCH6 BAL   R2,SYSNMADD             2. ADD NEW ALTERNATE INDEX       00503   
CLCIA1   DS    0H                                                       00504   
         BAL   R2,CLCIAALT              CLEAR CIA OF ENG & SYS KEYS     00505   
         B     RETURN                                                   00506   
         EJECT                                                          00507   
**********************************************************************  00508   
*         D E L E T E   A   R E C O R D  (AFTER READ FOR UPDATE)        00509   
**********************************************************************  00510   
*                                                                       00511   
DELETEU  DS    0H                                                       00512   
*     DO NOT ALLOW DELETES ON ANY FILE.   JUST ACCEPT DELETES           00513   
*     FOR THE FOLLOWING FILES.                                          00514   
*                                                                       00515   
         CLC   ELIODDNM,ELELPDE       FOR DATA ELEMENT FILE             00516   
         BE    VALDELU                                                  00517   
         CLC   ELIODDNM,ELELPRL       FOR RECORD LIST FILE              00518   
         BE    VALDELU                                                  00519   
         CLC   ELIODDNM,ELELPCV       FOR CODE VALUE FILE.              00520   
         BE    VALDELU                                                  00521   
         B     INVREQ                                                   00522   
*                                                                       00523   
VALDELU  DS    0H                                                       00524   
*                                                                       00525   
         EXEC CICS  DELETE   DATASET(ELIODDNM)                          00526   
*                                                                       00527   
*   SUCCESSFUL DELETE,  CHECK IF THIS WAS A DATA ELEMENT RECORD DELETE  00528   
*   IF YES WE MUST DELETE BOTH THE ENGLISH NAME & SYSTEM NAME           00529   
*   INDEX ENTRIES.                                                      00530   
         CLC   ELIODDNM,=CL8'ELPDE'                                     00531   
         BNE   RETURN                                                   00532   
*                                                                       00533   
         BAL   R2,ENGNMDEL       REMOVE ENG NAME ALT INDEX ENTRY        00534   
         CLI   CIASYSNM,C' '         NEW SYSTEM NAME ALL BLANKS         00535   
         BNE   DESYSCH1                 NO, DELETE ALTERNATE INDEX      00536   
         CLC   CIASYSNM+1(L'CIASYSNM-1),CIASYSNM                        00537   
         BE    RETURN                                                   00538   
DESYSCH1 CLI   CIASYSNM,X'00'        NEW SYSTEM NAME ALL LOW-VALUES     00539   
         BNE   DESYSCH2                 NO, DELETE ALTERNATE INDEX      00540   
         CLC   CIASYSNM+1(L'CIASYSNM-1),CIASYSNM                        00541   
         BE    RETURN                                                   00542   
DESYSCH2 BAL   R2,SYSNMDEL       REMOVE SYSTEM NAME ALT INDEX ENTRY     00543   
         B     RETURN                                                   00544   
         EJECT                                                          00545   
**********************************************************************  00546   
*         D E L E T E   A   R E C O R D   (DIRECTLY)                    00547   
**********************************************************************  00548   
*                                                                       00549   
DELETE   DS    0H                                                       00550   
         CLC   ELIODDNM,ELELPCV       FOR CODE VALUE FILE               00551   
         BE    OKDEL                                                    00552   
         CLC   ELIODDNM,ELELPDE       FOR DATA ELEMENT FILE             00553   
         BE    DELRD1ST                                                 00554   
         CLC   ELIODDNM,ELELPRL       FOR RECORD LIST FILE              00555   
         BNE   INVREQ                                                   00556   
         B     OKDEL                                                    00557   
*                                                                       00558   
DELRD1ST DS    0H                                                       00559   
*                                                                       00560   
         LH    R2,=Y(DELENGTH)                                          00561   
         STH   R2,LENGTH                                                00562   
         EXEC CICS  GETMAIN  SET(R8)  LENGTH(LENGTH)  INITIMG(X'00')    00563   
*                                                                       00564   
         EXEC CICS  READ  DATASET('ELPDE')   INTO(DEDATELM)            X00565   
                   RIDFLD(ELIOKEY)    LENGTH(ELIORL)                    00566   
         BAL   R2,PRCIAALT       MOVE THE ALTERNATE KEYS TO CIA         00567   
*                                                                       00568   
*                                                                       00569   
         EXEC CICS  FREEMAIN  DATA(DEDATELM)                            00570   
         L     R8,ELIORECA                                              00571   
*                                                                       00572   
OKDEL    DS    0H                                                       00573   
*                                                                       00574   
         EXEC CICS  DELETE   RIDFLD(ELIOKEY)  DATASET(ELIODDNM)         00575   
*                                                                       00576   
*                                                                       00577   
*   SUCCESSFUL DELETE,  CHECK IF THIS WAS A DATA ELEMENT RECORD DELETE  00578   
*   IF YES WE MUST DELETE BOTH THE ENGLISH NAME & SYSTEM NAME           00579   
*   INDEX ENTRIES.                                                      00580   
         CLC   ELIODDNM,=C'ELPDE   '                                    00581   
         BNE   RETURN                                                   00582   
*                                                                       00583   
         BAL   R2,ENGNMDEL       REMOVE ENG NAME ALT INDEX ENTRY        00584   
         CLI   CIASYSNM,C' '         NEW SYSTEM NAME ALL BLANKS         00585   
         BNE   DESYSCH3                 NO, DELETE ALTERNATE INDEX      00586   
         CLC   CIASYSNM+1(L'CIASYSNM-1),CIASYSNM                        00587   
         BE    RETURN                                                   00588   
DESYSCH3 CLI   CIASYSNM,X'00'        NEW SYSTEM NAME ALL LOW-VALUES     00589   
         BNE   DESYSCH4                 NO, DELETE ALTERNATE INDEX      00590   
         CLC   CIASYSNM+1(L'CIASYSNM-1),CIASYSNM                        00591   
         BE    RETURN                                                   00592   
DESYSCH4 BAL   R2,SYSNMDEL       REMOVE SYSTEM NAME ALT INDEX ENTRY     00593   
         B     RETURN                                                   00594   
         EJECT                                                          00595   
**********************************************************************  00596   
*      G E N E R I C   D E L E T E   (DELETE ALL RECORDS WITH THE       00597   
*                                     SAME GENERIC KEY)                 00598   
**********************************************************************  00599   
*                                                                       00600   
MASSDELT DS    0H                                                       00601   
         MVC   ELIOBKEY,ELIOKEY       SAVE THE KEY VALUE DELETED        00602   
         CLC   ELIODDNM,ELELPCV       FOR CODE VALUE FILE               00603   
         BE    OKMASDEL                                                 00604   
         CLC   ELIODDNM,ELELPRL       FOR RECORD LIST FILE              00605   
         BE    OKMASDEL                                                 00606   
         CLC   ELIODDNM,ELPAGEFL      FOR OUTPUT PAGE FILE              00607   
         BE    OKMASDEL                                                 00608   
         B     INVREQ                                                   00609   
*                                                                       00610   
*                                                                       00611   
OKMASDEL DS    0H                                                       00612   
*                                                                       00613   
         EXEC CICS  DELETE  GENERIC    DATASET(ELIODDNM)               X00614   
                  RIDFLD(ELIOBKEY)  KEYLENGTH(ELIOBKL)                  00615   
*                                                                       00616   
*                                                                       00617   
         B     RETURN                                                   00618   
         EJECT                                                          00619   
**********************************************************************  00620   
*     S T A R T   B R O W S E   O N   S P E C I F I E D   F I L E       00621   
*      1. START BROWSE EQUAL TO KEY                                     00622   
*      2. START BROWSE GREATER THAN OR EQUAL TO KEY                     00623   
* THE SAME CODE IS EXECUTED FOR START BROWSE & START BROWSE PREVIOUS    00624   
*                                                                       00625   
**********************************************************************  00626   
*                                                                       00627   
STARTBR  DS    0H                                                       00628   
         MVC   ELIOBKEY,ELIOKEY    MOVE KEY TO BROWSE WORK AREA         00629   
         CLC   ELIOALDD,ELELPCN    IF BROWSE USING ALTERNATE INDEX      00630   
         BNE   STBRSYNM            THEN                                 00631   
         BAL   R2,BRSSYSNM            PERFORM SYSTEM NAME BROWSE        00632   
         B     READNOAL               THEN READ THE RECORD REQUESTED    00633   
STBRSYNM CLC   ELIOALDD,ELELPEN    IF BROWSE WITH OTHER ALTERNATE INDEX 00634   
         BNE   STBRQUAL            THEN                                 00635   
         BAL   R2,BRSENGNM            PERFORM ENGLISH NAME BROWSE       00636   
         B     READNOAL               THEN READ THE RECORD REQUESTED    00637   
*                                  START BROWSE WITHOUT USING ALTERNATE 00638   
*                                  INDEX FILE.                          00639   
STBRQUAL CLC   ELIOQUAL,=C'EQ '    IF  QUALIFIER = EQUAL                00640   
         BE    STARTBR2                START BROWSE EQUAL               00641   
*                                  ELSE                                 00642   
         CLC   ELIOQUAL,=C'GTE'        IF QUALIFIER = EQUAL/GREATER     00643   
         BE    STARTBR1                   START BROWSE GREATER OR EQUAL 00644   
*                                  ELSE                                 00645   
         B     INVREQ                     THIS IS AN INVALID REQUEST    00646   
*                                                                       00647   
STARTBR1 DS    0H                      START BROWSE GREATER OR EQUAL.   00648   
*                                                                       00649   
         EXEC CICS  STARTBR   DATASET(ELIODDNM)  GTEQ                  X00650   
                   RIDFLD(ELIOBKEY)  REQID(ELIOBRID)                    00651   
*                                                                       00652   
         B     STARTEXT            EXIT ROUTINE                         00653   
*                                                                       00654   
STARTBR2 DS    0H                                                       00655   
*                                                                       00656   
         EXEC CICS  STARTBR   DATASET(ELIODDNM)   EQUAL                X00657   
                   RIDFLD(ELIOBKEY)  REQID(ELIOBRID)                    00658   
*                                                                       00659   
*                                                                       00660   
STARTEXT DS    0H                  START BROWSE FINIS                   00661   
         MVC   ELIOQUAL,=C'   '        RESET QUALIFIER                  00662   
         CLC   ELIOACES,=C'SB '    THEN READ NEXT?                      00663   
         BE    READNEXT               GO READ THE NEXT RECORD           00664   
         B     READPREV            ELSE GO READ THE PREVIOUS RECORD     00665   
*                                                                       00666   
         EJECT                                                          00667   
**********************************************************************  00668   
*          S T A R T   G E N E R I C   B R O W S E                      00669   
*             O N   S P E C I F I E D   F I L E                         00670   
*      1. START GENERIC BROWSE EQUAL TO KEY                             00671   
*      2. START GENERIC BROWSE GREATER THAN OR EQUAL TO KEY             00672   
*                                                                       00673   
**********************************************************************  00674   
*                                                                       00675   
GENBR    DS    0H                                                       00676   
         MVC   ELIOBKEY,ELIOKEY    MOVE KEY TO BROWSE WORK AREA         00677   
         CLC   ELIOALDD,ELELPCN                                         00678   
         BNE   GNBRSYNM                                                 00679   
         BAL   R2,BRSSYSNM                                              00680   
         B     READNOAL                                                 00681   
GNBRSYNM CLC   ELIOALDD,ELELPEN                                         00682   
         BNE   GNBRQUAL                                                 00683   
         BAL   R2,BRSENGNM                                              00684   
         B     READNOAL                                                 00685   
GNBRQUAL CLC   ELIOQUAL,=C'EQ '    IF  QUALIFIER = EQUAL                00686   
         BE    GENBR2              THEN                    T.R. 9/25/84 00687   
         CLC   ELIOQUAL,=C'GTE'        START BROWSE EQUAL               00688   
         BE    GENBR1              ELSE                    T.R. 9/25/84 00689   
*                                                                       00690   
GENBR1   DS    0H                      START BROWSE GREATER OR EQUAL.   00691   
*                                                                       00692   
         EXEC CICS  STARTBR   DATASET(ELIODDNM)   GENERIC  GTEQ        X00693   
                 KEYLENGTH(ELIOBKL)  RIDFLD(ELIOBKEY)  REQID(ELIOBRID)  00694   
*                                                                       00695   
         B     GENBREXT            EXIT ROUTINE.                        00696   
*                                                                       00697   
GENBR2   DS    0H                                                       00698   
*                                                                       00699   
         EXEC CICS  STARTBR   DATASET(ELIODDNM)   GENERIC  EQUAL       X00700   
                 KEYLENGTH(ELIOBKL)  RIDFLD(ELIOBKEY)  REQID(ELIOBRID)  00701   
*                                                                       00702   
GENBREXT DS    0H                      WHAT TYPE OF BROWSE REQUEST      00703   
         MVC   ELIOQUAL,=C'   '        RESET QUALIFIER                  00704   
         CLC   ELIOACES,=C'GB '    THEN READ NEXT?                      00705   
         BE    READNEXT               GO READ THE NEXT RECORD           00706   
         B     READPREV            ELSE GO READ THE PREVIOUS REC        00707   
*                                                                       00708   
         EJECT                                                          00709   
**********************************************************************  00710   
*         R E A D   T H E   N E X T   R E C O R D                       00711   
*        O N   T H E   S P E C I F I E D   F I L E                      00712   
**********************************************************************  00713   
*                                                                       00714   
READNEXT DS    0H                                                       00715   
*                                                                       00716   
*    MOVE THE MAX RECORD LENGTH TO THE ELIORL FIELD SO IT               00717   
*    WILL BE UPDATED FROM THE READ NEXT.                                00718   
         MVC   ELIORL,ELIOMXRL                                          00719   
*                                                                       00720   
         EXEC CICS  READNEXT   DATASET(ELIODDNM)  INTO(DEDATELM)       X00721   
                   LENGTH(ELIORL)  RIDFLD(ELIOBKEY)  REQID(ELIOBRID)    00722   
*                                                                       00723   
*   NO MATTER IF A 'GB' 'SB' OR 'RN' ACCESS GOT US HERE                 00724   
*   SET THE ACCESS CODE TO 'RN' FOR THE CALLER.                         00725   
         MVC   ELIOACES,=C'RN '                                         00726   
*                                                                       00727   
         B     RETURN                  RETURN TO CALLER                 00728   
         EJECT                                                          00729   
**********************************************************************  00730   
*     R E A D   T HE   P R E V I O U S   R E C O R D                    00731   
*        O N   T H E   S P E C I F I E D   F I L E                      00732   
**********************************************************************  00733   
*                                                                       00734   
READPREV DS    0H                                                       00735   
*                                                                       00736   
*    MOVE THE MAX RECORD LENGTH TO THE ELIORL FIELD SO IT               00737   
*    WILL BE UPDATED FROM THE READ NEXT.                                00738   
         MVC   ELIORL,ELIOMXRL                                          00739   
*                                                                       00740   
         EXEC CICS  READPREV   DATASET(ELIODDNM)  INTO(DEDATELM)       X00741   
                   LENGTH(ELIORL)  RIDFLD(ELIOBKEY)  REQID(ELIOBRID)    00742   
*                                                                       00743   
*   NO MATTER IF A 'GBP' 'SBP' OR 'RP' ACCESS GOT US HERE               00744   
*   SET THE ACCESS CODE TO 'RP' FOR THE CALLER.                         00745   
         MVC   ELIOACES,=C'RP '                                         00746   
*                                                                       00747   
         B     RETURN                  RETURN TO CALLER                 00748   
         EJECT                                                          00749   
**********************************************************************  00750   
*              E N D   B R O W S E                                      00751   
**********************************************************************  00752   
*                                                                       00753   
ENDBR    DS    0H                                                       00754   
*                                                                       00755   
         EXEC CICS  ENDBR  DATASET(ELIODDNM)  REQID(ELIOBRID)           00756   
*                                                                       00757   
         B     RETURN                                                   00758   
         EJECT                                                          00759   
**********************************************************************  00760   
*                 U N L O C K                                           00761   
**********************************************************************  00762   
*                                                                       00763   
UNLOCK   DS    0H                                                       00764   
*                                                                       00765   
         EXEC CICS  UNLOCK  DATASET(ELIODDNM)                           00766   
*                                                                       00767   
         CLC   ELIODDNM,=CL8'ELPDE'                                     00768   
         BNE   RETURN                                                   00769   
         BAL   R2,CLCIAALT               CLEAR CIA OF ENG & SYS KEYS    00770   
         B     RETURN                                                   00771   
         EJECT                                                          00772   
**********************************************************************  00773   
*                                                                       00774   
*           W R I T E  R E C O R D  (ADD NEW RECORD)                    00775   
*                                                                       00776   
*       THIS ROUTINE MUST REMAIN THE LAST I/O ROUTINE IN THIS           00777   
*       MODULE DUE TO THE HANDLE CONDITION IT CONTAINS.                 00778   
*                                                                       00779   
**********************************************************************  00780   
*                                                                       00781   
WRITE    DS    0H                                                       00782   
*                                                                       00783   
*        BEFORE ATTEMPTING TO ADD A RECORD:                             00784   
*        ATTEMPT TO READ THE RECORD FOR UPDATE                          00785   
*             A) IF THE READ IS SUCCESSFUL, THIS REPRESENTS             00786   
*                A DUPLICATE KEY SITUATION WHICH IS ONLY VALID          00787   
*                WITH A SPECIFIC REQUEST TYPE....                       00788   
*             B) IF THE READ IS NOT SUCCESSFUL, CONTINUE.               00789   
*                                                                       00790   
*                                                                       00791   
*    MOVE THE MAX RECORD LENGTH TO THE RECORD LENGTH2 SO IT             00792   
*    WILL BE UPDATED FROM THE READ.                                     00793   
         MVC   ELIORL2,ELIOMXRL                                         00794   
*                                                                       00795   
         EXEC CICS  GETMAIN  SET(R8)  LENGTH(ELIORL2)  INITIMG(X'00')   00796   
*                                                                       00797   
         ST    R8,ELIOADUP                                              00798   
*                                                                       00799   
         EXEC CICS  HANDLE  CONDITION   NOTFND(WRITEADD)                00800   
*                                                                       00801   
         EXEC CICS  READ  DATASET(ELIODDNM)  UPDATE  INTO(ELPDED)      X00802   
                   LENGTH(ELIORL2)  RIDFLD(ELIOKEY)                     00803   
*                                                                       00804   
         CLC   ELIOACES,=C'WR '    WRITE(ADD NEW RECORD) REQUEST???     00805   
         BE    INVREQ1              DUPREC EXISTS!!!!!!!!!!             00806   
         CLC   ELIOACES,=C'WDP'    WRITE(ADD NEW RECORD) REQUEST???     00807   
         BNE   INVREQ               DUPREC CONDITION EXPECTED!!!!!      00808   
         MVC   ELIORC,=C'12'           SET DUPREC RETURN CODE           00809   
*                                                                       00810   
*   THIS WAS A WDP REQUEST.   IF A DATA ELEMENT ADD THEN PRIME THE      00811   
*   CIA WITH THE ENGLISH AND SYSTEM NAME KEYS.                          00812   
*                                                                       00813   
         CLC   ELIODDNM,=C'ELPDE   '                                    00814   
         BNE   RETURN                                                   00815   
         BAL   R2,PRCIAALT                                              00816   
         B     RETURN                                                   00817   
*                                                                       00818   
WRITEADD DS    0H                                                       00819   
*  SOME OF OUR FILES CONTAIN VARIABLE LENGTH RECORDS.  FOR THOSE FILES  00820   
*  WE MUST CALCULATE THE RECORD LENGTH TO BE USED FOR THE REWRITE.      00821   
*                                                                       00822   
         BAL   R2,VARRECCK                                              00823   
*                                                                       00824   
         L     R8,ELIOADUP                                              00825   
*                                                                       00826   
         EXEC CICS  FREEMAIN  DATA(0(R8))                               00827   
*                                                                       00828   
*   CLEAR THE DUP RECORD ADDRESS AND GETMAIN FROM IOPARM AREA.          00829   
         XC    ELIOADUP,ELIOADUP                                        00830   
         XC    ELIORL2,ELIORL2                                          00831   
         L     R8,ELIORECA                                              00832   
*                                                                       00833   
         EXEC CICS  WRITE   DATASET(ELIODDNM)  FROM(ELPDED)            X00834   
                   LENGTH(ELIORL)  RIDFLD(ELIOKEY)                      00835   
*                                                                       00836   
*    THE RECORD HAS BEEN SUCCESSFULLY ADDED TO THE FILE,  IF IT IS      00837   
*    A DATA ELEMENT ADD THEN WE MUST ADD ENTRIES TO THE ENGLISH AND     00838   
*    SYSTEM NAME ALTERNATE INDICES.                                     00839   
*                                                                       00840   
         CLC   ELIODDNM,=CL8'ELPDE'                                     00841   
         BNE   RETURN                                                   00842   
         BAL   R2,ENGNMADD             ADD THE ENG NAME ALT ENTRY       00843   
*                                                                       00844   
         L     R8,ELIORECA                                              00845   
         CLI   DECOBLNM,C' '         NEW SYSTEM NAME ALL BLANKS         00846   
         BNE   WRSYSCH1                 NO, GO ADD NEW ALTERNATE INDEX  00847   
         CLC   DECOBLNM+1(L'DECOBLNM-1),DECOBLNM                        00848   
         BE    RETURN                   YES, DON'T ADD THE INDEX        00849   
WRSYSCH1 CLI   DECOBLNM,X'00'        NEW SYSTEM NAME ALL LOW-VALUES     00850   
         BNE   WRSYSCH2                 NO, GO ADD NEW ALTERNATE INDEX  00851   
         CLC   DECOBLNM+1(L'DECOBLNM-1),DECOBLNM                        00852   
         BE    RETURN                   YES, DON'T ADD THE INDEX        00853   
WRSYSCH2 BAL   R2,SYSNMADD             ADD THE SYS NAME ALT ENTRY.      00854   
*                                                                       00855   
         B     RETURN                                                   00856   
         EJECT                                                          00857   
**********************************************************************  00858   
*          A L T E R N A T E   I N D E X   P R O C E S S I N G          00859   
*        SUB ROUTINES FOR DATA ELEMENT ALTERNATE INDEX PROCESSING       00860   
**********************************************************************  00861   
*                                                                       00862   
**********************************************************************  00863   
*      PRIME THE CIA FIELDS WITH ENGLISH & SYSTEM ALTERNATE KEY FIELDS  00864   
**********************************************************************  00865   
PRCIAALT DS    0H                                                       00866   
*                                                                       00867   
         MVC   CIARECEN,DERECPRF        MOVE IN THE RECORD ID           00868   
         MVC   CIAENGNM,DEELMTNM        MOVE IN THE ENGLISH NAME        00869   
         MVC   CIARECSY,DERECPRF        MOVE IN THE RECORD ID           00870   
         MVC   CIASYSNM,DECOBLNM        MOVE IN THE SYSTEM NAME         00871   
*                                                                       00872   
         BR    R2                                                       00873   
*                                                                       00874   
**********************************************************************  00875   
*        CLEAR THE CIA FIELDS OF ENGLISH AND SYSTEM NAME KEYS           00876   
**********************************************************************  00877   
CLCIAALT DS    0H                                                       00878   
*                                                                       00879   
         MVI   CIAENNMK,X'00'                                           00880   
         MVC   CIAENNMK+1(L'CIAENNMK-1),CIAENNMK                        00881   
         MVC   CIASYNMK,CIAENNMK                                        00882   
         BR    R2                                                       00883   
*                                                                       00884   
         EJECT                                                          00885   
**********************************************************************  00886   
*         R E M O V E   A N   E N G L I S H   N A M E                   00887   
*         A L T E R N A T E   I N D E X   R E C O R D.                  00888   
*  (THIS IS DONE BECAUSE THEY HAVE DELETED THE CORRESPONDING DATA       00889   
*   ELEMENT RECORD FROM THE FILE, OR THEY HAVE UPDATED THE DATA         00890   
*   ELEMENT RECORD CHANGING THE ENGLISH NAME KEY.)                      00891   
**********************************************************************  00892   
SA2ENGDL DS    F                                                        00893   
ENGNMDEL DS    0H                                                       00894   
         ST    R2,SA2ENGDL                                              00895   
*                                                                       00896   
         EXEC CICS  HANDLE CONDITION   NOTFND(MISENGA)                  00897   
*                                                                       00898   
         EXEC CICS  DELETE   DATASET('ELPEN') RIDFLD(CIAENNMK)          00899   
*                                                                       00900   
         L     R2,SA2ENGDL                                              00901   
         BR    R2                                                       00902   
         EJECT                                                          00903   
*********************************************************************** 00904   
**          R E A D   T H E   E N G L I S H   N A M E                   00905   
**               A L T E R N A T E   I N D E X                          00906   
**  (READ THE ALTERNATE INDEX AND BUILD THE KEY FOR DATA ELEMENT FILE.) 00907   
*********************************************************************** 00908   
SA2RENG  DS    A(0)                REG SAVEAREA                         00909   
SA9RENG  DS    A(0)                REG SAVEAREA                         00910   
REDENGNM DS    0H                                                       00911   
         ST    R2,SA2RENG                                               00912   
         ST    R9,SA9RENG                                               00913   
         LH    R9,=Y(ENLENGTH)                                          00914   
         STH   R9,LENGTH                                                00915   
*                                                                       00916   
         EXEC CICS  GETMAIN   SET(R9)  LENGTH(LENGTH)  INITIMG(X'00')   00917   
*                                                                       00918   
         EXEC CICS  READ    DATASET('ELPEN')    INTO(ENENGLNM)         X00919   
                   LENGTH(LENGTH)    RIDFLD(ELIOKEY)                    00920   
*                                                                       00921   
         MVC   CIADDN,=CL8'ELPDE'     RESET DDNAME                      00922   
         MVC   DEMRECID,ENRECPRF      RECORD ID                         00923   
         ZAP   DEMELTNO,ENELMTNO      ELEMENT NUMBER                    00924   
         MVI   ELIOKEY,X'00'          INITIALIZE THE KEY AREA           00925   
         MVC   ELIOKEY+1(254),ELIOKEY  INITIALIZE THE KEY AREA          00926   
         MVC   ELIOALDD,ELIOKEY       RESET DDNAME                      00927   
         MVC   ELIOKEY(11),DTELNAME   MOVE THE DATA ELEMENT KEY         00928   
         MVC   ELIORL,ELIOMXRL                                          00929   
         MVC   ELIOQUAL,=CL3' '       RESET QUALIFIER                   00930   
         MVI   ELIOBKL,X'00'          RESET BROWSE KEY LENGTH           00931   
         MVI   ELIOBKL+1,X'00'        RESET BROWSE KEY LENGTH           00932   
         CLC   ELIOACES,=C'RU '       READ FOR UPDATE                   00933   
         BNE   REDENGFM               GO DO THE FREEMAIN                00934   
         MVC   CIARECEN,ENRECPRF      SAVE RECORD ID IN CIA             00935   
         MVC   CIAENGNM,ENELMTNM      SAVE ELEMENT NAME IN CIA          00936   
*                                                                       00937   
REDENGFM DS    0H                                                       00938   
         EXEC CICS  FREEMAIN   DATA(ENENGLNM)                           00939   
*                                                                       00940   
         EXEC CICS  HANDLE CONDITION   NOTFND(ALTNOFND)                 00941   
*                                                                       00942   
         L     R2,SA2RENG                                               00943   
         L     R9,SA9RENG                                               00944   
         BR    R2                                                       00945   
         EJECT                                                          00946   
*********************************************************************** 00947   
**      B R O W S E   T H E   E N G L I S H   N A M E                   00948   
**             A L T E R N A T E   I N D E X                            00949   
**  THIS PROGRAM WILL SETUP FOUR TYPE OF START BROWSE OPERATIONS        00950   
**      1. IF THE CALLING PGM KNOWS THE KEY & HAS SPECIFIED EQUAL.      00951   
**      2. IF THE CALLING PGM DOES NOT KNOW THE SPECIFIC KEY (HERE      00952   
**         THE CALLING PGM COULD HAVE REQUESTED LT, LTE, GT, OR GTE).   00953   
**      3. IF THE CALLING PGM KNOWS PART OF THE KEY AND SO HAS          00954   
**         SPECIFIED A GENERIC START BROWSE WITH AN EQUAL CONDITION.    00955   
**      4. THE CALLING PGM HAS A GENERAL UNDERSTANDING OF THE KEY SUCH  00956   
**         AS THE KEY IS LT, LTE, GT, OR GTE A VALUE WHICH OF ITSELF    00957   
**         IS NOT THE ENTIRE KEY SO A GENERIC OPERATION IS PERFORMED.   00958   
**  THEN BASED ON THE DIRECTION OF SEARCH IE. PREVIOUS OR SUBSEQUENT    00959   
**  READ UNTIL THE CONDITIONS HOLD TRUE.                                00960   
**      1. WE MAY READ PREVIOUS UNTIL THE RECORD HAS A KEY WHICH        00961   
**         IS LESS THAN OR EQUAL TO, OR LESS THAN THE VALUE SPECIFIED.  00962   
**      2. WE MAY READ SUBSEQUENT RECORDS UNTIL THE RECORD HAS A KEY    00963   
**         WHICH IS GREATER THAN OR EQUAL TO, OR GREATER THAN THE       00964   
**         VALUE SPECIFIED.                                             00965   
*********************************************************************** 00966   
SA2BENG  DS    A(0)                REG SAVEAREA                         00967   
SA9BENG  DS    A(0)                REG SAVEAREA                         00968   
BRSENGNM DS    0H                                                       00969   
         ST    R2,SA2BENG                                               00970   
         ST    R9,SA9BENG                                               00971   
         LH    R9,=Y(ENLENGTH)                                          00972   
         STH   R9,LENGTH                                                00973   
*                                                                       00974   
         EXEC CICS  GETMAIN   SET(R9)  LENGTH(LENGTH)  INITIMG(X'00')   00975   
*                                                                       00976   
         CLI   ELIOACES,C'G'       IS IT A GENERIC BROWSE TYPE          00977   
         BE    BGNENGNM                                                 00978   
         CLC   ELIOQUAL,=C'EQ '    IF  QUALIFIER = EQUAL                00979   
         BE    BR1ENGNM                                                 00980   
*                                      START BROWSE GREATER OR EQUAL.   00981   
*                                                                       00982   
         EXEC CICS  STARTBR  DATASET('ELPEN')   GTEQ  RIDFLD(ELIOBKEY) X00983   
                   REQID(ELIOBRID)                                      00984   
*                                                                       00985   
         B     BEXENGNM            EXIT ROUTINE                         00986   
*                                                                       00987   
BR1ENGNM DS    0H                                                       00988   
*                                                                       00989   
         EXEC CICS  STARTBR  DATASET('ELPEN')  EQUAL  RIDFLD(ELIOBKEY) X00990   
                   REQID(ELIOBRID)                                      00991   
*                                                                       00992   
         B     BEXENGNM            EXIT ROUTINE                         00993   
*                                                                       00994   
BGNENGNM DS    0H                                                       00995   
         CLC   ELIOQUAL,=C'EQ '    IF  QUALIFIER = EQUAL                00996   
         BE    BR2ENGNM                                                 00997   
*                                      START BROWSE GREATER OR EQUAL.   00998   
*                                                                       00999   
         EXEC CICS  STARTBR  DATASET('ELPEN')   GENERIC   GTEQ         X01000   
                  KEYLENGTH(ELIOBKL)  REQID(ELIOBRID)  RIDFLD(ELIOBKEY) 01001   
*                                                                       01002   
         B     BEXENGNM            EXIT ROUTINE                         01003   
*                                                                       01004   
BR2ENGNM DS    0H                                                       01005   
*                                                                       01006   
         EXEC CICS  STARTBR  DATASET('ELPEN')    GENERIC   EQUAL       X01007   
                  KEYLENGTH(ELIOBKL)  REQID(ELIOBRID)  RIDFLD(ELIOBKEY) 01008   
*                                                                       01009   
BEXENGNM CLI   ELIOACES+2,C'P'         WHAT TYPE OF BROWSE REQUEST      01010   
         BE    BPRENGNM                IF READPREV GOTO BPRENGNM        01011   
*                                                                       01012   
         EXEC CICS  READNEXT    DATASET('ELPEN')    INTO(ENENGLNM)     X01013   
                   LENGTH(LENGTH)   RIDFLD(ELIOBKEY)   REQID(ELIOBRID)  01014   
*                                                                       01015   
         CLC   ELIOQUAL,=CL3'GT '      WHAT TYPE OF BROWSE REQUEST      01016   
         BNE   BNXGTENG                                                 01017   
         CLC   ENKEYNAM,ELIOKEY        DOES THIS REC HAVE 'GT' KEY      01018   
         BH    BENGPRKY                                                 01019   
         B     BEXENGNM                                                 01020   
BNXGTENG CLC   ELIOQUAL,=CL3'GTE'      WHAT TYPE OF BROWSE REQUEST      01021   
         BNE   BNXEQENG                                                 01022   
         CLC   ENKEYNAM,ELIOKEY        DOES THIS REC HAVE 'GTE' KEY     01023   
         BNL   BENGPRKY                                                 01024   
         B     BEXENGNM                                                 01025   
BNXEQENG CLC   ELIOQUAL,=CL3'EQ '      WHAT TYPE OF BROWSE REQUEST      01026   
         BNE   INVREQ                                                   01027   
         CLC   ENKEYNAM,ELIOKEY        DOES THIS REC HAVE 'EQ' KEY      01028   
         BE    BENGPRKY                                                 01029   
         B     BEXENGNM                                                 01030   
BPRENGNM DS    0H                                                       01031   
         EXEC CICS  READPREV    DATASET('ELPEN')    INTO(ENENGLNM)     X01032   
                   LENGTH(LENGTH)   RIDFLD(ELIOBKEY)   REQID(ELIOBRID)  01033   
*                                                                       01034   
         CLC   ELIOQUAL,=CL3'LT '      WHAT TYPE OF BROWSE REQUEST      01035   
         BNE   BPRLTENG                                                 01036   
         CLC   ENKEYNAM,ELIOKEY        DOES THIS REC HAVE 'LT' KEY      01037   
         BL    BENGPRKY                                                 01038   
         B     BPRENGNM                                                 01039   
BPRLTENG CLC   ELIOQUAL,=CL3'LTE'      WHAT TYPE OF BROWSE REQUEST      01040   
         BNE   BPREQENG                                                 01041   
         CLC   ENKEYNAM,ELIOKEY        DOES THIS REC HAVE 'LTE' KEY     01042   
         BNH   BENGPRKY                                                 01043   
         B     BPRENGNM                                                 01044   
BPREQENG CLC   ELIOQUAL,=CL3'EQ '      WHAT TYPE OF BROWSE REQUEST      01045   
         BNE   INVREQ                                                   01046   
         CLC   ENKEYNAM,ELIOKEY        DOES THIS REC HAVE 'EQ' KEY      01047   
         BE    BENGPRKY                                                 01048   
         B     BPRENGNM                                                 01049   
BENGPRKY MVC   CIADDN,=CL8'ELPDE'    RESET DDNAME                       01050   
         MVC   DEMRECID,ENRECPRF      RECORD ID                         01051   
         ZAP   DEMELTNO,ENELMTNO      ELEMENT NUMBER                    01052   
         MVI   ELIOALDD,X'00'          RESET THE ALTERNATE KEY DDNAME   01053   
         MVC   ELIOALDD+1(7),ELIOALDD  RESET THE ALTERNATE KEY DDNAME   01054   
         MVI   ELIOKEY,X'00'          INITIALIZE THE KEY AREA           01055   
         MVC   ELIOKEY+1(254),ELIOKEY  INITIALIZE THE KEY AREA          01056   
         MVC   ELIOKEY(L'DEPRIKEY),DTELNAME   MOVE DATA ELEMENT KEY     01057   
         MVC   ELIORL,ELIOMXRL                                          01058   
         MVI   ELIOBKL,X'00'          RESET THE BROWSE KEY LENGTH       01059   
         MVI   ELIOBKL+1,X'00'        RESET THE BROWSE KEY LENGTH       01060   
         MVC   ELIOQUAL,=C'   '        RESET QUALIFIER                  01061   
*                                                                       01062   
         EXEC CICS  ENDBR   DATASET('ELPEN')   REQID(ELIOBRID)          01063   
*                                                                       01064   
         EXEC CICS  FREEMAIN   DATA(ENENGLNM)                           01065   
*                                                                       01066   
         EXEC CICS  HANDLE CONDITION   NOTFND(ALTNOFND)                 01067   
*                                                                       01068   
         L     R2,SA2BENG                                               01069   
         L     R9,SA9BENG                                               01070   
         BR    R2                                                       01071   
         EJECT                                                          01072   
**********************************************************************  01073   
*            A D D   A N   E N G L I S H   N A M E                      01074   
*         A L T E R N A T E   I N D E X   R E C O R D                   01075   
**********************************************************************  01076   
SA2ENGL  DS    A(0)                SAVEAREA FOR ENGLISH NAME ADD RTNE   01077   
SA9ENGL  DS    A(0)                SAVEAREA FOR ENGLISH NAME ADD RTNE   01078   
ENGNMADD DS    0H                                                       01079   
         ST    R2,SA2ENGL                                               01080   
         ST    R9,SA9ENGL                                               01081   
*                                                                       01082   
*     GETMAIN FOR A SLIGHTLY LARGER AREA THAN THE RECORD                01083   
         LH    R9,=Y(ENLENGTH)                                          01084   
         STH   R9,LENGTH                                                01085   
*                                                                       01086   
         EXEC CICS  GETMAIN  SET(R9)  LENGTH(LENGTH)  INITIMG(X'00')    01087   
*                                                                       01088   
         ST    R9,ALTGTMN          SAVE ALT/INDEX GETMAIN AREA          01089   
*                                                                       01090   
         L     R8,ELIORECA         ADDRESS OF UPDATED DATA ELEMENT      01091   
         MVC   ENMRECID,DERECPRF    MOVE RECORD ID INTO ENGL NAME KEY   01092   
         MVC   ENMENAME,DEELMTNM    MOVE ELEMENT NAME INTO ENGL NAME \
*                                                                       01094   
         EXEC CICS  HANDLE CONDITION   NOTFND(CRENGALT)                 01095   
*                                                                       01096   
         EXEC CICS  READ   DATASET('ELPEN')  INTO(ENENGLNM)            X01097   
                   LENGTH(LENGTH)  RIDFLD(ENGLNAME)                     01098   
*                                                                       01099   
         B     DUPALTID                                                 01100   
*                                                                       01101   
CRENGALT DS    0H                                                       01102   
*   NOT FOUND CONDITION ON READ OF ENGLISH NAME ALTERNATE INDEX FILE    01103   
*   CREATE A NEW RECORD AND ADD TO THE FILE                             01104   
*                                                                       01105   
         MVC   ENMRECID,DERECPRF      RECORD ID                         01106   
         MVC   ENMENAME,DEELMTNM      ENGLISH NAME                      01107   
         MVC   ENRECPRF,DERECPRF      RECORD ID TO ENGLISH NAME FILE    01108   
         MVC   ENELMTNM,DEELMTNM      ENGLISH NAME        \
         ZAP   ENELMTNO,DEELMTNO      ELEMENT NUMBER        \
         MVI   ENDELFLG,C' '          INITIALIZE DELETE FLAG  \
         LH   R2,=Y(ENLENGTH)                                           01112   
         STH  R2,LENGTH                                                 01113   
*                                                                       01114   
         EXEC CICS  WRITE   DATASET('ELPEN')  FROM(ENENGLNM)           X01115   
                   LENGTH(LENGTH)  RIDFLD(ENGLNAME)                     01116   
*                                                                       01117   
         EXEC CICS  FREEMAIN   DATA(ENENGLNM)                           01118   
*                                                                       01119   
*                                                                       01120   
         L     R9,SA9ENGL                                               01121   
         L     R2,SA2ENGL                                               01122   
         BR    R2                                                       01123   
         EJECT                                                          01124   
**********************************************************************  01125   
*          R E M O V E   A   S Y S T E M   N A M E                      01126   
*        A L T E R N A T E  I N D E X   R E C O R D                     01127   
*  (THIS IS DONE BECAUSE A DATA ELEMENT RECORD HAS BEEN DELETED.)       01128   
*   OR THEY HAVE UPDATED THE DATA ELEMENT RECORD CHANGING THE KEY.)     01129   
**********************************************************************  01130   
SA2DSYS  DS    A(0)                SAVEAREA FOR READ ELPDE VIA SYS NAME 01131   
SYSNMDEL DS    0H                                                       01132   
         ST    R2,SA2DSYS                                               01133   
*                                                                       01134   
         EXEC CICS  HANDLE CONDITION   NOTFND(MISSYNA)                  01135   
*                                                                       01136   
*       DELETE THE ENGLISH NAME ALTERNATE INDEX FROM THE FILE.          01137   
         EXEC CICS  DELETE   DATASET('ELPCN')  RIDFLD(CIASYNMK)         01138   
*                                                                       01139   
         L     R2,SA2DSYS                                               01140   
         BR    R2                                                       01141   
         EJECT                                                          01142   
**********************************************************************  01143   
**           R E A D   T H E   S Y S T E M   N A M E                    01144   
**               A L T E R N A T E   I N D E X                          01145   
**  (READ THE ALTERNATE INDEX AND BUILD THE KEY FOR DATA ELEMENT FILE.) 01146   
**********************************************************************  01147   
SA2ASYS  DS    A(0)                SAVEAREA FOR READ ELPDE VIA SYS NAME 01148   
SA9ASYS  DS    A(0)                SAVEAREA FOR READ ELPDE VIA SYS NAME 01149   
SYSNMADD DS    0H                                                       01150   
*                                                                       01151   
         ST    R2,SA2ASYS                                               01152   
         ST    R9,SA9ASYS                                               01153   
*                                                                       01154   
*     GETMAIN FOR A SLIGHTLY LARGER AREA THAN THE RECORD                01155   
         LH    R9,=Y(CNLENGTH)                                          01156   
         STH   R9,LENGTH                                                01157   
*                                                                       01158   
         EXEC CICS  GETMAIN   SET(R9)  LENGTH(LENGTH)  INITIMG(X'00')   01159   
*                                                                       01160   
         ST    R9,ALTGTMN          SAVE ALT/INDEX GETMAIN AREA          01161   
*                                                                       01162   
         L     R8,ELIORECA         ADDRESSABILITY TO UPDATED REC        01163   
         MVC   SNMRECID,DERECPRF      RECORD ID                         01164   
         MVC   SNMENAME,DECOBLNM      SYSTEM NAME                       01165   
*                                                                       01166   
         EXEC CICS  HANDLE CONDITION   NOTFND(CRSYNALT)                 01167   
*                                                                       01168   
         EXEC CICS  READ  DATASET('ELPCN')  INTO(ENENGLNM)             X01169   
                   LENGTH(LENGTH)   RIDFLD(SYSTNAME)                    01170   
*                                                                       01171   
         B     DUPALTID                                                 01172   
*                                                                       01173   
CRSYNALT DS    0H     S Y S T E M   N A M E   N O T   F O U N D ,       01174   
*   C R E A T E   A   N E W   R E C O R D   A N D   A D D   I T         01175   
*                                                                       01176   
         MVC  CNKEYNAM,SYSTNAME                                         01177   
         ZAP  CNELMTNO,DEELMTNO                                         01178   
         MVI  CNDELFLG,C' '                                             01179   
*                                                                       01180   
         LH   R2,=Y(CNLENGTH)                                           01181   
         STH  R2,LENGTH                                                 01182   
*                                                                       01183   
         EXEC CICS  WRITE   DATASET('ELPCN')   FROM(CNKEYNAM)          X01184   
                   LENGTH(LENGTH)  RIDFLD(SYSTNAME)                     01185   
*                                                                       01186   
         EXEC CICS  FREEMAIN  DATA(CNKEYNAM)                            01187   
*                                                                       01188   
*                                                                       01189   
         L     R9,SA9ASYS                                               01190   
         L     R2,SA2ASYS                                               01191   
         BR    R2                                                       01192   
         EJECT                                                          01193   
*********************************************************************** 01194   
**         R E A D   T H E   S Y S T E M   N A M E                      01195   
**            A L T E R N A T E   I N D E X                             01196   
**          (AND BUILD KEY FOR DATA ELEMENT FILE.)                      01197   
*********************************************************************** 01198   
SA2RSYS  DS    A(0)                SAVEAREA FOR READ ELPDE VIA SYS NAME 01199   
SA9RSYS  DS    A(0)                SAVEAREA FOR READ ELPDE VIA SYS NAME 01200   
REDSYSNM DS    0H                                                       01201   
         ST    R2,SA2RSYS                                               01202   
         ST    R9,SA9RSYS                                               01203   
         LH    R9,=Y(CNLENGTH)                                          01204   
         STH   R9,LENGTH                                                01205   
*                                                                       01206   
         EXEC CICS  GETMAIN   SET(R9)  LENGTH(LENGTH)  INITIMG(X'00')   01207   
*                                                                       01208   
         EXEC CICS  READ    DATASET('ELPCN')     INTO(CNSYSNAM)        X01209   
                   LENGTH(LENGTH)  RIDFLD(ELIOKEY)                      01210   
*                                                                       01211   
         MVC   CIADDN,=CL8'ELPDE'    RESET DDNAME                       01212   
         MVC   DEMRECID,CNRECPRF      RECORD ID                         01213   
         ZAP   DEMELTNO,CNELMTNO      ELEMENT NUMBER                    01214   
         MVI   ELIOKEY,X'00'          INITIALIZE THE KEY AREA           01215   
         MVC   ELIOKEY+1(254),ELIOKEY  INITIALIZE THE KEY AREA          01216   
         MVC   ELIOKEY(11),DTELNAME   MOVE THE DATA ELEMENT KEY         01217   
         MVC   ELIORL,ELIOMXRL                                          01218   
         MVI   ELIOALDD,X'00'          RESET THE ALTERNATE KEY DDNAME   01219   
         MVC   ELIOALDD+1(7),ELIOALDD  RESET THE ALTERNATE KEY DDNAME   01220   
         MVC   ELIOQUAL,=CL3' '       RESET QUALIFIER                   01221   
         MVI   ELIOBKL,X'00'          RESET BROWSE KEY LENGTH           01222   
         MVI   ELIOBKL+1,X'00'        RESET BROWSE KEY LENGTH           01223   
         CLC   ELIOACES,=C'RU '       READ FOR UPDATE                   01224   
         BNE   REDSYSFM               GO DO THE FREEMAIN                01225   
         MVC   CIARECSY,CNRECPRF      SAVE RECORD ID IN CIA             01226   
         MVC   CIASYSNM,CNCOBLNM      SAVE COBOL NAME IN CIA            01227   
*                                                                       01228   
REDSYSFM DS    0H                                                       01229   
         EXEC CICS  FREEMAIN   DATA(CNSYSNAM)                           01230   
*                                                                       01231   
         EXEC CICS  HANDLE CONDITION   NOTFND(ALTNOFND)                 01232   
*                                                                       01233   
         L     R2,SA2RSYS                                               01234   
         L     R9,SA9RSYS                                               01235   
         BR    R2                                                       01236   
         EJECT                                                          01237   
*********************************************************************** 01238   
**      B R O W S E   T H E   S Y S T E M   N A M E                     01239   
**             A L T E R N A T E   I N D E X                            01240   
**  THIS PROGRAM WILL SETUP FOUR TYPE OF START BROWSE OPERATIONS        01241   
**      1. IF THE CALLING PGM KNOWS THE KEY & HAS SPECIFIED EQUAL.      01242   
**      2. IF THE CALLING PGM DOES NOT KNOW THE SPECIFIC KEY (HERE      01243   
**         THE CALLING PGM COULD HAVE REQUESTED LT, LTE, GT, OR GTE).   01244   
**      3. IF THE CALLING PGM KNOWS PART OF THE KEY AND SO HAS          01245   
**         SPECIFIED A GENERIC START BROWSE WITH AN EQUAL CONDITION.    01246   
**      4. THE CALLING PGM HAS A GENERAL UNDERSTANDING OF THE KEY SUCH  01247   
**         AS THE KEY IS LT, LTE, GT, OR GTE A VALUE WHICH OF ITSELF    01248   
**         IS NOT THE ENTIRE KEY SO A GENERIC OPERATION IS PERFORMED.   01249   
**  THEN BASED ON THE DIRECTION OF SEARCH IE. PREVIOUS OR SUBSEQUENT    01250   
**  READ UNTIL THE CONDITIONS HOLD TRUE.                                01251   
**      1. WE MAY READ PREVIOUS UNTIL THE RECORD HAS A KEY WHICH        01252   
**         IS LESS THAN OR EQUAL TO, OR LESS THAN THE VALUE SPECIFIED.  01253   
**      2. WE MAY READ SUBSEQUENT RECORDS UNTIL THE RECORD HAS A KEY    01254   
**         WHICH IS GREATER THAN OR EQUAL TO, OR GREATER THAN THE       01255   
**         VALUE SPECIFIED.                                             01256   
*********************************************************************** 01257   
SA2BSYS  DS    A(0)                REG SAVEAREA                         01258   
SA9BSYS  DS    A(0)                REG SAVEAREA                         01259   
BRSSYSNM DS    0H                                                       01260   
         ST    R2,SA2BSYS                                               01261   
         ST    R9,SA9BSYS                                               01262   
         LH    R9,=Y(CNLENGTH)                                          01263   
         STH   R9,LENGTH                                                01264   
*                                                                       01265   
         EXEC CICS  GETMAIN   SET(R9)  LENGTH(LENGTH)  INITIMG(X'00')   01266   
*                                                                       01267   
         CLI   ELIOACES,C'G'       IS IT A GENERIC BROWSE TYPE          01268   
         BE    BGNSYSNM                                                 01269   
         CLC   ELIOQUAL,=C'EQ '    IF  QUALIFIER = EQUAL                01270   
         BE    BR1SYSNM                                                 01271   
*                                      START BROWSE GREATER OR EQUAL.   01272   
*                                                                       01273   
         EXEC CICS  STARTBR  DATASET('ELPCN')   GTEQ  RIDFLD(ELIOBKEY) X01274   
                   REQID(ELIOBRID)                                      01275   
*                                                                       01276   
         B     BEXSYSNM            EXIT ROUTINE                         01277   
*                                                                       01278   
BR1SYSNM DS    0H                                                       01279   
*                                                                       01280   
         EXEC CICS  STARTBR  DATASET('ELPCN')  EQUAL  RIDFLD(ELIOBKEY) X01281   
                   REQID(ELIOBRID)                                      01282   
*                                                                       01283   
         B     BEXSYSNM            EXIT ROUTINE                         01284   
*                                                                       01285   
BGNSYSNM DS    0H                                                       01286   
         CLC   ELIOQUAL,=C'EQ '    IF  QUALIFIER = EQUAL                01287   
         BE    BR2SYSNM                                                 01288   
*                                      START BROWSE GREATER OR EQUAL.   01289   
*                                                                       01290   
         EXEC CICS  STARTBR  DATASET('ELPCN')   GENERIC  GTEQ          X01291   
                  KEYLENGTH(ELIOBKL)  REQID(ELIOBRID)  RIDFLD(ELIOBKEY) 01292   
*                                                                       01293   
         B     BEXSYSNM            EXIT ROUTINE                         01294   
*                                                                       01295   
BR2SYSNM DS    0H                                                       01296   
*                                                                       01297   
         EXEC CICS  STARTBR   DATASET('ELPCN')   GENERIC   EQUAL       X01298   
                  KEYLENGTH(ELIOBKL)  REQID(ELIOBRID)  RIDFLD(ELIOBKEY) 01299   
*                                                                       01300   
BEXSYSNM CLI   ELIOACES+2,C'P'         WHAT TYPE OF BROWSE REQUEST      01301   
         BE    BPRSYSNM                IF READPREV GOTO BPRENGNM        01302   
         EXEC CICS  READNEXT    DATASET('ELPCN')    INTO(CNSYSNAM)     X01303   
                   LENGTH(LENGTH)   RIDFLD(ELIOBKEY)   REQID(ELIOBRID)  01304   
*                                                                       01305   
         CLC   ELIOQUAL,=CL3'GT '      WHAT TYPE OF BROWSE REQUEST      01306   
         BNE   BNXGTSYS                                                 01307   
         CLC   CNKEYNAM,ELIOKEY        DOES THIS REC HAVE 'GT' KEY      01308   
         BH    BSYSPRKY                                                 01309   
         B     BEXSYSNM                                                 01310   
BNXGTSYS CLC   ELIOQUAL,=CL3'GTE'      WHAT TYPE OF BROWSE REQUEST      01311   
         BNE   BNXEQSYS                                                 01312   
         CLC   CNKEYNAM,ELIOKEY        DOES THIS REC HAVE 'GTE' KEY     01313   
         BNL   BENGPRKY                                                 01314   
         B     BEXSYSNM                                                 01315   
BNXEQSYS CLC   ELIOQUAL,=CL3'EQ '      WHAT TYPE OF BROWSE REQUEST      01316   
         BNE   INVREQ                                                   01317   
         CLC   CNKEYNAM,ELIOKEY        DOES THIS REC HAVE 'EQ' KEY      01318   
         BE    BSYSPRKY                                                 01319   
         B     BEXSYSNM                                                 01320   
BPRSYSNM DS    0H                                                       01321   
         EXEC CICS  READPREV    DATASET('ELPCN')    INTO(CNKEYNAM)     X01322   
                   LENGTH(LENGTH)   RIDFLD(ELIOBKEY)   REQID(ELIOBRID)  01323   
*                                                                       01324   
         CLC   ELIOQUAL,=CL3'LT '      WHAT TYPE OF BROWSE REQUEST      01325   
         BNE   BPRLTSYS                                                 01326   
         CLC   CNKEYNAM,ELIOKEY        DOES THIS REC HAVE 'LT' KEY      01327   
         BL    BSYSPRKY                                                 01328   
         B     BPRSYSNM                                                 01329   
BPRLTSYS CLC   ELIOQUAL,=CL3'LTE'      WHAT TYPE OF BROWSE REQUEST      01330   
         BNE   BPREQSYS                                                 01331   
         CLC   CNKEYNAM,ELIOKEY        DOES THIS REC HAVE 'LTE' KEY     01332   
         BNH   BSYSPRKY                                                 01333   
         B     BPRSYSNM                                                 01334   
BPREQSYS CLC   ELIOQUAL,=CL3'EQ '      WHAT TYPE OF BROWSE REQUEST      01335   
         BNE   INVREQ                                                   01336   
         CLC   CNKEYNAM,ELIOKEY        DOES THIS REC HAVE 'EQ' KEY      01337   
         BE    BSYSPRKY                                                 01338   
         B     BPRSYSNM                                                 01339   
*                                                                       01340   
BSYSPRKY DS    0H                                                       01341   
         MVC   CIADDN,=C'ELPDE   '    RESET DDNAME                      01342   
         MVC   DEMRECID,CNRECPRF      RECORD ID                         01343   
         ZAP   DEMELTNO,CNELMTNO      ELEMENT NUMBER                    01344   
         MVI   ELIOALDD,X'00'          RESET THE ALTERNATE KEY DDNAME   01345   
         MVC   ELIOALDD+1(7),ELIOALDD  RESET THE ALTERNATE KEY DDNAME   01346   
         MVI   ELIOKEY,X'00'          INITIALIZE THE KEY AREA           01347   
         MVC   ELIOKEY+1(254),ELIOKEY  INITIALIZE THE KEY AREA          01348   
         MVC   ELIOKEY(L'DEPRIKEY),DTELNAME   MOVE DATA ELEMENT KEY     01349   
         MVC   ELIORL,ELIOMXRL                                          01350   
         MVI   ELIOBKL,X'00'          RESET THE BROWSE KEY LENGTH       01351   
         MVI   ELIOBKL+1,X'00'        RESET THE BROWSE KEY LENGTH       01352   
*                                                                       01353   
         EXEC CICS  ENDBR   DATASET('ELPCN')   REQID(ELIOBRID)          01354   
*                                                                       01355   
         MVI   ELIOALDD,X'00'          RESET THE ALTERNATE KEY DDNAME   01356   
         MVC   ELIOALDD+1(7),ELIOALDD  RESET THE ALTERNATE KEY DDNAME   01357   
         MVC   ELIOQUAL,=CL3' '       RESET QUALIFIER                   01358   
         MVI   ELIOBKL,X'00'          RESET BROWSE KEY LENGTH           01359   
         MVI   ELIOBKL+1,X'00'        RESET BROWSE KEY LENGTH           01360   
         EXEC CICS  FREEMAIN   DATA(CNSYSNAM)                           01361   
*                                                                       01362   
         EXEC CICS  HANDLE CONDITION   NOTFND(ALTNOFND)                 01363   
*                                                                       01364   
         L     R2,SA2BSYS                                               01365   
         L     R9,SA9BSYS                                               01366   
         BR    R2                                                       01367   
         EJECT                                                          01368   
SAR2VAR  DS    A                                                        01369   
SAR10VAR DS    A                                                        01370   
VARRECCK DS    0H                                                       01371   
*********************************************************************** 01372   
*     I F   V A R I A B L E   R E C O R D   C A L C U L A T E           01373   
*       T H E   L E N G T H   F O R   T H E   R E C O R D .             01374   
*                                                                       01375   
*     1.  DETERMINE IF THE RECORD BEING WRITTEN IS FOR A                01376   
*         VARIABLE LENGTH FILE THAT MUST HAVE A RECORD LENGTH           01377   
*         CALCULATION.  (USING VARRECTB AGAINST ELIODDNM)               01378   
*     2.  IF YES IT WILL CALCULATE THE LENGTH USING THE OCCURS          01379   
*         FIELD AND PLACE THAT LENGTH IN IOPARMS.                       01380   
*********************************************************************** 01381   
*                                                                       01382   
         ST    R2,SAR2VAR                                               01383   
         ST    R10,SAR10VAR                                             01384   
*                                                                       01385   
         LA    R12,VARRECTB          ADDRESS OF VARIABLE REC LEN TABLE  01386   
         USING VARRECDS,R12          SPECIFY ADDRESSABILITY             01387   
*                                                                       01388   
VARRECLP DS    0H          T R Y   T O   M A T C H   O N   D D N A M E  01389   
         CLC   VARRDDN,=8X'FF'         END OF TABLE                     01390   
         BE    NVARREC                 YES, GO BACK                     01391   
         CLC   VARRDDN,ELIODDNM        CHECK FOR MATCH ON DDNAME        01392   
         BE    YVARREC                 YES CONTINUE                     01393   
         AH    R12,=Y(VARRECLN)                                         01394   
         B     VARRECLP                                                 01395   
*                                                                       01396   
YVARREC  DS    0H                                                       01397   
**********************************************************************  01398   
*        CALCULATE THE ACTUAL RECORD LENGTH OF A VARIABLE RECORD     *  01399   
**********************************************************************  01400   
*                                                                       01401   
*     R12 IS POINTING TO THE TABLE ENTRY WE ARE CALCULATING.            01402   
*                                                                       01403   
*    SET UP ADDRESSABILITY FOR THE OCCURS COUNT                         01404   
         L     R10,ELIORECA       LOAD ADDRESS OF RECORD IN R10         01405   
         AH    R10,VARROCCT       ADD DISPLACEMENT TO PACKED OCCURS FLD 01406   
*                                                                       01407   
*    FIRST CHECK THAT THE OCCURS COUNT IS NOT OVER THE MAX NUMBER       01408   
*                                                                       01409   
         LH    R9,VARMAXOC        LOAD THE MAX OCCURS COUNT IN R9       01410   
*                                                                       01411   
         CVD   R9,PACK8           CONVERT TO A PACKED NUMBER            01412   
*                                                                       01413   
         CP   0(2,R10),PACK8      COMPARE THAT TO THE OCCURS COUNT      01414   
         BH   MAXOCCER            ERROR  -  IF RECORD GREATER           01415   
*    CONTINUE TO CALCULATE THE RECORD LENGTH                            01416   
         LH    R9,VARRVLEN        LOAD LENGTH OF VARIABLE MEMBER IN R9  01417   
*                                                                       01418   
         CVD   R9,PACK8           CONVERT TO A PACKED NUMBER            01419   
*                                                                       01420   
         MP    PACK8,0(2,R10)     MULT VARIABLE LEN BY NUM OF OCCURS    01421   
*                                                                       01422   
         CVB   R9,PACK8           CONVERT TOTAL VARIABLE LEN TO BINARY  01423   
*                                                                       01424   
         AH    R9,VARRFLEN     ADD FIXED AND VARIABLE SECTION TOGETHER  01425   
*                              R9 NOW CONTAINS TOTAL LENGTH OF RECORD.  01426   
*                                                                       01427   
*                                                                       01428   
*                                                                       01429   
*        CH    R9,HGIORL      COMPARE LENGTH PASSED BY THE APPLICATION  01430   
*        BNE   IORLERR        EQUALS THE CALC LENGTH.  IGNORE FOR NOW   01431   
*                                                                       01432   
*                                                                       01433   
         L     R10,SVNAMLN      RESTORE NAME/LENGTH ADDRESS             01434   
*                       CHECK THE LENGTH WE CALC'D DOESN'T EXCEED THE   01435   
         CH    R9,TABDDNLN      MAX LENGTH FOR THE RECORD.              01436   
         BH    RECLENER                                                 01437   
*                                                                       01438   
*                                  RECORD LENGTH PASSES                 01439   
         STH   R9,ELIORL         SAVE RECORD LENGTH IN IOPARMS          01440   
*                                                                       01441   
NVARREC  DS    0H                                                       01442   
*                                                                       01443   
*   RESTORE R2 & R10                                                    01444   
         L     R2,SAR2VAR                                               01445   
         L     R10,SAR10VAR                                             01446   
         BR    R2                                                       01447   
         EJECT                                                          01448   
**********************************************************************  01449   
*        E R R O R   C O N D I T I O N S                                01450   
**********************************************************************  01451   
*                                                                       01452   
**********************************************************************  01453   
*        E N D   O F   F I L E                                          01454   
**********************************************************************  01455   
ENDFILE  DS    0H                                                       01456   
         MVI   ELIOALDD,X'00'          RESET THE ALTERNATE KEY DDNAME   01457   
         MVC   ELIOALDD+1(7),ELIOALDD  RESET THE ALTERNATE KEY DDNAME   01458   
         MVC   ELIOQUAL,=CL3' '       RESET QUALIFIER                   01459   
         MVI   ELIOBKL,X'00'          RESET BROWSE KEY LENGTH           01460   
         MVI   ELIOBKL+1,X'00'        RESET BROWSE KEY LENGTH           01461   
         MVC   ELIORC,=C'03'           SET END OF FILE RETURN CODE      01462   
         MVC   ELIOMSG,=CL16'END OF FILE     '  ERROR MSG RETURNED      01463   
*                                                                       01464   
*                                THE BROWSE MUST BE ENDED.  DO THIS     01465   
         B     ENDBR             FAVOR FOR OUR NICE CALLER.             01466   
*                                                                       01467   
**********************************************************************  01468   
*        R E C O R D   N O T   F O U N D                                01469   
**********************************************************************  01470   
NOTFND   DS    0H                                                       01471   
         MVI   ELIOALDD,X'00'          RESET THE ALTERNATE KEY DDNAME   01472   
         MVC   ELIOALDD+1(7),ELIOALDD  RESET THE ALTERNATE KEY DDNAME   01473   
         MVC   ELIOQUAL,=CL3' '       RESET QUALIFIER                   01474   
         MVI   ELIOBKL,X'00'          RESET BROWSE KEY LENGTH           01475   
         MVI   ELIOBKL+1,X'00'        RESET BROWSE KEY LENGTH           01476   
         MVC   ELIORC,=C'01'           SET NOT FOUND RETURN CODE        01477   
         MVC   ELIOMSG,=CL16'RECORD NOT FOUND'  ERROR MSG RETURNED      01478   
*                                                                       01479   
         B     RETURN                  RETURN TO CALLER                 01480   
*                                                                       01481   
**********************************************************************  01482   
*     D U P L I C A T E   R E C O R D   A D D   A T T E M P T E D       01483   
**********************************************************************  01484   
DUPKEY   DS    0H                                                       01485   
         MVC   ELIORC,=C'10'           SET DUPLICATE KEY RETURN CODE    01486   
         MVC   ELIOMSG,=CL16'DUPLICATE KEY   '  ERROR MSG RETURNED      01487   
*                                                                       01488   
         B     RETURN                  RETURN TO CALLER                 01489   
*                                                                       01490   
**********************************************************************  01491   
*  I N V A L I D   F U N C T I O N   R E Q U E S T E D   -   A B E N D  01492   
**********************************************************************  01493   
INVREQ   DS    0H                                                       01494   
         MVC   ELIOMSG,=CL16'INVALID REQUEST '  ERROR MSG RETURNED      01495   
         MVC   ABCODE,=C'IVRQ'         MOVE ABEND TYPE                  01496   
*                                                                       01497   
         B     ABEND                                                    01498   
*                                                                       01499   
DUPALTID DS    0H                                                       01500   
         MVC   ELIOMSG,=CL16'DUP ALTER INDEX '  ERROR MSG RETURNED      01501   
         MVC   ABCODE,=C'DPAL'         MOVE ABEND TYPE                  01502   
*                                                                       01503   
         B     ABEND                                                    01504   
*                                                                       01505   
ALTNOFND DS    0H                                                       01506   
         MVC   ELIOMSG,=CL16'ALT IDX NO DATA '  ERROR MSG RETURNED      01507   
         MVC   ABCODE,=C'NDAT'         MOVE ABEND TYPE                  01508   
*                                                                       01509   
         B     ABEND                                                    01510   
*                                                                       01511   
INVREQ1  DS    0H                                                       01512   
         MVC   ELIOMSG,=CL16'DUPREC ON WR ADD'  ERROR MSG RETURNED      01513   
         MVC   ABCODE,=C'DPWR'         MOVE ABEND TYPE                  01514   
*                                                                       01515   
         B     ABEND                                                    01516   
*                                                                       01517   
INVREQNL DS    0H                                                       01518   
         MVC   ELIOMSG,=CL16'NEW LENGTH BAD  '  ERROR MSG RETURNED      01519   
         MVC   ABCODE,=C'NLBD'         MOVE ABEND TYPE                  01520   
*                                                                       01521   
         B     ABEND                                                    01522   
*                                                                       01523   
INVREQML DS    0H                                                       01524   
         MVC   ELIOMSG,=CL16'BAD MAX REC LEN '  ERROR MSG RETURNED      01525   
         MVC   ABCODE,=C'MLBD'         MOVE ABEND TYPE                  01526   
*                                                                       01527   
         B     ABEND                                                    01528   
*                                                                       01529   
MAXOCCER DS    0H                                                       01530   
         MVC   ELIOMSG,=CL16'OCCURS COUNT ERR'  ERROR MSG RETURNED      01531   
         MVC   ABCODE,=C'OCCT'         MOVE ABEND TYPE                  01532   
*                                                                       01533   
         B     ABEND                                                    01534   
*                                                                       01535   
RECLENER DS    0H                                                       01536   
         MVC   ELIOMSG,=CL16'REC LEN CAL ERR '  ERROR MSG RETURNED      01537   
         MVC   ABCODE,=C'RECL'         MOVE ABEND TYPE                  01538   
*                                                                       01539   
         B     ABEND                                                    01540   
*                                                                       01541   
INVREQRA DS    0H                                                       01542   
         MVC   ELIOMSG,=CL16'REC AREA MISSING'  ERROR MSG RETURNED      01543   
         MVC   ABCODE,=C'RANF'         MOVE ABEND TYPE                  01544   
*                                                                       01545   
         B     ABEND                                                    01546   
*                                                                       01547   
ERCN     DS    0H                                                       01548   
         MVC   ELIOMSG,=CL16'RCN ERROR ON ALT'  ERROR MSG RETURNED      01549   
         MVC   ABCODE,=C'ERCN'         MOVE ABEND TYPE                  01550   
*                                                                       01551   
         B     ABEND                                                    01552   
*                                                                       01553   
IORLERR  DS    0H                                                       01554   
         MVC   ELIOMSG,=CL16'IORL FIELD ERROR'  ERROR MSG RETURNED      01555   
         MVC   ABCODE,=C'IORL'         MOVE ABEND TYPE                  01556   
*                                                                       01557   
         B     ABEND                                                    01558   
*                                                                       01559   
INVREQSM DS    0H                                                       01560   
         MVC   ELIOMSG,=CL16'BAD STORAGE MODE'  ERROR MSG RETURNED      01561   
         MVC   ABCODE,=C'SMBD'         MOVE ABEND TYPE                  01562   
*                                                                       01563   
         B     ABEND                                                    01564   
*                                                                       01565   
INVREQDN DS    0H                                                       01566   
         MVC   ELIOMSG,=CL16'INVALID DDNAME  '  ERROR MSG RETURNED      01567   
         MVC   ABCODE,=C'DDNB'         MOVE ABEND TYPE                  01568   
*                                                                       01569   
         B     ABEND                                                    01570   
*                                                                       01571   
INVREQ99 DS    0H                                                       01572   
         MVC   ABCODE,=C'INVR'         MOVE ABEND TYPE                  01573   
*                                                                       01574   
         B     ABEND                                                    01575   
*                                                                       01576   
MISENGA  DS    0H                                                       01577   
         MVC   ABCODE,=C'MENG'         MOVE ABEND TYPE                  01578   
*                                                                       01579   
         B     ABEND                                                    01580   
*                                                                       01581   
MISSYNA  DS    0H                                                       01582   
         MVC   ABCODE,=C'MSYN'         MOVE ABEND TYPE                  01583   
*                                                                       01584   
         B     ABEND                                                    01585   
*                                                                       01586   
ABEND    DS    0H                                                       01587   
*                                                                       01588   
         EXEC CICS  ABEND   ABCODE(ABCODE)                              01589   
         EJECT                                                          01590   
*********************************************************************** 01591   
*         U N R E C O V E R A B L E   E R R O R S   -                   01592   
*               L I N K   T O   H C F C P E R R                         01593   
*********************************************************************** 01594   
*                                                                       01595   
ERROR    DS    0H                                                       01596   
         LA    R14,CEIAREA          SET UP COMMAREA FOR POSSIBLE        01597   
         USING CEIBERR,R14           CALL TO HCAFCPER.                  01598   
         MVC   CEIBFN,EIBFN            PASS EIB FUNCTION CODE           01599   
         MVC   CEIBRCDE,EIBRCODE       PASS EIB RETURN CODES            01600   
         MVC   CEIBDS,EIBDS            PASS DDNAME                      01601   
*                                                                       01602   
         EXEC CICS  LINK   PROGRAM('HGAFCPER')  COMMAREA(CEIAREA)      X01603   
                   LENGTH(=AL2(CEILEN))                                 01604   
         EJECT                                                          01605   
         COPY  ELADRLEN                                                 01606   
         EJECT                                                          01607   
*   THIS TABLE WILL LISTS DDNAME OF FILES THAT ARE VARIABLE IN          01608   
*   LENGTH AND MUST HAVE ROUTINES TO CALCULATE THE LENGTH OF            01609   
*   THE RECORD BEFORE AN 'ADD' OR 'WRITE/UPDATE' PROCESS CAN            01610   
*   OCCUR.   THE UPDATE OR ADD LOGIC WILL TRY TO MATCH FROM             01611   
*   THIS TABLE AND USE THE CORRECT SUBROUTINE TO                        01612   
*   CALCULATE THE LENGTH.  ANY NEW VARIABLE LENGTH FILES                01613   
*   SHOULD BE ADDED TO THE TABLE AND NO OTHER CODING CHANGES SHOULD     01614   
*   BE NECESSARY.                                                       01615   
*   IF THE TABLE CHANGES THE VARRECDS DSECT MUST ALSO BE CHANGED.       01616   
*                                                                       01617   
*   TABLE ENTRY CONSISTS OF                                             01618   
*   1.  DDNAME                                                          01619   
*   2.  DISPLACEMENT INTO THE RECORD OF THE OCCURS COUNT (HALFWORD)     01620   
*   3.  HALFWORD VALUE OF THE FIXED PORTION                             01621   
*   4.  HALFWORD VALUE OF THE INDIVIDUAL OCCURS SECTION LENGTH          01622   
*   5.  HALFWORD VALUE OF THE MAX NUMBER OR OCCURS SECTIONS             01623   
*                                                                       01624   
VARRECTB DS    0F                                                       01625   
*                                                                       01626   
*   CODE VALUE                                                          01627   
         DC    CL8'ELPCV'                                               01628   
         DC    Y(CVNBRVAL-ELPCVD)                                       01629   
         DC    Y(CVFXRECL),Y(CVVBRECL),Y(CVNOVARS)                      01630   
*                                                                       01631   
*   DATA ELEMENT FILE                                                   01632   
         DC    CL8'ELPDE'                                               01633   
         DC    Y(DENBRLNS-ELPDED)                                       01634   
         DC    Y(DEFXRECL),Y(DEVBRECL),Y(DENOVARS)                      01635   
*                                                                       01636   
         DC    8X'FF'                                                   01637   
         EJECT                                                          01638   
*                                                                       01639   
         LTORG                                                          01640   
         END                                                            01641   
