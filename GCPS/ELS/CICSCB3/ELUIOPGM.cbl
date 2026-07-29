00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELUIOPGM
00003  PROGRAM-ID.         ELUIOPGM.                                       LV005
00004                                                                   ELUIOPGM
00005  AUTHOR.             RICHARD J. LUKETICH.                         ELUIOPGM
00006                                                                   ELUIOPGM
00007                                                                   ELUIOPGM
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUIOPGM
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELUIOPGM
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUIOPGM
00011                      233 N. MICHIGAN AVE                          ELUIOPGM
00012                      CHICAGO, ILLINOIS 60601                      ELUIOPGM
00013                                                                   ELUIOPGM
00014  DATE-WRITTEN.       13-OCT-1986.                                 ELUIOPGM
00015                                                                   ELUIOPGM
00016  DATE-COMPILED.                                                   ELUIOPGM
00017                                                                   ELUIOPGM
00018  SECURITY.           COPYRIGHT 1986,                              ELUIOPGM
00019                      HEALTH CARE SERVICE CORPORATION              ELUIOPGM
00020      SKIP3                                                        ELUIOPGM
00021  TITLE 'ELS INPUT/OUTPUT PROGRAM                          '.      ELUIOPGM
00022  ENVIRONMENT DIVISION.                                            ELUIOPGM
00023                                                                   ELUIOPGM
00024  CONFIGURATION SECTION.                                           ELUIOPGM
00025  SOURCE-COMPUTER.    IBM-3033.                                    ELUIOPGM
00026  OBJECT-COMPUTER.    IBM-3033.                                    ELUIOPGM
00027      EJECT                                                        ELUIOPGM
00028 ******************************************************************ELUIOPGM
00029 *                                                                *ELUIOPGM
00030 *                    MAINTENANCE HISTORY                         *ELUIOPGM
00031 **AKK 12/06/05 REGEN FOR TEST                                    *ELUIOPGM
00032 *  MOD   DATE     BY                       ACTION                *ELUIOPGM
00033 * ----- --------- ---- ----------------------------------------- *ELUIOPGM
00034 * 01.00           RJL  CREATED                                   *ELUIOPGM
00035 *                                                                *ELUIOPGM
00036 *                                                                *ELUIOPGM
00037 * 01.01 16-MAR-90 AKK  REMOVED SEGIDERR, NOT SUPPORTED BY        *ELUIOPGM
00038 *                      CICS 1.7                                  *ELUIOPGM
00039 *                                                                *ELUIOPGM
00040 * 01.02 26-AUG-92 RJL CONVERTED LINK TO STORAGE MANGER TO CALL   *ELUIOPGM
00041 *                                                                *ELUIOPGM
00042 * 01.03 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELUIOPGM
00043 *                                                                *ELUIOPGM
00044 *       12-AUG-2003 AKK GEN IN QE TO TEST ORDER OF COMPILE       *ELUIOPGM
00045 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELUIOPGM
00046 *                                                                *ELUIOPGM
00047 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELUIOPGM
00048 *                                                                *ELUIOPGM
00049 * 02.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *ELUIOPGM
00050 *                                                                *ELUIOPGM
00051 ******************************************************************ELUIOPGM
00052  DATA DIVISION.                                                   ELUIOPGM
00053                                                                   ELUIOPGM
00054  FILE SECTION.                                                    ELUIOPGM
00055                                                                   ELUIOPGM
00056  WORKING-STORAGE SECTION.                                         ELUIOPGM
00057                                                                   ELUIOPGM
00058  01  WS-MISC-STUFF.                                               ELUIOPGM
00059      05  WS-DDNAME-HOLD          PICTURE  X(8).                   ELUIOPGM
00060      05  WS-AREA-LEN-HOLD        PICTURE  S9(8) COMP.             ELUIOPGM
00061      05  WS-FIRST-TO-PRINT       PICTURE  S9(4) COMP.             ELUIOPGM
00062      05  WS-LAST-TO-PRINT        PICTURE  S9(4) COMP.             ELUIOPGM
00063      05  WS-QUEUE-ID.                                             ELUIOPGM
00064          10  WS-QUEUE-TERM-ID    PICTURE  X(4).                   ELUIOPGM
00065          10  WS-QUEUE-QUAL       PICTURE  9(4).                   ELUIOPGM
00066      05  WS-QUEUE-ITEM           PICTURE  S9(4) COMP.             ELUIOPGM
00067      05  WS-QUEUE-LEN            PICTURE  S9(4) COMP.             ELUIOPGM
00068      05  WS-TIME-BREAK-DOWN      PICTURE  9(7).                   ELUIOPGM
00069      05  FILLER        REDEFINES WS-TIME-BREAK-DOWN.              ELUIOPGM
00070          10  FILLER              PICTURE  999.                    ELUIOPGM
00071          10  WS-TIME-MMSS        PICTURE  9(4).                   ELUIOPGM
00072      EJECT                                                        ELUIOPGM
00073  01  WS-PRINT-Q-MSGS.                                             ELUIOPGM
00074      05  WS-PRINT-Q-NORMAL.                                       ELUIOPGM
00075          10  FILLER              PICTURE X(35) VALUE              ELUIOPGM
00076              'PRINT REQUEST COMPLETED.  PRINT ID='.               ELUIOPGM
00077          10  WS-PRINT-Q-NORMAL-ID PICTURE X(8).                   ELUIOPGM
00078      05  WS-PRINT-Q-NO-SPACE     PICTURE X(50) VALUE              ELUIOPGM
00079          'UNABLE TO PRINT NOW.  TRY AGAIN LATER.'.                ELUIOPGM
00080      05  WS-NO-PRINTER-MSG.                                       ELUIOPGM
00081          10  FILLER              PICTURE X(30) VALUE              ELUIOPGM
00082              'NO PRINTER ASSIGNED--TERMINAL '.                    ELUIOPGM
00083          10  WS-NO-PRINTER-TERM  PICTURE X(4).                    ELUIOPGM
00084          10  FILLER              PICTURE X(16) VALUE              ELUIOPGM
00085              '. CALL HELP DESK'.                                  ELUIOPGM
00086                                                                   ELUIOPGM
00087      COPY HEXCOBOL.                                               ELUIOPGM
00088      EJECT                                                        ELUIOPGM
00089      COPY ELSPRPAC.                                               ELUIOPGM
00090      EJECT                                                        ELUIOPGM
00091  LINKAGE SECTION.                                                 ELUIOPGM
00092                                                                   ELUIOPGM
00093  01  DFHCOMMAREA.                                                 ELUIOPGM
00094      COPY ELSCOMMC.                                               ELUIOPGM
00095      EJECT                                                        ELUIOPGM
00096      COPY ELSCIA2C.                                               ELUIOPGM
00097      EJECT                                                        ELUIOPGM
00098      COPY ELSSMAC.                                                ELUIOPGM
00099      EJECT                                                        ELUIOPGM
00100      COPY ELSIOPMC.                                               ELUIOPGM
00101      EJECT                                                        ELUIOPGM
00102  01  LS-INPUT-OUTPUT-AREA        PICTURE  X(01).                  ELUIOPGM
00103      SKIP3                                                        ELUIOPGM
00104  01  LS-FROM-AREA                PICTURE  X(01).                  ELUIOPGM
00105      SKIP3                                                        ELUIOPGM
00106  01  LS-TO-AREA                  PICTURE  X(01).                  ELUIOPGM
00107      SKIP3                                                        ELUIOPGM
00108      COPY ELSPAGQC.                                               ELUIOPGM
00109      EJECT                                                        ELUIOPGM
00110      EJECT                                                        ELUIOPGM
00111  PROCEDURE DIVISION.                                              ELUIOPGM
00112 ************************************************************      ELUIOPGM
00113 *                                                          *      ELUIOPGM
00114 *                    PROCEDURE DIVISION                    *      ELUIOPGM
00115 *                                                          *      ELUIOPGM
00116 ************************************************************      ELUIOPGM
00117                                                                   ELUIOPGM
00118                                                                   ELUIOPGM
00119 ************************************************************      ELUIOPGM
00120 *                                                          *      ELUIOPGM
00121 *        PERFORM INPUT-OUTPUT FUNCTIONS                    *      ELUIOPGM
00122 *                                                          *      ELUIOPGM
00123 ************************************************************      ELUIOPGM
00124  00001-PERFORM-INPUT-OUTPUT-FUN.                                  ELUIOPGM
00125      PERFORM 00005-INITIALIZE.                                    ELUIOPGM
00126      PERFORM 00049-PROCESS-INPUT-OUTPUT-REQ.                      ELUIOPGM
00127      GOBACK.                                                      ELUIOPGM
00128                                                                   ELUIOPGM
00129                                                                   ELUIOPGM
00130 ************************************************************      ELUIOPGM
00131 *                                                          *      ELUIOPGM
00132 *        INITIALIZE                                        *      ELUIOPGM
00133 *                                                          *      ELUIOPGM
00134 ************************************************************      ELUIOPGM
00135  00005-INITIALIZE.                                                ELUIOPGM
00136      PERFORM 00509-CHECK-COMMAREA-LENGTH.                         ELUIOPGM
00137      SKIP1                                                        ELUIOPGM
00138 *                                                          *      ELUIOPGM
00139 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELUIOPGM
00140 *                                                          *      ELUIOPGM
00141          CALL 'ELUINISM' USING DFHCOMMAREA                        ELUIOPGM
00142          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELUIOPGM
00143      SKIP1                                                        ELUIOPGM
00144      SKIP1                                                        ELUIOPGM
00145 *                                                          *      ELUIOPGM
00146 *        ESTABLISH ADDRESSING TO STORAGE MANAGEMENT AREA   *      ELUIOPGM
00147 *                                                          *      ELUIOPGM
00148          MOVE CIA-DDNAME TO WS-DDNAME-HOLD                        ELUIOPGM
00149          MOVE CIA-AREA-LEN TO WS-AREA-LEN-HOLD                    ELUIOPGM
00150          SET CIA-ELSSMA-DDN TO TRUE                               ELUIOPGM
00151          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELUIOPGM
00152          ADDRESS OF SMA-STORAGE-MANAGEMENT-AREA                   ELUIOPGM
00153          MOVE WS-DDNAME-HOLD TO CIA-DDNAME                        ELUIOPGM
00154          MOVE WS-AREA-LEN-HOLD TO CIA-AREA-LEN.                   ELUIOPGM
00155      SKIP1                                                        ELUIOPGM
00156      PERFORM 00012-ESTABLISH-PARAMETER-BLOC.                      ELUIOPGM
00157      IF    IOP-ADD                                                ELUIOPGM
00158         OR (     IOP-DEL                                          ELUIOPGM
00159              AND IOP-FCQ-UPD )                                    ELUIOPGM
00160         OR IOP-UPD                                                ELUIOPGM
00161          PERFORM 00032-ESTABLISH-OUTPUT-RECORDX                   ELUIOPGM
00162      ELSE PERFORM 00035-ESTABLISH-INPUT-RECORD-A.                 ELUIOPGM
00163      EJECT                                                        ELUIOPGM
00164                                                                   ELUIOPGM
00165                                                                   ELUIOPGM
00166 ************************************************************      ELUIOPGM
00167 *                                                          *      ELUIOPGM
00168 *        ESTABLISH PARAMETER BLOCK                         *      ELUIOPGM
00169 *                                                          *      ELUIOPGM
00170 ************************************************************      ELUIOPGM
00171  00012-ESTABLISH-PARAMETER-BLOC.                                  ELUIOPGM
00172      SKIP1                                                        ELUIOPGM
00173 *                                                          *      ELUIOPGM
00174 *        LOCATE PARAMETER BLOCK                            *      ELUIOPGM
00175 *                                                          *      ELUIOPGM
00176          SEARCH ALL SMA-STG-MGT-TBL                               ELUIOPGM
00177             AT END                                                ELUIOPGM
00178                 SET CIA-AB-DDNAME TO TRUE                         ELUIOPGM
00179                 EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC       ELUIOPGM
00180             WHEN SMA-DDN (SMA-STG-MGT-IDX) = CIA-DDNAME           ELUIOPGM
00181                 CONTINUE.                                         ELUIOPGM
00182      SKIP1                                                        ELUIOPGM
00183      IF SMA-PTR (SMA-STG-MGT-IDX) IS NOT EQUAL TO NULL            ELUIOPGM
00184          PERFORM 00024-ESTABLISH-ADDRESSING-TOX                   ELUIOPGM
00185      SKIP1                                                        ELUIOPGM
00186 *                                                          *      ELUIOPGM
00187 *        SIGNAL PARAMETER BLOCK ADDRESSING ERROR           *      ELUIOPGM
00188 *                                                          *      ELUIOPGM
00189      ELSE                                                         ELUIOPGM
00190          SET CIA-AB-ELSIOPM-PTR TO TRUE                           ELUIOPGM
00191          EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.             ELUIOPGM
00192      SKIP1                                                        ELUIOPGM
00193      SKIP1                                                        ELUIOPGM
00194 *                                                          *      ELUIOPGM
00195 *        INITIALIZE IOP RETURN CODE                        *      ELUIOPGM
00196 *                                                          *      ELUIOPGM
00197          SET IOP-RC-OK TO TRUE.                                   ELUIOPGM
00198      SKIP1                                                        ELUIOPGM
00199                                                                   ELUIOPGM
00200                                                                   ELUIOPGM
00201 ************************************************************      ELUIOPGM
00202 *                                                          *      ELUIOPGM
00203 *        ESTABLISH ADDRESSING TO PARAMETER BLOCK           *      ELUIOPGM
00204 *                                                          *      ELUIOPGM
00205 ************************************************************      ELUIOPGM
00206  00024-ESTABLISH-ADDRESSING-TOX.                                  ELUIOPGM
00207      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS TO SMA-PTR        ELUIOPGM
00208          (SMA-STG-MGT-IDX).                                       ELUIOPGM
00209      SET SMA-ELSIOPM-PTR TO ADDRESS OF                            ELUIOPGM
00210          IOP-INPUT-OUTPUT-PARAMETERS.                             ELUIOPGM
00211      EJECT                                                        ELUIOPGM
00212                                                                   ELUIOPGM
00213                                                                   ELUIOPGM
00214 ************************************************************      ELUIOPGM
00215 *                                                          *      ELUIOPGM
00216 *        ESTABLISH OUTPUT RECORD AREA                      *      ELUIOPGM
00217 *                                                          *      ELUIOPGM
00218 ************************************************************      ELUIOPGM
00219  00032-ESTABLISH-OUTPUT-RECORDX.                                  ELUIOPGM
00220      IF IOP-REC-PTR IS NOT EQUAL TO NULL                          ELUIOPGM
00221          PERFORM 00044-ESTABLISH-ADDRESSING-TOX                   ELUIOPGM
00222      SKIP1                                                        ELUIOPGM
00223 *                                                          *      ELUIOPGM
00224 *        SIGNAL RECORD ADDRESSING ERROR                    *      ELUIOPGM
00225 *                                                          *      ELUIOPGM
00226      ELSE                                                         ELUIOPGM
00227          SET CIA-AB-IOREC-PTR TO TRUE                             ELUIOPGM
00228          EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.             ELUIOPGM
00229      SKIP1                                                        ELUIOPGM
00230      EJECT                                                        ELUIOPGM
00231                                                                   ELUIOPGM
00232                                                                   ELUIOPGM
00233 ************************************************************      ELUIOPGM
00234 *                                                          *      ELUIOPGM
00235 *        ESTABLISH INPUT RECORD AREA                       *      ELUIOPGM
00236 *                                                          *      ELUIOPGM
00237 ************************************************************      ELUIOPGM
00238  00035-ESTABLISH-INPUT-RECORD-A.                                  ELUIOPGM
00239      IF IOP-REC-PTR IS EQUAL TO IOP-FC-REC-PTR                    ELUIOPGM
00240          PERFORM 00038-DROP-LOCATE-MODE-RECORDX.                  ELUIOPGM
00241      IF     IOP-STG-MODE-MOVE                                     ELUIOPGM
00242         AND IOP-REC-PTR IS NOT EQUAL TO NULL                      ELUIOPGM
00243          PERFORM 00040-FREE-CURRENT-RECORD-AREA.                  ELUIOPGM
00244                                                                   ELUIOPGM
00245                                                                   ELUIOPGM
00246 ************************************************************      ELUIOPGM
00247 *                                                          *      ELUIOPGM
00248 *        DROP LOCATE MODE RECORD AREA                      *      ELUIOPGM
00249 *                                                          *      ELUIOPGM
00250 ************************************************************      ELUIOPGM
00251  00038-DROP-LOCATE-MODE-RECORDX.                                  ELUIOPGM
00252      SET IOP-REC-PTR TO NULL.                                     ELUIOPGM
00253                                                                   ELUIOPGM
00254                                                                   ELUIOPGM
00255 ************************************************************      ELUIOPGM
00256 *                                                          *      ELUIOPGM
00257 *        FREE CURRENT RECORD AREA                          *      ELUIOPGM
00258 *                                                          *      ELUIOPGM
00259 ************************************************************      ELUIOPGM
00260  00040-FREE-CURRENT-RECORD-AREA.                                  ELUIOPGM
00261      SET IOP-FREEMAIN-BOTH-REC TO TRUE.                           ELUIOPGM
00262      SET CIA-STG-FREEMAIN TO TRUE.                                ELUIOPGM
00263      PERFORM 00505-CALL-STORAGE-MANAGER.                          ELUIOPGM
00264                                                                   ELUIOPGM
00265                                                                   ELUIOPGM
00266 ************************************************************      ELUIOPGM
00267 *                                                          *      ELUIOPGM
00268 *        ESTABLISH ADDRESSING TO RECORD                    *      ELUIOPGM
00269 *                                                          *      ELUIOPGM
00270 ************************************************************      ELUIOPGM
00271  00044-ESTABLISH-ADDRESSING-TOX.                                  ELUIOPGM
00272      SET ADDRESS OF LS-INPUT-OUTPUT-AREA TO IOP-REC-PTR.          ELUIOPGM
00273      EJECT                                                        ELUIOPGM
00274                                                                   ELUIOPGM
00275                                                                   ELUIOPGM
00276 ************************************************************      ELUIOPGM
00277 *                                                          *      ELUIOPGM
00278 *        PROCESS INPUT-OUTPUT REQUEST                      *      ELUIOPGM
00279 *                                                          *      ELUIOPGM
00280 ************************************************************      ELUIOPGM
00281  00049-PROCESS-INPUT-OUTPUT-REQ.                                  ELUIOPGM
00282      SKIP1                                                        ELUIOPGM
00283 *                                                          *      ELUIOPGM
00284 *        TURN CICS INPUT-OUTPUT ERROR TRAPPING OFF         *      ELUIOPGM
00285 *                                                          *      ELUIOPGM
00286          EXEC CICS IGNORE CONDITION                               ELUIOPGM
00287                DSIDERR                                            ELUIOPGM
00288                DUPKEY                                             ELUIOPGM
00289                DUPREC                                             ELUIOPGM
00290                ENDFILE                                            ELUIOPGM
00291                ILLOGIC                                            ELUIOPGM
00292                INVREQ                                             ELUIOPGM
00293                IOERR                                              ELUIOPGM
00294                ISCINVREQ                                          ELUIOPGM
00295                ITEMERR                                            ELUIOPGM
00296                LENGERR                                            ELUIOPGM
00297                NOSPACE                                            ELUIOPGM
00298                NOTFND                                             ELUIOPGM
00299                NOTOPEN                                            ELUIOPGM
00300                QIDERR                                             ELUIOPGM
00301                SYSIDERR                                           ELUIOPGM
00302                END-EXEC.                                          ELUIOPGM
00303      SKIP1                                                        ELUIOPGM
00304      PERFORM 00073-PERFORM-REQUESTED-INPUTX.                      ELUIOPGM
00305      IF     EIBRCODE IS EQUAL TO LOW-VALUES                       ELUIOPGM
00306                   AND IOP-FCN-INPUT                               ELUIOPGM
00307          PERFORM 00102-COMPLETE-INPUT-PROCESSIN.                  ELUIOPGM
00308      IF EIBRCODE IS NOT EQUAL TO LOW-VALUES                       ELUIOPGM
00309          PERFORM 00250-DIAGNOSE-RESULT-OF-INPUT.                  ELUIOPGM
00310                                                                   ELUIOPGM
00311                                                                   ELUIOPGM
00312 ************************************************************      ELUIOPGM
00313 *                                                          *      ELUIOPGM
00314 *        PERFORM REQUESTED INPUT-OUTPUT FUNCTION           *      ELUIOPGM
00315 *                                                          *      ELUIOPGM
00316 ************************************************************      ELUIOPGM
00317  00073-PERFORM-REQUESTED-INPUTX.                                  ELUIOPGM
00318      IF SMA-TYP-IO (SMA-STG-MGT-IDX)                              ELUIOPGM
00319          PERFORM 00077-PERFORM-REQUESTED-FILE-I                   ELUIOPGM
00320      ELSE IF SMA-TYP-TEMPSTG (SMA-STG-MGT-IDX)                    ELUIOPGM
00321          PERFORM 00088-PERFORM-REQUESTED-QUEUEX                   ELUIOPGM
00322      ELSE PERFORM 00525-SIGNAL-UNKNOWN-ERROR.                     ELUIOPGM
00323      EJECT                                                        ELUIOPGM
00324                                                                   ELUIOPGM
00325                                                                   ELUIOPGM
00326 ************************************************************      ELUIOPGM
00327 *                                                          *      ELUIOPGM
00328 *        PERFORM REQUESTED FILE INPUT-OUTPUT FUNCTION      *      ELUIOPGM
00329 *                                                          *      ELUIOPGM
00330 ************************************************************      ELUIOPGM
00331  00077-PERFORM-REQUESTED-FILE-I.                                  ELUIOPGM
00332      IF IOP-ADD                                                   ELUIOPGM
00333          PERFORM 00283-ADD-RECORD                                 ELUIOPGM
00334      ELSE IF IOP-DEL                                              ELUIOPGM
00335          PERFORM 00302-DELETE-RECORD                              ELUIOPGM
00336      ELSE IF IOP-END-BR                                           ELUIOPGM
00337          PERFORM 00324-END-BROWSE                                 ELUIOPGM
00338      ELSE IF IOP-RD                                               ELUIOPGM
00339          PERFORM 00329-READ-RECORD                                ELUIOPGM
00340      ELSE IF IOP-RD-NXT                                           ELUIOPGM
00341          PERFORM 00387-READ-RECORD-NEXT                           ELUIOPGM
00342      ELSE IF IOP-RD-PRV                                           ELUIOPGM
00343          PERFORM 00395-READ-RECORD-PREVIOUS                       ELUIOPGM
00344      ELSE IF IOP-ST-BR                                            ELUIOPGM
00345          PERFORM 00403-START-BROWSE                               ELUIOPGM
00346      ELSE IF IOP-UNLK                                             ELUIOPGM
00347          PERFORM 00443-UNLOCK-RECORD-HELD-FOR-U                   ELUIOPGM
00348      ELSE IF IOP-UPD                                              ELUIOPGM
00349          PERFORM 00447-UPDATE-RECORD                              ELUIOPGM
00350      ELSE PERFORM 00528-SIGNAL-INVALID-INPUT-OUT.                 ELUIOPGM
00351      EJECT                                                        ELUIOPGM
00352                                                                   ELUIOPGM
00353                                                                   ELUIOPGM
00354 ************************************************************      ELUIOPGM
00355 *                                                          *      ELUIOPGM
00356 *        PERFORM REQUESTED QUEUE INPUT-OUTPUT FUNCTION     *      ELUIOPGM
00357 *                                                          *      ELUIOPGM
00358 ************************************************************      ELUIOPGM
00359  00088-PERFORM-REQUESTED-QUEUEX.                                  ELUIOPGM
00360 * TS QUEUE ID IS BUILT BY ELUSTGMG WHEN IT CREATES                ELUIOPGM
00361 * THE IO PARAMETERS BLOCK                                         ELUIOPGM
00362      IF IOP-ADD                                                   ELUIOPGM
00363          PERFORM 00453-ADD-ITEM                                   ELUIOPGM
00364      ELSE IF IOP-DEL                                              ELUIOPGM
00365          PERFORM 00460-DELETE-ITEM                                ELUIOPGM
00366      ELSE IF IOP-END-BR                                           ELUIOPGM
00367          PERFORM 00464-END-QUEUE-BROWSE                           ELUIOPGM
00368      ELSE IF IOP-RD                                               ELUIOPGM
00369          PERFORM 00466-READ-ITEM                                  ELUIOPGM
00370      ELSE IF IOP-RD-NXT                                           ELUIOPGM
00371          PERFORM 00473-READ-ITEM-NEXT                             ELUIOPGM
00372      ELSE IF IOP-RD-PRV                                           ELUIOPGM
00373          PERFORM 00481-READ-ITEM-PREVIOUS                         ELUIOPGM
00374      ELSE IF IOP-ST-BR                                            ELUIOPGM
00375          PERFORM 00489-START-QUEUE-BROWSE                         ELUIOPGM
00376      ELSE IF IOP-UPD                                              ELUIOPGM
00377          PERFORM 00497-UPDATE-ITEM                                ELUIOPGM
00378      ELSE IF IOP-PRINT-TSQ                                        ELUIOPGM
00379          PERFORM 00134-PRINT-QUEUE                                ELUIOPGM
00380      ELSE IF IOP-PURGE-TSQ                                        ELUIOPGM
00381          PERFORM 00238-PURGE-ALL-QUEUES                           ELUIOPGM
00382      ELSE PERFORM 00528-SIGNAL-INVALID-INPUT-OUT.                 ELUIOPGM
00383      EJECT                                                        ELUIOPGM
00384                                                                   ELUIOPGM
00385                                                                   ELUIOPGM
00386 ************************************************************      ELUIOPGM
00387 *                                                          *      ELUIOPGM
00388 *        COMPLETE INPUT PROCESSING                         *      ELUIOPGM
00389 *                                                          *      ELUIOPGM
00390 ************************************************************      ELUIOPGM
00391  00102-COMPLETE-INPUT-PROCESSIN.                                  ELUIOPGM
00392      IF IOP-STG-MODE-MOVE                                         ELUIOPGM
00393          PERFORM 00105-MOVE-RECORD-TO-NEW-CONTR                   ELUIOPGM
00394      SKIP1                                                        ELUIOPGM
00395 *                                                          *      ELUIOPGM
00396 *        SET RECORD POINTER TO INPUT AREA                  *      ELUIOPGM
00397 *                                                          *      ELUIOPGM
00398      ELSE                                                         ELUIOPGM
00399          SET IOP-REC-PTR TO IOP-FC-REC-PTR                        ELUIOPGM
00400          MOVE IOP-FC-REC-LEN TO IOP-REC-LEN.                      ELUIOPGM
00401      SKIP1                                                        ELUIOPGM
00402                                                                   ELUIOPGM
00403                                                                   ELUIOPGM
00404 ************************************************************      ELUIOPGM
00405 *                                                          *      ELUIOPGM
00406 *        MOVE RECORD TO NEW CONTROLLED STORAGE AREA        *      ELUIOPGM
00407 *                                                          *      ELUIOPGM
00408 ************************************************************      ELUIOPGM
00409  00105-MOVE-RECORD-TO-NEW-CONTR.                                  ELUIOPGM
00410      IF    IOP-REC-PTR IS EQUAL TO NULL                           ELUIOPGM
00411                   OR IOP-REC-LEN IS LESS THAN IOP-FC-REC-LEN      ELUIOPGM
00412          PERFORM 00108-ALLOCATE-RECORD-AREA.                      ELUIOPGM
00413      SKIP1                                                        ELUIOPGM
00414 *                                                          *      ELUIOPGM
00415 *        MOVE RECORD TO ALLOCATED AREA                     *      ELUIOPGM
00416 *                                                          *      ELUIOPGM
00417          SET ADDRESS OF LS-FROM-AREA TO IOP-FC-REC-PTR            ELUIOPGM
00418          SET ADDRESS OF LS-TO-AREA TO IOP-REC-PTR                 ELUIOPGM
00419          MOVE IOP-REC-LEN TO CIA-AREA-LEN                         ELUIOPGM
00420          CALL 'ELUMVCL'                                           ELUIOPGM
00421           USING LS-FROM-AREA                                      ELUIOPGM
00422                 CIA-AREA-LEN                                      ELUIOPGM
00423                 LS-TO-AREA                                        ELUIOPGM
00424                 CIA-AREA-LEN                                      ELUIOPGM
00425                 HEX-40.                                           ELUIOPGM
00426      SKIP1                                                        ELUIOPGM
00427                                                                   ELUIOPGM
00428                                                                   ELUIOPGM
00429 ************************************************************      ELUIOPGM
00430 *                                                          *      ELUIOPGM
00431 *        ALLOCATE RECORD AREA                              *      ELUIOPGM
00432 *                                                          *      ELUIOPGM
00433 ************************************************************      ELUIOPGM
00434  00108-ALLOCATE-RECORD-AREA.                                      ELUIOPGM
00435      IF IOP-REC-PTR IS NOT EQUAL TO NULL                          ELUIOPGM
00436          PERFORM 00111-FREE-PREVIOUS-RECORD-ARE.                  ELUIOPGM
00437      SKIP1                                                        ELUIOPGM
00438 *                                                          *      ELUIOPGM
00439 *        OBTAIN CONTROLLED STORAGE FOR RECORD              *      ELUIOPGM
00440 *                                                          *      ELUIOPGM
00441          SET IOP-GETMAIN-REC TO TRUE                              ELUIOPGM
00442          SET CIA-STG-GETMAIN TO TRUE                              ELUIOPGM
00443          MOVE IOP-FC-REC-LEN TO IOP-MAX-REC-LEN                   ELUIOPGM
00444          PERFORM 00505-CALL-STORAGE-MANAGER                       ELUIOPGM
00445          MOVE IOP-FC-REC-LEN TO IOP-MAX-REC-LEN.                  ELUIOPGM
00446      SKIP1                                                        ELUIOPGM
00447                                                                   ELUIOPGM
00448                                                                   ELUIOPGM
00449 ************************************************************      ELUIOPGM
00450 *                                                          *      ELUIOPGM
00451 *        FREE PREVIOUS RECORD AREA                         *      ELUIOPGM
00452 *                                                          *      ELUIOPGM
00453 ************************************************************      ELUIOPGM
00454  00111-FREE-PREVIOUS-RECORD-ARE.                                  ELUIOPGM
00455      SET IOP-FREEMAIN-REC TO TRUE.                                ELUIOPGM
00456      SET CIA-STG-FREEMAIN TO TRUE.                                ELUIOPGM
00457      PERFORM 00505-CALL-STORAGE-MANAGER.                          ELUIOPGM
00458      EJECT                                                        ELUIOPGM
00459                                                                   ELUIOPGM
00460                                                                   ELUIOPGM
00461 ************************************************************      ELUIOPGM
00462 *                                                          *      ELUIOPGM
00463 *        PRINT QUEUE                                       *      ELUIOPGM
00464 *                                                          *      ELUIOPGM
00465 ************************************************************      ELUIOPGM
00466  00134-PRINT-QUEUE.                                               ELUIOPGM
00467      PERFORM 00138-PRINT-QUEUE-INITIALIZATI.                      ELUIOPGM
00468      IF IOP-RC-OK                                                 ELUIOPGM
00469          PERFORM 00163-PRINT-QUEUE-PROCESSING.                    ELUIOPGM
00470      PERFORM 00190-PRINT-QUEUE-TERMINATION.                       ELUIOPGM
00471                                                                   ELUIOPGM
00472                                                                   ELUIOPGM
00473 ************************************************************      ELUIOPGM
00474 *                                                          *      ELUIOPGM
00475 *        PRINT QUEUE INITIALIZATION                        *      ELUIOPGM
00476 *                                                          *      ELUIOPGM
00477 ************************************************************      ELUIOPGM
00478  00138-PRINT-QUEUE-INITIALIZATI.                                  ELUIOPGM
00479      SET IOP-RC-OK TO TRUE.                                       ELUIOPGM
00480      PERFORM 00142-VALIDATE-PARAMETERS.                           ELUIOPGM
00481      IF IOP-RC-OK                                                 ELUIOPGM
00482          PERFORM 00146-GENERATE-AVAILABLE-QUEUE.                  ELUIOPGM
00483                                                                   ELUIOPGM
00484                                                                   ELUIOPGM
00485 ************************************************************      ELUIOPGM
00486 *                                                          *      ELUIOPGM
00487 *        VALIDATE PARAMETERS                               *      ELUIOPGM
00488 *                                                          *      ELUIOPGM
00489 ************************************************************      ELUIOPGM
00490  00142-VALIDATE-PARAMETERS.                                       ELUIOPGM
00491      IF (IOP-TSQ-FIRST-PRINT < ZERO OR                            ELUIOPGM
00492                                       > IOP-TSQ-LAST-PRINT)       ELUIOPGM
00493               OR (IOP-TSQ-LAST-PRINT  < ZERO OR                   ELUIOPGM
00494                                       < IOP-TSQ-FIRST-PRINT)      ELUIOPGM
00495               OR (IOP-TSQ-PRINTER-ID = SPACES OR LOW-VALUES       ELUIOPGM
00496          OR ZEROS)                                                ELUIOPGM
00497          PERFORM 00144-PRINT-QUEUE-INVALID.                       ELUIOPGM
00498                                                                   ELUIOPGM
00499                                                                   ELUIOPGM
00500 ************************************************************      ELUIOPGM
00501 *                                                          *      ELUIOPGM
00502 *        PRINT QUEUE INVALID                               *      ELUIOPGM
00503 *                                                          *      ELUIOPGM
00504 ************************************************************      ELUIOPGM
00505  00144-PRINT-QUEUE-INVALID.                                       ELUIOPGM
00506      SET IOP-RC-INVREQ TO TRUE.                                   ELUIOPGM
00507      EJECT                                                        ELUIOPGM
00508                                                                   ELUIOPGM
00509                                                                   ELUIOPGM
00510 ************************************************************      ELUIOPGM
00511 *                                                          *      ELUIOPGM
00512 *        GENERATE AVAILABLE QUEUE NAME                     *      ELUIOPGM
00513 *                                                          *      ELUIOPGM
00514 ************************************************************      ELUIOPGM
00515  00146-GENERATE-AVAILABLE-QUEUE.                                  ELUIOPGM
00516      MOVE EIBTRMID  TO  WS-QUEUE-TERM-ID.                         ELUIOPGM
00517      MOVE EIBTIME   TO  WS-TIME-BREAK-DOWN.                       ELUIOPGM
00518      MOVE WS-TIME-MMSS TO WS-QUEUE-QUAL.                          ELUIOPGM
00519      PERFORM 00152-VALIDATE-QUEUE-NAME.                           ELUIOPGM
00520      PERFORM 00152-VALIDATE-QUEUE-NAME                            ELUIOPGM
00521          UNTIL IOP-RC-TS-QIDERR                                   ELUIOPGM
00522                      OR IOP-RC-TS-INVREQ                          ELUIOPGM
00523                      OR IOP-RC-TS-IOERR.                          ELUIOPGM
00524      IF IOP-RC-TS-QIDERR                                          ELUIOPGM
00525          PERFORM 00196-IGNORE-ERROR-CODE.                         ELUIOPGM
00526      EJECT                                                        ELUIOPGM
00527                                                                   ELUIOPGM
00528                                                                   ELUIOPGM
00529 ************************************************************      ELUIOPGM
00530 *                                                          *      ELUIOPGM
00531 *        VALIDATE QUEUE NAME                               *      ELUIOPGM
00532 *                                                          *      ELUIOPGM
00533 ************************************************************      ELUIOPGM
00534  00152-VALIDATE-QUEUE-NAME.                                       ELUIOPGM
00535      EXEC CICS READQ TS                                           ELUIOPGM
00536                QUEUE(WS-QUEUE-ID)                                 ELUIOPGM
00537                ITEM(WS-QUEUE-ITEM)                                ELUIOPGM
00538                LENGTH(WS-QUEUE-LEN)                               ELUIOPGM
00539                SET(IOP-REC-PTR)                                   ELUIOPGM
00540          END-EXEC.                                                ELUIOPGM
00541      IF EIBRCODE NOT = LOW-VALUE                                  ELUIOPGM
00542          PERFORM 00193-DIAGNOSE-RESULT-OF-READQ.                  ELUIOPGM
00543      IF NOT IOP-RC-TS-QIDERR                                      ELUIOPGM
00544          PERFORM 00161-MODIFY-QUEUE-ID.                           ELUIOPGM
00545                                                                   ELUIOPGM
00546                                                                   ELUIOPGM
00547 ************************************************************      ELUIOPGM
00548 *                                                          *      ELUIOPGM
00549 *        MODIFY QUEUE ID                                   *      ELUIOPGM
00550 *                                                          *      ELUIOPGM
00551 ************************************************************      ELUIOPGM
00552  00161-MODIFY-QUEUE-ID.                                           ELUIOPGM
00553      ADD 1 TO WS-QUEUE-QUAL.                                      ELUIOPGM
00554      EJECT                                                        ELUIOPGM
00555                                                                   ELUIOPGM
00556                                                                   ELUIOPGM
00557 ************************************************************      ELUIOPGM
00558 *                                                          *      ELUIOPGM
00559 *        PRINT QUEUE PROCESSING                            *      ELUIOPGM
00560 *                                                          *      ELUIOPGM
00561 ************************************************************      ELUIOPGM
00562  00163-PRINT-QUEUE-PROCESSING.                                    ELUIOPGM
00563      PERFORM 00168-COPY-QUEUE-INITIALIZATIO.                      ELUIOPGM
00564      PERFORM 00177-COPY-QUEUE                                     ELUIOPGM
00565          VARYING IOP-TSQ-ITEM-NBR                                 ELUIOPGM
00566                       FROM WS-FIRST-TO-PRINT BY 1                 ELUIOPGM
00567                       UNTIL WS-FIRST-TO-PRINT >                   ELUIOPGM
00568              WS-LAST-TO-PRINT                                     ELUIOPGM
00569                          OR NOT IOP-RC-OK.                        ELUIOPGM
00570      IF IOP-RC-OK OR IOP-RC-TS-ITEMERR                            ELUIOPGM
00571          PERFORM 00196-IGNORE-ERROR-CODE                          ELUIOPGM
00572      ELSE PERFORM 00250-DIAGNOSE-RESULT-OF-INPUT.                 ELUIOPGM
00573                                                                   ELUIOPGM
00574                                                                   ELUIOPGM
00575 ************************************************************      ELUIOPGM
00576 *                                                          *      ELUIOPGM
00577 *        COPY QUEUE INITIALIZATION                         *      ELUIOPGM
00578 *                                                          *      ELUIOPGM
00579 ************************************************************      ELUIOPGM
00580  00168-COPY-QUEUE-INITIALIZATIO.                                  ELUIOPGM
00581      MOVE IOP-TSQ-FIRST-PRINT     TO  WS-FIRST-TO-PRINT.          ELUIOPGM
00582      MOVE IOP-TSQ-LAST-PRINT      TO  WS-LAST-TO-PRINT.           ELUIOPGM
00583      IF WS-FIRST-TO-PRINT < 1                                     ELUIOPGM
00584          PERFORM 00173-USE-DEFAULT-START.                         ELUIOPGM
00585      IF WS-LAST-TO-PRINT < 1                                      ELUIOPGM
00586          PERFORM 00175-USE-DEFAULT-END.                           ELUIOPGM
00587                                                                   ELUIOPGM
00588                                                                   ELUIOPGM
00589 ************************************************************      ELUIOPGM
00590 *                                                          *      ELUIOPGM
00591 *        USE DEFAULT START                                 *      ELUIOPGM
00592 *                                                          *      ELUIOPGM
00593 ************************************************************      ELUIOPGM
00594  00173-USE-DEFAULT-START.                                         ELUIOPGM
00595      MOVE 1 TO WS-FIRST-TO-PRINT.                                 ELUIOPGM
00596                                                                   ELUIOPGM
00597                                                                   ELUIOPGM
00598 ************************************************************      ELUIOPGM
00599 *                                                          *      ELUIOPGM
00600 *        USE DEFAULT END                                   *      ELUIOPGM
00601 *                                                          *      ELUIOPGM
00602 ************************************************************      ELUIOPGM
00603  00175-USE-DEFAULT-END.                                           ELUIOPGM
00604      MOVE 32000 TO WS-LAST-TO-PRINT.                              ELUIOPGM
00605      EJECT                                                        ELUIOPGM
00606                                                                   ELUIOPGM
00607                                                                   ELUIOPGM
00608 ************************************************************      ELUIOPGM
00609 *                                                          *      ELUIOPGM
00610 *        COPY QUEUE                                        *      ELUIOPGM
00611 *                                                          *      ELUIOPGM
00612 ************************************************************      ELUIOPGM
00613  00177-COPY-QUEUE.                                                ELUIOPGM
00614      PERFORM 00466-READ-ITEM.                                     ELUIOPGM
00615      IF EIBRCODE NOT = LOW-VALUE                                  ELUIOPGM
00616          PERFORM 00193-DIAGNOSE-RESULT-OF-READQ.                  ELUIOPGM
00617      IF IOP-RC-OK                                                 ELUIOPGM
00618          PERFORM 00181-ADD-TO-NEW-QUEUE.                          ELUIOPGM
00619                                                                   ELUIOPGM
00620                                                                   ELUIOPGM
00621 ************************************************************      ELUIOPGM
00622 *                                                          *      ELUIOPGM
00623 *        ADD TO NEW QUEUE                                  *      ELUIOPGM
00624 *                                                          *      ELUIOPGM
00625 ************************************************************      ELUIOPGM
00626  00181-ADD-TO-NEW-QUEUE.                                          ELUIOPGM
00627      SET ADDRESS OF PQ-PAGE-QUEUE TO IOP-FC-REC-PTR.              ELUIOPGM
00628      EXEC CICS WRITEQ TS                                          ELUIOPGM
00629                FROM(PQ-PAGE-QUEUE)                                ELUIOPGM
00630                ITEM(WS-QUEUE-ITEM)                                ELUIOPGM
00631                LENGTH(IOP-FC-REC-LEN)                             ELUIOPGM
00632                QUEUE(WS-QUEUE-ID)                                 ELUIOPGM
00633           END-EXEC.                                               ELUIOPGM
00634      IF EIBRCODE NOT = LOW-VALUE                                  ELUIOPGM
00635          PERFORM 00193-DIAGNOSE-RESULT-OF-READQ.                  ELUIOPGM
00636                                                                   ELUIOPGM
00637                                                                   ELUIOPGM
00638 ************************************************************      ELUIOPGM
00639 *                                                          *      ELUIOPGM
00640 *        PRINT QUEUE TERMINATION                           *      ELUIOPGM
00641 *                                                          *      ELUIOPGM
00642 ************************************************************      ELUIOPGM
00643  00190-PRINT-QUEUE-TERMINATION.                                   ELUIOPGM
00644      IF IOP-RC-OK                                                 ELUIOPGM
00645          PERFORM 00216-START-THE-PRINT-TASK.                      ELUIOPGM
00646      PERFORM 00198-DETERMINE-PRINT-STATUS.                        ELUIOPGM
00647                                                                   ELUIOPGM
00648                                                                   ELUIOPGM
00649 ************************************************************      ELUIOPGM
00650 *                                                          *      ELUIOPGM
00651 *        DIAGNOSE RESULT OF READQ                          *      ELUIOPGM
00652 *                                                          *      ELUIOPGM
00653 ************************************************************      ELUIOPGM
00654  00193-DIAGNOSE-RESULT-OF-READQ.                                  ELUIOPGM
00655      MOVE ZERO TO IOP-RET-CD.                                     ELUIOPGM
00656      MOVE EIBRCODE TO IOP-RET-CD-LO.                              ELUIOPGM
00657                                                                   ELUIOPGM
00658                                                                   ELUIOPGM
00659 ************************************************************      ELUIOPGM
00660 *                                                          *      ELUIOPGM
00661 *        IGNORE ERROR CODE                                 *      ELUIOPGM
00662 *                                                          *      ELUIOPGM
00663 ************************************************************      ELUIOPGM
00664  00196-IGNORE-ERROR-CODE.                                         ELUIOPGM
00665      SET IOP-RC-OK TO TRUE.                                       ELUIOPGM
00666      EJECT                                                        ELUIOPGM
00667                                                                   ELUIOPGM
00668                                                                   ELUIOPGM
00669 ************************************************************      ELUIOPGM
00670 *                                                          *      ELUIOPGM
00671 *        DETERMINE PRINT STATUS                            *      ELUIOPGM
00672 *                                                          *      ELUIOPGM
00673 ************************************************************      ELUIOPGM
00674  00198-DETERMINE-PRINT-STATUS.                                    ELUIOPGM
00675      IF IOP-RC-OK OR IOP-RC-NOTFND                                ELUIOPGM
00676          PERFORM 00205-PRINT-NORMAL-COMPLETION                    ELUIOPGM
00677      ELSE IF IOP-RC-INVREQ                                        ELUIOPGM
00678          PERFORM 00208-INDICATE-INVALID-PRINT-R                   ELUIOPGM
00679      ELSE IF IOP-RC-DSIDERR                                       ELUIOPGM
00680          PERFORM 00528-SIGNAL-INVALID-INPUT-OUT                   ELUIOPGM
00681      ELSE IF IOP-RC-IOERR                                         ELUIOPGM
00682          PERFORM 00531-SIGNAL-INPUT-OUTPUT-ERRO                   ELUIOPGM
00683      ELSE IF IOP-RC-NOSPACE                                       ELUIOPGM
00684          PERFORM 00214-PRINT-QUEUE-NO-SPACE                       ELUIOPGM
00685      ELSE PERFORM 00525-SIGNAL-UNKNOWN-ERROR.                     ELUIOPGM
00686      EJECT                                                        ELUIOPGM
00687                                                                   ELUIOPGM
00688                                                                   ELUIOPGM
00689 ************************************************************      ELUIOPGM
00690 *                                                          *      ELUIOPGM
00691 *        PRINT NORMAL COMPLETION                           *      ELUIOPGM
00692 *                                                          *      ELUIOPGM
00693 ************************************************************      ELUIOPGM
00694  00205-PRINT-NORMAL-COMPLETION.                                   ELUIOPGM
00695      MOVE WS-QUEUE-ID              TO  WS-PRINT-Q-NORMAL-ID.      ELUIOPGM
00696      MOVE WS-PRINT-Q-NORMAL        TO  IOP-TSQ-PRINT-MSG.         ELUIOPGM
00697      EJECT                                                        ELUIOPGM
00698                                                                   ELUIOPGM
00699                                                                   ELUIOPGM
00700 ************************************************************      ELUIOPGM
00701 *                                                          *      ELUIOPGM
00702 *        INDICATE INVALID PRINT REQUEST                    *      ELUIOPGM
00703 *                                                          *      ELUIOPGM
00704 ************************************************************      ELUIOPGM
00705  00208-INDICATE-INVALID-PRINT-R.                                  ELUIOPGM
00706      IF IOP-TSQ-PRINTER-ID = SPACES OR LOW-VALUES OR ZEROS        ELUIOPGM
00707          PERFORM 00211-PRINTER-UNDEFINED                          ELUIOPGM
00708      ELSE PERFORM 00528-SIGNAL-INVALID-INPUT-OUT.                 ELUIOPGM
00709                                                                   ELUIOPGM
00710                                                                   ELUIOPGM
00711 ************************************************************      ELUIOPGM
00712 *                                                          *      ELUIOPGM
00713 *        PRINTER UNDEFINED                                 *      ELUIOPGM
00714 *                                                          *      ELUIOPGM
00715 ************************************************************      ELUIOPGM
00716  00211-PRINTER-UNDEFINED.                                         ELUIOPGM
00717      MOVE EIBTRMID  TO WS-NO-PRINTER-TERM.                        ELUIOPGM
00718      MOVE WS-NO-PRINTER-MSG TO IOP-TSQ-PRINT-MSG.                 ELUIOPGM
00719      EJECT                                                        ELUIOPGM
00720                                                                   ELUIOPGM
00721                                                                   ELUIOPGM
00722 ************************************************************      ELUIOPGM
00723 *                                                          *      ELUIOPGM
00724 *        PRINT QUEUE NO SPACE                              *      ELUIOPGM
00725 *                                                          *      ELUIOPGM
00726 ************************************************************      ELUIOPGM
00727  00214-PRINT-QUEUE-NO-SPACE.                                      ELUIOPGM
00728      MOVE WS-PRINT-Q-NO-SPACE      TO  IOP-TSQ-PRINT-MSG.         ELUIOPGM
00729      EJECT                                                        ELUIOPGM
00730                                                                   ELUIOPGM
00731                                                                   ELUIOPGM
00732 ************************************************************      ELUIOPGM
00733 *                                                          *      ELUIOPGM
00734 *        START THE PRINT TASK                              *      ELUIOPGM
00735 *                                                          *      ELUIOPGM
00736 ************************************************************      ELUIOPGM
00737  00216-START-THE-PRINT-TASK.                                      ELUIOPGM
00738      SKIP1                                                        ELUIOPGM
00739 *                                                          *      ELUIOPGM
00740 *        INITIALIZE PRINT PARM BLOCK                       *      ELUIOPGM
00741 *                                                          *      ELUIOPGM
00742          MOVE EIBDATE                  TO  PPB-TRAN-DATE          ELUIOPGM
00743          MOVE EIBTIME                  TO  PPB-TRAN-TIME          ELUIOPGM
00744          MOVE WS-QUEUE-ID              TO  PPB-TSQ-NAME           ELUIOPGM
00745          MOVE EIBTRMID                 TO  PPB-TERM-ID            ELUIOPGM
00746          MOVE IOP-TSQ-PRINTER-ID       TO  PPB-PRINTER-ID         ELUIOPGM
00747          EXEC CICS ASSIGN                                         ELUIOPGM
00748                APPLID(PPB-OWNER-APPLID)                           ELUIOPGM
00749           END-EXEC                                                ELUIOPGM
00750          EXEC CICS ASSIGN                                         ELUIOPGM
00751                SYSID(PPB-OWNER-SYSID)                             ELUIOPGM
00752           END-EXEC.                                               ELUIOPGM
00753      SKIP1                                                        ELUIOPGM
00754      SKIP1                                                        ELUIOPGM
00755 *                                                          *      ELUIOPGM
00756 *        START ELPP                                        *      ELUIOPGM
00757 *                                                          *      ELUIOPGM
00758          EXEC CICS START                                          ELUIOPGM
00759                TRANSID('ELPP')                                    ELUIOPGM
00760                INTERVAL(0)                                        ELUIOPGM
00761                FROM(PPB-PRINT-PARAMETER-BLOCK)                    ELUIOPGM
00762                TERMID(PPB-PRINTER-ID)                             ELUIOPGM
00763           END-EXEC.                                               ELUIOPGM
00764      SKIP1                                                        ELUIOPGM
00765      EJECT                                                        ELUIOPGM
00766                                                                   ELUIOPGM
00767                                                                   ELUIOPGM
00768 ************************************************************      ELUIOPGM
00769 *                                                          *      ELUIOPGM
00770 *        PURGE ALL QUEUES                                  *      ELUIOPGM
00771 *                                                          *      ELUIOPGM
00772 ************************************************************      ELUIOPGM
00773  00238-PURGE-ALL-QUEUES.                                          ELUIOPGM
00774      PERFORM 00240-SEARCH-CIA-STORAGE-TABLE                       ELUIOPGM
00775          VARYING SMA-STG-MGT-IDX FROM 1 BY 1                      ELUIOPGM
00776                      UNTIL SMA-STG-MGT-IDX >                      ELUIOPGM
00777              SMA-NUMBER-OF-ENTRIES.                               ELUIOPGM
00778                                                                   ELUIOPGM
00779                                                                   ELUIOPGM
00780 ************************************************************      ELUIOPGM
00781 *                                                          *      ELUIOPGM
00782 *        SEARCH CIA STORAGE TABLE FOR TSQ                  *      ELUIOPGM
00783 *                                                          *      ELUIOPGM
00784 ************************************************************      ELUIOPGM
00785  00240-SEARCH-CIA-STORAGE-TABLE.                                  ELUIOPGM
00786      IF SMA-TYP-TEMPSTG (SMA-STG-MGT-IDX)                         ELUIOPGM
00787          PERFORM 00242-PURGE-THE-QUEUE.                           ELUIOPGM
00788                                                                   ELUIOPGM
00789                                                                   ELUIOPGM
00790 ************************************************************      ELUIOPGM
00791 *                                                          *      ELUIOPGM
00792 *        PURGE THE QUEUE                                   *      ELUIOPGM
00793 *                                                          *      ELUIOPGM
00794 ************************************************************      ELUIOPGM
00795  00242-PURGE-THE-QUEUE.                                           ELUIOPGM
00796      MOVE SMA-DDN (SMA-STG-MGT-IDX) TO CIA-DDNAME.                ELUIOPGM
00797      SET CIA-STG-GETMAIN TO TRUE.                                 ELUIOPGM
00798      PERFORM 00505-CALL-STORAGE-MANAGER.                          ELUIOPGM
00799      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                   ELUIOPGM
00800          TO SMA-PTR (SMA-STG-MGT-IDX).                            ELUIOPGM
00801      SET IOP-DEL TO TRUE.                                         ELUIOPGM
00802      PERFORM 00460-DELETE-ITEM.                                   ELUIOPGM
00803      EJECT                                                        ELUIOPGM
00804                                                                   ELUIOPGM
00805                                                                   ELUIOPGM
00806 ************************************************************      ELUIOPGM
00807 *                                                          *      ELUIOPGM
00808 *        DIAGNOSE RESULT OF INPUT-OUTPUT FUNCTION          *      ELUIOPGM
00809 *                                                          *      ELUIOPGM
00810 ************************************************************      ELUIOPGM
00811  00250-DIAGNOSE-RESULT-OF-INPUT.                                  ELUIOPGM
00812      MOVE EIBFN TO IOP-CICS-FCN.                                  ELUIOPGM
00813      MOVE ZERO TO IOP-RET-CD.                                     ELUIOPGM
00814      MOVE EIBRCODE TO IOP-RET-CD-LO.                              ELUIOPGM
00815      IF IOP-CICS-TS                                               ELUIOPGM
00816          PERFORM 00257-CONVERT-TEMPORARY-STORAG.                  ELUIOPGM
00817      IF    IOP-RC-DSIDERR                                         ELUIOPGM
00818         OR IOP-RC-ILLOGIC                                         ELUIOPGM
00819         OR IOP-RC-INVREQ                                          ELUIOPGM
00820         OR IOP-RC-IOERR                                           ELUIOPGM
00821         OR IOP-RC-NOSPACE                                         ELUIOPGM
00822         OR IOP-RC-SYSIDERR                                        ELUIOPGM
00823         OR IOP-RC-ISCINVREQ                                       ELUIOPGM
00824         OR IOP-RC-LENGERR                                         ELUIOPGM
00825          PERFORM 00277-SIGNAL-CRITICAL-INPUT-OU                   ELUIOPGM
00826      ELSE IF IOP-RC-NOTOPEN                                       ELUIOPGM
00827          PERFORM 00280-SIGNAL-FILE-NOT-OPEN-ERR.                  ELUIOPGM
00828      EJECT                                                        ELUIOPGM
00829                                                                   ELUIOPGM
00830                                                                   ELUIOPGM
00831 ************************************************************      ELUIOPGM
00832 *                                                          *      ELUIOPGM
00833 *        CONVERT TEMPORARY STORAGE ERROR TO FILE CONTROL EQ*      ELUIOPGM
00834 *                                                          *      ELUIOPGM
00835 ************************************************************      ELUIOPGM
00836  00257-CONVERT-TEMPORARY-STORAG.                                  ELUIOPGM
00837 * ERRORS OTHER THAN THOSE LISTED BELOW ARE NOT CONVERTED          ELUIOPGM
00838      IF IOP-RC-TS-ITEMERR                                         ELUIOPGM
00839          PERFORM 00265-CONVERT-ITEMERR-TO-NOTFN                   ELUIOPGM
00840      ELSE IF IOP-RC-TS-QIDERR AND NOT IOP-DEL                     ELUIOPGM
00841          PERFORM 00267-CONVERT-QIDERR-TO-DSIDER                   ELUIOPGM
00842      ELSE IF IOP-RC-TS-QIDERR AND IOP-DEL                         ELUIOPGM
00843          PERFORM 00269-CONVERT-QIDERR-TO-NO-ERR                   ELUIOPGM
00844      ELSE IF IOP-RC-TS-IOERR                                      ELUIOPGM
00845          PERFORM 00271-CONVERT-IOERR-TO-IOERR                     ELUIOPGM
00846      ELSE IF IOP-RC-TS-NOSPACE                                    ELUIOPGM
00847          PERFORM 00273-CONVERT-NOSPACE-TO-NOSPA                   ELUIOPGM
00848      ELSE IF IOP-RC-TS-INVREQ                                     ELUIOPGM
00849          PERFORM 00275-CONVERT-INVREQ-TO-INVREQ.                  ELUIOPGM
00850      EJECT                                                        ELUIOPGM
00851                                                                   ELUIOPGM
00852                                                                   ELUIOPGM
00853 ************************************************************      ELUIOPGM
00854 *                                                          *      ELUIOPGM
00855 *        CONVERT ITEMERR TO NOTFND                         *      ELUIOPGM
00856 *                                                          *      ELUIOPGM
00857 ************************************************************      ELUIOPGM
00858  00265-CONVERT-ITEMERR-TO-NOTFN.                                  ELUIOPGM
00859      SET IOP-RC-NOTFND TO TRUE.                                   ELUIOPGM
00860      EJECT                                                        ELUIOPGM
00861                                                                   ELUIOPGM
00862                                                                   ELUIOPGM
00863 ************************************************************      ELUIOPGM
00864 *                                                          *      ELUIOPGM
00865 *        CONVERT QIDERR TO DSIDERR                         *      ELUIOPGM
00866 *                                                          *      ELUIOPGM
00867 ************************************************************      ELUIOPGM
00868  00267-CONVERT-QIDERR-TO-DSIDER.                                  ELUIOPGM
00869      SET IOP-RC-DSIDERR TO TRUE.                                  ELUIOPGM
00870      EJECT                                                        ELUIOPGM
00871                                                                   ELUIOPGM
00872                                                                   ELUIOPGM
00873 ************************************************************      ELUIOPGM
00874 *                                                          *      ELUIOPGM
00875 *        CONVERT QIDERR TO NO ERROR                        *      ELUIOPGM
00876 *                                                          *      ELUIOPGM
00877 ************************************************************      ELUIOPGM
00878  00269-CONVERT-QIDERR-TO-NO-ERR.                                  ELUIOPGM
00879      SET IOP-RC-OK TO TRUE.                                       ELUIOPGM
00880      EJECT                                                        ELUIOPGM
00881                                                                   ELUIOPGM
00882                                                                   ELUIOPGM
00883 ************************************************************      ELUIOPGM
00884 *                                                          *      ELUIOPGM
00885 *        CONVERT IOERR TO IOERR                            *      ELUIOPGM
00886 *                                                          *      ELUIOPGM
00887 ************************************************************      ELUIOPGM
00888  00271-CONVERT-IOERR-TO-IOERR.                                    ELUIOPGM
00889      SET IOP-RC-IOERR TO TRUE.                                    ELUIOPGM
00890      EJECT                                                        ELUIOPGM
00891                                                                   ELUIOPGM
00892                                                                   ELUIOPGM
00893 ************************************************************      ELUIOPGM
00894 *                                                          *      ELUIOPGM
00895 *        CONVERT NOSPACE TO NOSPACE                        *      ELUIOPGM
00896 *                                                          *      ELUIOPGM
00897 ************************************************************      ELUIOPGM
00898  00273-CONVERT-NOSPACE-TO-NOSPA.                                  ELUIOPGM
00899      SET IOP-RC-NOSPACE TO TRUE.                                  ELUIOPGM
00900      EJECT                                                        ELUIOPGM
00901                                                                   ELUIOPGM
00902                                                                   ELUIOPGM
00903 ************************************************************      ELUIOPGM
00904 *                                                          *      ELUIOPGM
00905 *        CONVERT INVREQ TO INVREQ                          *      ELUIOPGM
00906 *                                                          *      ELUIOPGM
00907 ************************************************************      ELUIOPGM
00908  00275-CONVERT-INVREQ-TO-INVREQ.                                  ELUIOPGM
00909      SET IOP-RC-INVREQ TO TRUE.                                   ELUIOPGM
00910                                                                   ELUIOPGM
00911                                                                   ELUIOPGM
00912 ************************************************************      ELUIOPGM
00913 *                                                          *      ELUIOPGM
00914 *        SIGNAL CRITICAL INPUT-OUTPUT ERROR                *      ELUIOPGM
00915 *                                                          *      ELUIOPGM
00916 ************************************************************      ELUIOPGM
00917  00277-SIGNAL-CRITICAL-INPUT-OU.                                  ELUIOPGM
00918      SET CIA-AB-CRITIO TO TRUE.                                   ELUIOPGM
00919      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUIOPGM
00920                                                                   ELUIOPGM
00921                                                                   ELUIOPGM
00922 ************************************************************      ELUIOPGM
00923 *                                                          *      ELUIOPGM
00924 *        SIGNAL FILE NOT OPEN ERROR                        *      ELUIOPGM
00925 *                                                          *      ELUIOPGM
00926 ************************************************************      ELUIOPGM
00927  00280-SIGNAL-FILE-NOT-OPEN-ERR.                                  ELUIOPGM
00928      SET CIA-AB-NOTOPEN TO TRUE.                                  ELUIOPGM
00929      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUIOPGM
00930      EJECT                                                        ELUIOPGM
00931                                                                   ELUIOPGM
00932                                                                   ELUIOPGM
00933 ************************************************************      ELUIOPGM
00934 *                                                          *      ELUIOPGM
00935 *        ADD RECORD                                        *      ELUIOPGM
00936 *                                                          *      ELUIOPGM
00937 ************************************************************      ELUIOPGM
00938  00283-ADD-RECORD.                                                ELUIOPGM
00939      IF IOP-FCQ-NONE                                              ELUIOPGM
00940          PERFORM 00287-ADD-RECORD-RANDOM                          ELUIOPGM
00941      ELSE IF IOP-FCQ-MAS                                          ELUIOPGM
00942          PERFORM 00294-ADD-RECORD-MASS-INSERT                     ELUIOPGM
00943      ELSE PERFORM 00528-SIGNAL-INVALID-INPUT-OUT.                 ELUIOPGM
00944                                                                   ELUIOPGM
00945                                                                   ELUIOPGM
00946 ************************************************************      ELUIOPGM
00947 *                                                          *      ELUIOPGM
00948 *        ADD RECORD RANDOM                                 *      ELUIOPGM
00949 *                                                          *      ELUIOPGM
00950 ************************************************************      ELUIOPGM
00951  00287-ADD-RECORD-RANDOM.                                         ELUIOPGM
00952      EXEC CICS WRITE                                              ELUIOPGM
00953                DATASET (IOP-FILE-DDNAME)                          ELUIOPGM
00954                FROM    (LS-INPUT-OUTPUT-AREA)                     ELUIOPGM
00955                LENGTH  (IOP-REC-LEN)                              ELUIOPGM
00956                RIDFLD  (IOP-FILE-KEY)                             ELUIOPGM
00957                END-EXEC.                                          ELUIOPGM
00958      EJECT                                                        ELUIOPGM
00959                                                                   ELUIOPGM
00960                                                                   ELUIOPGM
00961 ************************************************************      ELUIOPGM
00962 *                                                          *      ELUIOPGM
00963 *        ADD RECORD MASS INSERT                            *      ELUIOPGM
00964 *                                                          *      ELUIOPGM
00965 ************************************************************      ELUIOPGM
00966  00294-ADD-RECORD-MASS-INSERT.                                    ELUIOPGM
00967      EXEC CICS WRITE                                              ELUIOPGM
00968                DATASET (IOP-FILE-DDNAME)                          ELUIOPGM
00969                FROM    (LS-INPUT-OUTPUT-AREA)                     ELUIOPGM
00970                LENGTH  (IOP-REC-LEN)                              ELUIOPGM
00971                MASSINSERT                                         ELUIOPGM
00972                RIDFLD  (IOP-FILE-KEY)                             ELUIOPGM
00973                END-EXEC.                                          ELUIOPGM
00974      EJECT                                                        ELUIOPGM
00975                                                                   ELUIOPGM
00976                                                                   ELUIOPGM
00977 ************************************************************      ELUIOPGM
00978 *                                                          *      ELUIOPGM
00979 *        DELETE RECORD                                     *      ELUIOPGM
00980 *                                                          *      ELUIOPGM
00981 ************************************************************      ELUIOPGM
00982  00302-DELETE-RECORD.                                             ELUIOPGM
00983      IF IOP-FCQ-NONE                                              ELUIOPGM
00984          PERFORM 00307-DELETE-RECORD-BY-KEY                       ELUIOPGM
00985      ELSE IF IOP-FCQ-GEN                                          ELUIOPGM
00986          PERFORM 00312-DELETE-RECORD-BY-GENERIC                   ELUIOPGM
00987      ELSE IF IOP-FCQ-UPD                                          ELUIOPGM
00988          PERFORM 00320-DELETE-RECORD-HELD-FOR-U                   ELUIOPGM
00989      ELSE PERFORM 00528-SIGNAL-INVALID-INPUT-OUT.                 ELUIOPGM
00990                                                                   ELUIOPGM
00991                                                                   ELUIOPGM
00992 ************************************************************      ELUIOPGM
00993 *                                                          *      ELUIOPGM
00994 *        DELETE RECORD BY KEY                              *      ELUIOPGM
00995 *                                                          *      ELUIOPGM
00996 ************************************************************      ELUIOPGM
00997  00307-DELETE-RECORD-BY-KEY.                                      ELUIOPGM
00998      EXEC CICS DELETE                                             ELUIOPGM
00999                DATASET (IOP-FILE-DDNAME)                          ELUIOPGM
01000                RIDFLD  (IOP-FILE-KEY)                             ELUIOPGM
01001                END-EXEC.                                          ELUIOPGM
01002      EJECT                                                        ELUIOPGM
01003                                                                   ELUIOPGM
01004                                                                   ELUIOPGM
01005 ************************************************************      ELUIOPGM
01006 *                                                          *      ELUIOPGM
01007 *        DELETE RECORD BY GENERIC KEY                      *      ELUIOPGM
01008 *                                                          *      ELUIOPGM
01009 ************************************************************      ELUIOPGM
01010  00312-DELETE-RECORD-BY-GENERIC.                                  ELUIOPGM
01011      EXEC CICS DELETE                                             ELUIOPGM
01012                DATASET   (IOP-FILE-DDNAME)                        ELUIOPGM
01013                GENERIC                                            ELUIOPGM
01014                KEYLENGTH (IOP-KEY-LEN)                            ELUIOPGM
01015                NUMREC    (IOP-NBR-RECS-DEL)                       ELUIOPGM
01016                RIDFLD    (IOP-FILE-KEY)                           ELUIOPGM
01017                END-EXEC.                                          ELUIOPGM
01018                                                                   ELUIOPGM
01019                                                                   ELUIOPGM
01020 ************************************************************      ELUIOPGM
01021 *                                                          *      ELUIOPGM
01022 *        DELETE RECORD HELD FOR UPDATE                     *      ELUIOPGM
01023 *                                                          *      ELUIOPGM
01024 ************************************************************      ELUIOPGM
01025  00320-DELETE-RECORD-HELD-FOR-U.                                  ELUIOPGM
01026      EXEC CICS DELETE                                             ELUIOPGM
01027                DATASET (IOP-FILE-DDNAME)                          ELUIOPGM
01028                END-EXEC.                                          ELUIOPGM
01029      EJECT                                                        ELUIOPGM
01030                                                                   ELUIOPGM
01031                                                                   ELUIOPGM
01032 ************************************************************      ELUIOPGM
01033 *                                                          *      ELUIOPGM
01034 *        END BROWSE                                        *      ELUIOPGM
01035 *                                                          *      ELUIOPGM
01036 ************************************************************      ELUIOPGM
01037  00324-END-BROWSE.                                                ELUIOPGM
01038      EXEC CICS ENDBR                                              ELUIOPGM
01039                DATASET (IOP-FILE-DDNAME)                          ELUIOPGM
01040                REQID   (IOP-BROWSE-REQ-ID)                        ELUIOPGM
01041                END-EXEC.                                          ELUIOPGM
01042      EJECT                                                        ELUIOPGM
01043                                                                   ELUIOPGM
01044                                                                   ELUIOPGM
01045 ************************************************************      ELUIOPGM
01046 *                                                          *      ELUIOPGM
01047 *        READ RECORD                                       *      ELUIOPGM
01048 *                                                          *      ELUIOPGM
01049 ************************************************************      ELUIOPGM
01050  00329-READ-RECORD.                                               ELUIOPGM
01051      IF IOP-FCQ-NONE                                              ELUIOPGM
01052          PERFORM 00334-READ-RECORD-RANDOM                         ELUIOPGM
01053      ELSE IF IOP-FCQ-GEN                                          ELUIOPGM
01054          PERFORM 00354-READ-RECORD-GENERIC                        ELUIOPGM
01055      ELSE IF IOP-FCQ-UPD                                          ELUIOPGM
01056          PERFORM 00378-READ-RECORD-RANDOM-FOR-U                   ELUIOPGM
01057      ELSE PERFORM 00528-SIGNAL-INVALID-INPUT-OUT.                 ELUIOPGM
01058      EJECT                                                        ELUIOPGM
01059                                                                   ELUIOPGM
01060                                                                   ELUIOPGM
01061 ************************************************************      ELUIOPGM
01062 *                                                          *      ELUIOPGM
01063 *        READ RECORD RANDOM                                *      ELUIOPGM
01064 *                                                          *      ELUIOPGM
01065 ************************************************************      ELUIOPGM
01066  00334-READ-RECORD-RANDOM.                                        ELUIOPGM
01067      IF    IOP-KVQ-EQ                                             ELUIOPGM
01068         OR IOP-KVQ-NONE                                           ELUIOPGM
01069          PERFORM 00338-READ-RECORD-RANDOM-BY-KE                   ELUIOPGM
01070      ELSE IF IOP-KVQ-GTE                                          ELUIOPGM
01071          PERFORM 00346-READ-RECORD-RANDOM-BY-NE                   ELUIOPGM
01072      ELSE PERFORM 00528-SIGNAL-INVALID-INPUT-OUT.                 ELUIOPGM
01073                                                                   ELUIOPGM
01074                                                                   ELUIOPGM
01075 ************************************************************      ELUIOPGM
01076 *                                                          *      ELUIOPGM
01077 *        READ RECORD RANDOM BY KEY                         *      ELUIOPGM
01078 *                                                          *      ELUIOPGM
01079 ************************************************************      ELUIOPGM
01080  00338-READ-RECORD-RANDOM-BY-KE.                                  ELUIOPGM
01081      EXEC CICS READ                                               ELUIOPGM
01082                DATASET (IOP-FILE-DDNAME)                          ELUIOPGM
01083                EQUAL                                              ELUIOPGM
01084                LENGTH  (IOP-FC-REC-LEN)                           ELUIOPGM
01085                RIDFLD  (IOP-FILE-KEY)                             ELUIOPGM
01086                SET     (IOP-FC-REC-PTR)                           ELUIOPGM
01087                END-EXEC.                                          ELUIOPGM
01088      EJECT                                                        ELUIOPGM
01089                                                                   ELUIOPGM
01090                                                                   ELUIOPGM
01091 ************************************************************      ELUIOPGM
01092 *                                                          *      ELUIOPGM
01093 *        READ RECORD RANDOM BY NEXT KEY                    *      ELUIOPGM
01094 *                                                          *      ELUIOPGM
01095 ************************************************************      ELUIOPGM
01096  00346-READ-RECORD-RANDOM-BY-NE.                                  ELUIOPGM
01097      EXEC CICS READ                                               ELUIOPGM
01098                DATASET   (IOP-FILE-DDNAME)                        ELUIOPGM
01099                GTEQ                                               ELUIOPGM
01100                LENGTH    (IOP-FC-REC-LEN)                         ELUIOPGM
01101                RIDFLD    (IOP-FILE-KEY)                           ELUIOPGM
01102                SET       (IOP-FC-REC-PTR)                         ELUIOPGM
01103                END-EXEC.                                          ELUIOPGM
01104      EJECT                                                        ELUIOPGM
01105                                                                   ELUIOPGM
01106                                                                   ELUIOPGM
01107 ************************************************************      ELUIOPGM
01108 *                                                          *      ELUIOPGM
01109 *        READ RECORD GENERIC                               *      ELUIOPGM
01110 *                                                          *      ELUIOPGM
01111 ************************************************************      ELUIOPGM
01112  00354-READ-RECORD-GENERIC.                                       ELUIOPGM
01113      IF    IOP-KVQ-EQ                                             ELUIOPGM
01114         OR IOP-KVQ-NONE                                           ELUIOPGM
01115          PERFORM 00358-READ-RECORD-GENERIC-BY-K                   ELUIOPGM
01116      ELSE IF IOP-KVQ-GTE                                          ELUIOPGM
01117          PERFORM 00368-READ-RECORD-GENERIC-BY-N                   ELUIOPGM
01118      ELSE PERFORM 00528-SIGNAL-INVALID-INPUT-OUT.                 ELUIOPGM
01119                                                                   ELUIOPGM
01120                                                                   ELUIOPGM
01121 ************************************************************      ELUIOPGM
01122 *                                                          *      ELUIOPGM
01123 *        READ RECORD GENERIC BY KEY                        *      ELUIOPGM
01124 *                                                          *      ELUIOPGM
01125 ************************************************************      ELUIOPGM
01126  00358-READ-RECORD-GENERIC-BY-K.                                  ELUIOPGM
01127      EXEC CICS READ                                               ELUIOPGM
01128                DATASET   (IOP-FILE-DDNAME)                        ELUIOPGM
01129                EQUAL                                              ELUIOPGM
01130                GENERIC                                            ELUIOPGM
01131                KEYLENGTH (IOP-KEY-LEN)                            ELUIOPGM
01132                LENGTH    (IOP-FC-REC-LEN)                         ELUIOPGM
01133                RIDFLD    (IOP-FILE-KEY)                           ELUIOPGM
01134                SET       (IOP-FC-REC-PTR)                         ELUIOPGM
01135                END-EXEC.                                          ELUIOPGM
01136      EJECT                                                        ELUIOPGM
01137                                                                   ELUIOPGM
01138                                                                   ELUIOPGM
01139 ************************************************************      ELUIOPGM
01140 *                                                          *      ELUIOPGM
01141 *        READ RECORD GENERIC BY NEXT KEY                   *      ELUIOPGM
01142 *                                                          *      ELUIOPGM
01143 ************************************************************      ELUIOPGM
01144  00368-READ-RECORD-GENERIC-BY-N.                                  ELUIOPGM
01145      EXEC CICS READ                                               ELUIOPGM
01146                DATASET   (IOP-FILE-DDNAME)                        ELUIOPGM
01147                GTEQ                                               ELUIOPGM
01148                GENERIC                                            ELUIOPGM
01149                KEYLENGTH (IOP-KEY-LEN)                            ELUIOPGM
01150                LENGTH    (IOP-FC-REC-LEN)                         ELUIOPGM
01151                RIDFLD    (IOP-FILE-KEY)                           ELUIOPGM
01152                SET       (IOP-FC-REC-PTR)                         ELUIOPGM
01153                END-EXEC.                                          ELUIOPGM
01154      EJECT                                                        ELUIOPGM
01155                                                                   ELUIOPGM
01156                                                                   ELUIOPGM
01157 ************************************************************      ELUIOPGM
01158 *                                                          *      ELUIOPGM
01159 *        READ RECORD RANDOM FOR UPDATE                     *      ELUIOPGM
01160 *                                                          *      ELUIOPGM
01161 ************************************************************      ELUIOPGM
01162  00378-READ-RECORD-RANDOM-FOR-U.                                  ELUIOPGM
01163      EXEC CICS READ                                               ELUIOPGM
01164                DATASET   (IOP-FILE-DDNAME)                        ELUIOPGM
01165                EQUAL                                              ELUIOPGM
01166                LENGTH    (IOP-FC-REC-LEN)                         ELUIOPGM
01167                RIDFLD    (IOP-FILE-KEY)                           ELUIOPGM
01168                SET       (IOP-FC-REC-PTR)                         ELUIOPGM
01169                UPDATE                                             ELUIOPGM
01170                END-EXEC.                                          ELUIOPGM
01171      EJECT                                                        ELUIOPGM
01172                                                                   ELUIOPGM
01173                                                                   ELUIOPGM
01174 ************************************************************      ELUIOPGM
01175 *                                                          *      ELUIOPGM
01176 *        READ RECORD NEXT                                  *      ELUIOPGM
01177 *                                                          *      ELUIOPGM
01178 ************************************************************      ELUIOPGM
01179  00387-READ-RECORD-NEXT.                                          ELUIOPGM
01180      EXEC CICS READNEXT                                           ELUIOPGM
01181                DATASET (IOP-FILE-DDNAME)                          ELUIOPGM
01182                LENGTH  (IOP-FC-REC-LEN)                           ELUIOPGM
01183                REQID   (IOP-BROWSE-REQ-ID)                        ELUIOPGM
01184                RIDFLD  (IOP-FILE-KEY)                             ELUIOPGM
01185                SET     (IOP-FC-REC-PTR)                           ELUIOPGM
01186                END-EXEC.                                          ELUIOPGM
01187      EJECT                                                        ELUIOPGM
01188                                                                   ELUIOPGM
01189                                                                   ELUIOPGM
01190 ************************************************************      ELUIOPGM
01191 *                                                          *      ELUIOPGM
01192 *        READ RECORD PREVIOUS                              *      ELUIOPGM
01193 *                                                          *      ELUIOPGM
01194 ************************************************************      ELUIOPGM
01195  00395-READ-RECORD-PREVIOUS.                                      ELUIOPGM
01196      EXEC CICS READPREV                                           ELUIOPGM
01197                DATASET (IOP-FILE-DDNAME)                          ELUIOPGM
01198                LENGTH  (IOP-FC-REC-LEN)                           ELUIOPGM
01199                REQID   (IOP-BROWSE-REQ-ID)                        ELUIOPGM
01200                RIDFLD  (IOP-FILE-KEY)                             ELUIOPGM
01201                SET     (IOP-FC-REC-PTR)                           ELUIOPGM
01202                END-EXEC.                                          ELUIOPGM
01203      EJECT                                                        ELUIOPGM
01204                                                                   ELUIOPGM
01205                                                                   ELUIOPGM
01206 ************************************************************      ELUIOPGM
01207 *                                                          *      ELUIOPGM
01208 *        START BROWSE                                      *      ELUIOPGM
01209 *                                                          *      ELUIOPGM
01210 ************************************************************      ELUIOPGM
01211  00403-START-BROWSE.                                              ELUIOPGM
01212      IF IOP-FCQ-NONE                                              ELUIOPGM
01213          PERFORM 00407-START-BROWSE-ON-SPECIFIC                   ELUIOPGM
01214      ELSE IF IOP-FCQ-GEN                                          ELUIOPGM
01215          PERFORM 00423-START-BROWSE-ON-GENERICX                   ELUIOPGM
01216      ELSE PERFORM 00528-SIGNAL-INVALID-INPUT-OUT.                 ELUIOPGM
01217                                                                   ELUIOPGM
01218                                                                   ELUIOPGM
01219 ************************************************************      ELUIOPGM
01220 *                                                          *      ELUIOPGM
01221 *        START BROWSE ON SPECIFIC KEY                      *      ELUIOPGM
01222 *                                                          *      ELUIOPGM
01223 ************************************************************      ELUIOPGM
01224  00407-START-BROWSE-ON-SPECIFIC.                                  ELUIOPGM
01225      IF IOP-KVQ-EQ                                                ELUIOPGM
01226          PERFORM 00411-START-BROWSE-ON-EXACT-KE                   ELUIOPGM
01227      ELSE IF IOP-KVQ-GTE                                          ELUIOPGM
01228          PERFORM 00417-START-BROWSE-ON-NEXT-KEY                   ELUIOPGM
01229      ELSE PERFORM 00528-SIGNAL-INVALID-INPUT-OUT.                 ELUIOPGM
01230      EJECT                                                        ELUIOPGM
01231                                                                   ELUIOPGM
01232                                                                   ELUIOPGM
01233 ************************************************************      ELUIOPGM
01234 *                                                          *      ELUIOPGM
01235 *        START BROWSE ON EXACT KEY                         *      ELUIOPGM
01236 *                                                          *      ELUIOPGM
01237 ************************************************************      ELUIOPGM
01238  00411-START-BROWSE-ON-EXACT-KE.                                  ELUIOPGM
01239      EXEC CICS STARTBR                                            ELUIOPGM
01240                DATASET (IOP-FILE-DDNAME)                          ELUIOPGM
01241                EQUAL                                              ELUIOPGM
01242                RIDFLD  (IOP-FILE-KEY)                             ELUIOPGM
01243                END-EXEC.                                          ELUIOPGM
01244      EJECT                                                        ELUIOPGM
01245                                                                   ELUIOPGM
01246                                                                   ELUIOPGM
01247 ************************************************************      ELUIOPGM
01248 *                                                          *      ELUIOPGM
01249 *        START BROWSE ON NEXT KEY                          *      ELUIOPGM
01250 *                                                          *      ELUIOPGM
01251 ************************************************************      ELUIOPGM
01252  00417-START-BROWSE-ON-NEXT-KEY.                                  ELUIOPGM
01253      EXEC CICS STARTBR                                            ELUIOPGM
01254                DATASET (IOP-FILE-DDNAME)                          ELUIOPGM
01255                GTEQ                                               ELUIOPGM
01256                RIDFLD  (IOP-FILE-KEY)                             ELUIOPGM
01257                END-EXEC.                                          ELUIOPGM
01258      EJECT                                                        ELUIOPGM
01259                                                                   ELUIOPGM
01260                                                                   ELUIOPGM
01261 ************************************************************      ELUIOPGM
01262 *                                                          *      ELUIOPGM
01263 *        START BROWSE ON GENERIC KEY                       *      ELUIOPGM
01264 *                                                          *      ELUIOPGM
01265 ************************************************************      ELUIOPGM
01266  00423-START-BROWSE-ON-GENERICX.                                  ELUIOPGM
01267      IF IOP-KVQ-EQ                                                ELUIOPGM
01268          PERFORM 00427-START-BROWSE-ON-EXACT-GE                   ELUIOPGM
01269      ELSE IF IOP-KVQ-GTE                                          ELUIOPGM
01270          PERFORM 00435-START-BROWSE-ON-NEXT-GEN                   ELUIOPGM
01271      ELSE PERFORM 00528-SIGNAL-INVALID-INPUT-OUT.                 ELUIOPGM
01272                                                                   ELUIOPGM
01273                                                                   ELUIOPGM
01274 ************************************************************      ELUIOPGM
01275 *                                                          *      ELUIOPGM
01276 *        START BROWSE ON EXACT GENERIC KEY                 *      ELUIOPGM
01277 *                                                          *      ELUIOPGM
01278 ************************************************************      ELUIOPGM
01279  00427-START-BROWSE-ON-EXACT-GE.                                  ELUIOPGM
01280      EXEC CICS STARTBR                                            ELUIOPGM
01281                DATASET   (IOP-FILE-DDNAME)                        ELUIOPGM
01282                EQUAL                                              ELUIOPGM
01283                GENERIC                                            ELUIOPGM
01284                KEYLENGTH (IOP-KEY-LEN)                            ELUIOPGM
01285                RIDFLD    (IOP-FILE-KEY)                           ELUIOPGM
01286                END-EXEC.                                          ELUIOPGM
01287      EJECT                                                        ELUIOPGM
01288                                                                   ELUIOPGM
01289                                                                   ELUIOPGM
01290 ************************************************************      ELUIOPGM
01291 *                                                          *      ELUIOPGM
01292 *        START BROWSE ON NEXT GENERIC KEY                  *      ELUIOPGM
01293 *                                                          *      ELUIOPGM
01294 ************************************************************      ELUIOPGM
01295  00435-START-BROWSE-ON-NEXT-GEN.                                  ELUIOPGM
01296      EXEC CICS STARTBR                                            ELUIOPGM
01297                DATASET   (IOP-FILE-DDNAME)                        ELUIOPGM
01298                GTEQ                                               ELUIOPGM
01299                GENERIC                                            ELUIOPGM
01300                KEYLENGTH (IOP-KEY-LEN)                            ELUIOPGM
01301                RIDFLD    (IOP-FILE-KEY)                           ELUIOPGM
01302                END-EXEC.                                          ELUIOPGM
01303      EJECT                                                        ELUIOPGM
01304                                                                   ELUIOPGM
01305                                                                   ELUIOPGM
01306 ************************************************************      ELUIOPGM
01307 *                                                          *      ELUIOPGM
01308 *        UNLOCK RECORD HELD FOR UPDATE                     *      ELUIOPGM
01309 *                                                          *      ELUIOPGM
01310 ************************************************************      ELUIOPGM
01311  00443-UNLOCK-RECORD-HELD-FOR-U.                                  ELUIOPGM
01312      EXEC CICS UNLOCK                                             ELUIOPGM
01313                DATASET (IOP-FILE-DDNAME)                          ELUIOPGM
01314                END-EXEC.                                          ELUIOPGM
01315      EJECT                                                        ELUIOPGM
01316                                                                   ELUIOPGM
01317                                                                   ELUIOPGM
01318 ************************************************************      ELUIOPGM
01319 *                                                          *      ELUIOPGM
01320 *        UPDATE RECORD                                     *      ELUIOPGM
01321 *                                                          *      ELUIOPGM
01322 ************************************************************      ELUIOPGM
01323  00447-UPDATE-RECORD.                                             ELUIOPGM
01324      EXEC CICS REWRITE                                            ELUIOPGM
01325                DATASET (IOP-FILE-DDNAME)                          ELUIOPGM
01326                FROM    (LS-INPUT-OUTPUT-AREA)                     ELUIOPGM
01327                LENGTH  (IOP-REC-LEN)                              ELUIOPGM
01328                END-EXEC.                                          ELUIOPGM
01329      EJECT                                                        ELUIOPGM
01330                                                                   ELUIOPGM
01331                                                                   ELUIOPGM
01332 ************************************************************      ELUIOPGM
01333 *                                                          *      ELUIOPGM
01334 *        ADD ITEM                                          *      ELUIOPGM
01335 *                                                          *      ELUIOPGM
01336 ************************************************************      ELUIOPGM
01337  00453-ADD-ITEM.                                                  ELUIOPGM
01338      EXEC CICS WRITEQ TS                                          ELUIOPGM
01339                FROM   (LS-INPUT-OUTPUT-AREA)                      ELUIOPGM
01340                ITEM   (IOP-TSQ-ITEM-NBR)                          ELUIOPGM
01341                LENGTH (IOP-REC-LEN)                               ELUIOPGM
01342                QUEUE  (IOP-TSQ-ID)                                ELUIOPGM
01343                END-EXEC.                                          ELUIOPGM
01344      EJECT                                                        ELUIOPGM
01345                                                                   ELUIOPGM
01346                                                                   ELUIOPGM
01347 ************************************************************      ELUIOPGM
01348 *                                                          *      ELUIOPGM
01349 *        DELETE ITEM                                       *      ELUIOPGM
01350 *                                                          *      ELUIOPGM
01351 ************************************************************      ELUIOPGM
01352  00460-DELETE-ITEM.                                               ELUIOPGM
01353      EXEC CICS DELETEQ TS                                         ELUIOPGM
01354                QUEUE (IOP-TSQ-ID)                                 ELUIOPGM
01355                END-EXEC.                                          ELUIOPGM
01356      EJECT                                                        ELUIOPGM
01357                                                                   ELUIOPGM
01358                                                                   ELUIOPGM
01359 ************************************************************      ELUIOPGM
01360 *                                                          *      ELUIOPGM
01361 *        END QUEUE BROWSE                                  *      ELUIOPGM
01362 *                                                          *      ELUIOPGM
01363 ************************************************************      ELUIOPGM
01364  00464-END-QUEUE-BROWSE.                                          ELUIOPGM
01365      CONTINUE.                                                    ELUIOPGM
01366      EJECT                                                        ELUIOPGM
01367                                                                   ELUIOPGM
01368                                                                   ELUIOPGM
01369 ************************************************************      ELUIOPGM
01370 *                                                          *      ELUIOPGM
01371 *        READ ITEM                                         *      ELUIOPGM
01372 *                                                          *      ELUIOPGM
01373 ************************************************************      ELUIOPGM
01374  00466-READ-ITEM.                                                 ELUIOPGM
01375      EXEC CICS READQ TS                                           ELUIOPGM
01376                ITEM   (IOP-TSQ-ITEM-NBR)                          ELUIOPGM
01377                LENGTH (IOP-FC-REC-LEN)                            ELUIOPGM
01378                QUEUE  (IOP-TSQ-ID)                                ELUIOPGM
01379                SET    (IOP-FC-REC-PTR)                            ELUIOPGM
01380                END-EXEC.                                          ELUIOPGM
01381      EJECT                                                        ELUIOPGM
01382                                                                   ELUIOPGM
01383                                                                   ELUIOPGM
01384 ************************************************************      ELUIOPGM
01385 *                                                          *      ELUIOPGM
01386 *        READ ITEM NEXT                                    *      ELUIOPGM
01387 *                                                          *      ELUIOPGM
01388 ************************************************************      ELUIOPGM
01389  00473-READ-ITEM-NEXT.                                            ELUIOPGM
01390      ADD 1 TO IOP-TSQ-ITEM-NBR.                                   ELUIOPGM
01391      EXEC CICS READQ TS                                           ELUIOPGM
01392                ITEM   (IOP-TSQ-ITEM-NBR)                          ELUIOPGM
01393                LENGTH (IOP-FC-REC-LEN)                            ELUIOPGM
01394                QUEUE  (IOP-TSQ-ID)                                ELUIOPGM
01395                SET    (IOP-FC-REC-PTR)                            ELUIOPGM
01396                END-EXEC.                                          ELUIOPGM
01397      EJECT                                                        ELUIOPGM
01398                                                                   ELUIOPGM
01399                                                                   ELUIOPGM
01400 ************************************************************      ELUIOPGM
01401 *                                                          *      ELUIOPGM
01402 *        READ ITEM PREVIOUS                                *      ELUIOPGM
01403 *                                                          *      ELUIOPGM
01404 ************************************************************      ELUIOPGM
01405  00481-READ-ITEM-PREVIOUS.                                        ELUIOPGM
01406      SUBTRACT 1 FROM IOP-TSQ-ITEM-NBR.                            ELUIOPGM
01407      EXEC CICS READQ TS                                           ELUIOPGM
01408                ITEM   (IOP-TSQ-ITEM-NBR)                          ELUIOPGM
01409                LENGTH (IOP-FC-REC-LEN)                            ELUIOPGM
01410                QUEUE  (IOP-TSQ-ID)                                ELUIOPGM
01411                SET    (IOP-FC-REC-PTR)                            ELUIOPGM
01412                END-EXEC.                                          ELUIOPGM
01413      EJECT                                                        ELUIOPGM
01414                                                                   ELUIOPGM
01415                                                                   ELUIOPGM
01416 ************************************************************      ELUIOPGM
01417 *                                                          *      ELUIOPGM
01418 *        START QUEUE BROWSE                                *      ELUIOPGM
01419 *                                                          *      ELUIOPGM
01420 ************************************************************      ELUIOPGM
01421  00489-START-QUEUE-BROWSE.                                        ELUIOPGM
01422      EXEC CICS READQ TS                                           ELUIOPGM
01423                ITEM   (IOP-TSQ-ITEM-NBR)                          ELUIOPGM
01424                LENGTH (IOP-FC-REC-LEN)                            ELUIOPGM
01425                QUEUE  (IOP-TSQ-ID)                                ELUIOPGM
01426                SET    (IOP-FC-REC-PTR)                            ELUIOPGM
01427                END-EXEC.                                          ELUIOPGM
01428      SUBTRACT 1 FROM IOP-TSQ-ITEM-NBR.                            ELUIOPGM
01429      EJECT                                                        ELUIOPGM
01430                                                                   ELUIOPGM
01431                                                                   ELUIOPGM
01432 ************************************************************      ELUIOPGM
01433 *                                                          *      ELUIOPGM
01434 *        UPDATE ITEM                                       *      ELUIOPGM
01435 *                                                          *      ELUIOPGM
01436 ************************************************************      ELUIOPGM
01437  00497-UPDATE-ITEM.                                               ELUIOPGM
01438      EXEC CICS WRITEQ TS                                          ELUIOPGM
01439                FROM   (LS-INPUT-OUTPUT-AREA)                      ELUIOPGM
01440                ITEM   (IOP-TSQ-ITEM-NBR)                          ELUIOPGM
01441                LENGTH (IOP-REC-LEN)                               ELUIOPGM
01442                QUEUE  (IOP-TSQ-ID)                                ELUIOPGM
01443                REWRITE                                            ELUIOPGM
01444                END-EXEC.                                          ELUIOPGM
01445                                                                   ELUIOPGM
01446                                                                   ELUIOPGM
01447 ************************************************************      ELUIOPGM
01448 *                                                          *      ELUIOPGM
01449 *        CALL STORAGE MANAGER                              *      ELUIOPGM
01450 *                                                          *      ELUIOPGM
01451 ************************************************************      ELUIOPGM
01452  00505-CALL-STORAGE-MANAGER.                                      ELUIOPGM
01453      CALL 'ELUSTGMG' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELUIOPGM
01454      EJECT                                                        ELUIOPGM
01455                                                                   ELUIOPGM
01456                                                                   ELUIOPGM
01457 ************************************************************      ELUIOPGM
01458 *                                                          *      ELUIOPGM
01459 *        CHECK COMMAREA LENGTH                             *      ELUIOPGM
01460 *                                                          *      ELUIOPGM
01461 ************************************************************      ELUIOPGM
01462  00509-CHECK-COMMAREA-LENGTH.                                     ELUIOPGM
01463      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELUIOPGM
01464          PERFORM 00511-SIGNAL-COMMAREA-LENGTH-E.                  ELUIOPGM
01465                                                                   ELUIOPGM
01466                                                                   ELUIOPGM
01467 ************************************************************      ELUIOPGM
01468 *                                                          *      ELUIOPGM
01469 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELUIOPGM
01470 *                                                          *      ELUIOPGM
01471 ************************************************************      ELUIOPGM
01472  00511-SIGNAL-COMMAREA-LENGTH-E.                                  ELUIOPGM
01473      SET CIA-AB-DFHCOMMAREA TO TRUE.                              ELUIOPGM
01474      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUIOPGM
01475      EJECT                                                        ELUIOPGM
01476                                                                   ELUIOPGM
01477                                                                   ELUIOPGM
01478 ************************************************************      ELUIOPGM
01479 *                                                          *      ELUIOPGM
01480 *        SIGNAL UNKNOWN ERROR                              *      ELUIOPGM
01481 *                                                          *      ELUIOPGM
01482 ************************************************************      ELUIOPGM
01483  00525-SIGNAL-UNKNOWN-ERROR.                                      ELUIOPGM
01484      SET CIA-AB-UNDEF TO TRUE.                                    ELUIOPGM
01485      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUIOPGM
01486      EJECT                                                        ELUIOPGM
01487                                                                   ELUIOPGM
01488                                                                   ELUIOPGM
01489 ************************************************************      ELUIOPGM
01490 *                                                          *      ELUIOPGM
01491 *        SIGNAL INVALID INPUT-OUTPUT FUNCTION REQUEST      *      ELUIOPGM
01492 *                                                          *      ELUIOPGM
01493 ************************************************************      ELUIOPGM
01494  00528-SIGNAL-INVALID-INPUT-OUT.                                  ELUIOPGM
01495      SET CIA-AB-IO-INVREQ TO TRUE.                                ELUIOPGM
01496      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUIOPGM
01497      EJECT                                                        ELUIOPGM
01498                                                                   ELUIOPGM
01499                                                                   ELUIOPGM
01500 ************************************************************      ELUIOPGM
01501 *                                                          *      ELUIOPGM
01502 *        SIGNAL INPUT-OUTPUT ERROR                         *      ELUIOPGM
01503 *                                                          *      ELUIOPGM
01504 ************************************************************      ELUIOPGM
01505  00531-SIGNAL-INPUT-OUTPUT-ERRO.                                  ELUIOPGM
01506      SET CIA-AB-CRITIO TO TRUE.                                   ELUIOPGM
01507      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUIOPGM
01508      EJECT                                                        ELUIOPGM
01509  GOBACK-PARAGRAPH.                                                ELUIOPGM
01510 ************************************************************      ELUIOPGM
01511 *                                                          *      ELUIOPGM
01512 *                         STOP RUN                         *      ELUIOPGM
01513 *                                                          *      ELUIOPGM
01514 ************************************************************      ELUIOPGM
01515      GOBACK.                                                      ELUIOPGM
