00001  IDENTIFICATION DIVISION.                                         09/03/03
00002 *                                                                 ELGGXXB 
00003  PROGRAM-ID.         ELGGXXB.                                        LV002
00004 *                                                                 ELGGXXB 
00005  AUTHOR.             DAVID SECOR  OF  A.C.I.                      ELGGXXB 
00006 *                                                                 ELGGXXB 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGGXXB 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELGGXXB 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGGXXB 
00010                      233 N. MICHIGAN AVE                          ELGGXXB 
00011                      CHICAGO, ILLINOIS 60601                      ELGGXXB 
00012 *                                                                 ELGGXXB 
00013  DATE-WRITTEN.       28-JUL-1987.                                 ELGGXXB 
00014 *                                                                 ELGGXXB 
00015  DATE-COMPILED.                                                   ELGGXXB 
00016 *                                                                 ELGGXXB 
00017  SECURITY.           COPYRIGHT 1987,                              ELGGXXB 
00018                      HEALTH CARE SERVICE CORPORATION              ELGGXXB 
00019 *                                                                 ELGGXXB 
00020 ******************************************************************ELGGXXB 
00021 *   ELGGXXB                                                      *ELGGXXB 
00022 *                                                                *ELGGXXB 
00023 *                        PROGRAM ABSTRACT                        *ELGGXXB 
00024 *                                                                *ELGGXXB 
00025 *   PROGRAM NAME:   E.L.S. GROUP SPECIFIC RELATED SERVICES       *ELGGXXB 
00026 *                   COST CONTAINMENT SUBROUTINE                  *ELGGXXB 
00027 *                                                                *ELGGXXB 
00028 *   PROGRAM I.D.:   ELGGXXB                                      *ELGGXXB 
00029 *                                                                *ELGGXXB 
00030 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE FIELDS *ELGGXXB 
00031 *              FOR RELATED SERVICES BENEFIT COST CONTAINMENT.    *ELGGXXB 
00032 *              SINCE THE RECORD FORMATS FOR THE VARIOUS COST     *ELGGXXB 
00033 *              CONTAINMENT RECORDS ARE IDENTICAL ONLY ONE        *ELGGXXB 
00034 *              FORMAT HAS BEEN USED IN THIS PROGRAM.             *ELGGXXB 
00035 *                                                                *ELGGXXB 
00036 *   RECORDS                                                      *ELGGXXB 
00037 *   ACCESSED:  GFSB, GHOB, GMDB, GMDN, GMOB, GMPB, GMSB, AND     *ELGGXXB 
00038 *              GPAB ALL GROUP SPECIFIC TABULAR RECORDS           *ELGGXXB 
00039 *                                                                *ELGGXXB 
00040 ******************************************************************ELGGXXB 
00041 *                                                                *ELGGXXB 
00042 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGGXXB 
00043 *       *-*         U P D A T E   H I S T O R Y         *-*      *ELGGXXB 
00044 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGGXXB 
00045 *                                                                *ELGGXXB 
00046 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELGGXXB 
00047 *                                                                *ELGGXXB 
00048 *  ELS 2.0   07/28/87  DES  INCEPTION.                           *ELGGXXB 
00049 *                                                                *ELGGXXB 
00050 *      2.1   03/18/92  BAK  ADD #GMCS TO TAB-TYPE-DEF.  ALSO     *ELGGXXB 
00051 *                           REMOVE TERMINATION ROUTINE AND       *ELGGXXB 
00052 *                           REPLACE WITH GOBACK TO ELIMINATE     *ELGGXXB 
00053 *                           'W' COMPILE ERRORS.                  *ELGGXXB 
00054 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGGXXB 
00055 ******************************************************************ELGGXXB 
00056 /                                                                 ELGGXXB 
00057  ENVIRONMENT DIVISION.                                            ELGGXXB 
00058  CONFIGURATION SECTION.                                           ELGGXXB 
00059  SOURCE-COMPUTER.    IBM-3081.                                    ELGGXXB 
00060  OBJECT-COMPUTER.    IBM-3090.                                    ELGGXXB 
00061 /                                                                 ELGGXXB 
00062  DATA DIVISION.                                                   ELGGXXB 
00063  WORKING-STORAGE SECTION.                                         ELGGXXB 
00064  01  WS-BEGIN               PIC X(24)  VALUE                      ELGGXXB 
00065      'ELGGXXB WORKING STORAGE*'.                                  ELGGXXB 
00066 /     W O R K F I E L D S   A N D   S W I T C H E S               ELGGXXB 
00067  01  WS-MISC-WORK.                                                ELGGXXB 
00068      05  WS-SUB             PIC S9999  COMP SYNC VALUE ZEROES.    ELGGXXB 
00069      05  WS-I-E-IND         PIC X.                                ELGGXXB 
00070      05  WS-FIRST-TIME-IND-VAL   PIC X.                           ELGGXXB 
00071        88  WS-FIRST-TIME-THIS-IND    VALUE 'Y'.                   ELGGXXB 
00072      05  WS-TAB-TYPE-DEF.                                         ELGGXXB 
00073        10  WS-GFSB          PIC X(6)   VALUE '#GFSB '.            ELGGXXB 
00074        10  WS-GHOB          PIC X(6)   VALUE '#GHOB '.            ELGGXXB 
00075        10  WS-GMDB          PIC X(6)   VALUE '#GMDB '.            ELGGXXB 
00076        10  WS-GMDN          PIC X(6)   VALUE '#GMDN '.            ELGGXXB 
00077        10  WS-GMOB          PIC X(6)   VALUE '#GMOB '.            ELGGXXB 
00078        10  WS-GMPB          PIC X(6)   VALUE '#GMPB '.            ELGGXXB 
00079        10  WS-GMSB          PIC X(6)   VALUE '#GMSB '.            ELGGXXB 
00080        10  WS-GPAB          PIC X(6)   VALUE '#GPAB '.            ELGGXXB 
00081        10  WS-GMCS          PIC X(6)   VALUE '#GMCS '.            ELGGXXB 
00082      05  WS-TAB-ID-TABLE   REDEFINES   WS-TAB-TYPE-DEF.           ELGGXXB 
00083        10  WS-TAB-TYPE      PIC X(6) OCCURS  9  TIMES             ELGGXXB 
00084                             INDEXED BY  WS-IDX.                   ELGGXXB 
00085      05  WS-BEN-ID.                                               ELGGXXB 
00086          10  WS-BEN-ID-5    PIC X(5)   VALUE SPACES.              ELGGXXB 
00087          10  WS-BEN-ID-1    PIC X      VALUE SPACES.              ELGGXXB 
00088                                                                   ELGGXXB 
00089  01  WS-END                 PIC X(24)  VALUE                      ELGGXXB 
00090      '*** ELGGXXB W/S ENDS ***'.                                  ELGGXXB 
00091 /             L I N K A G E   S E C T I O N                       ELGGXXB 
00092  LINKAGE SECTION.                                                 ELGGXXB 
00093  01  DFHCOMMAREA.                                                 ELGGXXB 
00094      COPY ELSCOMMC.                                               ELGGXXB 
00095 /    C O M M O N   I N T E R F A C E   A R E A                    ELGGXXB 
00096      COPY ELSCIA2C.                                               ELGGXXB 
00097 /    C O D E S   M A N U A L   D E S C R I P T I O N   L I N E S  ELGGXXB 
00098      COPY ELSCMDSC.                                               ELGGXXB 
00099 /    C O D E S   M A N U A L   C N T L .   B L O C K              ELGGXXB 
00100      COPY ELSCMIFC.                                               ELGGXXB 
00101 /    I / O   P A R A M E T E R   B L O C K                        ELGGXXB 
00102      COPY ELSIOPMC.                                               ELGGXXB 
00103 /    W O R K   A R E A   T O   B U I L D   K E Y S                ELGGXXB 
00104      COPY ELSKEYSC.                                               ELGGXXB 
00105 /    C N T L   B L O C K   -   O U T P U T   P A G E   B L D R    ELGGXXB 
00106      COPY ELSOUTPC.                                               ELGGXXB 
00107 /    T E X T   C O M P R E S S I O N   W O R K   A R E A          ELGGXXB 
00108      COPY ELSTCWAC.                                               ELGGXXB 
00109 /    S E L E C T O R   S T A T U S   C O N T R O L   B L O C K    ELGGXXB 
00110      COPY ELSSSCBC.                                               ELGGXXB 
00111 /    S U B R O U T I N E   P A R A M E T E R   L I S T            ELGGXXB 
00112      COPY ELSSRTPC.                                               ELGGXXB 
00113 /    G R O U P   S P E C I F I C   # G F S B   T A B U L A R      ELGGXXB 
00114  01  GSU-TABULAR-REC-AREA.                                        ELGGXXB 
00115      COPY GCTGFSBC.                                               ELGGXXB 
00116      EJECT                                                        ELGGXXB 
00117  PROCEDURE DIVISION.                                              ELGGXXB 
00118 ************************************************************      ELGGXXB 
00119 *                                                          *      ELGGXXB 
00120 *                    PROCEDURE DIVISION                    *      ELGGXXB 
00121 *                                                          *      ELGGXXB 
00122 ************************************************************      ELGGXXB 
00123                                                                   ELGGXXB 
00124                                                                   ELGGXXB 
00125 ************************************************************      ELGGXXB 
00126 *                                                          *      ELGGXXB 
00127 *        GXXB TABULAR MAINLINE                             *      ELGGXXB 
00128 *                                                          *      ELGGXXB 
00129 ************************************************************      ELGGXXB 
00130  00001-GXXB-TABULAR-MAINLINE.                                     ELGGXXB 
00131      PERFORM 00005-INITIALIZATION-ROUTINE.                        ELGGXXB 
00132      PERFORM 00033-MAIN-ROUTINE.                                  ELGGXXB 
00133      GOBACK.                                                      ELGGXXB 
00134      EJECT                                                        ELGGXXB 
00135                                                                   ELGGXXB 
00136                                                                   ELGGXXB 
00137 ************************************************************      ELGGXXB 
00138 *                                                          *      ELGGXXB 
00139 *        INITIALIZATION ROUTINE                            *      ELGGXXB 
00140 *                                                          *      ELGGXXB 
00141 ************************************************************      ELGGXXB 
00142  00005-INITIALIZATION-ROUTINE.                                    ELGGXXB 
00143      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELGGXXB 
00144          PERFORM 00030-COMMAREA-LENGTH-ERROR.                     ELGGXXB 
00145      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGGXXB 
00146          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELGGXXB 
00147      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGGXXB 
00148      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXB 
00149          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELGGXXB 
00150      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGGXXB 
00151      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXB 
00152          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELGGXXB 
00153      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGGXXB 
00154      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXB 
00155          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELGGXXB 
00156      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGGXXB 
00157      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXB 
00158          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELGGXXB 
00159      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELGGXXB 
00160      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXB 
00161          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELGGXXB 
00162      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGGXXB 
00163      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXB 
00164          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELGGXXB 
00165      INITIALIZE CMF-CODES-MANUAL-INTERFACE                        ELGGXXB 
00166                 TCAR-FROM-AREA.                                   ELGGXXB 
00167      MOVE 'Y'  TO  WS-FIRST-TIME-IND-VAL.                         ELGGXXB 
00168      EJECT                                                        ELGGXXB 
00169                                                                   ELGGXXB 
00170                                                                   ELGGXXB 
00171 ************************************************************      ELGGXXB 
00172 *                                                          *      ELGGXXB 
00173 *        COMMAREA LENGTH ERROR                             *      ELGGXXB 
00174 *                                                          *      ELGGXXB 
00175 ************************************************************      ELGGXXB 
00176  00030-COMMAREA-LENGTH-ERROR.                                     ELGGXXB 
00177      SET CIA-AB-DFHCOMMAREA  TO  TRUE.                            ELGGXXB 
00178      EXEC CICS  ABEND  ABCODE(CIA-ABCODE) END-EXEC.               ELGGXXB 
00179      EJECT                                                        ELGGXXB 
00180                                                                   ELGGXXB 
00181                                                                   ELGGXXB 
00182 ************************************************************      ELGGXXB 
00183 *                                                          *      ELGGXXB 
00184 *        MAIN ROUTINE                                      *      ELGGXXB 
00185 *                                                          *      ELGGXXB 
00186 ************************************************************      ELGGXXB 
00187  00033-MAIN-ROUTINE.                                              ELGGXXB 
00188      SKIP1                                                        ELGGXXB 
00189 *                                                          *      ELGGXXB 
00190 *        SEARCH COST CONTAINMENT TABLE                     *      ELGGXXB 
00191 *                                                          *      ELGGXXB 
00192          SET WS-IDX  TO  1                                        ELGGXXB 
00193          SEARCH WS-TAB-TYPE                                       ELGGXXB 
00194            VARYING WS-IDX                                         ELGGXXB 
00195            AT END                                                 ELGGXXB 
00196               SET CIA-AB-TAB-UNDEF  TO  TRUE                      ELGGXXB 
00197               EXEC CICS  ABEND  ABCODE(CIA-ABCODE) END-EXEC       ELGGXXB 
00198            WHEN WS-TAB-TYPE(WS-IDX)  =  SRP-TABULAR-ID            ELGGXXB 
00199                NEXT SENTENCE                                      ELGGXXB 
00200         END-SEARCH.                                               ELGGXXB 
00201      SKIP1                                                        ELGGXXB 
00202      PERFORM 00052-READ-INTERNAL-TABULAR-RE.                      ELGGXXB 
00203      IF GSU-BENEFIT-CODE(1)  NOT =  HIGH-VALUES                   ELGGXXB 
00204          PERFORM 00044-SAVE-FIRSTS-I-E-IND                        ELGGXXB 
00205          PERFORM 00141-INSERT-BLANK-LINE                          ELGGXXB 
00206          PERFORM 00046-SEARCH-TABLE-FOR-FIRST                     ELGGXXB 
00207              VARYING GSU-INDEX  FROM  1  BY  1                    ELGGXXB 
00208               UNTIL GSU-INDEX  >  GSU-ENTRY-COUNT OR              ELGGXXB 
00209                GSU-BENEFIT-CODE(GSU-INDEX)  =  HIGH-VALUES        ELGGXXB 
00210          PERFORM 00141-INSERT-BLANK-LINE                          ELGGXXB 
00211          MOVE 'Y'  TO  WS-FIRST-TIME-IND-VAL                      ELGGXXB 
00212          PERFORM 00048-SEARCH-TABLE-FOR-OTHER-T                   ELGGXXB 
00213              VARYING GSU-INDEX  FROM  1  BY  1                    ELGGXXB 
00214               UNTIL GSU-INDEX  >  GSU-ENTRY-COUNT OR              ELGGXXB 
00215                GSU-BENEFIT-CODE(GSU-INDEX)  =  HIGH-VALUES        ELGGXXB 
00216          PERFORM 00141-INSERT-BLANK-LINE                          ELGGXXB 
00217      ELSE PERFORM 00169-EMPTY-COST-CONTAINMENT-T.                 ELGGXXB 
00218      EJECT                                                        ELGGXXB 
00219                                                                   ELGGXXB 
00220                                                                   ELGGXXB 
00221 ************************************************************      ELGGXXB 
00222 *                                                          *      ELGGXXB 
00223 *        SAVE FIRSTS I-E IND                               *      ELGGXXB 
00224 *                                                          *      ELGGXXB 
00225 ************************************************************      ELGGXXB 
00226  00044-SAVE-FIRSTS-I-E-IND.                                       ELGGXXB 
00227      MOVE GSU-INCLUDE-EXCLUDE-IND(1)  TO  WS-I-E-IND.             ELGGXXB 
00228      EJECT                                                        ELGGXXB 
00229                                                                   ELGGXXB 
00230                                                                   ELGGXXB 
00231 ************************************************************      ELGGXXB 
00232 *                                                          *      ELGGXXB 
00233 *        SEARCH TABLE FOR FIRST                            *      ELGGXXB 
00234 *                                                          *      ELGGXXB 
00235 ************************************************************      ELGGXXB 
00236  00046-SEARCH-TABLE-FOR-FIRST.                                    ELGGXXB 
00237      IF GSU-INCLUDE-EXCLUDE-IND(GSU-INDEX)  =  WS-I-E-IND         ELGGXXB 
00238          PERFORM 00078-LIST-SERVICE-RELATED-BEN.                  ELGGXXB 
00239      EJECT                                                        ELGGXXB 
00240                                                                   ELGGXXB 
00241                                                                   ELGGXXB 
00242 ************************************************************      ELGGXXB 
00243 *                                                          *      ELGGXXB 
00244 *        SEARCH TABLE FOR OTHER THAN FIRST                 *      ELGGXXB 
00245 *                                                          *      ELGGXXB 
00246 ************************************************************      ELGGXXB 
00247  00048-SEARCH-TABLE-FOR-OTHER-T.                                  ELGGXXB 
00248      IF GSU-INCLUDE-EXCLUDE-IND(GSU-INDEX)  NOT =  WS-I-E-IND     ELGGXXB 
00249          PERFORM 00078-LIST-SERVICE-RELATED-BEN.                  ELGGXXB 
00250                                                                   ELGGXXB 
00251                                                                   ELGGXXB 
00252 ************************************************************      ELGGXXB 
00253 *                                                          *      ELGGXXB 
00254 *        READ INTERNAL TABULAR RECORD                      *      ELGGXXB 
00255 *                                                          *      ELGGXXB 
00256 ************************************************************      ELGGXXB 
00257  00052-READ-INTERNAL-TABULAR-RE.                                  ELGGXXB 
00258      SET CIA-GCTABULR-DDN  TO  TRUE.                              ELGGXXB 
00259      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXB 
00260          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGGXXB 
00261      IF CIA-RC-PTR-NULL                                           ELGGXXB 
00262          PERFORM 00072-GETMAIN-IO-PARM-AREA.                      ELGGXXB 
00263      SET CIA-GCTABULR-DDN  TO  TRUE.                              ELGGXXB 
00264      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXB 
00265          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGGXXB 
00266      MOVE SRP-INTERNAL-TAB  TO  KWA-GCTABULR-KEY.                 ELGGXXB 
00267      SET IOP-RD  TO  TRUE.                                        ELGGXXB 
00268      SET IOP-FCQ-NONE  TO  TRUE.                                  ELGGXXB 
00269      SET IOP-KVQ-EQ  TO  TRUE.                                    ELGGXXB 
00270      MOVE KWA-GCTABULR-KEY  TO  IOP-FILE-KEY.                     ELGGXXB 
00271      MOVE SPACES  TO  IOP-AIX-DDNAME.                             ELGGXXB 
00272      SET IOP-REC-PTR  TO  NULL.                                   ELGGXXB 
00273      SET IOP-STG-MODE-MOVE  TO  TRUE.                             ELGGXXB 
00274      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELGGXXB 
00275                       COMMAREA(DFHCOMMAREA) END-EXEC.             ELGGXXB 
00276      IF IOP-RC-OK                                                 ELGGXXB 
00277          PERFORM 00076-ADDRESS-TABULAR-AREA                       ELGGXXB 
00278      ELSE PERFORM 00178-TABULAR-NOT-FOUND.                        ELGGXXB 
00279      EJECT                                                        ELGGXXB 
00280                                                                   ELGGXXB 
00281                                                                   ELGGXXB 
00282 ************************************************************      ELGGXXB 
00283 *                                                          *      ELGGXXB 
00284 *        GETMAIN IO PARM AREA                              *      ELGGXXB 
00285 *                                                          *      ELGGXXB 
00286 ************************************************************      ELGGXXB 
00287  00072-GETMAIN-IO-PARM-AREA.                                      ELGGXXB 
00288      SET CIA-STG-GETMAIN  TO  TRUE.                               ELGGXXB 
00289      EXEC CICS  LINK  PROGRAM('ELUSTGMG')                         ELGGXXB 
00290                       COMMAREA(DFHCOMMAREA)   END-EXEC.           ELGGXXB 
00291      EJECT                                                        ELGGXXB 
00292                                                                   ELGGXXB 
00293                                                                   ELGGXXB 
00294 ************************************************************      ELGGXXB 
00295 *                                                          *      ELGGXXB 
00296 *        ADDRESS TABULAR AREA                              *      ELGGXXB 
00297 *                                                          *      ELGGXXB 
00298 ************************************************************      ELGGXXB 
00299  00076-ADDRESS-TABULAR-AREA.                                      ELGGXXB 
00300      SET ADDRESS OF GSU-TABULAR-REC-AREA  TO  IOP-REC-PTR.        ELGGXXB 
00301                                                                   ELGGXXB 
00302                                                                   ELGGXXB 
00303 ************************************************************      ELGGXXB 
00304 *                                                          *      ELGGXXB 
00305 *        LIST SERVICE RELATED BENEFITS                     *      ELGGXXB 
00306 *                                                          *      ELGGXXB 
00307 ************************************************************      ELGGXXB 
00308  00078-LIST-SERVICE-RELATED-BEN.                                  ELGGXXB 
00309      IF WS-FIRST-TIME-THIS-IND                                    ELGGXXB 
00310          PERFORM 00083-TRANSLATE-INCLUDE-EXCLUD.                  ELGGXXB 
00311      SKIP1                                                        ELGGXXB 
00312 *                                                          *      ELGGXXB 
00313 *        TRANSLATE BEN PROV ID                             *      ELGGXXB 
00314 *                                                          *      ELGGXXB 
00315          MOVE 0  TO  TCAR-FROM-SUB                                ELGGXXB 
00316          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELGGXXB 
00317          MOVE GSU-BENEFIT-CODE(GSU-INDEX)  TO  WS-BEN-ID          ELGGXXB 
00318          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELGGXXB 
00319          MOVE WS-BEN-ID-5  TO  CMF-CODE-VALUE                     ELGGXXB 
00320          EXEC CICS  LINK  PROGRAM('ELUCMIF')                      ELGGXXB 
00321                       COMMAREA(DFHCOMMAREA)  END-EXEC             ELGGXXB 
00322          SET CIA-ELSCMDSC-DDN TO TRUE                             ELGGXXB 
00323          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELGGXXB 
00324          ADDRESS OF CMF-DESCR                                     ELGGXXB 
00325          PERFORM 00148-MOVE-DESCRIPTION-PHRASE                    ELGGXXB 
00326          VARYING WS-SUB  FROM  1  BY  1                           ELGGXXB 
00327          UNTIL WS-SUB  GREATER THAN  CMF-NBR-DESCR-LINES.         ELGGXXB 
00328      SKIP1                                                        ELGGXXB 
00329      PERFORM 00121-TRANSLATE-BEN-PROV-SUFFI.                      ELGGXXB 
00330      SKIP1                                                        ELGGXXB 
00331 *                                                          *      ELGGXXB 
00332 *        FORMAT BEN PROV DESCRIPTION                       *      ELGGXXB 
00333 *                                                          *      ELGGXXB 
00334          PERFORM 00151-DO-TEXT-COMPRESSION                        ELGGXXB 
00335          MOVE TCAR-TO-SUB  TO  TCAR-L                             ELGGXXB 
00336          MOVE +02  TO  TCAR-OUTPUT-FIELD-COUNT                    ELGGXXB 
00337          MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                    ELGGXXB 
00338                    TCAR-OUTPUT-FIELD-2-LEN                        ELGGXXB 
00339          PERFORM 00153-DO-TEXT-UNSTRING                           ELGGXXB 
00340          PERFORM 00145-MOVE-COMPRESSED-PHRASE                     ELGGXXB 
00341          VARYING WS-SUB  FROM  1  BY  1                           ELGGXXB 
00342           UNTIL WS-SUB  GREATER THAN  TCAR-OUTPUT-FIELDS-USED     ELGGXXB 
00343          PERFORM 00155-CALL-OUTPUT.                               ELGGXXB 
00344      SKIP1                                                        ELGGXXB 
00345      EJECT                                                        ELGGXXB 
00346                                                                   ELGGXXB 
00347                                                                   ELGGXXB 
00348 ************************************************************      ELGGXXB 
00349 *                                                          *      ELGGXXB 
00350 *        TRANSLATE INCLUDE-EXCLUDE IND                     *      ELGGXXB 
00351 *                                                          *      ELGGXXB 
00352 ************************************************************      ELGGXXB 
00353  00083-TRANSLATE-INCLUDE-EXCLUD.                                  ELGGXXB 
00354      MOVE 'N'  TO  WS-FIRST-TIME-IND-VAL.                         ELGGXXB 
00355      INITIALIZE TCAR-FROM-AREA.                                   ELGGXXB 
00356      MOVE SRP-TABULAR-ID  TO  CMF-RECORD-PREFIX.                  ELGGXXB 
00357      MOVE 'INCLUDE-EXCLUDE-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.     ELGGXXB 
00358      MOVE GSU-INCLUDE-EXCLUDE-IND(GSU-INDEX)  TO                  ELGGXXB 
00359          CMF-CODE-VALUE.                                          ELGGXXB 
00360      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELGGXXB 
00361                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGGXXB 
00362      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGGXXB 
00363      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXB 
00364          ADDRESS OF CMF-DESCR.                                    ELGGXXB 
00365      STRING 'THIS ',  SRP-CCP-NAME DELIMITED BY '  '              ELGGXXB 
00366             ' PROGRAM  '   DELIMITED BY SIZE                      ELGGXXB 
00367             CMF-DESCR-LINE(1)  DELIMITED BY '  '                  ELGGXXB 
00368             ' THE FOLLOWING BENEFIT PROVISIONS:'  DELIMITED       ELGGXXB 
00369          BY SIZE                                                  ELGGXXB 
00370             INTO TCAR-FROM-AREA.                                  ELGGXXB 
00371      PERFORM 00151-DO-TEXT-COMPRESSION.                           ELGGXXB 
00372      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELGGXXB 
00373      MOVE +04  TO  TCAR-OUTPUT-FIELD-COUNT.                       ELGGXXB 
00374      MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                        ELGGXXB 
00375                    TCAR-OUTPUT-FIELD-2-LEN                        ELGGXXB 
00376                    TCAR-OUTPUT-FIELD-3-LEN                        ELGGXXB 
00377                    TCAR-OUTPUT-FIELD-4-LEN.                       ELGGXXB 
00378      PERFORM 00153-DO-TEXT-UNSTRING.                              ELGGXXB 
00379      PERFORM 00145-MOVE-COMPRESSED-PHRASE                         ELGGXXB 
00380          VARYING WS-SUB  FROM  1  BY  1                           ELGGXXB 
00381          UNTIL WS-SUB  GREATER THAN  TCAR-OUTPUT-FIELDS-USED.     ELGGXXB 
00382      PERFORM 00155-CALL-OUTPUT.                                   ELGGXXB 
00383                                                                   ELGGXXB 
00384                                                                   ELGGXXB 
00385 ************************************************************      ELGGXXB 
00386 *                                                          *      ELGGXXB 
00387 *        TRANSLATE BEN PROV SUFFIX                         *      ELGGXXB 
00388 *                                                          *      ELGGXXB 
00389 ************************************************************      ELGGXXB 
00390  00121-TRANSLATE-BEN-PROV-SUFFI.                                  ELGGXXB 
00391      IF WS-BEN-ID-1  =  'A' OR  'B' OR  'W'                       ELGGXXB 
00392          PERFORM 00124-STRING-INSTITUTIONAL-PHR                   ELGGXXB 
00393      ELSE IF WS-BEN-ID-1  =  'C' OR  'D' OR  'E'                  ELGGXXB 
00394          PERFORM 00128-STRING-PROFESSIONAL-PHRA.                  ELGGXXB 
00395                                                                   ELGGXXB 
00396                                                                   ELGGXXB 
00397 ************************************************************      ELGGXXB 
00398 *                                                          *      ELGGXXB 
00399 *        STRING INSTITUTIONAL PHRASE                       *      ELGGXXB 
00400 *                                                          *      ELGGXXB 
00401 ************************************************************      ELGGXXB 
00402  00124-STRING-INSTITUTIONAL-PHR.                                  ELGGXXB 
00403      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELGGXXB 
00404             ' INSTITUTIONAL ' DELIMITED BY SIZE                   ELGGXXB 
00405             INTO TCAR-FROM-AREA.                                  ELGGXXB 
00406      EJECT                                                        ELGGXXB 
00407                                                                   ELGGXXB 
00408                                                                   ELGGXXB 
00409 ************************************************************      ELGGXXB 
00410 *                                                          *      ELGGXXB 
00411 *        STRING PROFESSIONAL PHRASE                        *      ELGGXXB 
00412 *                                                          *      ELGGXXB 
00413 ************************************************************      ELGGXXB 
00414  00128-STRING-PROFESSIONAL-PHRA.                                  ELGGXXB 
00415      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELGGXXB 
00416             ' PROFESSIONAL ' DELIMITED BY SIZE                    ELGGXXB 
00417             INTO TCAR-FROM-AREA.                                  ELGGXXB 
00418      EJECT                                                        ELGGXXB 
00419                                                                   ELGGXXB 
00420                                                                   ELGGXXB 
00421 ************************************************************      ELGGXXB 
00422 *                                                          *      ELGGXXB 
00423 *        INSERT BLANK LINE                                 *      ELGGXXB 
00424 *                                                          *      ELGGXXB 
00425 ************************************************************      ELGGXXB 
00426  00141-INSERT-BLANK-LINE.                                         ELGGXXB 
00427      ADD  1  TO  COF-NBR-DTL-LINES.                               ELGGXXB 
00428      MOVE SPACES  TO  COF-DTL-LINE(COF-NBR-DTL-LINES).            ELGGXXB 
00429      PERFORM 00155-CALL-OUTPUT.                                   ELGGXXB 
00430      EJECT                                                        ELGGXXB 
00431                                                                   ELGGXXB 
00432                                                                   ELGGXXB 
00433 ************************************************************      ELGGXXB 
00434 *                                                          *      ELGGXXB 
00435 *        MOVE COMPRESSED PHRASE                            *      ELGGXXB 
00436 *                                                          *      ELGGXXB 
00437 ************************************************************      ELGGXXB 
00438  00145-MOVE-COMPRESSED-PHRASE.                                    ELGGXXB 
00439      ADD 1  TO  COF-NBR-DTL-LINES.                                ELGGXXB 
00440      MOVE TCAR-OPF-DATA(WS-SUB)  TO                               ELGGXXB 
00441          COF-DTL-LINE(COF-NBR-DTL-LINES).                         ELGGXXB 
00442                                                                   ELGGXXB 
00443                                                                   ELGGXXB 
00444 ************************************************************      ELGGXXB 
00445 *                                                          *      ELGGXXB 
00446 *        MOVE DESCRIPTION PHRASE                           *      ELGGXXB 
00447 *                                                          *      ELGGXXB 
00448 ************************************************************      ELGGXXB 
00449  00148-MOVE-DESCRIPTION-PHRASE.                                   ELGGXXB 
00450      ADD 1  TO  TCAR-FROM-SUB.                                    ELGGXXB 
00451      MOVE CMF-DESCR-LINE(WS-SUB)  TO                              ELGGXXB 
00452          TCAR-FROM-LINE(TCAR-FROM-SUB).                           ELGGXXB 
00453      EJECT                                                        ELGGXXB 
00454                                                                   ELGGXXB 
00455                                                                   ELGGXXB 
00456 ************************************************************      ELGGXXB 
00457 *                                                          *      ELGGXXB 
00458 *        DO TEXT COMPRESSION                               *      ELGGXXB 
00459 *                                                          *      ELGGXXB 
00460 ************************************************************      ELGGXXB 
00461  00151-DO-TEXT-COMPRESSION.                                       ELGGXXB 
00462      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGGXXB 
00463      EJECT                                                        ELGGXXB 
00464                                                                   ELGGXXB 
00465                                                                   ELGGXXB 
00466 ************************************************************      ELGGXXB 
00467 *                                                          *      ELGGXXB 
00468 *        DO TEXT UNSTRING                                  *      ELGGXXB 
00469 *                                                          *      ELGGXXB 
00470 ************************************************************      ELGGXXB 
00471  00153-DO-TEXT-UNSTRING.                                          ELGGXXB 
00472      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGGXXB 
00473                                                                   ELGGXXB 
00474                                                                   ELGGXXB 
00475 ************************************************************      ELGGXXB 
00476 *                                                          *      ELGGXXB 
00477 *        CALL OUTPUT                                       *      ELGGXXB 
00478 *                                                          *      ELGGXXB 
00479 ************************************************************      ELGGXXB 
00480  00155-CALL-OUTPUT.                                               ELGGXXB 
00481      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELGGXXB 
00482                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGGXXB 
00483      INITIALIZE TCAR-FROM-AREA.                                   ELGGXXB 
00484      EJECT                                                        ELGGXXB 
00485                                                                   ELGGXXB 
00486                                                                   ELGGXXB 
00487 ************************************************************      ELGGXXB 
00488 *                                                          *      ELGGXXB 
00489 *        EMPTY COST CONTAINMENT TAB                        *      ELGGXXB 
00490 *                                                          *      ELGGXXB 
00491 ************************************************************      ELGGXXB 
00492  00169-EMPTY-COST-CONTAINMENT-T.                                  ELGGXXB 
00493      ADD 1  TO  COF-NBR-DTL-LINES.                                ELGGXXB 
00494      MOVE SPACES  TO  COF-DTL-LINE(COF-NBR-DTL-LINES).            ELGGXXB 
00495      ADD 1  TO  COF-NBR-DTL-LINES.                                ELGGXXB 
00496      STRING 'THIS ',  SRP-CCP-NAME DELIMITED BY '  '              ELGGXXB 
00497             ' PROGRAM RECORD IS EMPTY '    DELIMITED BY SIZE      ELGGXXB 
00498             INTO    COF-DTL-LINE(COF-NBR-DTL-LINES).              ELGGXXB 
00499      PERFORM 00155-CALL-OUTPUT.                                   ELGGXXB 
00500      EJECT                                                        ELGGXXB 
00501                                                                   ELGGXXB 
00502                                                                   ELGGXXB 
00503 ************************************************************      ELGGXXB 
00504 *                                                          *      ELGGXXB 
00505 *        TABULAR NOT FOUND                                 *      ELGGXXB 
00506 *                                                          *      ELGGXXB 
00507 ************************************************************      ELGGXXB 
00508  00178-TABULAR-NOT-FOUND.                                         ELGGXXB 
00509      SET CIA-AB-NOTFND-GCTABULR  TO  TRUE.                        ELGGXXB 
00510      EXEC CICS  ABEND  ABCODE(CIA-ABCODE)  END-EXEC.              ELGGXXB 
