00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELTGVLPL
00003  PROGRAM-ID.         ELTGVLPL.                                       LV002
00004                                                                   ELTGVLPL
00005  AUTHOR.             ANNE KEFFER KING.                            ELTGVLPL
00006                      COMPLETED BY JOHN BEIRNE.                    ELTGVLPL
00007                                                                   ELTGVLPL
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTGVLPL
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELTGVLPL
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTGVLPL
00011                      233 N. MICHIGAN AVE                          ELTGVLPL
00012                      CHICAGO, ILLINOIS 60601                      ELTGVLPL
00013                                                                   ELTGVLPL
00014  DATE-WRITTEN.       10-20-1993.                                  ELTGVLPL
00015                                                                   ELTGVLPL
00016  DATE-COMPILED.                                                   ELTGVLPL
00017                                                                   ELTGVLPL
00018  SECURITY.           COPYRIGHT 1993,                              ELTGVLPL
00019                      HEALTH CARE SERVICE CORPORATION              ELTGVLPL
00020      SKIP3                                                        ELTGVLPL
00021  ENVIRONMENT DIVISION.                                            ELTGVLPL
00022                                                                   ELTGVLPL
00023  CONFIGURATION SECTION.                                           ELTGVLPL
00024  SOURCE-COMPUTER.    IBM-3033.                                    ELTGVLPL
00025  OBJECT-COMPUTER.    IBM-3033.                                    ELTGVLPL
00026 ****************************************************************  ELTGVLPL
00027 *                                                              *  ELTGVLPL
00028 *    PROGRAM:    ELTGVLPL                                      *  ELTGVLPL
00029 *    DATE:       14-SEP-1993                                   *  ELTGVLPL
00030 *    AUTHOR:     JOHN BEIRNE                                   *  ELTGVLPL
00031 *    FUNCTION:   DISPLAYS PROVIDER LIST                        *  ELTGVLPL
00032 *                                                              *  ELTGVLPL
00033 ****************************************************************  ELTGVLPL
00034 *                                                              *  ELTGVLPL
00035 *                   MAINTENANCE HISTORY                        *  ELTGVLPL
00036 *                                                              *  ELTGVLPL
00037 *  MOD     DATE     BY  DRPT              ACTION               *  ELTGVLPL
00038 * ----- ----------- --- ---- --------------------------------- *  ELTGVLPL
00039 * 01.00 10-OCT-1993 AKK      CREATED                           *  ELTGVLPL
00040 *                                                              *  ELTGVLPL
00041 * 01.01 22-DEC-1993 JPB      ADDED LOGIC TO CHECK THE ENTIRE   *  ELTGVLPL
00042 *                            WORKFILE FOR PROFESSIONAL GVL'S   *  ELTGVLPL
00043 *                            IF SSB-PROF WAS SELECTED.  THIS   *  ELTGVLPL
00044 *                            ADDITION WAS NECESSARY FOR RE-    *  ELTGVLPL
00045 *                            SELECTION.                        *  ELTGVLPL
00046 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTGVLPL
00047 *                                                              *  ELTGVLPL
00048 ****************************************************************  ELTGVLPL
00049      EJECT                                                        ELTGVLPL
00050  DATA DIVISION.                                                   ELTGVLPL
00051  WORKING-STORAGE SECTION.                                         ELTGVLPL
00052  01  WS-MISC.                                                     ELTGVLPL
00053                                                                   ELTGVLPL
00054      05  INTRO-TEXT-SWITCH                PIC X(01).              ELTGVLPL
00055          88  WS-TEXT-DISPLAYED            VALUE 'D'.              ELTGVLPL
00056          88  WS-TEXT-NOT-DISPLAYED        VALUE '0'.              ELTGVLPL
00057                                                                   ELTGVLPL
00058      05  FIRST-READ-SWITCH                PIC X(01).              ELTGVLPL
00059          88  FIRST-READ                   VALUE '1'.              ELTGVLPL
00060          88  NOT-FIRST-READ               VALUE '0'.              ELTGVLPL
00061                                                                   ELTGVLPL
00062      05  WKFL4-EOF-SWITCH                 PIC X(01).              ELTGVLPL
00063          88  END-OF-WKFL4                 VALUE '1'.              ELTGVLPL
00064          88  NOT-END-OF-WKFL4             VALUE '0'.              ELTGVLPL
00065                                                                   ELTGVLPL
00066      05  PROVIDER-ACCEPTANCE-SWITCH       PIC X(01).              ELTGVLPL
00067          88  PROVIDER-ACCEPTED            VALUE 'Y'.              ELTGVLPL
00068          88  PROVIDER-NOT-ACCEPTED        VALUE 'N'.              ELTGVLPL
00069                                                                   ELTGVLPL
00070      05  WS-SPC-LST-HDR.                                          ELTGVLPL
00071          10  FILLER                       PIC X(15)  VALUE SPACES.ELTGVLPL
00072          10  HEADER-DETAIL                PIC X(50)  VALUE        ELTGVLPL
00073          'SPECIAL PROVIDER CONSIDERATIONS - PROVIDER LISTING'.    ELTGVLPL
00074                                                                   ELTGVLPL
00075      05  WS-LST-DTL-MSG.                                          ELTGVLPL
00076          10  WS-LST-DTL-MSG-1             PIC X(08)  VALUE        ELTGVLPL
00077          'BETWEEN '.                                              ELTGVLPL
00078          10  WS-TO-DT-MSG-2         PIC 99/99/99.                 ELTGVLPL
00079          10  WS-LST-DTL-AND         PIC X(05)  VALUE ' AND '.     ELTGVLPL
00080          10  WS-FRM-DT-MSG-2        PIC 99/99/99.                 ELTGVLPL
00081          10  WS-LST-DTL-COMMA       PIC X(02)  VALUE ', '.        ELTGVLPL
00082          10  WS-LST-DTL-MSG-2       PIC X(77)  VALUE              ELTGVLPL
00083          ' THE FOLLOWING PROVIDERS PARTICIPATED IN A SPECIAL PROVIELTGVLPL
00084 -        'DER NETWORK FOR THIS'.                                  ELTGVLPL
00085          10  WS-LST-DTL-MSG-3              PIC X(76)  VALUE       ELTGVLPL
00086          ' GROUP AT SOME TIME.  TO VIEW THE SPECIFIC DATES AND TYPELTGVLPL
00087 -        'E OF PARTICIPATION,'.                                   ELTGVLPL
00088          10  WS-LST-DTL-MSG-4              PIC X(75)  VALUE       ELTGVLPL
00089          ' RETURN TO THE SPECIAL PROVIDER CONSIDERATIONS OPTIONS SELTGVLPL
00090 -        'CREEN AND ENTER THE'.                                   ELTGVLPL
00091          10  WS-LST-DTL-MSG-5              PIC X(61)  VALUE       ELTGVLPL
00092          ' PROVIDER NUMBER (OR NAME) OF THE PROVIDER YOU WISH TO VELTGVLPL
00093 -        'IEW.'.                                                  ELTGVLPL
00094                                                                   ELTGVLPL
00095       05  WS-INST-STATEMENT              PIC X(76)  VALUE         ELTGVLPL
00096          'THE FOLLOWING IS A LIST OF INSTITUTIONAL PROVIDERS FOR TELTGVLPL
00097 -        'HE REQUESTED GROUP:'.                                   ELTGVLPL
00098                                                                   ELTGVLPL
00099       05  WS-PROF-STATEMENT              PIC X(75)  VALUE         ELTGVLPL
00100          'THE FOLLOWING IS A LIST OF PROFESSIONAL PROVIDERS FOR THELTGVLPL
00101 -        'E REQUESTED GROUP:'.                                    ELTGVLPL
00102                                                                   ELTGVLPL
00103       05  WS-NO-INST-PROV-MSG            PIC X(49)  VALUE         ELTGVLPL
00104          'NO INSTITUTIONAL PROVIDERS EXIST FOR THESE DATES.'.     ELTGVLPL
00105                                                                   ELTGVLPL
00106       05  WS-NO-PROF-PROV-MSG            PIC X(48)  VALUE         ELTGVLPL
00107          'NO PROFESSIONAL PROVIDERS EXIST FOR THESE DATES.'.      ELTGVLPL
00108                                                                   ELTGVLPL
00109       05  WS-HOLD-AREA.                                           ELTGVLPL
00110          10  WS-NAME-HOLD                 PIC X(33) VALUE SPACE.  ELTGVLPL
00111          10  WS-NUMBER-HOLD               PIC X(10) VALUE SPACE.  ELTGVLPL
00112                                                                   ELTGVLPL
00113  01 HGADATES-PARM-LIST.                                           ELTGVLPL
00114     COPY HGCDAT01.                                                ELTGVLPL
00115                                                                   ELTGVLPL
00116  LINKAGE SECTION.                                                 ELTGVLPL
00117  01  DFHCOMMAREA.                                                 ELTGVLPL
00118  COPY ELSCOMMC.                                                   ELTGVLPL
00119      EJECT                                                        ELTGVLPL
00120  COPY ELSCIA2C.                                                   ELTGVLPL
00121      EJECT                                                        ELTGVLPL
00122  COPY ELSIOPMC.                                                   ELTGVLPL
00123      EJECT                                                        ELTGVLPL
00124  COPY ELSSSCBC.                                                   ELTGVLPL
00125      EJECT                                                        ELTGVLPL
00126  COPY ELSTCWAC.                                                   ELTGVLPL
00127      EJECT                                                        ELTGVLPL
00128  COPY ELSOUTPC.                                                   ELTGVLPL
00129      EJECT                                                        ELTGVLPL
00130  COPY ELSGVL4C.                                                   ELTGVLPL
00131      EJECT                                                        ELTGVLPL
00132  PROCEDURE DIVISION.                                              ELTGVLPL
00133 ************************************************************      ELTGVLPL
00134 *                                                          *      ELTGVLPL
00135 *        ELSGVLM1 MAINLINE                                 *      ELTGVLPL
00136 *                                                          *      ELTGVLPL
00137 ************************************************************      ELTGVLPL
00138  0000-ELTGVLPL-MAINLINE.                                          ELTGVLPL
00139      PERFORM 0100-INITIALIZE.                                     ELTGVLPL
00140      PERFORM 1000-PROCESS-GVL.                                    ELTGVLPL
00141      GOBACK.                                                      ELTGVLPL
00142                                                                   ELTGVLPL
00143 ************************************************************      ELTGVLPL
00144 *                                                          *      ELTGVLPL
00145 *        INITIALIZATION                                    *      ELTGVLPL
00146 *                                                          *      ELTGVLPL
00147 ************************************************************      ELTGVLPL
00148  0100-INITIALIZE.                                                 ELTGVLPL
00149      PERFORM 0200-ESTABLISH-ENVIRONMENT.                          ELTGVLPL
00150      PERFORM 0300-EST-ADDRS-OF-SSB.                               ELTGVLPL
00151      PERFORM 0400-EST-ADDRS-OF-TCWA.                              ELTGVLPL
00152      PERFORM 0500-EST-ADDRS-OF-OUTP.                              ELTGVLPL
00153      PERFORM 0600-EST-ADDRS-OF-WKFL4.                             ELTGVLPL
00154                                                                   ELTGVLPL
00155 ************************************************************      ELTGVLPL
00156 *                                                          *      ELTGVLPL
00157 *             ESTABLISH ENVIRONMENT                        *      ELTGVLPL
00158 *                                                          *      ELTGVLPL
00159 ************************************************************      ELTGVLPL
00160  0200-ESTABLISH-ENVIRONMENT.                                      ELTGVLPL
00161      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELTGVLPL
00162         EXEC CICS ABEND                                           ELTGVLPL
00163                   ABCODE ('EL01')                                 ELTGVLPL
00164         END-EXEC                                                  ELTGVLPL
00165      ELSE                                                         ELTGVLPL
00166         CALL 'ELUINISM' USING DFHCOMMAREA                         ELTGVLPL
00167               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELTGVLPL
00168         IF ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA = NULLS       ELTGVLPL
00169            EXEC CICS ABEND                                        ELTGVLPL
00170                      ABCODE ('EL02')                              ELTGVLPL
00171            END-EXEC.                                              ELTGVLPL
00172                                                                   ELTGVLPL
00173 ************************************************************      ELTGVLPL
00174 *                                                          *      ELTGVLPL
00175 *        ESTABLISH ADDRESSABILITY TO SELECTOR STATUS BLOCK *      ELTGVLPL
00176 *                                                          *      ELTGVLPL
00177 ************************************************************      ELTGVLPL
00178  0300-EST-ADDRS-OF-SSB.                                           ELTGVLPL
00179      SET CIA-ELSSSCB-DDN TO TRUE                                  ELTGVLPL
00180      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTGVLPL
00181                            ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK ELTGVLPL
00182      END-CALL                                                     ELTGVLPL
00183      IF CIA-RC-PTR-NULL                                           ELTGVLPL
00184         SET CIA-AB-PARM-MISSING TO TRUE                           ELTGVLPL
00185         PERFORM 9999-LINK-TO-ABEND.                               ELTGVLPL
00186                                                                   ELTGVLPL
00187 ************************************************************      ELTGVLPL
00188 *                                                          *      ELTGVLPL
00189 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WRK  *      ELTGVLPL
00190 *                                                          *      ELTGVLPL
00191 ************************************************************      ELTGVLPL
00192  0400-EST-ADDRS-OF-TCWA.                                          ELTGVLPL
00193      SET CIA-ELSTCWA-DDN TO TRUE                                  ELTGVLPL
00194      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTGVLPL
00195                     ADDRESS OF TCAR-COMPRESSION-WORK-AREA         ELTGVLPL
00196      END-CALL.                                                    ELTGVLPL
00197      IF CIA-RC-PTR-NULL                                           ELTGVLPL
00198         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTGVLPL
00199         PERFORM 9999-LINK-TO-ABEND.                               ELTGVLPL
00200                                                                   ELTGVLPL
00201 ************************************************************      ELTGVLPL
00202 *                                                          *      ELTGVLPL
00203 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTGVLPL
00204 *                                                          *      ELTGVLPL
00205 ************************************************************      ELTGVLPL
00206  0500-EST-ADDRS-OF-OUTP.                                          ELTGVLPL
00207      SET CIA-ELSOUTP-DDN TO TRUE                                  ELTGVLPL
00208      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTGVLPL
00209                     ADDRESS OF COF-OUTPUT-INTERFACE               ELTGVLPL
00210      END-CALL.                                                    ELTGVLPL
00211      IF CIA-RC-PTR-NULL                                           ELTGVLPL
00212         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTGVLPL
00213         PERFORM 9999-LINK-TO-ABEND.                               ELTGVLPL
00214                                                                   ELTGVLPL
00215 ************************************************************      ELTGVLPL
00216 *                                                          *      ELTGVLPL
00217 *        ESTABLISH ADDRESSABILITY OF WORKFILE 4            *      ELTGVLPL
00218 *                                                          *      ELTGVLPL
00219 ************************************************************      ELTGVLPL
00220  0600-EST-ADDRS-OF-WKFL4.                                         ELTGVLPL
00221      SET CIA-ELSWKFL4-DDN TO TRUE.                                ELTGVLPL
00222      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTGVLPL
00223                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS       ELTGVLPL
00224      END-CALL.                                                    ELTGVLPL
00225      IF CIA-RC-PTR-NULL                                           ELTGVLPL
00226         SET CIA-ELSWKFL4-DDN TO TRUE                              ELTGVLPL
00227         SET CIA-STG-GETMAIN TO TRUE                               ELTGVLPL
00228         CALL 'ELUSTGMG' USING DFHEIBLK                            ELTGVLPL
00229                            DFHCOMMAREA                            ELTGVLPL
00230         SET CIA-ELSWKFL4-DDN TO TRUE                              ELTGVLPL
00231         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTGVLPL
00232                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS       ELTGVLPL
00233         END-CALL                                                  ELTGVLPL
00234      END-IF.                                                      ELTGVLPL
00235                                                                   ELTGVLPL
00236 ************************************************************      ELTGVLPL
00237 *                                                          *      ELTGVLPL
00238 *        PROCESS VARIABLE PROVIDER LIST                    *      ELTGVLPL
00239 *                                                          *      ELTGVLPL
00240 ************************************************************      ELTGVLPL
00241  1000-PROCESS-GVL.                                                ELTGVLPL
00242      PERFORM 1100-OBTN-TPC-HDR.                                   ELTGVLPL
00243      MOVE +1 TO IOP-TSQ-ITEM-NBR.                                 ELTGVLPL
00244      SET FIRST-READ    TO TRUE.                                   ELTGVLPL
00245      SET NOT-END-OF-WKFL4 TO TRUE.                                ELTGVLPL
00246      SET IOP-RD        TO TRUE.                                   ELTGVLPL
00247      PERFORM 6000-READ-THE-WORK-FILE.                             ELTGVLPL
00248      IF NOT IOP-RC-OK                                             ELTGVLPL
00249         SET CIA-AB-CRITIO TO TRUE                                 ELTGVLPL
00250         EXEC CICS ABEND                                           ELTGVLPL
00251                   ABCODE(CIA-ABCODE)                              ELTGVLPL
00252         END-EXEC                                                  ELTGVLPL
00253      ELSE                                                         ELTGVLPL
00254         SET ADDRESS OF GVL4-OVERLAY-RECORD TO IOP-REC-PTR         ELTGVLPL
00255      END-IF.                                                      ELTGVLPL
00256      SET NOT-FIRST-READ TO TRUE.                                  ELTGVLPL
00257      SET WS-TEXT-NOT-DISPLAYED TO TRUE.                           ELTGVLPL
00258      IF SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH                ELTGVLPL
00259         PERFORM 1700-CREATE-INST-SPC-DISPLAY.                     ELTGVLPL
00260      IF NOT-END-OF-WKFL4                                          ELTGVLPL
00261         IF SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH             ELTGVLPL
00262            PERFORM 2000-CREATE-PROF-SPC-DISPLAY.                  ELTGVLPL
00263      SET COF-CONTINUE TO TRUE.                                    ELTGVLPL
00264      PERFORM 9000-CALL-ELUOUTPT.                                  ELTGVLPL
00265      MOVE 0 TO COF-NBR-DTL-LINES.                                 ELTGVLPL
00266      SET COF-END TO TRUE.                                         ELTGVLPL
00267      PERFORM 9000-CALL-ELUOUTPT.                                  ELTGVLPL
00268                                                                   ELTGVLPL
00269 ************************************************************      ELTGVLPL
00270 *                                                          *      ELTGVLPL
00271 *        OBTAIN TOPIC HEADER                               *      ELTGVLPL
00272 *                                                          *      ELTGVLPL
00273 ************************************************************      ELTGVLPL
00274  1100-OBTN-TPC-HDR.                                               ELTGVLPL
00275      MOVE 2 TO COF-NBR-HDR-LINES.                                 ELTGVLPL
00276      MOVE 0 TO COF-NBR-DTL-LINES.                                 ELTGVLPL
00277      MOVE WS-SPC-LST-HDR TO COF-HDR-LINE(COF-NBR-HDR-LINES).      ELTGVLPL
00278      ADD 1 TO COF-NBR-HDR-LINES.                                  ELTGVLPL
00279      MOVE SPACES TO COF-HDR-LINE(COF-NBR-HDR-LINES).              ELTGVLPL
00280      SET COF-NEW-PAGE TO TRUE.                                    ELTGVLPL
00281                                                                   ELTGVLPL
00282 ************************************************************      ELTGVLPL
00283 *                                                          *      ELTGVLPL
00284 *        OBTAIN DATES                                      *      ELTGVLPL
00285 *                                                          *      ELTGVLPL
00286 ************************************************************      ELTGVLPL
00287  1300-OBTAIN-DATES.                                               ELTGVLPL
00288      MOVE SSB-SRV-FROM-DATE TO HGADATE-JULIAN1.                   ELTGVLPL
00289      PERFORM 1400-CONVERT-DATE.                                   ELTGVLPL
00290      MOVE HGADATE-DATE2     TO WS-FRM-DT-MSG-2.                   ELTGVLPL
00291      ADD +1 TO TCAR-FROM-SUB.                                     ELTGVLPL
00292      MOVE WS-FRM-DT-MSG-2 TO TCAR-FROM-LINE (TCAR-FROM-SUB).      ELTGVLPL
00293      ADD +1 TO TCAR-FROM-SUB.                                     ELTGVLPL
00294      MOVE WS-LST-DTL-AND  TO TCAR-FROM-LINE (TCAR-FROM-SUB).      ELTGVLPL
00295      MOVE SSB-SRV-TO-DATE   TO HGADATE-JULIAN1.                   ELTGVLPL
00296      PERFORM 1400-CONVERT-DATE.                                   ELTGVLPL
00297      MOVE HGADATE-DATE2     TO WS-TO-DT-MSG-2.                    ELTGVLPL
00298      ADD +1 TO TCAR-FROM-SUB.                                     ELTGVLPL
00299      MOVE WS-TO-DT-MSG-2 TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELTGVLPL
00300      ADD +1 TO TCAR-FROM-SUB.                                     ELTGVLPL
00301      MOVE WS-LST-DTL-COMMA TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTGVLPL
00302                                                                   ELTGVLPL
00303 ************************************************************      ELTGVLPL
00304 *                                                          *      ELTGVLPL
00305 *        CONVERT DATES FROM JULIAN TO GREGORIAN            *      ELTGVLPL
00306 *                                                          *      ELTGVLPL
00307 ************************************************************      ELTGVLPL
00308  1400-CONVERT-DATE.                                               ELTGVLPL
00309      MOVE 'CNV' TO HGADATE-FUNC.                                  ELTGVLPL
00310      MOVE 'J'   TO HGADATE-FORM1.                                 ELTGVLPL
00311      MOVE 'M'   TO HGADATE-FORM2.                                 ELTGVLPL
00312      MOVE ZEROS TO HGADATE-RETURN, HGADATE-AMOUNT,                ELTGVLPL
00313                    HGADATE-DATE2.                                 ELTGVLPL
00314      EXEC CICS LINK PROGRAM ('HGADATES')                          ELTGVLPL
00315                     COMMAREA (HGADATES-PARM-LIST)                 ELTGVLPL
00316                     END-EXEC.                                     ELTGVLPL
00317                                                                   ELTGVLPL
00318 ************************************************************      ELTGVLPL
00319 *                                                          *      ELTGVLPL
00320 *  CREATE INSTITUTIONAL SPECIAL PROVIDER CONTROL DISPLAY   *      ELTGVLPL
00321 *                                                          *      ELTGVLPL
00322 ************************************************************      ELTGVLPL
00323  1700-CREATE-INST-SPC-DISPLAY.                                    ELTGVLPL
00324      IF SSB-PROV-CLASS-INST                                       ELTGVLPL
00325         IF NOT GVL4-INSTITUTIONAL                                 ELTGVLPL
00326            ADD +1 TO COF-NBR-DTL-LINES                            ELTGVLPL
00327            MOVE SPACES                                            ELTGVLPL
00328                 TO COF-DTL-LINE (COF-NBR-DTL-LINES)               ELTGVLPL
00329            ADD +1 TO COF-NBR-DTL-LINES                            ELTGVLPL
00330            MOVE WS-NO-INST-PROV-MSG                               ELTGVLPL
00331                 TO COF-DTL-LINE (COF-NBR-DTL-LINES)               ELTGVLPL
00332         ELSE                                                      ELTGVLPL
00333            PERFORM 5700-PREPARE-DTL-MSG-FOR-DSPLY                 ELTGVLPL
00334            SET WS-TEXT-DISPLAYED TO TRUE                          ELTGVLPL
00335            ADD +1 TO COF-NBR-DTL-LINES                            ELTGVLPL
00336            MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES)        ELTGVLPL
00337            ADD +1 TO COF-NBR-DTL-LINES                            ELTGVLPL
00338            MOVE WS-INST-STATEMENT                                 ELTGVLPL
00339              TO COF-DTL-LINE (COF-NBR-DTL-LINES)                  ELTGVLPL
00340            PERFORM 1800-CREATE-INST-DISP                          ELTGVLPL
00341                UNTIL NOT GVL4-INSTITUTIONAL OR END-OF-WKFL4       ELTGVLPL
00342      ELSE                                                         ELTGVLPL
00343         IF GVL4-INSTITUTIONAL                                     ELTGVLPL
00344            PERFORM 5700-PREPARE-DTL-MSG-FOR-DSPLY                 ELTGVLPL
00345            SET WS-TEXT-DISPLAYED TO TRUE                          ELTGVLPL
00346            ADD +1 TO COF-NBR-DTL-LINES                            ELTGVLPL
00347            MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES)        ELTGVLPL
00348            ADD +1 TO COF-NBR-DTL-LINES                            ELTGVLPL
00349            MOVE WS-INST-STATEMENT                                 ELTGVLPL
00350              TO COF-DTL-LINE (COF-NBR-DTL-LINES)                  ELTGVLPL
00351            PERFORM 1800-CREATE-INST-DISP                          ELTGVLPL
00352                UNTIL NOT GVL4-INSTITUTIONAL OR END-OF-WKFL4.      ELTGVLPL
00353                                                                   ELTGVLPL
00354 ************************************************************      ELTGVLPL
00355 *                                                          *      ELTGVLPL
00356 *  CREATE VARIABLE PROVIDER INSTITUTIONAL DISPLAY          *      ELTGVLPL
00357 *                                                          *      ELTGVLPL
00358 ************************************************************      ELTGVLPL
00359  1800-CREATE-INST-DISP.                                           ELTGVLPL
00360      IF NOT IOP-RC-OK                                             ELTGVLPL
00361         PERFORM 9500-SIGNAL-FILE-PROBLEM                          ELTGVLPL
00362      ELSE                                                         ELTGVLPL
00363         PERFORM 1900-PRCSS-RMNNG-INST-WKFL.                       ELTGVLPL
00364      SET NOT-FIRST-READ TO TRUE.                                  ELTGVLPL
00365                                                                   ELTGVLPL
00366 ************************************************************      ELTGVLPL
00367 *                                                          *      ELTGVLPL
00368 *  PROCESS REMAINING INSTITUTIONAL WORKFILE                *      ELTGVLPL
00369 *                                                          *      ELTGVLPL
00370 ************************************************************      ELTGVLPL
00371  1900-PRCSS-RMNNG-INST-WKFL.                                      ELTGVLPL
00372      SET PROVIDER-NOT-ACCEPTED TO TRUE.                           ELTGVLPL
00373      PERFORM 5000-DISPLAY-PROVIDERS.                              ELTGVLPL
00374                                                                   ELTGVLPL
00375 ************************************************************      ELTGVLPL
00376 *                                                          *      ELTGVLPL
00377 *  CREATE PROFESSIONAL SPECIAL PROVIDER DISPLAY            *      ELTGVLPL
00378 *                                                          *      ELTGVLPL
00379 ************************************************************      ELTGVLPL
00380  2000-CREATE-PROF-SPC-DISPLAY.                                    ELTGVLPL
00381         PERFORM 5500-SCAN-FOR-PROF                                ELTGVLPL
00382            UNTIL END-OF-WKFL4                                     ELTGVLPL
00383               OR GVL4-PROFESSIONAL.                               ELTGVLPL
00384         IF NOT-END-OF-WKFL4                                       ELTGVLPL
00385            IF WS-TEXT-NOT-DISPLAYED                               ELTGVLPL
00386               PERFORM 5700-PREPARE-DTL-MSG-FOR-DSPLY              ELTGVLPL
00387               SET WS-TEXT-DISPLAYED TO TRUE                       ELTGVLPL
00388            END-IF                                                 ELTGVLPL
00389            PERFORM 5600-MOVE-PROF-STATEMENT                       ELTGVLPL
00390            PERFORM 5800-CREATE-PROF-DISP                          ELTGVLPL
00391                UNTIL NOT GVL4-PROFESSIONAL OR END-OF-WKFL4        ELTGVLPL
00392         ELSE                                                      ELTGVLPL
00393             ADD +1 TO COF-NBR-DTL-LINES                           ELTGVLPL
00394             MOVE SPACES                                           ELTGVLPL
00395                  TO COF-DTL-LINE (COF-NBR-DTL-LINES)              ELTGVLPL
00396             ADD +1 TO COF-NBR-DTL-LINES                           ELTGVLPL
00397             MOVE WS-NO-PROF-PROV-MSG                              ELTGVLPL
00398                  TO COF-DTL-LINE (COF-NBR-DTL-LINES)              ELTGVLPL
00399         END-IF.                                                   ELTGVLPL
00400                                                                   ELTGVLPL
00401 ************************************************************      ELTGVLPL
00402 *                                                          *      ELTGVLPL
00403 *  DISPLAY PROVIDERS                                       *      ELTGVLPL
00404 *                                                          *      ELTGVLPL
00405 ************************************************************      ELTGVLPL
00406  5000-DISPLAY-PROVIDERS.                                          ELTGVLPL
00407      PERFORM 7000-SCAN-FOR-ACCEPTED-PRVDR                         ELTGVLPL
00408              VARYING GVL4-INDEX FROM 1 BY 1                       ELTGVLPL
00409              UNTIL PROVIDER-ACCEPTED                              ELTGVLPL
00410                 OR GVL4-INDEX > GVL4-PRVDR-CNT.                   ELTGVLPL
00411      SET IOP-RD-NXT TO TRUE.                                      ELTGVLPL
00412      PERFORM 6000-READ-THE-WORK-FILE.                             ELTGVLPL
00413      IF IOP-RC-OK                                                 ELTGVLPL
00414         SET ADDRESS OF GVL4-OVERLAY-RECORD TO IOP-REC-PTR         ELTGVLPL
00415      ELSE                                                         ELTGVLPL
00416         IF IOP-RC-NOTFND                                          ELTGVLPL
00417            SET END-OF-WKFL4 TO TRUE                               ELTGVLPL
00418         ELSE                                                      ELTGVLPL
00419            SET CIA-AB-CRITIO TO TRUE                              ELTGVLPL
00420            PERFORM 9999-LINK-TO-ABEND                             ELTGVLPL
00421         END-IF                                                    ELTGVLPL
00422      END-IF.                                                      ELTGVLPL
00423                                                                   ELTGVLPL
00424 ************************************************************      ELTGVLPL
00425 *                                                          *      ELTGVLPL
00426 *  SCAN FOR PROFESSIONAL                                   *      ELTGVLPL
00427 *                                                          *      ELTGVLPL
00428 ************************************************************      ELTGVLPL
00429  5500-SCAN-FOR-PROF.                                              ELTGVLPL
00430      SET IOP-RD-NXT TO TRUE.                                      ELTGVLPL
00431      PERFORM 6000-READ-THE-WORK-FILE.                             ELTGVLPL
00432      IF IOP-RC-OK                                                 ELTGVLPL
00433         IF GVL4-PROFESSIONAL                                      ELTGVLPL
00434            CONTINUE                                               ELTGVLPL
00435         END-IF                                                    ELTGVLPL
00436      ELSE                                                         ELTGVLPL
00437         IF IOP-RC-NOTFND                                          ELTGVLPL
00438            SET END-OF-WKFL4 TO TRUE                               ELTGVLPL
00439         ELSE                                                      ELTGVLPL
00440            SET CIA-AB-CRITIO TO TRUE                              ELTGVLPL
00441            PERFORM 9999-LINK-TO-ABEND                             ELTGVLPL
00442         END-IF                                                    ELTGVLPL
00443      END-IF.                                                      ELTGVLPL
00444                                                                   ELTGVLPL
00445 ************************************************************      ELTGVLPL
00446 *                                                          *      ELTGVLPL
00447 *        MOVE PROFESSIONAL STATEMENT                       *      ELTGVLPL
00448 *                                                          *      ELTGVLPL
00449 ************************************************************      ELTGVLPL
00450  5600-MOVE-PROF-STATEMENT.                                        ELTGVLPL
00451      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTGVLPL
00452      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTGVLPL
00453      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTGVLPL
00454      MOVE WS-PROF-STATEMENT                                       ELTGVLPL
00455        TO COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELTGVLPL
00456                                                                   ELTGVLPL
00457 ************************************************************      ELTGVLPL
00458 *                                                          *      ELTGVLPL
00459 *        PREPARE DETAIL MESSAGE FOR DISPLAY                *      ELTGVLPL
00460 *                                                          *      ELTGVLPL
00461 ************************************************************      ELTGVLPL
00462  5700-PREPARE-DTL-MSG-FOR-DSPLY.                                  ELTGVLPL
00463      INITIALIZE TCAR-FROM-AREA.                                   ELTGVLPL
00464      MOVE +1 TO TCAR-FROM-SUB.                                    ELTGVLPL
00465      MOVE WS-LST-DTL-MSG-1 TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTGVLPL
00466      PERFORM 1300-OBTAIN-DATES.                                   ELTGVLPL
00467      ADD +1 TO TCAR-FROM-SUB.                                     ELTGVLPL
00468      MOVE WS-LST-DTL-MSG-2 TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTGVLPL
00469      ADD +1 TO TCAR-FROM-SUB.                                     ELTGVLPL
00470      MOVE WS-LST-DTL-MSG-3 TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTGVLPL
00471      ADD +1 TO TCAR-FROM-SUB.                                     ELTGVLPL
00472      MOVE WS-LST-DTL-MSG-4 TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTGVLPL
00473      ADD +1 TO TCAR-FROM-SUB.                                     ELTGVLPL
00474      MOVE WS-LST-DTL-MSG-5 TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTGVLPL
00475      PERFORM 5900-PRPR-CMPRSSN-UNSTRNG.                           ELTGVLPL
00476      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTGVLPL
00477      PERFORM VARYING TCAR-FROM-SUB                                ELTGVLPL
00478          FROM 1 BY 1 UNTIL                                        ELTGVLPL
00479               TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED             ELTGVLPL
00480         MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO                      ELTGVLPL
00481           COF-DTL-LINE (COF-NBR-DTL-LINES)                        ELTGVLPL
00482         ADD 1 TO COF-NBR-DTL-LINES                                ELTGVLPL
00483      END-PERFORM.                                                 ELTGVLPL
00484                                                                   ELTGVLPL
00485 ************************************************************      ELTGVLPL
00486 *                                                          *      ELTGVLPL
00487 *  CREATE VARIABLE PROVIDER PROFESSIONAL DISPLAY           *      ELTGVLPL
00488 *                                                          *      ELTGVLPL
00489 ************************************************************      ELTGVLPL
00490  5800-CREATE-PROF-DISP.                                           ELTGVLPL
00491      IF IOP-RC-OK                                                 ELTGVLPL
00492         SET PROVIDER-NOT-ACCEPTED TO TRUE                         ELTGVLPL
00493         PERFORM 5000-DISPLAY-PROVIDERS.                           ELTGVLPL
00494      SET NOT-FIRST-READ TO TRUE.                                  ELTGVLPL
00495                                                                   ELTGVLPL
00496 ************************************************************      ELTGVLPL
00497 *                                                          *      ELTGVLPL
00498 *        PREPARE TO DO COMPARISON UNSTRING                 *      ELTGVLPL
00499 *                                                          *      ELTGVLPL
00500 ************************************************************      ELTGVLPL
00501  5900-PRPR-CMPRSSN-UNSTRNG.                                       ELTGVLPL
00502      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTGVLPL
00503      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTGVLPL
00504      PERFORM 5950-UNSTRING-TEXT.                                  ELTGVLPL
00505                                                                   ELTGVLPL
00506 ************************************************************      ELTGVLPL
00507 *                                                          *      ELTGVLPL
00508 *        UNSTRING TEXT                                     *      ELTGVLPL
00509 *                                                          *      ELTGVLPL
00510 ************************************************************      ELTGVLPL
00511  5950-UNSTRING-TEXT.                                              ELTGVLPL
00512      MOVE +5 TO TCAR-OUTPUT-FIELD-COUNT.                          ELTGVLPL
00513      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTGVLPL
00514      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTGVLPL
00515      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTGVLPL
00516      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTGVLPL
00517      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTGVLPL
00518      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTGVLPL
00519                                                                   ELTGVLPL
00520 ************************************************************      ELTGVLPL
00521 *                                                          *      ELTGVLPL
00522 *        READ THE WORK FILE                                *      ELTGVLPL
00523 *                                                          *      ELTGVLPL
00524 ************************************************************      ELTGVLPL
00525  6000-READ-THE-WORK-FILE.                                         ELTGVLPL
00526      SET CIA-ELSWKFL4-DDN  TO TRUE.                               ELTGVLPL
00527      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTGVLPL
00528                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.      ELTGVLPL
00529      SET CIA-ELSWKFL4-DDN  TO TRUE.                               ELTGVLPL
00530      SET IOP-REC-PTR       TO NULL.                               ELTGVLPL
00531      SET IOP-FCQ-NONE      TO TRUE.                               ELTGVLPL
00532      SET IOP-KVQ-NONE      TO TRUE.                               ELTGVLPL
00533      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELTGVLPL
00534      CALL 'ELUIOPGM' USING DFHEIBLK                               ELTGVLPL
00535                            DFHCOMMAREA.                           ELTGVLPL
00536                                                                   ELTGVLPL
00537 ************************************************************      ELTGVLPL
00538 *                                                          *      ELTGVLPL
00539 *  SCAN WORK FILE DETAIL FOR ACCEPTED PROVIDERS            *      ELTGVLPL
00540 *                                                          *      ELTGVLPL
00541 ************************************************************      ELTGVLPL
00542  7000-SCAN-FOR-ACCEPTED-PRVDR.                                    ELTGVLPL
00543      IF SSB-SRV-FROM-DT-CEN <= GVL4-PRVDR-TRMTNDT-CEN             ELTGVLPL
00544              (GVL4-INDEX)  AND                                    ELTGVLPL
00545       SSB-SRV-TO-DT-CEN   >= GVL4-PRVDR-EFFDT-CEN (GVL4-INDEX)    ELTGVLPL
00546           PERFORM 8000-FORMAT-OUTPUT-DISPLAY.                     ELTGVLPL
00547                                                                   ELTGVLPL
00548 ************************************************************      ELTGVLPL
00549 *                                                          *      ELTGVLPL
00550 *  FORMAT OUTPUT DISPLAY                                   *      ELTGVLPL
00551 *                                                          *      ELTGVLPL
00552 ************************************************************      ELTGVLPL
00553  8000-FORMAT-OUTPUT-DISPLAY.                                      ELTGVLPL
00554      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTGVLPL
00555      MOVE GVL4-PRVDR-NM TO WS-NAME-HOLD.                          ELTGVLPL
00556      MOVE GVL4-PRVDR-NBR TO WS-NUMBER-HOLD.                       ELTGVLPL
00557      MOVE WS-HOLD-AREA TO COF-DTL-LINE (COF-NBR-DTL-LINES).       ELTGVLPL
00558      SET PROVIDER-ACCEPTED TO TRUE.                               ELTGVLPL
00559      IF COF-NBR-DTL-LINES > 17                                    ELTGVLPL
00560         SET COF-CONTINUE TO TRUE                                  ELTGVLPL
00561         PERFORM 9000-CALL-ELUOUTPT.                               ELTGVLPL
00562                                                                   ELTGVLPL
00563 ************************************************************      ELTGVLPL
00564 *                                                          *      ELTGVLPL
00565 *  CALL ELUOUTPT                                           *      ELTGVLPL
00566 *                                                          *      ELTGVLPL
00567 ************************************************************      ELTGVLPL
00568  9000-CALL-ELUOUTPT.                                              ELTGVLPL
00569      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTGVLPL
00570                            DFHCOMMAREA.                           ELTGVLPL
00571      MOVE 0 TO COF-NBR-DTL-LINES.                                 ELTGVLPL
00572                                                                   ELTGVLPL
00573 ************************************************************      ELTGVLPL
00574 *                                                          *      ELTGVLPL
00575 *  SIGNAL FILE PROBLEM                                     *      ELTGVLPL
00576 *                                                          *      ELTGVLPL
00577 ************************************************************      ELTGVLPL
00578  9500-SIGNAL-FILE-PROBLEM.                                        ELTGVLPL
00579      IF FIRST-READ                                                ELTGVLPL
00580         SET CIA-AB-PGM-LOGIC TO TRUE                              ELTGVLPL
00581         PERFORM 9999-LINK-TO-ABEND                                ELTGVLPL
00582      ELSE                                                         ELTGVLPL
00583         SET CIA-AB-CRITIO TO TRUE.                                ELTGVLPL
00584                                                                   ELTGVLPL
00585 ************************************************************      ELTGVLPL
00586 *                                                          *      ELTGVLPL
00587 *        LINK TO ABEND                                     *      ELTGVLPL
00588 *                                                          *      ELTGVLPL
00589 ************************************************************      ELTGVLPL
00590  9999-LINK-TO-ABEND.                                              ELTGVLPL
00591      EXEC CICS ABEND                                              ELTGVLPL
00592                ABCODE(CIA-ABCODE)                                 ELTGVLPL
00593                                                                   ELTGVLPL
00594                END-EXEC.                                          ELTGVLPL
00595                                                                   ELTGVLPL
