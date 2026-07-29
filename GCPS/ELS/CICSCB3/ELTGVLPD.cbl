00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELTGVLPD
00003  PROGRAM-ID.         ELTGVLPD.                                       LV002
00004                                                                   ELTGVLPD
00005  AUTHOR.             ANNE KEFFER KING.                            ELTGVLPD
00006                                                                   ELTGVLPD
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTGVLPD
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTGVLPD
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTGVLPD
00010                      233 N. MICHIGAN AVE                          ELTGVLPD
00011                      CHICAGO, ILLINOIS 60601                      ELTGVLPD
00012                                                                   ELTGVLPD
00013  DATE-WRITTEN.       01-20-1994.                                  ELTGVLPD
00014                                                                   ELTGVLPD
00015  DATE-COMPILED.                                                   ELTGVLPD
00016                                                                   ELTGVLPD
00017  SECURITY.           COPYRIGHT 1993,                              ELTGVLPD
00018                      HEALTH CARE SERVICE CORPORATION              ELTGVLPD
00019      SKIP3                                                        ELTGVLPD
00020  ENVIRONMENT DIVISION.                                            ELTGVLPD
00021                                                                   ELTGVLPD
00022  CONFIGURATION SECTION.                                           ELTGVLPD
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELTGVLPD
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELTGVLPD
00025 ****************************************************************  ELTGVLPD
00026 *                                                              *  ELTGVLPD
00027 *    PROGRAM:    ELTGVLPD                                      *  ELTGVLPD
00028 *    DATE:       20-OCT-1994                                   *  ELTGVLPD
00029 *    AUTHOR:     ANNE KEFFER KING                              *  ELTGVLPD
00030 *    FUNCTION:   DISPLAYS PROVIDER DETAIL                      *  ELTGVLPD
00031 *                                                              *  ELTGVLPD
00032 ****************************************************************  ELTGVLPD
00033 *                                                              *  ELTGVLPD
00034 *                   MAINTENANCE HISTORY                        *  ELTGVLPD
00035 *                                                              *  ELTGVLPD
00036 *  MOD     DATE     BY  DRPT              ACTION               *  ELTGVLPD
00037 * ----- ----------- --- ---- --------------------------------- *  ELTGVLPD
00038 * 01.00 20-JAN-1994 AKK      CREATED                           *  ELTGVLPD
00039 *                                                              *  ELTGVLPD
00040 * 02.00 05-AUG-1998 AKK      CHANGED FOR YR 2000.              *  ELTGVLPD
00041 *                                                              *  ELTGVLPD
00042 *       11-JAN-2000 JP       ADDED MLDATE ROUTINE TO CONVERT   *  ELTGVLPD
00043 *                            TERMINATION DATE TO MM/DD/CCYY    *  ELTGVLPD
00044 *                            FORMAT.                           *  ELTGVLPD
00045 *                                                              *  ELTGVLPD
00046 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE     *  ELTGVLPD
00047 *                                                              *  ELTGVLPD
00046 *       20-JUN-2006 AKK       COMPILING FOR UPDATE OF COPYBOOK *  ELTGVLPD
00047 *                             ELSGVX1C UPDATE TO 2500 RECORD   *  ELTGVLPD
00048 ****************************************************************  ELTGVLPD
00049      EJECT                                                        ELTGVLPD
00050  DATA DIVISION.                                                   ELTGVLPD
00051  WORKING-STORAGE SECTION.                                         ELTGVLPD
00052  01  WS-MISC.                                                     ELTGVLPD
00053      05  WS-NMBR-LINES                    PIC S9(04) COMP.        ELTGVLPD
00054      05  WS-CMPR-PRVDR-NMBR PIC X(10).                            ELTGVLPD
00055      05  WS-SPCL-FCLTY                    PIC X(24)  VALUE        ELTGVLPD
00056                               'SPECIAL FACILTY PROVIDER'.         ELTGVLPD
00057      05  WS-PRFRD-PRVDR                   PIC X(24)  VALUE        ELTGVLPD
00058                               'PREFERRED PROVIDER      '.         ELTGVLPD
00059      05  WS-AFLTD-PRVDR                   PIC X(24)  VALUE        ELTGVLPD
00060                               'AFFILIATED PROVIDER     '.         ELTGVLPD
00061      05  WS-DECISION-ENTRY                PIC X(01)  VALUE SPACES.ELTGVLPD
00062          88  WS-DECISION-MADE                        VALUE '1'.   ELTGVLPD
00063          88  WS-NO-DECISION-MADE                     VALUE '0'.   ELTGVLPD
00064      05  WS-PROVIDER-DETAIL-SW            PIC X(01)  VALUE SPACES.ELTGVLPD
00065          88  WS-END-PRVDR-DTL                        VALUE '1'.   ELTGVLPD
00066          88  WS-NO-END-PRVDR-DTL                     VALUE '0'.   ELTGVLPD
00067      05  WS-DATE-SETUP-SW                 PIC X(01)  VALUE SPACES.ELTGVLPD
00068          88  WS-DATE-SETUP-DONE                      VALUE '1'.   ELTGVLPD
00069          88  WS-NO-DATE-SETUP-DONE                   VALUE '0'.   ELTGVLPD
00070                                                                   ELTGVLPD
00071      05  WS-SPC-LST-HDR.                                          ELTGVLPD
00072          10  FILLER                       PIC X(15)  VALUE SPACES.ELTGVLPD
00073          10  HEADER-DETAIL                PIC X(50)  VALUE        ELTGVLPD
00074          'SPECIAL PROVIDER CONSIDERATIONS - PROVIDER DISPLAY'.    ELTGVLPD
00075                                                                   ELTGVLPD
00076      05  WS-PRVDR-DTL-LINE1.                                      ELTGVLPD
00077          10  WS-PRVDR-NM                PIC X(33)  VALUE SPACES.  ELTGVLPD
00078          10  FILLER                     PIC X(04)  VALUE SPACES.  ELTGVLPD
00079          10  WS-PRVDR-NMBR              PIC X(10)  VALUE SPACES.  ELTGVLPD
00080          10  FILLER                     PIC X(32)  VALUE SPACES.  ELTGVLPD
00081                                                                   ELTGVLPD
00082      05  WS-PRVDR-DTL-LINE2.                                      ELTGVLPD
00083          10  FILLER                     PIC X(04)  VALUE SPACES.  ELTGVLPD
00084          10  FILLER                     PIC X(73)  VALUE          ELTGVLPD
00085          'PARTICIPATES IN A NETWORK OF PROVIDERS FOR THIS GROUP ASELTGVLPD
00086 -        ' FOLLOWS:'.                                             ELTGVLPD
00087          10  FILLER                     PIC X(02)  VALUE SPACES.  ELTGVLPD
00088                                                                   ELTGVLPD
00089      05  WS-PRVDR-DTL-LINE3.                                      ELTGVLPD
00090          10  FILLER                     PIC X(07)  VALUE SPACES.  ELTGVLPD
00091          10  FILLER                     PIC X(05)  VALUE 'FROM:'. ELTGVLPD
00092          10  FILLER                     PIC X(02)  VALUE SPACES.  ELTGVLPD
00093          10  WS-FROM-DT                 PIC 99/99/99.             ELTGVLPD
00094          10  FILLER                     PIC X(02)  VALUE SPACES.  ELTGVLPD
00095          10  FILLER                     PIC X(03)  VALUE 'TO:'.   ELTGVLPD
00096          10  FILLER                     PIC X(02)  VALUE SPACES.  ELTGVLPD
00097          10  WS-TO-DT                   PIC 99/99/9999.           ELTGVLPD
00098          10  FILLER                     PIC X(02)  VALUE SPACES.  ELTGVLPD
00099          10  WS-PRVDR-CNTRL             PIC X(24)  VALUE SPACES.  ELTGVLPD
00100          10  FILLER                     PIC X(14)  VALUE SPACES.  ELTGVLPD
00101                                                                   ELTGVLPD
00102      05  WS-PRVDR-DTL-LINE3A.                                     ELTGVLPD
00103          10  FILLER                     PIC X(46)  VALUE SPACES.  ELTGVLPD
00104          10  WS-PRVDR-CNTRL2            PIC X(24)  VALUE SPACES.  ELTGVLPD
00105          10  FILLER                     PIC X(16)  VALUE SPACES.  ELTGVLPD
00106                                                                   ELTGVLPD
00107      05  WS-PRVDR-DTL-LINE4.                                      ELTGVLPD
00108          10  FILLER                     PIC X(16)  VALUE          ELTGVLPD
00109              'PROVIDER NUMBER '.                                  ELTGVLPD
00110          10  WS-PRVDR-NMBR-4            PIC X(10)  VALUE SPACES.  ELTGVLPD
00111          10  FILLER                     PIC X(43)  VALUE          ELTGVLPD
00112              ' DOES NOT PARTICIPATE FOR THE ABOVE DATES.'.        ELTGVLPD
00113          10  FILLER                     PIC X(10)  VALUE SPACES.  ELTGVLPD
00114                                                                   ELTGVLPD
00115                                                                   ELTGVLPD
00116       05  WS-HOLD-AREA.                                           ELTGVLPD
00117          10  WS-HOLD-PRVDR-NM             PIC X(33) VALUE SPACE.  ELTGVLPD
00118          10  WS-HOLD-PRVDR-NMBR           PIC X(10) VALUE SPACE.  ELTGVLPD
00119                                                                   ELTGVLPD
00120  01 HGADATES-PARM-LIST.                                           ELTGVLPD
00121     COPY HGCDAT01.                                                ELTGVLPD
00122     COPY MLDATE01.                                                ELTGVLPD
00123                                                                   ELTGVLPD
00124  LINKAGE SECTION.                                                 ELTGVLPD
00125  01  DFHCOMMAREA.                                                 ELTGVLPD
00126  COPY ELSCOMMC.                                                   ELTGVLPD
00127      EJECT                                                        ELTGVLPD
00128  COPY ELSCIA2C.                                                   ELTGVLPD
00129      EJECT                                                        ELTGVLPD
00130  COPY ELSIOPMC.                                                   ELTGVLPD
00131      EJECT                                                        ELTGVLPD
00132  COPY ELSSSCBC.                                                   ELTGVLPD
00133      EJECT                                                        ELTGVLPD
00134  COPY ELSTCWAC.                                                   ELTGVLPD
00135      EJECT                                                        ELTGVLPD
00136  COPY ELSOUTPC.                                                   ELTGVLPD
00137      EJECT                                                        ELTGVLPD
00138  COPY ELSGVL4C.                                                   ELTGVLPD
00139      EJECT                                                        ELTGVLPD
00140  COPY ELSGVX1C.                                                   ELTGVLPD
00141      EJECT                                                        ELTGVLPD
00142  PROCEDURE DIVISION.                                              ELTGVLPD
00143 ************************************************************      ELTGVLPD
00144 *                                                          *      ELTGVLPD
00145 *        ELSGVLPD MAINLINE                                 *      ELTGVLPD
00146 *                                                          *      ELTGVLPD
00147 ************************************************************      ELTGVLPD
00148  0000-ELTGVLPD-MAINLINE.                                          ELTGVLPD
00149      PERFORM 0100-INITIALIZE.                                     ELTGVLPD
00150      PERFORM 1000-PROCESS-GVL-DISPLAY.                            ELTGVLPD
00151      SET COF-END TO TRUE.                                         ELTGVLPD
00152      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTGVLPD
00153                            DFHCOMMAREA.                           ELTGVLPD
00154      GOBACK.                                                      ELTGVLPD
00155                                                                   ELTGVLPD
00156 ************************************************************      ELTGVLPD
00157 *                                                          *      ELTGVLPD
00158 *        INITIALIZATION                                    *      ELTGVLPD
00159 *                                                          *      ELTGVLPD
00160 ************************************************************      ELTGVLPD
00161  0100-INITIALIZE.                                                 ELTGVLPD
00162      PERFORM 0200-ESTABLISH-ENVIRONMENT.                          ELTGVLPD
00163      PERFORM 0300-EST-ADDRS-OF-SSB.                               ELTGVLPD
00164      PERFORM 0400-EST-ADDRS-OF-TCWA.                              ELTGVLPD
00165      PERFORM 0500-EST-ADDRS-OF-OUTP.                              ELTGVLPD
00166      PERFORM 0600-EST-ADDRS-OF-WKFL4.                             ELTGVLPD
00167      PERFORM 0700-EST-ADDRS-OF-WKFL1.                             ELTGVLPD
00168                                                                   ELTGVLPD
00169 ************************************************************      ELTGVLPD
00170 *                                                          *      ELTGVLPD
00171 *             ESTABLISH ENVIRONMENT                        *      ELTGVLPD
00172 *                                                          *      ELTGVLPD
00173 ************************************************************      ELTGVLPD
00174  0200-ESTABLISH-ENVIRONMENT.                                      ELTGVLPD
00175      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELTGVLPD
00176         EXEC CICS ABEND                                           ELTGVLPD
00177                   ABCODE ('EL01')                                 ELTGVLPD
00178         END-EXEC                                                  ELTGVLPD
00179      ELSE                                                         ELTGVLPD
00180         CALL 'ELUINISM' USING DFHCOMMAREA                         ELTGVLPD
00181               ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA            ELTGVLPD
00182         IF ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA = NULLS       ELTGVLPD
00183            EXEC CICS ABEND                                        ELTGVLPD
00184                      ABCODE ('EL02')                              ELTGVLPD
00185            END-EXEC.                                              ELTGVLPD
00186                                                                   ELTGVLPD
00187 ************************************************************      ELTGVLPD
00188 *                                                          *      ELTGVLPD
00189 *        ESTABLISH ADDRESSABILITY TO SELECTOR STATUS BLOCK *      ELTGVLPD
00190 *                                                          *      ELTGVLPD
00191 ************************************************************      ELTGVLPD
00192  0300-EST-ADDRS-OF-SSB.                                           ELTGVLPD
00193      SET CIA-ELSSSCB-DDN TO TRUE                                  ELTGVLPD
00194      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTGVLPD
00195                      ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK       ELTGVLPD
00196      END-CALL                                                     ELTGVLPD
00197      IF CIA-RC-PTR-NULL                                           ELTGVLPD
00198         SET CIA-AB-PARM-MISSING TO TRUE                           ELTGVLPD
00199         EXEC CICS ABEND                                           ELTGVLPD
00200                   ABCODE(CIA-ABCODE)                              ELTGVLPD
00201         END-EXEC.                                                 ELTGVLPD
00202                                                                   ELTGVLPD
00203 ************************************************************      ELTGVLPD
00204 *                                                          *      ELTGVLPD
00205 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WRK  *      ELTGVLPD
00206 *                                                          *      ELTGVLPD
00207 ************************************************************      ELTGVLPD
00208  0400-EST-ADDRS-OF-TCWA.                                          ELTGVLPD
00209      SET CIA-ELSTCWA-DDN TO TRUE                                  ELTGVLPD
00210      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTGVLPD
00211                     ADDRESS OF TCAR-COMPRESSION-WORK-AREA         ELTGVLPD
00212      END-CALL.                                                    ELTGVLPD
00213      IF CIA-RC-PTR-NULL                                           ELTGVLPD
00214         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTGVLPD
00215         EXEC CICS ABEND                                           ELTGVLPD
00216                   ABCODE(CIA-ABCODE)                              ELTGVLPD
00217         END-EXEC                                                  ELTGVLPD
00218      END-IF.                                                      ELTGVLPD
00219                                                                   ELTGVLPD
00220 ************************************************************      ELTGVLPD
00221 *                                                          *      ELTGVLPD
00222 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTGVLPD
00223 *                                                          *      ELTGVLPD
00224 ************************************************************      ELTGVLPD
00225  0500-EST-ADDRS-OF-OUTP.                                          ELTGVLPD
00226      SET CIA-ELSOUTP-DDN TO TRUE                                  ELTGVLPD
00227      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTGVLPD
00228                     ADDRESS OF COF-OUTPUT-INTERFACE               ELTGVLPD
00229      END-CALL.                                                    ELTGVLPD
00230      IF CIA-RC-PTR-NULL                                           ELTGVLPD
00231         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELTGVLPD
00232         EXEC CICS ABEND                                           ELTGVLPD
00233                   ABCODE(CIA-ABCODE)                              ELTGVLPD
00234         END-EXEC                                                  ELTGVLPD
00235      END-IF.                                                      ELTGVLPD
00236                                                                   ELTGVLPD
00237 ************************************************************      ELTGVLPD
00238 *                                                          *      ELTGVLPD
00239 *        ESTABLISH ADDRESSABILITY OF WORKFILE 4            *      ELTGVLPD
00240 *                                                          *      ELTGVLPD
00241 ************************************************************      ELTGVLPD
00242  0600-EST-ADDRS-OF-WKFL4.                                         ELTGVLPD
00243      SET CIA-ELSWKFL4-DDN TO TRUE.                                ELTGVLPD
00244      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTGVLPD
00245                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS       ELTGVLPD
00246      END-CALL.                                                    ELTGVLPD
00247      IF CIA-RC-PTR-NULL                                           ELTGVLPD
00248         SET CIA-ELSWKFL4-DDN TO TRUE                              ELTGVLPD
00249         SET CIA-STG-GETMAIN TO TRUE                               ELTGVLPD
00250         CALL 'ELUSTGMG' USING DFHEIBLK                            ELTGVLPD
00251                            DFHCOMMAREA                            ELTGVLPD
00252         SET CIA-ELSWKFL4-DDN TO TRUE                              ELTGVLPD
00253         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTGVLPD
00254                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS       ELTGVLPD
00255         END-CALL                                                  ELTGVLPD
00256      END-IF.                                                      ELTGVLPD
00257                                                                   ELTGVLPD
00258 ************************************************************      ELTGVLPD
00259 *                                                          *      ELTGVLPD
00260 *        ESTABLISH ADDRESSABILITY OF WORKFILE 1            *      ELTGVLPD
00261 *                                                          *      ELTGVLPD
00262 ************************************************************      ELTGVLPD
00263  0700-EST-ADDRS-OF-WKFL1.                                         ELTGVLPD
00264      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELTGVLPD
00265      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTGVLPD
00266                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS       ELTGVLPD
00267      END-CALL.                                                    ELTGVLPD
00268      IF CIA-RC-PTR-NULL                                           ELTGVLPD
00269         SET CIA-ELSWKFL1-DDN TO TRUE                              ELTGVLPD
00270         SET CIA-STG-GETMAIN TO TRUE                               ELTGVLPD
00271         CALL 'ELUSTGMG' USING DFHEIBLK                            ELTGVLPD
00272                            DFHCOMMAREA                            ELTGVLPD
00273         SET CIA-ELSWKFL1-DDN TO TRUE                              ELTGVLPD
00274         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTGVLPD
00275                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS       ELTGVLPD
00276         END-CALL                                                  ELTGVLPD
00277      END-IF.                                                      ELTGVLPD
00278                                                                   ELTGVLPD
00279 ************************************************************      ELTGVLPD
00280 *                                                          *      ELTGVLPD
00281 *        PROCESS VARIABLE PROVIDER DISPLAY                 *      ELTGVLPD
00282 *                                                          *      ELTGVLPD
00283 ************************************************************      ELTGVLPD
00284  1000-PROCESS-GVL-DISPLAY.                                        ELTGVLPD
00285      SET GVX-IDX                                                  ELTGVLPD
00286          GVX-HOLD-IDX TO 1.                                       ELTGVLPD
00287      SET IOP-RD TO TRUE.                                          ELTGVLPD
00288      PERFORM 7000-READ-WKFL1.                                     ELTGVLPD
00289      PERFORM 1500-SRCH-PRVDR-NMBR-OVRLY                           ELTGVLPD
00290          VARYING SSB-MNU-IDX FROM 1 BY 1                          ELTGVLPD
00291              UNTIL SSB-MNU-IDX > SSB-MNU-NUM-CHOICES.             ELTGVLPD
00292                                                                   ELTGVLPD
00293 ************************************************************      ELTGVLPD
00294 *                                                          *      ELTGVLPD
00295 *        SEARCH PROVIDER NUMBER DISPLAY                    *      ELTGVLPD
00296 *                                                          *      ELTGVLPD
00297 ************************************************************      ELTGVLPD
00298  1500-SRCH-PRVDR-NMBR-OVRLY.                                      ELTGVLPD
00299      MOVE 0 TO COF-NBR-DTL-LINES.                                 ELTGVLPD
00300      SET WS-NO-DECISION-MADE TO TRUE.                             ELTGVLPD
00301 *    MOVE GVX-PRVDR-NMBR(GVX-IDX) TO WS-CMPR-PRVDR-NMBR.          ELTGVLPD
00302      IF GVX-PRVDR-NMBR(GVX-IDX)                                   ELTGVLPD
00303               = SSB-CS-RESPONSE(SSB-CS-RESP-IDX)                  ELTGVLPD
00304         SET GVX-IDX TO GVX-HOLD-IDX                               ELTGVLPD
00305      ELSE                                                         ELTGVLPD
00306         SET GVX-IDX TO 1                                          ELTGVLPD
00307         MOVE 1 TO IOP-TSQ-ITEM-NBR                                ELTGVLPD
00308      END-IF.                                                      ELTGVLPD
00309      PERFORM 1600-EXMN-PRVDR-NMBR-FL                              ELTGVLPD
00310          VARYING GVX-IDX FROM GVX-IDX BY 1                        ELTGVLPD
00311             UNTIL WS-DECISION-MADE                                ELTGVLPD
00312             OR    (GVX-IDX > GVX-NMBR-PRVDR-ENTRS).               ELTGVLPD
00313         IF (GVX-IDX > GVX-NMBR-PRVDR-ENTRS                        ELTGVLPD
00314             AND WS-NO-DECISION-MADE)                              ELTGVLPD
00315            PERFORM 1601-PRPR-OTPT-LNS                             ELTGVLPD
00316            PERFORM 4000-DSPLY-PRVDR-NOT-FND                       ELTGVLPD
00317         END-IF.                                                   ELTGVLPD
00318      SET COF-NEW-PAGE TO TRUE.                                    ELTGVLPD
00319      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTGVLPD
00320                            DFHCOMMAREA.                           ELTGVLPD
00321                                                                   ELTGVLPD
00322 ************************************************************      ELTGVLPD
00323 *                                                          *      ELTGVLPD
00324 *   EXAMINE PROVIDER NUMBER FILE                           *      ELTGVLPD
00325 *                                                          *      ELTGVLPD
00326 ************************************************************      ELTGVLPD
00327  1600-EXMN-PRVDR-NMBR-FL.                                         ELTGVLPD
00328      SET WS-NO-END-PRVDR-DTL TO TRUE.                             ELTGVLPD
00329      IF GVX-PRVDR-NMBR (GVX-IDX)                                  ELTGVLPD
00330                      = SSB-CS-RESPONSE(SSB-CS-RESP-IDX)           ELTGVLPD
00331         SET CIA-ELSWKFL4-DDN TO TRUE                              ELTGVLPD
00332         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTGVLPD
00333               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS              ELTGVLPD
00334         SET IOP-TSQ-ITEM-NBR, GVX-HOLD-IDX TO GVX-IDX             ELTGVLPD
00335         SET IOP-RD TO TRUE                                        ELTGVLPD
00336         PERFORM 6000-READ-WKFL4                                   ELTGVLPD
00337         PERFORM 1625-PRPR-DSPLY-PRVDR-DTL                         ELTGVLPD
00338         SET WS-DECISION-MADE TO TRUE                              ELTGVLPD
00339      ELSE                                                         ELTGVLPD
00340         IF WS-CMPR-PRVDR-NMBR >                                   ELTGVLPD
00341                        SSB-CS-RESPONSE(SSB-CS-RESP-IDX)           ELTGVLPD
00342               PERFORM 4000-DSPLY-PRVDR-NOT-FND                    ELTGVLPD
00343               SET WS-DECISION-MADE TO TRUE                        ELTGVLPD
00344         END-IF                                                    ELTGVLPD
00345      END-IF.                                                      ELTGVLPD
00346                                                                   ELTGVLPD
00347                                                                   ELTGVLPD
00348 ************************************************************      ELTGVLPD
00349 *                                                          *      ELTGVLPD
00350 *   PREPARE OUTPUT LINES                                   *      ELTGVLPD
00351 *                                                          *      ELTGVLPD
00352 ************************************************************      ELTGVLPD
00353  1601-PRPR-OTPT-LNS.                                              ELTGVLPD
00354      MOVE COF-NBR-DTL-LINES TO WS-NMBR-LINES                      ELTGVLPD
00355      PERFORM VARYING COF-NBR-DTL-LINES FROM                       ELTGVLPD
00356        1 BY 1 UNTIL COF-NBR-DTL-LINES > WS-NMBR-LINES             ELTGVLPD
00357           MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES)         ELTGVLPD
00358      END-PERFORM.                                                 ELTGVLPD
00359      MOVE 0 TO COF-NBR-DTL-LINES.                                 ELTGVLPD
00360                                                                   ELTGVLPD
00361 ************************************************************      ELTGVLPD
00362 *                                                          *      ELTGVLPD
00363 *   PREPARE AND DISPLAY PROVIDER DETAIL                    *      ELTGVLPD
00364 *                                                          *      ELTGVLPD
00365 ************************************************************      ELTGVLPD
00366  1625-PRPR-DSPLY-PRVDR-DTL.                                       ELTGVLPD
00367      SET WS-NO-DATE-SETUP-DONE TO TRUE.                           ELTGVLPD
00368      SET GVX-HOLD-IDX TO GVX-IDX.                                 ELTGVLPD
00369      PERFORM VARYING GVL4-INDEX FROM 1 BY 1 UNTIL                 ELTGVLPD
00370           WS-END-PRVDR-DTL OR GVL4-INDEX > GVL4-PRVDR-CNT         ELTGVLPD
00371          IF SSB-SRV-FROM-DT-CEN <= GVL4-PRVDR-TRMTNDT-CEN         ELTGVLPD
00372                       (GVL4-INDEX) AND                            ELTGVLPD
00373           SSB-SRV-TO-DT-CEN  >= GVL4-PRVDR-EFFDT-CEN (GVL4-INDEX) ELTGVLPD
00374             IF WS-NO-DATE-SETUP-DONE                              ELTGVLPD
00375                PERFORM 1700-DSPLY-PRVDR-NM-NMBR                   ELTGVLPD
00376             END-IF                                                ELTGVLPD
00377             PERFORM 1750-DSPLY-APPLCBL-DATES                      ELTGVLPD
00378          END-IF                                                   ELTGVLPD
00379          IF GVL4-INDEX > GVL4-PRVDR-CNT AND                       ELTGVLPD
00380             (GVL4-PRVDR-CNT = 100)                                ELTGVLPD
00381               MOVE GVL4-PRVDR-NBR TO WS-HOLD-PRVDR-NMBR           ELTGVLPD
00382               SET IOP-RD-NXT TO TRUE                              ELTGVLPD
00383               PERFORM 6000-READ-WKFL4                             ELTGVLPD
00384               IF GVL4-PRVDR-NBR = WS-HOLD-PRVDR-NMBR              ELTGVLPD
00385                   SET GVL4-INDEX TO 1                             ELTGVLPD
00386               ELSE                                                ELTGVLPD
00387                  SET WS-END-PRVDR-DTL TO TRUE                     ELTGVLPD
00388               END-IF                                              ELTGVLPD
00389          ELSE                                                     ELTGVLPD
00390             IF GVL4-INDEX > GVL4-PRVDR-CNT                        ELTGVLPD
00391                SET WS-END-PRVDR-DTL TO TRUE                       ELTGVLPD
00392             ELSE                                                  ELTGVLPD
00393                CONTINUE                                           ELTGVLPD
00394             END-IF                                                ELTGVLPD
00395          END-IF                                                   ELTGVLPD
00396      END-PERFORM.                                                 ELTGVLPD
00397      IF WS-NO-DATE-SETUP-DONE                                     ELTGVLPD
00398           PERFORM 1601-PRPR-OTPT-LNS                              ELTGVLPD
00399           PERFORM 4000-DSPLY-PRVDR-NOT-FND                        ELTGVLPD
00400      END-IF.                                                      ELTGVLPD
00401                                                                   ELTGVLPD
00402 ************************************************************      ELTGVLPD
00403 *                                                          *      ELTGVLPD
00404 *   DISPLAY PROVIDER DETAIL                                *      ELTGVLPD
00405 *                                                          *      ELTGVLPD
00406 ************************************************************      ELTGVLPD
00407  1700-DSPLY-PRVDR-NM-NMBR.                                        ELTGVLPD
00408      MOVE GVL4-PRVDR-NM TO WS-PRVDR-NM.                           ELTGVLPD
00409      MOVE GVL4-PRVDR-NBR TO WS-PRVDR-NMBR.                        ELTGVLPD
00410      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTGVLPD
00411      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTGVLPD
00412      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTGVLPD
00413      MOVE WS-PRVDR-DTL-LINE1 TO COF-DTL-LINE (COF-NBR-DTL-LINES). ELTGVLPD
00414      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTGVLPD
00415      MOVE WS-PRVDR-DTL-LINE2 TO COF-DTL-LINE (COF-NBR-DTL-LINES). ELTGVLPD
00416      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTGVLPD
00417      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTGVLPD
00418                                                                   ELTGVLPD
00419 ************************************************************      ELTGVLPD
00420 *                                                          *      ELTGVLPD
00421 *   DISPLAY PROVIDER DETAIL                                *      ELTGVLPD
00422 *                                                          *      ELTGVLPD
00423 ************************************************************      ELTGVLPD
00424  1750-DSPLY-APPLCBL-DATES.                                        ELTGVLPD
00425      PERFORM 3300-OBTAIN-DATES.                                   ELTGVLPD
00426      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTGVLPD
00427      PERFORM 1775-DSPLY-PRVDR-CNTRL                               ELTGVLPD
00428      MOVE WS-PRVDR-DTL-LINE3 TO COF-DTL-LINE (COF-NBR-DTL-LINES). ELTGVLPD
00429      SET WS-DATE-SETUP-DONE TO TRUE.                              ELTGVLPD
00430      IF COF-NBR-DTL-LINES > 17                                    ELTGVLPD
00431         SET COF-CONTINUE TO TRUE                                  ELTGVLPD
00432         CALL 'ELUOUTPT' USING DFHEIBLK                            ELTGVLPD
00433                               DFHCOMMAREA                         ELTGVLPD
00434         MOVE 0 TO COF-NBR-DTL-LINES                               ELTGVLPD
00435      END-IF.                                                      ELTGVLPD
00436                                                                   ELTGVLPD
00437 ************************************************************      ELTGVLPD
00438 *                                                          *      ELTGVLPD
00439 *        DISPLAY PROVIDER CONTROL                          *      ELTGVLPD
00440 *                                                          *      ELTGVLPD
00441 ************************************************************      ELTGVLPD
00442  1775-DSPLY-PRVDR-CNTRL.                                          ELTGVLPD
00443      IF GVL4-PVDR-CTL-1(GVL4-INDEX) NOT = 'ZZ'                    ELTGVLPD
00444         IF GVL4-PVDR-CTL-1(GVL4-INDEX) = 'F'                      ELTGVLPD
00445            MOVE WS-SPCL-FCLTY TO WS-PRVDR-CNTRL                   ELTGVLPD
00446         ELSE                                                      ELTGVLPD
00447            IF GVL4-PVDR-CTL-1(GVL4-INDEX) = 'P'                   ELTGVLPD
00448               MOVE WS-PRFRD-PRVDR TO WS-PRVDR-CNTRL               ELTGVLPD
00449            ELSE                                                   ELTGVLPD
00450               IF GVL4-PVDR-CTL-1(GVL4-INDEX) = 'Q'                ELTGVLPD
00451                  MOVE WS-AFLTD-PRVDR TO WS-PRVDR-CNTRL            ELTGVLPD
00452               ELSE                                                ELTGVLPD
00453                  CONTINUE                                         ELTGVLPD
00454               END-IF                                              ELTGVLPD
00455            END-IF                                                 ELTGVLPD
00456         END-IF.                                                   ELTGVLPD
00457      MOVE WS-PRVDR-DTL-LINE3 TO COF-DTL-LINE (COF-NBR-DTL-LINES). ELTGVLPD
00458      PERFORM 1776-DSPLY-2ND-PRVDR-CNTRL.                          ELTGVLPD
00459                                                                   ELTGVLPD
00460 ************************************************************      ELTGVLPD
00461 *                                                          *      ELTGVLPD
00462 *        DISPLAY 2ND PROVIDER CONTROL                      *      ELTGVLPD
00463 *                                                          *      ELTGVLPD
00464 ************************************************************      ELTGVLPD
00465  1776-DSPLY-2ND-PRVDR-CNTRL.                                      ELTGVLPD
00466      IF GVL4-PVDR-CTL-2(GVL4-INDEX) NOT = 'ZZ'                    ELTGVLPD
00467         IF GVL4-PVDR-CTL-2(GVL4-INDEX) = 'F'                      ELTGVLPD
00468            MOVE WS-SPCL-FCLTY TO WS-PRVDR-CNTRL2                  ELTGVLPD
00469         ELSE                                                      ELTGVLPD
00470            IF GVL4-PVDR-CTL-2(GVL4-INDEX) = 'P'                   ELTGVLPD
00471               MOVE WS-PRFRD-PRVDR TO WS-PRVDR-CNTRL2              ELTGVLPD
00472            ELSE                                                   ELTGVLPD
00473               IF GVL4-PVDR-CTL-2(GVL4-INDEX) = 'Q'                ELTGVLPD
00474                  MOVE WS-AFLTD-PRVDR TO WS-PRVDR-CNTRL2           ELTGVLPD
00475               ELSE                                                ELTGVLPD
00476                  CONTINUE                                         ELTGVLPD
00477               END-IF                                              ELTGVLPD
00478            END-IF                                                 ELTGVLPD
00479         END-IF.                                                   ELTGVLPD
00480      MOVE WS-PRVDR-DTL-LINE3A TO COF-DTL-LINE (COF-NBR-DTL-LINES).ELTGVLPD
00481                                                                   ELTGVLPD
00482 ************************************************************      ELTGVLPD
00483 *                                                          *      ELTGVLPD
00484 *        OBTAIN DATES                                      *      ELTGVLPD
00485 *                                                          *      ELTGVLPD
00486 ************************************************************      ELTGVLPD
00487  3300-OBTAIN-DATES.                                               ELTGVLPD
00488      MOVE GVL4-PRVDR-EFF-DT (GVL4-INDEX) TO HGADATE-JULIAN1.      ELTGVLPD
00489      PERFORM 3400-CONVERT-DATE.                                   ELTGVLPD
00490      MOVE HGADATE-DATE2     TO WS-FROM-DT.                        ELTGVLPD
00491      MOVE GVL4-PRVDR-TRMTNDT-CEN (GVL4-INDEX) TO MLDATE-DATE1.    ELTGVLPD
00492      PERFORM 3500-CEN-CONVERT-DATE.                               ELTGVLPD
00493      MOVE MLDATE-DATE2     TO WS-TO-DT.                           ELTGVLPD
00494                                                                   ELTGVLPD
00495 ************************************************************      ELTGVLPD
00496 *                                                          *      ELTGVLPD
00497 *        CONVERT DATES FROM JULIAN TO GREGORIAN            *      ELTGVLPD
00498 *                                                          *      ELTGVLPD
00499 ************************************************************      ELTGVLPD
00500  3400-CONVERT-DATE.                                               ELTGVLPD
00501      MOVE 'CNV' TO HGADATE-FUNC.                                  ELTGVLPD
00502      MOVE 'J'   TO HGADATE-FORM1.                                 ELTGVLPD
00503      MOVE 'M'   TO HGADATE-FORM2.                                 ELTGVLPD
00504      MOVE ZEROS TO HGADATE-RETURN, HGADATE-AMOUNT,                ELTGVLPD
00505                    HGADATE-DATE2.                                 ELTGVLPD
00506      EXEC CICS LINK PROGRAM ('HGADATES')                          ELTGVLPD
00507                     COMMAREA (HGADATES-PARM-LIST)                 ELTGVLPD
00508                     END-EXEC.                                     ELTGVLPD
00509                                                                   ELTGVLPD
00510 ************************************************************      ELTGVLPD
00511 *                                                          *      ELTGVLPD
00512 *        CONVERT DATES FROM JULIAN TO GREGORIAN            *      ELTGVLPD
00513 *        IN FORMAT MMDDCCYY                                *      ELTGVLPD
00514 *                                                          *      ELTGVLPD
00515 ************************************************************      ELTGVLPD
00516  3500-CEN-CONVERT-DATE.                                           ELTGVLPD
00517      MOVE 'CNV' TO  MLDATE-FUNC.                                  ELTGVLPD
00518      MOVE 'J'   TO  MLDATE-FORM1.                                 ELTGVLPD
00519      MOVE 'M'   TO  MLDATE-FORM2.                                 ELTGVLPD
00520      MOVE ZEROS TO  MLDATE-RETURN,  MLDATE-AMOUNT.                ELTGVLPD
00521      EXEC CICS LINK PROGRAM ('MLDATEC')                           ELTGVLPD
00522                     COMMAREA (MLDATE01)                           ELTGVLPD
00523                     LENGTH   (28)                                 ELTGVLPD
00524                     END-EXEC.                                     ELTGVLPD
00525 ************************************************************      ELTGVLPD
00526 *                                                          *      ELTGVLPD
00527 *   DISPLAY PROVIDER NOT FOUND                             *      ELTGVLPD
00528 *                                                          *      ELTGVLPD
00529 ************************************************************      ELTGVLPD
00530  4000-DSPLY-PRVDR-NOT-FND.                                        ELTGVLPD
00531      ADD +1 TO COF-NBR-DTL-LINES.                                 ELTGVLPD
00532      MOVE SSB-CS-RESPONSE (SSB-CS-RESP-IDX)                       ELTGVLPD
00533                        TO WS-PRVDR-NMBR-4.                        ELTGVLPD
00534      MOVE WS-PRVDR-DTL-LINE4                                      ELTGVLPD
00535                     TO COF-DTL-LINE (COF-NBR-DTL-LINES).          ELTGVLPD
00536                                                                   ELTGVLPD
00537 ************************************************************      ELTGVLPD
00538 *                                                          *      ELTGVLPD
00539 *   READ WORK FILE4                                        *      ELTGVLPD
00540 *                                                          *      ELTGVLPD
00541 ************************************************************      ELTGVLPD
00542  6000-READ-WKFL4.                                                 ELTGVLPD
00543      SET CIA-ELSWKFL4-DDN TO TRUE.                                ELTGVLPD
00544      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTGVLPD
00545                          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.  ELTGVLPD
00546      SET IOP-KVQ-NONE                                             ELTGVLPD
00547          IOP-FCQ-NONE                                             ELTGVLPD
00548          IOP-STG-MODE-LOCATE TO TRUE.                             ELTGVLPD
00549      CALL 'ELUIOPGM' USING DFHEIBLK                               ELTGVLPD
00550                            DFHCOMMAREA.                           ELTGVLPD
00551      IF IOP-RC-OK                                                 ELTGVLPD
00552            OR (IOP-RC-NOTFND AND IOP-RD-NXT)                      ELTGVLPD
00553         SET ADDRESS OF GVL4-OVERLAY-RECORD                        ELTGVLPD
00554            TO IOP-REC-PTR                                         ELTGVLPD
00555      ELSE                                                         ELTGVLPD
00556         SET CIA-AB-CRITIO TO TRUE                                 ELTGVLPD
00557         EXEC CICS ABEND                                           ELTGVLPD
00558                   ABCODE(CIA-ABCODE)                              ELTGVLPD
00559         END-EXEC                                                  ELTGVLPD
00560      END-IF.                                                      ELTGVLPD
00561                                                                   ELTGVLPD
00562 ************************************************************      ELTGVLPD
00563 *                                                          *      ELTGVLPD
00564 *   READ WORK FILE1                                        *      ELTGVLPD
00565 *                                                          *      ELTGVLPD
00566 ************************************************************      ELTGVLPD
00567  7000-READ-WKFL1.                                                 ELTGVLPD
00568      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELTGVLPD
00569      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTGVLPD
00570                          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.  ELTGVLPD
00571      MOVE 1 TO IOP-TSQ-ITEM-NBR.                                  ELTGVLPD
00572      SET IOP-KVQ-NONE                                             ELTGVLPD
00573          IOP-FCQ-NONE                                             ELTGVLPD
00574          IOP-STG-MODE-LOCATE TO TRUE.                             ELTGVLPD
00575      CALL 'ELUIOPGM' USING DFHEIBLK                               ELTGVLPD
00576                            DFHCOMMAREA.                           ELTGVLPD
00577      IF IOP-RC-OK                                                 ELTGVLPD
00578         SET ADDRESS OF GVX-PROVIDER-NUMBER-OVERLAY                ELTGVLPD
00579            TO IOP-REC-PTR                                         ELTGVLPD
00580      ELSE                                                         ELTGVLPD
00581         SET CIA-AB-CRITIO TO TRUE                                 ELTGVLPD
00582         EXEC CICS ABEND                                           ELTGVLPD
00583                   ABCODE(CIA-ABCODE)                              ELTGVLPD
00584         END-EXEC                                                  ELTGVLPD
00585      END-IF.                                                      ELTGVLPD
00586                                                                   ELTGVLPD
