00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUGVLSV
00003  PROGRAM-ID.         ELUGVLSV.                                       LV001
00004                                                                   ELUGVLSV
00005  AUTHOR.             ANNE KING                                    ELUGVLSV
00006                                                                   ELUGVLSV
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUGVLSV
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELUGVLSV
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUGVLSV
00010                      233 N. MICHIGAN AVE                          ELUGVLSV
00011                      CHICAGO, ILLINOIS 60601                      ELUGVLSV
00012                                                                   ELUGVLSV
00013  DATE-WRITTEN.       19-OCT-1993.                                 ELUGVLSV
00014                                                                   ELUGVLSV
00015  DATE-COMPILED.                                                   ELUGVLSV
00016                                                                   ELUGVLSV
00017  SECURITY.           COPYRIGHT 1989,                              ELUGVLSV
00018                      HEALTH CARE SERVICE CORPORATION              ELUGVLSV
00019      SKIP3                                                        ELUGVLSV
00020  ENVIRONMENT DIVISION.                                            ELUGVLSV
00021 *                                                                 ELUGVLSV
00022  CONFIGURATION SECTION.                                           ELUGVLSV
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELUGVLSV
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELUGVLSV
00025 ****************************************************************  ELUGVLSV
00026 * !!!!!!! PLEASE READ!!!!!! I HAVE SET A CRITICAL IO ERROR     *  ELUGVLSV
00026 * !!!!!!! ABEND IN THIS PGM WHEN THE COUNTER IN THE XXXXXXX    *  ELUGVLSV
00026 * !!!!!!! COPYBOOK HAS EXCEEDED 5000.  THIS IS TO AVOID A      *  ELUGVLSV
50026 * !!!!!!! STORAGE VIOLATION.  AKK 06/21/06                     *  ELUGVLSV
00027 *    PROGRAM:    ELUGVLSV                                      *  ELUGVLSV
00028 *    DATE:       06-NOV-1993                                   *  ELUGVLSV
00029 *    AUTHOR:     ANNE KEFFER KING                              *  ELUGVLSV
00030 *    FUNCTION:   CREATE SPC LIST OF PROVIDERS FOR USE BY       *  ELUGVLSV
00031 *                ELTGVLPL, ELSGVLM1 AND ELTGVLPD               *  ELUGVLSV
00032 *                                                              *  ELUGVLSV
00033 ****************************************************************  ELUGVLSV
00034 *                                                              *  ELUGVLSV
00035 *                   MAINTENANCE HISTORY                        *  ELUGVLSV
00036 *                                                              *  ELUGVLSV
00037 *  MOD     DATE     BY  DRPT              ACTION               *  ELUGVLSV
00038 * ----- ----------- --- ---- --------------------------------- *  ELUGVLSV
00039 * 01.00 05-NOV-1993 AKK      CREATED                           *  ELUGVLSV
00040 *                                                              *  ELUGVLSV
00041 * 01.01 24-JAN-1994 AKK      ADD CODE TO CREATE A NEW TSQ OF   *  ELUGVLSV
00042 *                            PROVIDER NUMBERS TO BE USED BY    *  ELUGVLSV
00043 *                            ELTGVLPD TO HELP FIND THE RE-     *  ELUGVLSV
00044 *                            QUESTED PROVIDER NUMBERS WITH     *  ELUGVLSV
00045 *                            GREATER EASE.                     *  ELUGVLSV
00041 * 01.02 20-JUN-2006 AKK      RECOMPILE DUE TO CHANGES IN COPY- *  ELUGVLSV
00042 *                            BOOK ELSGVX1C                     *  ELUGVLSV
00041 * 01.03 21-JUN-2006 AKK      ADD MAX CHECK FOR ELSGVX1C TO     *  ELUGVLSV
00042 *                            AVOID STORAGE VIOLATION           *  ELUGVLSV
00046 ****************************************************************  ELUGVLSV
00047 *                                                                 ELUGVLSV
00048      EJECT                                                        ELUGVLSV
00049  DATA DIVISION.                                                   ELUGVLSV
00050  WORKING-STORAGE SECTION.                                         ELUGVLSV
00051  01  WS-MISC.                                                     ELUGVLSV
00052      05  WS-NMBR-GVLS-FND                       PIC S9(04) COMP-3.ELUGVLSV
00053      05  WS-GVL-MAX-INDEX                       USAGE INDEX.      ELUGVLSV
00054                                                                   ELUGVLSV
00055  01  WS-POINTERS.                                                 ELUGVLSV
00056      05  WS-ELSWKFL1-PTR                        POINTER.          ELUGVLSV
00057      05  WS-ELSWKFL2-PTR                        POINTER.          ELUGVLSV
00058      05  WS-ELSWKFL3-PTR                        POINTER.          ELUGVLSV
00059      05  WS-ELSWKFL4-PTR                        POINTER.          ELUGVLSV
00060      05  WS-GNRC-WKFL-PTR                       POINTER.          ELUGVLSV
00061      05  WS-GCPRVTB2-PTR                        POINTER.          ELUGVLSV
00062      05  WS-ELSWKFL1-DDN                        PIC X(08).        ELUGVLSV
00063      05  WS-ELSWKFL2-DDN                        PIC X(08).        ELUGVLSV
00064      05  WS-ELSWKFL3-DDN                        PIC X(08).        ELUGVLSV
00065      05  WS-ELSWKFL4-DDN                        PIC X(08).        ELUGVLSV
00066      05  WS-GNRC-DDN                            PIC X(08).        ELUGVLSV
00067                                                                   ELUGVLSV
00068  01  WS-SWITCHES.                                                 ELUGVLSV
00069        05  MS-PROCESSING-SW                     PIC X(01).        ELUGVLSV
00070            88  WS-PROCESSING-INST                   VALUE 'I'.    ELUGVLSV
00071            88  WS-PROCESSING-PROF                   VALUE 'P'.    ELUGVLSV
00072        05  WS-FIRST-READ-SW                     PIC X(01).        ELUGVLSV
00073            88  WS-FIRST-READ                        VALUE 'R'.    ELUGVLSV
00074            88  WS-NOT-FIRST-READ                    VALUE 'N'.    ELUGVLSV
00075        05  WS-END-OF-TABULAR-SW                 PIC X(01).        ELUGVLSV
00076            88  WS-END-OF-TABULAR                    VALUE 'Y'.    ELUGVLSV
00077            88  WS-NOT-END-OF-TABULAR                VALUE 'N'.    ELUGVLSV
00078 *THE FOLLOWING ARE OVERLAY RECORD BUFFERS, 1 FOR EACH WORK FILE   ELUGVLSV
00079                                                                   ELUGVLSV
00080  01  GVL1B-OVRLY-RCRD-BUFFER.                                     ELUGVLSV
00081        05  GVL1B-PRVDR-DTL.                                       ELUGVLSV
00082            10  GVL1B-PRVDR-NM               PIC X(33).            ELUGVLSV
00083            10  GVL1B-PRVDR-NBR              PIC X(10).            ELUGVLSV
00084            10  GVL1B-PRVDR-SQ-NBR   COMP    PIC S9(04).           ELUGVLSV
00085            10  GVL1B-PRVDR-CNT      COMP-3  PIC S9(03).           ELUGVLSV
00086            10  GVL1B-PRVDR-TYPE-RQUST       PIC 9(01).            ELUGVLSV
00087        05  GVL1B-ENTRY.                                           ELUGVLSV
00088            10  GVL1B-PRVDR-EFF-DT   COMP-3  PIC S9(07).           ELUGVLSV
00089            10  GVL1B-PVDR-TRMTN-DT  COMP-3  PIC S9(07).           ELUGVLSV
00090            10  GVL1B-PVDR-CTL-1             PIC  X(02).           ELUGVLSV
00091            10  GVL1B-PVDR-CTL-2             PIC  X(02).           ELUGVLSV
00092                                                                   ELUGVLSV
00093  01  GVL2B-OVRLY-RCRD-BUFFER.                                     ELUGVLSV
00094        05  GVL2B-PRVDR-DTL.                                       ELUGVLSV
00095            10  GVL2B-PRVDR-NM               PIC X(33).            ELUGVLSV
00096            10  GVL2B-PRVDR-NBR              PIC X(10).            ELUGVLSV
00097            10  GVL2B-PRVDR-SQ-NBR   COMP    PIC S9(04).           ELUGVLSV
00098            10  GVL2B-PRVDR-CNT      COMP-3  PIC S9(03).           ELUGVLSV
00099            10  GVL2B-PRVDR-TYPE-RQUST       PIC 9(01).            ELUGVLSV
00100        05  GVL2B-ENTRY.                                           ELUGVLSV
00101            10  GVL2B-PRVDR-EFF-DT   COMP-3  PIC S9(07).           ELUGVLSV
00102            10  GVL2B-PVDR-TRMTN-DT  COMP-3  PIC S9(07).           ELUGVLSV
00103            10  GVL2B-PVDR-CTL-1             PIC  X(02).           ELUGVLSV
00104            10  GVL2B-PVDR-CTL-2             PIC  X(02).           ELUGVLSV
00105                                                                   ELUGVLSV
00106  01  GVL3B-OVRLY-RCRD-BUFFER.                                     ELUGVLSV
00107        05  GVL3B-PRVDR-DTL.                                       ELUGVLSV
00108            10  GVL3B-PRVDR-NM               PIC X(33).            ELUGVLSV
00109            10  GVL3B-PRVDR-NBR              PIC X(10).            ELUGVLSV
00110            10  GVL3B-PRVDR-SQ-NBR   COMP    PIC S9(04).           ELUGVLSV
00111            10  GVL3B-PRVDR-CNT      COMP-3  PIC S9(03).           ELUGVLSV
00112            10  GVL3B-PRVDR-TYPE-RQUST       PIC 9(01).            ELUGVLSV
00113        05  GVL3B-ENTRY.                                           ELUGVLSV
00114            10  GVL3B-PRVDR-EFF-DT  COMP-3  PIC S9(07).            ELUGVLSV
00115            10  GVL3B-PVDR-TRMTN-DT COMP-3  PIC S9(07).            ELUGVLSV
00116            10  GVL3B-PVDR-CTL-1            PIC  X(02).            ELUGVLSV
00117            10  GVL3B-PVDR-CTL-2            PIC  X(02).            ELUGVLSV
00118                                                                   ELUGVLSV
00119  01  GVL4B-OVRLY-RCRD-BUFFER.                                     ELUGVLSV
00120        05  GVL4B-PRVDR-DTL.                                       ELUGVLSV
00121            10  GVL4B-PRVDR-NM               PIC X(33).            ELUGVLSV
00122            10  GVL4B-PRVDR-NBR              PIC X(10).            ELUGVLSV
00123            10  GVL4B-PRVDR-SQ-NBR   COMP    PIC S9(04).           ELUGVLSV
00124            10  GVL4B-PRVDR-CNT      COMP-3  PIC S9(03).           ELUGVLSV
00125            10  GVL4B-PRVDR-TYPE-RQUST       PIC 9(01).            ELUGVLSV
00126        05  GVL4B-ENTRY.                                           ELUGVLSV
00127            10  GVL4B-PRVDR-EFF-DT  COMP-3  PIC S9(07).            ELUGVLSV
00128            10  GVL4B-PVDR-TRMTN-DT COMP-3  PIC S9(07).            ELUGVLSV
00129            10  GVL4B-PVDR-CTL-1            PIC  X(02).            ELUGVLSV
00130            10  GVL4B-PVDR-CTL-2            PIC  X(02).            ELUGVLSV
00131                                                                   ELUGVLSV
00132  LINKAGE SECTION.                                                 ELUGVLSV
00133                                                                   ELUGVLSV
00134  01  DFHCOMMAREA.                                                 ELUGVLSV
00135  COPY ELSCOMMC.                                                   ELUGVLSV
00136     EJECT                                                         ELUGVLSV
00137  COPY ELSCIA2C.                                                   ELUGVLSV
00138     EJECT                                                         ELUGVLSV
00139  COPY ELSGVL1C.                                                   ELUGVLSV
00140     EJECT                                                         ELUGVLSV
00141  COPY ELSGVL2C.                                                   ELUGVLSV
00142     EJECT                                                         ELUGVLSV
00143  COPY ELSGVL3C.                                                   ELUGVLSV
00144     EJECT                                                         ELUGVLSV
00145  COPY ELSGVL4C.                                                   ELUGVLSV
00146     EJECT                                                         ELUGVLSV
00147  COPY ELSGVX1C.                                                   ELUGVLSV
00148     EJECT                                                         ELUGVLSV
00149  COPY ELSIOPMC.                                                   ELUGVLSV
00150     EJECT                                                         ELUGVLSV
00151  COPY ELSKEYSC.                                                   ELUGVLSV
00152     EJECT                                                         ELUGVLSV
00153  COPY ELSSMAC.                                                    ELUGVLSV
00154     EJECT                                                         ELUGVLSV
00155  COPY ELSSLNRC.                                                   ELUGVLSV
00156     EJECT                                                         ELUGVLSV
00157  COPY ELSSSCBC.                                                   ELUGVLSV
00158     EJECT                                                         ELUGVLSV
00159  01 GVL-GCPS-RECORD.                                              ELUGVLSV
00160     COPY GCTGVLC.                                                 ELUGVLSV
00161     EJECT                                                         ELUGVLSV
00162  01 PDB-IO-AREA.                                                  ELUGVLSV
00163     COPY DBPIOPMC.                                                ELUGVLSV
00164     EJECT                                                         ELUGVLSV
00165  01 PRVDR-MSTR-RCRD.                                              ELUGVLSV
00166     COPY PROVMSTR.                                                ELUGVLSV
00167     EJECT                                                         ELUGVLSV
00168                                                                   ELUGVLSV
00169 /                                                                 ELUGVLSV
00170  PROCEDURE DIVISION.                                              ELUGVLSV
00171      PERFORM 0000-MAIN-INITIALIZATION.                            ELUGVLSV
00172      PERFORM 1000-CRT-SPC-FILE.                                   ELUGVLSV
00173      PERFORM 2000-CRT-PRVDR-OVRLY.                                ELUGVLSV
00174      GOBACK.                                                      ELUGVLSV
00175                                                                   ELUGVLSV
00176  0000-MAIN-INITIALIZATION.                                        ELUGVLSV
00177      PERFORM 0100-ESTBLSH-ENVRNMNT.                               ELUGVLSV
00178      PERFORM 0150-ESTBLSH-ADDRSS-SSB.                             ELUGVLSV
00179      PERFORM 0200-ESTBLSH-ADDRSS-KWA.                             ELUGVLSV
00180      PERFORM 0300-ESTBLSH-ADDRSS-SLN.                             ELUGVLSV
00181      PERFORM 0400-ESTBLSH-ADRS-DBP-IO-AREA.                       ELUGVLSV
00182      PERFORM 0500-ESTBLSH-ADRS-GVL-FILE-IO.                       ELUGVLSV
00183      PERFORM 9062-ESTBLSH-ADRS-GVX-FILE-IO.                       ELUGVLSV
00184                                                                   ELUGVLSV
00185  0150-ESTBLSH-ADDRSS-SSB.                                         ELUGVLSV
00186      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELUGVLSV
00187      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
00188                      ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.      ELUGVLSV
00189      IF CIA-RC-PTR-NULL                                           ELUGVLSV
00190         SET CIA-AB-PARM-MISSING TO TRUE                           ELUGVLSV
00191         PERFORM 9999-ABEND                                        ELUGVLSV
00192      END-IF.                                                      ELUGVLSV
00193                                                                   ELUGVLSV
00194  0100-ESTBLSH-ENVRNMNT.                                           ELUGVLSV
00195      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELUGVLSV
00196         EXEC CICS ABEND                                           ELUGVLSV
00197                   ABCODE ('EL01')                                 ELUGVLSV
00198         END-EXEC                                                  ELUGVLSV
00199      END-IF.                                                      ELUGVLSV
00200      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUGVLSV
00201                   ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.       ELUGVLSV
00202      IF ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA = NULL           ELUGVLSV
00203            EXEC CICS ABEND                                        ELUGVLSV
00204                 ABCODE ('EL02')                                   ELUGVLSV
00205            END-EXEC                                               ELUGVLSV
00206      END-IF.                                                      ELUGVLSV
00207                                                                   ELUGVLSV
00208  0200-ESTBLSH-ADDRSS-KWA.                                         ELUGVLSV
00209      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELUGVLSV
00210      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
00211                      ADDRESS OF KWA-FILE-KEY-WORK-AREA.           ELUGVLSV
00212      IF CIA-RC-PTR-NULL                                           ELUGVLSV
00213         SET CIA-AB-TAB-UNDEF TO TRUE                              ELUGVLSV
00214         PERFORM 9999-ABEND                                        ELUGVLSV
00215      END-IF.                                                      ELUGVLSV
00216      MOVE LOW-VALUES TO KWA-GCPRVTB2-KEY.                         ELUGVLSV
00217                                                                   ELUGVLSV
00218  0300-ESTBLSH-ADDRSS-SLN.                                         ELUGVLSV
00219      SET CIA-ELSSLNR-DDN TO TRUE.                                 ELUGVLSV
00220      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
00221                      ADDRESS OF SLN-SELECTION-SLOT-NUMBERS.       ELUGVLSV
00222      IF CIA-RC-PTR-NULL                                           ELUGVLSV
00223         SET CIA-ELSSLNR-DDN TO TRUE                               ELUGVLSV
00224         SET CIA-STG-RETRIEVE TO TRUE                              ELUGVLSV
00225         CALL 'ELUSTGMG' USING DFHEIBLK                            ELUGVLSV
00226                               DFHCOMMAREA                         ELUGVLSV
00227         SET CIA-ELSSLNR-DDN TO TRUE                               ELUGVLSV
00228         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELUGVLSV
00229                         ADDRESS OF SLN-SELECTION-SLOT-NUMBERS     ELUGVLSV
00230      END-IF.                                                      ELUGVLSV
00231                                                                   ELUGVLSV
00232  0400-ESTBLSH-ADRS-DBP-IO-AREA.                                   ELUGVLSV
00233      SET CIA-DBPIOPM-DDN TO TRUE.                                 ELUGVLSV
00234      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
00235                      ADDRESS OF PDB-IO-AREA.                      ELUGVLSV
00236      IF CIA-RC-PTR-NULL                                           ELUGVLSV
00237         SET CIA-STG-GETMAIN TO TRUE                               ELUGVLSV
00238         CALL 'ELUSTGMG' USING DFHEIBLK                            ELUGVLSV
00239                               DFHCOMMAREA                         ELUGVLSV
00240         SET CIA-DBPIOPM-DDN TO TRUE                               ELUGVLSV
00241         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELUGVLSV
00242                      ADDRESS OF PDB-IO-AREA                       ELUGVLSV
00243      END-IF.                                                      ELUGVLSV
00244                                                                   ELUGVLSV
00245  0500-ESTBLSH-ADRS-GVL-FILE-IO.                                   ELUGVLSV
00246      SET CIA-GCPRVTB2-DDN TO TRUE.                                ELUGVLSV
00247      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
00248                    ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.        ELUGVLSV
00249      IF CIA-RC-PTR-NULL                                           ELUGVLSV
00250         SET CIA-GCPRVTB2-DDN TO TRUE                              ELUGVLSV
00251         SET CIA-STG-GETMAIN TO TRUE                               ELUGVLSV
00252         CALL 'ELUSTGMG' USING DFHEIBLK                            ELUGVLSV
00253                               DFHCOMMAREA                         ELUGVLSV
00254         SET CIA-GCPRVTB2-DDN TO TRUE                              ELUGVLSV
00255         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELUGVLSV
00256                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS       ELUGVLSV
00257      END-IF.                                                      ELUGVLSV
00258      SET WS-GCPRVTB2-PTR TO                                       ELUGVLSV
00259          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELUGVLSV
00260                                                                   ELUGVLSV
00261  1000-CRT-SPC-FILE.                                               ELUGVLSV
00262      PERFORM 9060-ESTBLSH-ADRSS-ALCTE-WKFL4.                      ELUGVLSV
00263      MOVE LOW-VALUES TO KWA-GCPRVTB2-KEY.                         ELUGVLSV
00264      PERFORM 1500-CREATE-INST-SPC-FILE.                           ELUGVLSV
00265      SET WS-NOT-END-OF-TABULAR TO TRUE.                           ELUGVLSV
00266      PERFORM 2500-CREATE-PROF-SPC-FILE.                           ELUGVLSV
00267                                                                   ELUGVLSV
00268  1500-CREATE-INST-SPC-FILE.                                       ELUGVLSV
00269      MOVE 0 TO WS-NMBR-GVLS-FND.                                  ELUGVLSV
00270      SET WS-PROCESSING-INST TO TRUE.                              ELUGVLSV
00271      IF SLN-GVLF-SLOT-NO NOT = ZERO                               ELUGVLSV
00272          PERFORM 1510-ADD-ONE-FOR-GVLF-FND                        ELUGVLSV
00273      END-IF.                                                      ELUGVLSV
00274      IF SLN-GVLG-SLOT-NO NOT = ZERO                               ELUGVLSV
00275          PERFORM 1520-ADD-ONE-FOR-GVLG-FND                        ELUGVLSV
00276      END-IF.                                                      ELUGVLSV
00277      IF SLN-GVLH-SLOT-NO NOT = ZERO                               ELUGVLSV
00278          PERFORM 1530-ADD-ONE-FOR-GVLH-FND                        ELUGVLSV
00279      END-IF.                                                      ELUGVLSV
00280                                                                   ELUGVLSV
00281      IF WS-NMBR-GVLS-FND = 0                                      ELUGVLSV
00282          CONTINUE                                                 ELUGVLSV
00283      ELSE                                                         ELUGVLSV
00284         IF WS-NMBR-GVLS-FND = 1                                   ELUGVLSV
00285             PERFORM 2550-READ-GVL-FILE-BUILD-WKFL4                ELUGVLSV
00286         ELSE                                                      ELUGVLSV
00287             PERFORM 4000-MERGE-THREE-INST-GVL-TAB                 ELUGVLSV
00288         END-IF                                                    ELUGVLSV
00289      END-IF.                                                      ELUGVLSV
00290                                                                   ELUGVLSV
00291  1510-ADD-ONE-FOR-GVLF-FND.                                       ELUGVLSV
00292      ADD +1 TO WS-NMBR-GVLS-FND.                                  ELUGVLSV
00293      MOVE SLN-GVLF-ID TO KWA-GVL-PRVDR-ID.                        ELUGVLSV
00294      MOVE SLN-GVLF-SLOT-NO TO KWA-GVL-PRVDR-SLOT-NO.              ELUGVLSV
00295                                                                   ELUGVLSV
00296  1520-ADD-ONE-FOR-GVLG-FND.                                       ELUGVLSV
00297      ADD +1 TO WS-NMBR-GVLS-FND.                                  ELUGVLSV
00298      MOVE SLN-GVLG-ID TO KWA-GVL-PRVDR-ID.                        ELUGVLSV
00299      MOVE SLN-GVLG-SLOT-NO  TO KWA-GVL-PRVDR-SLOT-NO.             ELUGVLSV
00300                                                                   ELUGVLSV
00301  1530-ADD-ONE-FOR-GVLH-FND.                                       ELUGVLSV
00302      ADD +1 TO WS-NMBR-GVLS-FND.                                  ELUGVLSV
00303      MOVE SLN-GVLH-ID TO KWA-GVL-PRVDR-ID.                        ELUGVLSV
00304      MOVE SLN-GVLH-SLOT-NO TO KWA-GVL-PRVDR-SLOT-NO.              ELUGVLSV
00305                                                                   ELUGVLSV
00306  2000-CRT-PRVDR-OVRLY.                                            ELUGVLSV
00307      PERFORM 2001-DLT-WRK-FLS.                                    ELUGVLSV
00308      PERFORM 9000-ESTBLSH-ADRSS-ALCTE-WKFL1.                      ELUGVLSV
00309      SET IOP-REC-PTR TO                                           ELUGVLSV
00310          ADDRESS OF GVX-PROVIDER-NUMBER-OVERLAY.                  ELUGVLSV
00311      MOVE LENGTH OF GVX-PROVIDER-NUMBER-OVERLAY                   ELUGVLSV
00312         TO IOP-REC-LEN.                                           ELUGVLSV
00313      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELUGVLSV
00314      SET IOP-ADD TO TRUE.                                         ELUGVLSV
00315      SET IOP-FCQ-NONE TO TRUE.                                    ELUGVLSV
00316      SET IOP-KVQ-NONE TO TRUE.                                    ELUGVLSV
00317      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
00318                            DFHCOMMAREA.                           ELUGVLSV
00319                                                                   ELUGVLSV
00320  2001-DLT-WRK-FLS.                                                ELUGVLSV
00321      IF WS-ELSWKFL1-PTR NOT = NULL                                ELUGVLSV
00322         PERFORM 9010-DELETE-WRKFL1                                ELUGVLSV
00323      END-IF.                                                      ELUGVLSV
00324      IF WS-ELSWKFL2-PTR NOT = NULL                                ELUGVLSV
00325         PERFORM 9030-DELETE-WRKFL2                                ELUGVLSV
00326      END-IF.                                                      ELUGVLSV
00327      IF WS-ELSWKFL3-PTR NOT = NULL                                ELUGVLSV
00328         PERFORM 9050-DELETE-WRKFL3                                ELUGVLSV
00329      END-IF.                                                      ELUGVLSV
00330                                                                   ELUGVLSV
00331  2500-CREATE-PROF-SPC-FILE.                                       ELUGVLSV
00332      MOVE 0 TO WS-NMBR-GVLS-FND.                                  ELUGVLSV
00333      SET WS-PROCESSING-PROF TO TRUE.                              ELUGVLSV
00334      IF SLN-GVLP-SLOT-NO NOT = ZERO                               ELUGVLSV
00335          PERFORM 2510-ADD-ONE-FOR-GVLP-FND                        ELUGVLSV
00336      END-IF.                                                      ELUGVLSV
00337      IF SLN-GVLQ-SLOT-NO NOT = ZERO                               ELUGVLSV
00338          PERFORM 2520-ADD-ONE-FOR-GVLQ-FND                        ELUGVLSV
00339      END-IF.                                                      ELUGVLSV
00340      IF SLN-GVLR-SLOT-NO NOT = ZERO                               ELUGVLSV
00341          PERFORM 2530-ADD-ONE-FOR-GVLR-FND                        ELUGVLSV
00342      END-IF.                                                      ELUGVLSV
00343      IF WS-NMBR-GVLS-FND = 0                                      ELUGVLSV
00344          CONTINUE                                                 ELUGVLSV
00345      ELSE                                                         ELUGVLSV
00346         IF WS-NMBR-GVLS-FND = 1                                   ELUGVLSV
00347             PERFORM 2550-READ-GVL-FILE-BUILD-WKFL4                ELUGVLSV
00348         ELSE                                                      ELUGVLSV
00349             PERFORM 5000-MERGE-THREE-PROF-GVL-TAB                 ELUGVLSV
00350      END-IF                                                       ELUGVLSV
00351        END-IF.                                                    ELUGVLSV
00352                                                                   ELUGVLSV
00353  2510-ADD-ONE-FOR-GVLP-FND.                                       ELUGVLSV
00354      ADD +1 TO WS-NMBR-GVLS-FND.                                  ELUGVLSV
00355      MOVE SLN-GVLP-ID TO KWA-GVL-PRVDR-ID.                        ELUGVLSV
00356      MOVE SLN-GVLP-SLOT-NO TO KWA-GVL-PRVDR-SLOT-NO.              ELUGVLSV
00357                                                                   ELUGVLSV
00358  2520-ADD-ONE-FOR-GVLQ-FND.                                       ELUGVLSV
00359      ADD +1 TO WS-NMBR-GVLS-FND.                                  ELUGVLSV
00360      MOVE SLN-GVLQ-ID TO KWA-GVL-PRVDR-ID.                        ELUGVLSV
00361      MOVE SLN-GVLQ-SLOT-NO  TO KWA-GVL-PRVDR-SLOT-NO.             ELUGVLSV
00362                                                                   ELUGVLSV
00363  2530-ADD-ONE-FOR-GVLR-FND.                                       ELUGVLSV
00364      ADD +1 TO WS-NMBR-GVLS-FND.                                  ELUGVLSV
00365      MOVE SLN-GVLR-ID TO KWA-GVL-PRVDR-ID.                        ELUGVLSV
00366      MOVE SLN-GVLR-SLOT-NO TO KWA-GVL-PRVDR-SLOT-NO.              ELUGVLSV
00367                                                                   ELUGVLSV
00368  2550-READ-GVL-FILE-BUILD-WKFL4.                                  ELUGVLSV
00369      PERFORM 9100-SETUP-EXEC-BRSW.                                ELUGVLSV
00370      IF IOP-RC-OK                                                 ELUGVLSV
00371         PERFORM 2700-BLD-WKFL4-EXTRCT-FL                          ELUGVLSV
00372              UNTIL WS-END-OF-TABULAR                              ELUGVLSV
00373         SET IOP-END-BR TO TRUE                                    ELUGVLSV
00374         CALL 'ELUIOPGM' USING DFHEIBLK                            ELUGVLSV
00375                               DFHCOMMAREA                         ELUGVLSV
00376      ELSE                                                         ELUGVLSV
00377         PERFORM 9555-SIGNAL-GCPRVTB2-ERROR                        ELUGVLSV
00378      END-IF.                                                      ELUGVLSV
00379                                                                   ELUGVLSV
00380  2700-BLD-WKFL4-EXTRCT-FL.                                        ELUGVLSV
00381      PERFORM 9125-SETUP-EXEC-READ.                                ELUGVLSV
00382      IF IOP-RC-OK                                                 ELUGVLSV
00383        IF ((KWA-GVL-PRVDR-ID = GVL-PROVIDER-ID) AND               ELUGVLSV
00384               (KWA-GVL-PRVDR-SLOT-NO =                            ELUGVLSV
00385                       GVL-PROVIDER-SLOT-NO))                      ELUGVLSV
00386           PERFORM 2725-WRT-WRK-FL-RCRD                            ELUGVLSV
00387      ELSE                                                         ELUGVLSV
00388         IF IOP-RC-ENDFILE                                         ELUGVLSV
00389             OR ((KWA-GVL-PRVDR-ID NOT =  GVL-PROVIDER-ID)         ELUGVLSV
00390              OR (KWA-GVL-PRVDR-SLOT-NO NOT =                      ELUGVLSV
00391                       GVL-PROVIDER-SLOT-NO))                      ELUGVLSV
00392            PERFORM 9535-CHECK-FOR-EOF                             ELUGVLSV
00393         ELSE                                                      ELUGVLSV
00394            PERFORM 9550-SIGNAL-CRITICAL-IO-ERROR                  ELUGVLSV
00395         END-IF                                                    ELUGVLSV
00396      END-IF.                                                      ELUGVLSV
00397      SET WS-NOT-FIRST-READ TO TRUE.                               ELUGVLSV
00398                                                                   ELUGVLSV
00399  2725-WRT-WRK-FL-RCRD.                                            ELUGVLSV
00400      PERFORM 8750-LOAD-WKFL4.                                     ELUGVLSV
00401      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                   ELUGVLSV
00402            TO WS-ELSWKFL4-PTR.                                    ELUGVLSV
00403      SET CIA-ELSWKFL4-DDN TO TRUE.                                ELUGVLSV
00404      SET IOP-ADD TO TRUE.                                         ELUGVLSV
00405      SET IOP-FCQ-NONE TO TRUE.                                    ELUGVLSV
00406      SET IOP-KVQ-NONE TO TRUE.                                    ELUGVLSV
00407      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
00408                            DFHCOMMAREA.                           ELUGVLSV
00409      SET GVX-IDX TO IOP-TSQ-ITEM-NBR.                             ELUGVLSV
00410      MOVE GVL4-PRVDR-NBR TO GVX-PRVDR-NMBR (GVX-IDX).             ELUGVLSV
00411      ADD 1 TO GVX-NMBR-PRVDR-ENTRS.                               ELUGVLSV
           PERFORM 9900-CHECK-MAX-ENTRIES.                                      
00412                                                                   ELUGVLSV
00413  4000-MERGE-THREE-INST-GVL-TAB.                                   ELUGVLSV
00414      PERFORM 9000-ESTBLSH-ADRSS-ALCTE-WKFL1.                      ELUGVLSV
00415      PERFORM 9020-ESTBLSH-ADRSS-ALCTE-WKFL2.                      ELUGVLSV
00416      PERFORM 9040-ESTBLSH-ADRSS-ALCTE-WKFL3.                      ELUGVLSV
00417      PERFORM 4100-COPY-INST-GCPS-TO-WRK.                          ELUGVLSV
00418      SET WS-NOT-END-OF-TABULAR TO TRUE.                           ELUGVLSV
00419      PERFORM 6000-MRG-WRK-FLS-EXTRCT-FLS.                         ELUGVLSV
00420                                                                   ELUGVLSV
00421  4100-COPY-INST-GCPS-TO-WRK.                                      ELUGVLSV
00422      IF SLN-GVLF-SLOT-NO > 0                                      ELUGVLSV
00423         MOVE SLN-GVLF-ID TO KWA-GVL-PRVDR-ID                      ELUGVLSV
00424         MOVE SLN-GVLF-SLOT-NO TO KWA-GVL-PRVDR-SLOT-NO            ELUGVLSV
00425         SET WS-GNRC-WKFL-PTR TO WS-ELSWKFL1-PTR                   ELUGVLSV
00426         SET WS-NOT-END-OF-TABULAR TO TRUE                         ELUGVLSV
00427         MOVE WS-ELSWKFL1-DDN TO WS-GNRC-DDN                       ELUGVLSV
00428         PERFORM 8100-CRT-GNRC-WRK-FL                              ELUGVLSV
00429      END-IF.                                                      ELUGVLSV
00430      IF SLN-GVLG-SLOT-NO > 0                                      ELUGVLSV
00431         MOVE SLN-GVLG-ID TO KWA-GVL-PRVDR-ID                      ELUGVLSV
00432         MOVE SLN-GVLG-SLOT-NO TO KWA-GVL-PRVDR-SLOT-NO            ELUGVLSV
00433         SET WS-GNRC-WKFL-PTR TO WS-ELSWKFL2-PTR                   ELUGVLSV
00434         MOVE WS-ELSWKFL2-DDN TO WS-GNRC-DDN                       ELUGVLSV
00435         SET WS-NOT-END-OF-TABULAR TO TRUE                         ELUGVLSV
00436         PERFORM 8100-CRT-GNRC-WRK-FL                              ELUGVLSV
00437      END-IF.                                                      ELUGVLSV
00438      IF SLN-GVLH-SLOT-NO > 0                                      ELUGVLSV
00439         MOVE SLN-GVLH-ID TO KWA-GVL-PRVDR-ID                      ELUGVLSV
00440         MOVE SLN-GVLH-SLOT-NO TO KWA-GVL-PRVDR-SLOT-NO            ELUGVLSV
00441         SET WS-GNRC-WKFL-PTR TO WS-ELSWKFL3-PTR                   ELUGVLSV
00442         MOVE WS-ELSWKFL3-DDN TO WS-GNRC-DDN                       ELUGVLSV
00443         SET WS-NOT-END-OF-TABULAR TO TRUE                         ELUGVLSV
00444         PERFORM 8100-CRT-GNRC-WRK-FL                              ELUGVLSV
00445      END-IF.                                                      ELUGVLSV
00446                                                                   ELUGVLSV
00447   5000-MERGE-THREE-PROF-GVL-TAB.                                  ELUGVLSV
00448       PERFORM 9000-ESTBLSH-ADRSS-ALCTE-WKFL1.                     ELUGVLSV
00449       PERFORM 9020-ESTBLSH-ADRSS-ALCTE-WKFL2.                     ELUGVLSV
00450       PERFORM 9040-ESTBLSH-ADRSS-ALCTE-WKFL3.                     ELUGVLSV
00451       PERFORM 9060-ESTBLSH-ADRSS-ALCTE-WKFL4.                     ELUGVLSV
00452       PERFORM 5100-COPY-PROF-GCPS-TO-WRK.                         ELUGVLSV
00453       PERFORM 6000-MRG-WRK-FLS-EXTRCT-FLS.                        ELUGVLSV
00454                                                                   ELUGVLSV
00455  5100-COPY-PROF-GCPS-TO-WRK.                                      ELUGVLSV
00456      IF SLN-GVLP-SLOT-NO > 0                                      ELUGVLSV
00457         MOVE SLN-GVLP-ID TO KWA-GVL-PRVDR-ID                      ELUGVLSV
00458         MOVE SLN-GVLP-SLOT-NO TO KWA-GVL-PRVDR-SLOT-NO            ELUGVLSV
00459         SET WS-GNRC-WKFL-PTR TO WS-ELSWKFL1-PTR                   ELUGVLSV
00460         MOVE WS-ELSWKFL1-DDN TO WS-GNRC-DDN                       ELUGVLSV
00461         SET WS-NOT-END-OF-TABULAR TO TRUE                         ELUGVLSV
00462         PERFORM 8100-CRT-GNRC-WRK-FL                              ELUGVLSV
00463      END-IF.                                                      ELUGVLSV
00464      IF SLN-GVLQ-SLOT-NO > 0                                      ELUGVLSV
00465         MOVE SLN-GVLQ-ID TO KWA-GVL-PRVDR-ID                      ELUGVLSV
00466         MOVE SLN-GVLQ-SLOT-NO TO KWA-GVL-PRVDR-SLOT-NO            ELUGVLSV
00467         SET WS-GNRC-WKFL-PTR TO WS-ELSWKFL2-PTR                   ELUGVLSV
00468         MOVE WS-ELSWKFL2-DDN TO WS-GNRC-DDN                       ELUGVLSV
00469         SET WS-NOT-END-OF-TABULAR TO TRUE                         ELUGVLSV
00470         PERFORM 8100-CRT-GNRC-WRK-FL                              ELUGVLSV
00471      END-IF.                                                      ELUGVLSV
00472      IF SLN-GVLR-SLOT-NO > 0                                      ELUGVLSV
00473         MOVE SLN-GVLR-ID TO KWA-GVL-PRVDR-ID                      ELUGVLSV
00474         MOVE SLN-GVLR-SLOT-NO TO KWA-GVL-PRVDR-SLOT-NO            ELUGVLSV
00475         SET WS-GNRC-WKFL-PTR TO WS-ELSWKFL3-PTR                   ELUGVLSV
00476         MOVE WS-ELSWKFL3-DDN TO WS-GNRC-DDN                       ELUGVLSV
00477         SET WS-NOT-END-OF-TABULAR TO TRUE                         ELUGVLSV
00478         PERFORM 8100-CRT-GNRC-WRK-FL                              ELUGVLSV
00479      END-IF.                                                      ELUGVLSV
00480                                                                   ELUGVLSV
00481  6000-MRG-WRK-FLS-EXTRCT-FLS.                                     ELUGVLSV
00482      MOVE HIGH-VALUES TO GVL1B-OVRLY-RCRD-BUFFER                  ELUGVLSV
00483                          GVL2B-OVRLY-RCRD-BUFFER                  ELUGVLSV
00484                          GVL3B-OVRLY-RCRD-BUFFER.                 ELUGVLSV
00485      MOVE 9999999 TO GVL1B-PRVDR-EFF-DT                           ELUGVLSV
00486                    GVL1B-PVDR-TRMTN-DT                            ELUGVLSV
00487                    GVL2B-PRVDR-EFF-DT                             ELUGVLSV
00488                    GVL2B-PVDR-TRMTN-DT                            ELUGVLSV
00489                    GVL3B-PRVDR-EFF-DT                             ELUGVLSV
00490                    GVL3B-PVDR-TRMTN-DT.                           ELUGVLSV
00491      IF (WS-PROCESSING-INST AND SLN-GVLF-SLOT-NO > ZERO) OR       ELUGVLSV
00492         (WS-PROCESSING-PROF AND SLN-GVLP-SLOT-NO > ZERO)          ELUGVLSV
00493           PERFORM 6025-GET-FRST-WKFK1-OCRNC                       ELUGVLSV
00494      END-IF.                                                      ELUGVLSV
00495      IF (WS-PROCESSING-INST AND SLN-GVLG-SLOT-NO > ZERO) OR       ELUGVLSV
00496         (WS-PROCESSING-PROF AND SLN-GVLQ-SLOT-NO > ZERO)          ELUGVLSV
00497           PERFORM 6050-GET-FRST-WKFK2-OCRNC                       ELUGVLSV
00498      END-IF.                                                      ELUGVLSV
00499      IF (WS-PROCESSING-INST AND SLN-GVLH-SLOT-NO > ZERO) OR       ELUGVLSV
00500         (WS-PROCESSING-PROF AND SLN-GVLR-SLOT-NO > ZERO)          ELUGVLSV
00501           PERFORM 6075-GET-FRST-WKFK3-OCRNC                       ELUGVLSV
00502      END-IF.                                                      ELUGVLSV
00503      PERFORM 6085-INTL-FRST-GVL4-RCRD.                            ELUGVLSV
00504      PERFORM 7000-MRG-WRK-EXRCT-OCRNCS                            ELUGVLSV
00505             UNTIL GVL1B-PRVDR-NBR = HIGH-VALUES AND               ELUGVLSV
00506                   GVL2B-PRVDR-NBR = HIGH-VALUES AND               ELUGVLSV
00507                   GVL3B-PRVDR-NBR = HIGH-VALUES.                  ELUGVLSV
00508      PERFORM 7900-WRT-LAST-WKFL4-RCRD.                            ELUGVLSV
00509                                                                   ELUGVLSV
00510  6025-GET-FRST-WKFK1-OCRNC.                                       ELUGVLSV
00511      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                   ELUGVLSV
00512          TO WS-ELSWKFL1-PTR.                                      ELUGVLSV
00513      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELUGVLSV
00514      MOVE 1 TO IOP-TSQ-ITEM-NBR.                                  ELUGVLSV
00515      SET IOP-RD TO TRUE.                                          ELUGVLSV
00516      SET IOP-KVQ-NONE TO TRUE.                                    ELUGVLSV
00517      SET IOP-FCQ-NONE TO TRUE.                                    ELUGVLSV
00518      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELUGVLSV
00519      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
00520                            DFHCOMMAREA.                           ELUGVLSV
00521      IF NOT IOP-RC-OK                                             ELUGVLSV
00522         MOVE HIGH-VALUES TO GVL1B-OVRLY-RCRD-BUFFER               ELUGVLSV
00523         MOVE 9999999 TO GVL1B-PRVDR-EFF-DT                        ELUGVLSV
00524                       GVL1B-PVDR-TRMTN-DT                         ELUGVLSV
00525      ELSE                                                         ELUGVLSV
00526         SET ADDRESS OF GVL1-OVERLAY-RECORD TO IOP-REC-PTR         ELUGVLSV
00527         SET GVL1-INDEX TO 1                                       ELUGVLSV
00528         SET GVL1-MAX-INDEX TO GVL1-PRVDR-CNT                      ELUGVLSV
00529         MOVE GVL1-PRVDR-DTL TO GVL1B-PRVDR-DTL                    ELUGVLSV
00530         MOVE GVL1-ENTRY (GVL1-INDEX) TO GVL1B-ENTRY               ELUGVLSV
00531         PERFORM 9626-SET-PRVDR-CNTRL1                             ELUGVLSV
00532      END-IF.                                                      ELUGVLSV
00533                                                                   ELUGVLSV
00534  6050-GET-FRST-WKFK2-OCRNC.                                       ELUGVLSV
00535      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                   ELUGVLSV
00536          TO WS-ELSWKFL2-PTR.                                      ELUGVLSV
00537      SET CIA-ELSWKFL2-DDN TO TRUE.                                ELUGVLSV
00538      MOVE 1 TO IOP-TSQ-ITEM-NBR.                                  ELUGVLSV
00539      SET IOP-RD TO TRUE.                                          ELUGVLSV
00540      SET IOP-KVQ-NONE TO TRUE.                                    ELUGVLSV
00541      SET IOP-FCQ-NONE TO TRUE.                                    ELUGVLSV
00542      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELUGVLSV
00543      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
00544                            DFHCOMMAREA.                           ELUGVLSV
00545      IF NOT IOP-RC-OK                                             ELUGVLSV
00546         MOVE HIGH-VALUES TO GVL2B-OVRLY-RCRD-BUFFER               ELUGVLSV
00547         MOVE 9999999 TO GVL2B-PRVDR-EFF-DT                        ELUGVLSV
00548                       GVL2B-PVDR-TRMTN-DT                         ELUGVLSV
00549      ELSE                                                         ELUGVLSV
00550         SET ADDRESS OF GVL2-OVERLAY-RECORD TO IOP-REC-PTR         ELUGVLSV
00551         SET GVL2-INDEX TO 1                                       ELUGVLSV
00552         SET GVL2-MAX-INDEX TO GVL2-PRVDR-CNT                      ELUGVLSV
00553         MOVE GVL2-PRVDR-DTL TO GVL2B-PRVDR-DTL                    ELUGVLSV
00554         MOVE GVL2-ENTRY (GVL2-INDEX) TO GVL2B-ENTRY               ELUGVLSV
00555         PERFORM 9627-SET-PRVDR-CNTRL2                             ELUGVLSV
00556      END-IF.                                                      ELUGVLSV
00557                                                                   ELUGVLSV
00558  6075-GET-FRST-WKFK3-OCRNC.                                       ELUGVLSV
00559      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                   ELUGVLSV
00560          TO WS-ELSWKFL3-PTR.                                      ELUGVLSV
00561      SET CIA-ELSWKFL3-DDN TO TRUE.                                ELUGVLSV
00562      MOVE 1 TO IOP-TSQ-ITEM-NBR.                                  ELUGVLSV
00563      SET IOP-RD TO TRUE.                                          ELUGVLSV
00564      SET IOP-KVQ-NONE TO TRUE.                                    ELUGVLSV
00565      SET IOP-FCQ-NONE TO TRUE.                                    ELUGVLSV
00566      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELUGVLSV
00567      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
00568                            DFHCOMMAREA.                           ELUGVLSV
00569      IF NOT IOP-RC-OK                                             ELUGVLSV
00570         MOVE HIGH-VALUES TO GVL3B-OVRLY-RCRD-BUFFER               ELUGVLSV
00571         MOVE 9999999     TO GVL3B-PRVDR-EFF-DT                    ELUGVLSV
00572                             GVL3B-PVDR-TRMTN-DT                   ELUGVLSV
00573      ELSE                                                         ELUGVLSV
00574         SET ADDRESS OF GVL3-OVERLAY-RECORD TO IOP-REC-PTR         ELUGVLSV
00575         SET GVL3-INDEX TO 1                                       ELUGVLSV
00576         SET GVL3-MAX-INDEX TO GVL3-PRVDR-CNT                      ELUGVLSV
00577         MOVE GVL3-PRVDR-DTL TO GVL3B-PRVDR-DTL                    ELUGVLSV
00578         MOVE GVL3-ENTRY (GVL3-INDEX) TO GVL3B-ENTRY               ELUGVLSV
00579         PERFORM 9628-SET-PRVDR-CNTRL3                             ELUGVLSV
00580      END-IF.                                                      ELUGVLSV
00581                                                                   ELUGVLSV
00582  6085-INTL-FRST-GVL4-RCRD.                                        ELUGVLSV
00583      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS TO                ELUGVLSV
00584         WS-ELSWKFL4-PTR.                                          ELUGVLSV
00585      SET ADDRESS OF GVL4-OVERLAY-RECORD TO IOP-REC-PTR.           ELUGVLSV
00586      INITIALIZE GVL4-PRVDR-DTL.                                   ELUGVLSV
00587      PERFORM VARYING GVL4-INDEX                                   ELUGVLSV
00588          FROM 1 BY 1 UNTIL GVL4-INDEX > 100                       ELUGVLSV
00589         INITIALIZE GVL4-ENTRY (GVL4-INDEX)                        ELUGVLSV
00590      END-PERFORM.                                                 ELUGVLSV
00591      SET GVL4-INDEX TO 1.                                         ELUGVLSV
00592      IF GVL1B-PRVDR-NBR < GVL2B-PRVDR-NBR                         ELUGVLSV
00593          PERFORM 6087-DCD-BTWN-GVLB1-GVLB3                        ELUGVLSV
00594      ELSE                                                         ELUGVLSV
00595          PERFORM 6089-DCD-BTWN-GVLB2-GVLB3                        ELUGVLSV
00596      END-IF.                                                      ELUGVLSV
00597      SET GVL4-MAX-INDEX TO 100.                                   ELUGVLSV
00598      MOVE 0 TO GVL4-PRVDR-CNT.                                    ELUGVLSV
00599      SET GVL4-INDEX TO 1.                                         ELUGVLSV
00600                                                                   ELUGVLSV
00601  6087-DCD-BTWN-GVLB1-GVLB3.                                       ELUGVLSV
00602      IF GVL1B-PRVDR-NBR < GVL3B-PRVDR-NBR                         ELUGVLSV
00603          MOVE GVL1B-PRVDR-DTL TO GVL4-PRVDR-DTL                   ELUGVLSV
00604      ELSE                                                         ELUGVLSV
00605          MOVE GVL3B-PRVDR-DTL TO GVL4-PRVDR-DTL                   ELUGVLSV
00606      END-IF.                                                      ELUGVLSV
00607                                                                   ELUGVLSV
00608  6089-DCD-BTWN-GVLB2-GVLB3.                                       ELUGVLSV
00609      IF GVL2B-PRVDR-NBR < GVL3B-PRVDR-NBR                         ELUGVLSV
00610          MOVE GVL2B-PRVDR-DTL TO GVL4-PRVDR-DTL                   ELUGVLSV
00611      ELSE                                                         ELUGVLSV
00612          MOVE GVL3B-PRVDR-DTL TO GVL4-PRVDR-DTL                   ELUGVLSV
00613      END-IF.                                                      ELUGVLSV
00614                                                                   ELUGVLSV
00615  7000-MRG-WRK-EXRCT-OCRNCS.                                       ELUGVLSV
00616      IF GVL1B-PRVDR-NBR < GVL2B-PRVDR-NBR  OR                     ELUGVLSV
00617        (GVL1B-PRVDR-NBR = GVL2B-PRVDR-NBR AND                     ELUGVLSV
00618         GVL1B-PRVDR-EFF-DT < GVL2B-PRVDR-EFF-DT)                  ELUGVLSV
00619         PERFORM 7025-DCD-BTWN-OCNRCS-1-3                          ELUGVLSV
00620      ELSE                                                         ELUGVLSV
00621         PERFORM 7055-DCD-BTWN-OCNRCS-2-3                          ELUGVLSV
00622      END-IF.                                                      ELUGVLSV
00623                                                                   ELUGVLSV
00624  7025-DCD-BTWN-OCNRCS-1-3.                                        ELUGVLSV
00625      IF GVL1B-PRVDR-NBR < GVL3B-PRVDR-NBR  OR                     ELUGVLSV
00626        (GVL1B-PRVDR-NBR = GVL2B-PRVDR-NBR AND                     ELUGVLSV
00627         GVL1B-PRVDR-EFF-DT < GVL3B-PRVDR-EFF-DT)                  ELUGVLSV
00628         MOVE GVL1B-OVRLY-RCRD-BUFFER TO                           ELUGVLSV
00629                       GVL4B-OVRLY-RCRD-BUFFER                     ELUGVLSV
00630         PERFORM 8200-PUT-WKFL4-OCRNC                              ELUGVLSV
00631         PERFORM 8300-GET-NEXT-WKFL1-OCRNC                         ELUGVLSV
00632      ELSE                                                         ELUGVLSV
00633         MOVE GVL3B-OVRLY-RCRD-BUFFER TO                           ELUGVLSV
00634                       GVL4B-OVRLY-RCRD-BUFFER                     ELUGVLSV
00635         PERFORM 8200-PUT-WKFL4-OCRNC                              ELUGVLSV
00636         PERFORM 8500-GET-NEXT-WKFL3-OCRNC                         ELUGVLSV
00637      END-IF.                                                      ELUGVLSV
00638                                                                   ELUGVLSV
00639  7055-DCD-BTWN-OCNRCS-2-3.                                        ELUGVLSV
00640      IF GVL2B-PRVDR-NBR < GVL3B-PRVDR-NBR OR                      ELUGVLSV
00641        (GVL2B-PRVDR-NBR = GVL3B-PRVDR-NBR AND                     ELUGVLSV
00642         GVL2B-PRVDR-EFF-DT < GVL3B-PRVDR-EFF-DT)                  ELUGVLSV
00643         MOVE GVL2B-OVRLY-RCRD-BUFFER TO                           ELUGVLSV
00644                       GVL4B-OVRLY-RCRD-BUFFER                     ELUGVLSV
00645         PERFORM 8200-PUT-WKFL4-OCRNC                              ELUGVLSV
00646         PERFORM 8400-GET-NEXT-WKFL2-OCRNC                         ELUGVLSV
00647      ELSE                                                         ELUGVLSV
00648         MOVE GVL3B-OVRLY-RCRD-BUFFER TO                           ELUGVLSV
00649                       GVL4B-OVRLY-RCRD-BUFFER                     ELUGVLSV
00650         PERFORM 8200-PUT-WKFL4-OCRNC                              ELUGVLSV
00651         PERFORM 8500-GET-NEXT-WKFL3-OCRNC                         ELUGVLSV
00652      END-IF.                                                      ELUGVLSV
00653                                                                   ELUGVLSV
00654  7900-WRT-LAST-WKFL4-RCRD.                                        ELUGVLSV
00655      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                   ELUGVLSV
00656              TO WS-ELSWKFL4-PTR.                                  ELUGVLSV
00657      SET CIA-ELSWKFL4-DDN TO TRUE.                                ELUGVLSV
00658      SET IOP-ADD TO TRUE.                                         ELUGVLSV
00659      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
00660                           DFHCOMMAREA.                            ELUGVLSV
00661      SET GVX-IDX TO IOP-TSQ-ITEM-NBR.                             ELUGVLSV
00662      MOVE GVL4-PRVDR-NBR TO GVX-PRVDR-NMBR (GVX-IDX).             ELUGVLSV
00663      ADD 1 TO GVX-NMBR-PRVDR-ENTRS.                               ELUGVLSV
           PERFORM 9900-CHECK-MAX-ENTRIES.                                      
00664                                                                   ELUGVLSV
00665  8055-INDCT-PRVDR-FL-PRBLM.                                       ELUGVLSV
00666      MOVE 'PROVIDER NUMBER NOT FOUND' TO                          ELUGVLSV
00667          GVL4-PRVDR-NM.                                           ELUGVLSV
00668                                                                   ELUGVLSV
00669  8100-CRT-GNRC-WRK-FL.                                            ELUGVLSV
00670      PERFORM 9100-SETUP-EXEC-BRSW.                                ELUGVLSV
00671      IF IOP-RC-OK                                                 ELUGVLSV
00672         PERFORM 8150-BLD-GNRC-WRK-FL                              ELUGVLSV
00673              UNTIL WS-END-OF-TABULAR                              ELUGVLSV
00674      ELSE                                                         ELUGVLSV
00675         PERFORM 9550-SIGNAL-CRITICAL-IO-ERROR                     ELUGVLSV
00676      END-IF.                                                      ELUGVLSV
00677      SET IOP-END-BR TO TRUE.                                      ELUGVLSV
00678      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
00679                            DFHCOMMAREA.                           ELUGVLSV
00680                                                                   ELUGVLSV
00681  8150-BLD-GNRC-WRK-FL.                                            ELUGVLSV
00682      PERFORM 9125-SETUP-EXEC-READ.                                ELUGVLSV
00683      IF IOP-RC-OK                                                 ELUGVLSV
00684        IF ((KWA-GVL-PRVDR-ID = GVL-PROVIDER-ID) AND               ELUGVLSV
00685               (KWA-GVL-PRVDR-SLOT-NO =                            ELUGVLSV
00686                       GVL-PROVIDER-SLOT-NO))                      ELUGVLSV
00687           PERFORM 8175-WRT-GNRC-WRK-RCRD                          ELUGVLSV
00688         ELSE                                                      ELUGVLSV
00689            PERFORM 9535-CHECK-FOR-EOF                             ELUGVLSV
00690        END-IF                                                     ELUGVLSV
00691      ELSE                                                         ELUGVLSV
00692         IF IOP-RC-ENDFILE                                         ELUGVLSV
00693            PERFORM 9535-CHECK-FOR-EOF                             ELUGVLSV
00694         ELSE                                                      ELUGVLSV
00695            PERFORM 9550-SIGNAL-CRITICAL-IO-ERROR                  ELUGVLSV
00696         END-IF                                                    ELUGVLSV
00697      END-IF.                                                      ELUGVLSV
00698      SET WS-NOT-FIRST-READ TO TRUE.                               ELUGVLSV
00699                                                                   ELUGVLSV
00700  8175-WRT-GNRC-WRK-RCRD.                                          ELUGVLSV
00701      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS TO                ELUGVLSV
00702         WS-GNRC-WKFL-PTR.                                         ELUGVLSV
00703      MOVE WS-GNRC-DDN TO CIA-DDNAME.                              ELUGVLSV
00704      SET ADDRESS OF GVL4-OVERLAY-RECORD TO IOP-REC-PTR.           ELUGVLSV
00705      PERFORM 8750-LOAD-WKFL4.                                     ELUGVLSV
00706      SET IOP-ADD TO TRUE.                                         ELUGVLSV
00707      SET IOP-FCQ-NONE TO TRUE.                                    ELUGVLSV
00708      SET IOP-KVQ-EQ TO TRUE                                       ELUGVLSV
00709      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
00710                            DFHCOMMAREA.                           ELUGVLSV
00711      SET GVX-IDX TO IOP-TSQ-ITEM-NBR.                             ELUGVLSV
00712      MOVE GVL4-PRVDR-NBR TO GVX-PRVDR-NMBR (GVX-IDX).             ELUGVLSV
00713      ADD 1 TO GVX-NMBR-PRVDR-ENTRS.                               ELUGVLSV
           PERFORM 9900-CHECK-MAX-ENTRIES.                                      
00714                                                                   ELUGVLSV
00715  8200-PUT-WKFL4-OCRNC.                                            ELUGVLSV
00716      IF GVL4B-PRVDR-NBR = GVL4-PRVDR-NBR                          ELUGVLSV
00717         PERFORM 8800-ADD-NXT-OCRNC-SAME-PRVDR                     ELUGVLSV
00718      ELSE                                                         ELUGVLSV
00719         PERFORM 8850-WRT-WKFL4-RCRD                               ELUGVLSV
00720      END-IF.                                                      ELUGVLSV
00721                                                                   ELUGVLSV
00722  8300-GET-NEXT-WKFL1-OCRNC.                                       ELUGVLSV
00723      SET GVL1-INDEX UP BY 1.                                      ELUGVLSV
00724      IF GVL1-INDEX > GVL1-MAX-INDEX                               ELUGVLSV
00725         PERFORM 8325-READ-NEXT-WKFL1-RCRD                         ELUGVLSV
00726      ELSE                                                         ELUGVLSV
00727         MOVE GVL1-PRVDR-DTL TO GVL1B-PRVDR-DTL                    ELUGVLSV
00728         MOVE GVL1-ENTRY (GVL1-INDEX) TO GVL1B-ENTRY               ELUGVLSV
00729         PERFORM 9626-SET-PRVDR-CNTRL1                             ELUGVLSV
00730      END-IF.                                                      ELUGVLSV
00731                                                                   ELUGVLSV
00732  8325-READ-NEXT-WKFL1-RCRD.                                       ELUGVLSV
00733      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                   ELUGVLSV
00734                       TO WS-ELSWKFL1-PTR.                         ELUGVLSV
00735      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELUGVLSV
00736      PERFORM 8525-READ-PARMS-FOR-WRK-FLS.                         ELUGVLSV
00737      SET ADDRESS OF GVL1-OVERLAY-RECORD TO IOP-REC-PTR.           ELUGVLSV
00738      IF NOT IOP-RC-OK                                             ELUGVLSV
00739         MOVE HIGH-VALUES TO GVL1B-OVRLY-RCRD-BUFFER               ELUGVLSV
00740         MOVE 9999999     TO GVL1B-PRVDR-EFF-DT                    ELUGVLSV
00741                             GVL1B-PVDR-TRMTN-DT                   ELUGVLSV
00742      ELSE                                                         ELUGVLSV
00743         SET GVL1-INDEX TO 1                                       ELUGVLSV
00744         SET GVL1-MAX-INDEX TO GVL1-PRVDR-CNT                      ELUGVLSV
00745         MOVE GVL1-PRVDR-DTL TO GVL1B-PRVDR-DTL                    ELUGVLSV
00746         MOVE GVL1-ENTRY (GVL1-INDEX) TO GVL1B-ENTRY               ELUGVLSV
00747         PERFORM 9626-SET-PRVDR-CNTRL1                             ELUGVLSV
00748      END-IF.                                                      ELUGVLSV
00749                                                                   ELUGVLSV
00750  8400-GET-NEXT-WKFL2-OCRNC.                                       ELUGVLSV
00751      SET GVL2-INDEX UP BY 1.                                      ELUGVLSV
00752      IF GVL2-INDEX > GVL2-MAX-INDEX                               ELUGVLSV
00753         PERFORM 8425-READ-NEXT-WKFL2-RCRD                         ELUGVLSV
00754      ELSE                                                         ELUGVLSV
00755         MOVE GVL2-PRVDR-DTL TO GVL2B-PRVDR-DTL                    ELUGVLSV
00756         MOVE GVL2-ENTRY (GVL2-INDEX) TO GVL2B-ENTRY               ELUGVLSV
00757         PERFORM 9627-SET-PRVDR-CNTRL2                             ELUGVLSV
00758      END-IF.                                                      ELUGVLSV
00759                                                                   ELUGVLSV
00760  8425-READ-NEXT-WKFL2-RCRD.                                       ELUGVLSV
00761      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                   ELUGVLSV
00762                     TO WS-ELSWKFL2-PTR.                           ELUGVLSV
00763      SET CIA-ELSWKFL2-DDN TO TRUE.                                ELUGVLSV
00764      PERFORM 8525-READ-PARMS-FOR-WRK-FLS.                         ELUGVLSV
00765      SET ADDRESS OF GVL2-OVERLAY-RECORD TO IOP-REC-PTR.           ELUGVLSV
00766      IF NOT IOP-RC-OK                                             ELUGVLSV
00767         MOVE HIGH-VALUES TO GVL2B-OVRLY-RCRD-BUFFER               ELUGVLSV
00768         MOVE 9999999     TO GVL2B-PRVDR-EFF-DT                    ELUGVLSV
00769                             GVL2B-PVDR-TRMTN-DT                   ELUGVLSV
00770      ELSE                                                         ELUGVLSV
00771         SET GVL2-INDEX TO 1                                       ELUGVLSV
00772         SET GVL2-MAX-INDEX TO GVL2-PRVDR-CNT                      ELUGVLSV
00773         MOVE GVL2-PRVDR-DTL TO GVL2B-PRVDR-DTL                    ELUGVLSV
00774         MOVE GVL2-ENTRY (GVL2-INDEX) TO GVL2B-ENTRY               ELUGVLSV
00775         PERFORM 9627-SET-PRVDR-CNTRL2                             ELUGVLSV
00776      END-IF.                                                      ELUGVLSV
00777                                                                   ELUGVLSV
00778  8500-GET-NEXT-WKFL3-OCRNC.                                       ELUGVLSV
00779      SET GVL3-INDEX UP BY 1.                                      ELUGVLSV
00780      IF GVL3-INDEX > GVL3-MAX-INDEX                               ELUGVLSV
00781         PERFORM 8525-READ-NEXT-WKFL3-RCRD                         ELUGVLSV
00782      ELSE                                                         ELUGVLSV
00783         MOVE GVL3-PRVDR-DTL TO GVL3B-PRVDR-DTL                    ELUGVLSV
00784         MOVE GVL3-ENTRY(GVL3-INDEX) TO GVL3B-ENTRY                ELUGVLSV
00785         PERFORM 9628-SET-PRVDR-CNTRL3                             ELUGVLSV
00786      END-IF.                                                      ELUGVLSV
00787                                                                   ELUGVLSV
00788  8525-READ-NEXT-WKFL3-RCRD.                                       ELUGVLSV
00789      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS TO                ELUGVLSV
00790                    WS-ELSWKFL3-PTR.                               ELUGVLSV
00791      SET CIA-ELSWKFL3-DDN TO TRUE.                                ELUGVLSV
00792      PERFORM 8525-READ-PARMS-FOR-WRK-FLS.                         ELUGVLSV
00793      SET ADDRESS OF GVL3-OVERLAY-RECORD TO IOP-REC-PTR.           ELUGVLSV
00794      IF NOT IOP-RC-OK                                             ELUGVLSV
00795         MOVE HIGH-VALUES TO GVL3B-OVRLY-RCRD-BUFFER               ELUGVLSV
00796         MOVE 9999999     TO GVL3B-PRVDR-EFF-DT                    ELUGVLSV
00797                             GVL3B-PVDR-TRMTN-DT                   ELUGVLSV
00798      ELSE                                                         ELUGVLSV
00799         SET GVL3-INDEX TO 1                                       ELUGVLSV
00800         SET GVL3-MAX-INDEX TO GVL3-PRVDR-CNT                      ELUGVLSV
00801         MOVE GVL3-PRVDR-DTL TO GVL3B-PRVDR-DTL                    ELUGVLSV
00802         MOVE GVL3-ENTRY (GVL3-INDEX) TO GVL3B-ENTRY               ELUGVLSV
00803         PERFORM 9628-SET-PRVDR-CNTRL3                             ELUGVLSV
00804      END-IF.                                                      ELUGVLSV
00805                                                                   ELUGVLSV
00806  8525-READ-PARMS-FOR-WRK-FLS.                                     ELUGVLSV
00807      SET IOP-RD-NXT TO TRUE.                                      ELUGVLSV
00808      SET IOP-KVQ-EQ TO TRUE.                                      ELUGVLSV
00809      SET IOP-FCQ-NONE TO TRUE.                                    ELUGVLSV
00810      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
00811                            DFHCOMMAREA.                           ELUGVLSV
00812  8750-LOAD-WKFL4.                                                 ELUGVLSV
00813      SET GVL4-INDEX TO 1.                                         ELUGVLSV
00814      IF WS-PROCESSING-INST                                        ELUGVLSV
00815         SET GVL4-INSTITUTIONAL TO TRUE                            ELUGVLSV
00816      ELSE                                                         ELUGVLSV
00817         SET GVL4-PROFESSIONAL TO TRUE                             ELUGVLSV
00818      END-IF.                                                      ELUGVLSV
00819      MOVE GVL-PROVIDER-NBR TO GVL4-PRVDR-NBR.                     ELUGVLSV
00820      MOVE GVL-PROVIDER-SEQ-NBR TO GVL4-PRVDR-SQ-NBR.              ELUGVLSV
00821      MOVE GVL-PROVIDER-CNT TO GVL4-PRVDR-CNT.                     ELUGVLSV
00822      PERFORM 8900-GET-PRVDR-NAME.                                 ELUGVLSV
00823      SET GVL-INDEX TO GVL-PROVIDER-CNT.                           ELUGVLSV
00824      SET WS-GVL-MAX-INDEX TO GVL-INDEX.                           ELUGVLSV
00825      PERFORM VARYING GVL-INDEX FROM 1 BY 1                        ELUGVLSV
00826           UNTIL GVL-INDEX > WS-GVL-MAX-INDEX                      ELUGVLSV
00827         MOVE GVL-ENTRY (GVL-INDEX) TO GVL4-ENTRY (GVL4-INDEX)     ELUGVLSV
00828         PERFORM 8751-LD-PRVDR-CNTRLS                              ELUGVLSV
00829         SET GVL4-INDEX UP BY 1                                    ELUGVLSV
00830      END-PERFORM.                                                 ELUGVLSV
00831                                                                   ELUGVLSV
00832  8751-LD-PRVDR-CNTRLS.                                            ELUGVLSV
00833      IF WS-PROCESSING-INST                                        ELUGVLSV
00834        IF SLN-GVLF-SLOT-NO NOT = ZERO                             ELUGVLSV
00835           MOVE 'F' TO GVL4-PVDR-CTL-1(GVL4-INDEX)                 ELUGVLSV
00836           IF GVL4-PVDR-CTL-2(GVL4-INDEX) NOT = 'ZZ'               ELUGVLSV
00837                   AND > ZERO                                      ELUGVLSV
00838               MOVE 'F' TO GVL4-PVDR-CTL-1(GVL4-INDEX)             ELUGVLSV
00839        END-IF                                                     ELUGVLSV
00840      END-IF.                                                      ELUGVLSV
00841      IF WS-PROCESSING-PROF                                        ELUGVLSV
00842        EVALUATE TRUE                                              ELUGVLSV
00843          WHEN SLN-GVLP-SLOT-NO NOT = ZERO                         ELUGVLSV
00844            MOVE 'P' TO GVL4-PVDR-CTL-1(GVL4-INDEX)                ELUGVLSV
00845            IF GVL4-PVDR-CTL-2(GVL4-INDEX) NOT = 'ZZ'              ELUGVLSV
00846                           AND > ZERO                              ELUGVLSV
00847                MOVE 'P' TO GVL4-PVDR-CTL-2(GVL4-INDEX)            ELUGVLSV
00848            END-IF                                                 ELUGVLSV
00849          WHEN SLN-GVLQ-SLOT-NO NOT = ZERO                         ELUGVLSV
00850            MOVE 'Q' TO GVL4-PVDR-CTL-1(GVL4-INDEX)                ELUGVLSV
00851            IF GVL4-PVDR-CTL-2(GVL4-INDEX) NOT = 'ZZ'              ELUGVLSV
00852                        AND > ZERO                                 ELUGVLSV
00853                MOVE 'Q' TO GVL4-PVDR-CTL-2(GVL4-INDEX)            ELUGVLSV
00854            END-IF                                                 ELUGVLSV
00855        END-EVALUATE                                               ELUGVLSV
00856      END-IF.                                                      ELUGVLSV
00857                                                                   ELUGVLSV
00858                                                                   ELUGVLSV
00859  8800-ADD-NXT-OCRNC-SAME-PRVDR.                                   ELUGVLSV
00860      IF GVL4-INDEX > GVL4-MAX-INDEX                               ELUGVLSV
00861         PERFORM 8850-WRT-WKFL4-RCRD                               ELUGVLSV
00862      ELSE                                                         ELUGVLSV
00863         ADD 1 TO GVL4-PRVDR-CNT                                   ELUGVLSV
00864         MOVE GVL4B-ENTRY TO GVL4-ENTRY (GVL4-INDEX)               ELUGVLSV
00865         SET GVL4-INDEX UP BY 1                                    ELUGVLSV
00866         IF WS-PROCESSING-INST                                     ELUGVLSV
00867            SET GVL4-INSTITUTIONAL TO TRUE                         ELUGVLSV
00868         ELSE                                                      ELUGVLSV
00869            SET GVL4-PROFESSIONAL TO TRUE                          ELUGVLSV
00870         END-IF                                                    ELUGVLSV
00871         PERFORM 8885-LOAD-PRVDR-CNTRLS                            ELUGVLSV
00872      END-IF.                                                      ELUGVLSV
00873                                                                   ELUGVLSV
00874  8850-WRT-WKFL4-RCRD.                                             ELUGVLSV
00875      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                   ELUGVLSV
00876        TO WS-ELSWKFL4-PTR.                                        ELUGVLSV
00877      SET CIA-ELSWKFL4-DDN TO TRUE.                                ELUGVLSV
00878      SET IOP-ADD TO TRUE.                                         ELUGVLSV
00879      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
00880                            DFHCOMMAREA.                           ELUGVLSV
00881      SET GVX-IDX TO IOP-TSQ-ITEM-NBR.                             ELUGVLSV
00882      MOVE GVL4-PRVDR-NBR TO GVX-PRVDR-NMBR(GVX-IDX).              ELUGVLSV
00883      ADD 1 TO GVX-NMBR-PRVDR-ENTRS.                               ELUGVLSV
           PERFORM 9900-CHECK-MAX-ENTRIES.                                      
00884      PERFORM 8875-INITIALIZE-GVL4-RCRD.                           ELUGVLSV
00885      IF WS-PROCESSING-INST                                        ELUGVLSV
00886         SET GVL4-INSTITUTIONAL TO TRUE                            ELUGVLSV
00887      ELSE                                                         ELUGVLSV
00888         SET GVL4-PROFESSIONAL TO TRUE                             ELUGVLSV
00889      END-IF.                                                      ELUGVLSV
00890      PERFORM 8900-GET-PRVDR-NAME.                                 ELUGVLSV
00891      MOVE GVL4B-PRVDR-NBR TO GVL4-PRVDR-NBR.                      ELUGVLSV
00892      MOVE GVL4B-ENTRY TO GVL4-ENTRY (GVL4-INDEX).                 ELUGVLSV
00893      PERFORM 8885-LOAD-PRVDR-CNTRLS.                              ELUGVLSV
00894      PERFORM 8900-GET-PRVDR-NAME.                                 ELUGVLSV
00895      SET GVL4-INDEX TO 1.                                         ELUGVLSV
00896                                                                   ELUGVLSV
00897  8885-LOAD-PRVDR-CNTRLS.                                          ELUGVLSV
00898      IF WS-PROCESSING-INST                                        ELUGVLSV
00899        IF KWA-GVL-PRVDR-ID = '  GVLF'                             ELUGVLSV
00900           MOVE 'F' TO GVL4-PVDR-CTL-1(GVL4-INDEX)                 ELUGVLSV
00901           IF GVL4-PVDR-CTL-2(GVL4-INDEX) NOT = 'ZZ'               ELUGVLSV
00902                   AND > ZERO                                      ELUGVLSV
00903               MOVE 'F' TO GVL4-PVDR-CTL-1(GVL4-INDEX)             ELUGVLSV
00904        END-IF                                                     ELUGVLSV
00905      END-IF.                                                      ELUGVLSV
00906      IF WS-PROCESSING-PROF                                        ELUGVLSV
00907        EVALUATE TRUE                                              ELUGVLSV
00908           WHEN KWA-GVL-PRVDR-ID = '  GVLP'                        ELUGVLSV
00909            MOVE 'P' TO GVL4-PVDR-CTL-1(GVL4-INDEX)                ELUGVLSV
00910            IF GVL4-PVDR-CTL-2(GVL4-INDEX) NOT = 'ZZ'              ELUGVLSV
00911                           AND > ZERO                              ELUGVLSV
00912                MOVE 'P' TO GVL4-PVDR-CTL-2(GVL4-INDEX)            ELUGVLSV
00913            END-IF                                                 ELUGVLSV
00914           WHEN KWA-GVL-PRVDR-ID = '  GVLQ'                        ELUGVLSV
00915            MOVE 'Q' TO GVL4-PVDR-CTL-1(GVL4-INDEX)                ELUGVLSV
00916            IF GVL4-PVDR-CTL-2(GVL4-INDEX) NOT = 'ZZ'              ELUGVLSV
00917                        AND > ZERO                                 ELUGVLSV
00918                MOVE 'Q' TO GVL4-PVDR-CTL-2(GVL4-INDEX)            ELUGVLSV
00919            END-IF                                                 ELUGVLSV
00920        END-EVALUATE                                               ELUGVLSV
00921      END-IF.                                                      ELUGVLSV
00922                                                                   ELUGVLSV
00923  8875-INITIALIZE-GVL4-RCRD.                                       ELUGVLSV
00924      INITIALIZE GVL4-PRVDR-DTL.                                   ELUGVLSV
00925      PERFORM VARYING GVL4-INDEX                                   ELUGVLSV
00926          FROM 1 BY 1 UNTIL GVL4-INDEX > WS-GVL-MAX-INDEX          ELUGVLSV
00927             INITIALIZE GVL4-ENTRY (GVL4-INDEX)                    ELUGVLSV
00928      END-PERFORM.                                                 ELUGVLSV
00929      SET GVL4-INDEX TO 1.                                         ELUGVLSV
00930      ADD 1 TO GVL4-PRVDR-CNT.                                     ELUGVLSV
00931      MOVE GVL4B-ENTRY TO GVL4-ENTRY (GVL4-INDEX).                 ELUGVLSV
00932      SET GVL4-INDEX UP BY 1.                                      ELUGVLSV
00933                                                                   ELUGVLSV
00934  8900-GET-PRVDR-NAME.                                             ELUGVLSV
00935      MOVE GVL4-PRVDR-NBR TO PDB-I-PROVIDER-NBR.                   ELUGVLSV
00936      MOVE 'PRVDR' TO PDB-I-REQUEST-TYPE-N.                        ELUGVLSV
00937      MOVE '02' TO PDB-I-VERSION.                                  ELUGVLSV
00938      MOVE 'D' TO PDB-I-ACCESS-MODE.                               ELUGVLSV
00939      SET PDB-I-SS-ACTIVE-INACTIVE-PROV TO TRUE.                   ELUGVLSV
00940      SET PDB-I-UA-UNLIMITED-ACCESS TO TRUE.                       ELUGVLSV
00941      EXEC CICS LINK PROGRAM ('DBPIOC')                            ELUGVLSV
00942                     COMMAREA (PDB-IO-AREA)                        ELUGVLSV
00943                     END-EXEC.                                     ELUGVLSV
00944      IF PDB-O-RC-SUCCESSFUL                                       ELUGVLSV
00945         PERFORM 8950-MOVE-PRVDR-NM-TO-WKFL4                       ELUGVLSV
00946      ELSE                                                         ELUGVLSV
00947         PERFORM 8055-INDCT-PRVDR-FL-PRBLM                         ELUGVLSV
00948      END-IF.                                                      ELUGVLSV
00949                                                                   ELUGVLSV
00950  8950-MOVE-PRVDR-NM-TO-WKFL4.                                     ELUGVLSV
00951      SET ADDRESS OF PRVDR-MSTR-RCRD                               ELUGVLSV
00952         TO ADDRESS OF PDB-O-RECORD-AREA.                          ELUGVLSV
00953      MOVE PFM-OFFICE-NAME TO GVL4-PRVDR-NM.                       ELUGVLSV
00954                                                                   ELUGVLSV
00955  9000-ESTBLSH-ADRSS-ALCTE-WKFL1.                                  ELUGVLSV
00956      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELUGVLSV
00957      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
00958            ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                ELUGVLSV
00959      IF CIA-RC-PTR-NULL                                           ELUGVLSV
00960         PERFORM 9005-ALCT-WKFL1-IOP-BLK                           ELUGVLSV
00961      END-IF.                                                      ELUGVLSV
00962      SET WS-ELSWKFL1-PTR TO                                       ELUGVLSV
00963             ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.               ELUGVLSV
00964      MOVE CIA-DDNAME TO WS-ELSWKFL1-DDN.                          ELUGVLSV
00965      PERFORM 9010-DELETE-WRKFL1.                                  ELUGVLSV
00966      IF IOP-REC-PTR = NULL                                        ELUGVLSV
00967         PERFORM 9015-ALCT-WKFL1-DATA-REC                          ELUGVLSV
00968      END-IF.                                                      ELUGVLSV
00969      SET ADDRESS OF GVL1-OVERLAY-RECORD TO IOP-REC-PTR.           ELUGVLSV
00970                                                                   ELUGVLSV
00971  9005-ALCT-WKFL1-IOP-BLK.                                         ELUGVLSV
00972      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELUGVLSV
00973      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGVLSV
00974      CALL 'ELUSTGMG' USING DFHEIBLK                               ELUGVLSV
00975                           DFHCOMMAREA.                            ELUGVLSV
00976      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELUGVLSV
00977      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
00978           ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                 ELUGVLSV
00979                                                                   ELUGVLSV
00980  9010-DELETE-WRKFL1.                                              ELUGVLSV
00981      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELUGVLSV
00982      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
00983                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.      ELUGVLSV
00984      SET IOP-DEL TO TRUE.                                         ELUGVLSV
00985      SET IOP-FCQ-NONE TO TRUE.                                    ELUGVLSV
00986      SET IOP-KVQ-NONE TO TRUE.                                    ELUGVLSV
00987      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
00988                            DFHCOMMAREA.                           ELUGVLSV
00989                                                                   ELUGVLSV
00990  9015-ALCT-WKFL1-DATA-REC.                                        ELUGVLSV
00991      COMPUTE IOP-MAX-REC-LEN =                                    ELUGVLSV
00992        LENGTH OF GVL1-PRVDR-DTL + (100 * LENGTH OF GVL1-ENTRY).   ELUGVLSV
00993      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELUGVLSV
00994      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGVLSV
00995      SET IOP-GETMAIN-REC TO TRUE.                                 ELUGVLSV
00996      CALL 'ELUSTGMG' USING DFHEIBLK                               ELUGVLSV
00997                            DFHCOMMAREA.                           ELUGVLSV
00998                                                                   ELUGVLSV
00999  9020-ESTBLSH-ADRSS-ALCTE-WKFL2.                                  ELUGVLSV
01000      SET CIA-ELSWKFL2-DDN TO TRUE.                                ELUGVLSV
01001      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
01002            ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                ELUGVLSV
01003      IF CIA-RC-PTR-NULL                                           ELUGVLSV
01004         PERFORM 9025-ALCT-WKFL2-IOP-BLK                           ELUGVLSV
01005      END-IF.                                                      ELUGVLSV
01006      SET WS-ELSWKFL2-PTR TO                                       ELUGVLSV
01007             ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.               ELUGVLSV
01008      MOVE CIA-DDNAME TO WS-ELSWKFL2-DDN.                          ELUGVLSV
01009      PERFORM 9030-DELETE-WRKFL2.                                  ELUGVLSV
01010      IF IOP-REC-PTR = NULL                                        ELUGVLSV
01011         PERFORM 9035-ALCT-WKFL2-DATA-REC                          ELUGVLSV
01012      END-IF.                                                      ELUGVLSV
01013      SET ADDRESS OF GVL2-OVERLAY-RECORD TO IOP-REC-PTR.           ELUGVLSV
01014                                                                   ELUGVLSV
01015  9025-ALCT-WKFL2-IOP-BLK.                                         ELUGVLSV
01016      SET CIA-ELSWKFL2-DDN TO TRUE.                                ELUGVLSV
01017      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGVLSV
01018      CALL 'ELUSTGMG' USING DFHEIBLK                               ELUGVLSV
01019                           DFHCOMMAREA.                            ELUGVLSV
01020      SET CIA-ELSWKFL2-DDN TO TRUE.                                ELUGVLSV
01021      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
01022           ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                 ELUGVLSV
01023                                                                   ELUGVLSV
01024  9030-DELETE-WRKFL2.                                              ELUGVLSV
01025      SET CIA-ELSWKFL2-DDN TO TRUE.                                ELUGVLSV
01026      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
01027                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.      ELUGVLSV
01028      SET IOP-DEL TO TRUE.                                         ELUGVLSV
01029      SET IOP-FCQ-NONE TO TRUE.                                    ELUGVLSV
01030      SET IOP-KVQ-NONE TO TRUE.                                    ELUGVLSV
01031      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
01032                            DFHCOMMAREA.                           ELUGVLSV
01033                                                                   ELUGVLSV
01034  9035-ALCT-WKFL2-DATA-REC.                                        ELUGVLSV
01035      COMPUTE IOP-MAX-REC-LEN =                                    ELUGVLSV
01036        LENGTH OF GVL2-PRVDR-DTL + (100 * LENGTH OF GVL2-ENTRY).   ELUGVLSV
01037      SET CIA-ELSWKFL2-DDN TO TRUE.                                ELUGVLSV
01038      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGVLSV
01039      SET IOP-GETMAIN-REC TO TRUE.                                 ELUGVLSV
01040      CALL 'ELUSTGMG' USING DFHEIBLK                               ELUGVLSV
01041                            DFHCOMMAREA.                           ELUGVLSV
01042                                                                   ELUGVLSV
01043  9040-ESTBLSH-ADRSS-ALCTE-WKFL3.                                  ELUGVLSV
01044      SET CIA-ELSWKFL3-DDN TO TRUE.                                ELUGVLSV
01045      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
01046            ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                ELUGVLSV
01047      IF CIA-RC-PTR-NULL                                           ELUGVLSV
01048         PERFORM 9045-ALCT-WKFL3-IOP-BLK                           ELUGVLSV
01049      END-IF.                                                      ELUGVLSV
01050      SET WS-ELSWKFL3-PTR TO                                       ELUGVLSV
01051             ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.               ELUGVLSV
01052      MOVE CIA-DDNAME TO WS-ELSWKFL3-DDN.                          ELUGVLSV
01053      PERFORM 9050-DELETE-WRKFL3.                                  ELUGVLSV
01054      IF IOP-REC-PTR = NULL                                        ELUGVLSV
01055         PERFORM 9055-ALCT-WKFL3-DATA-REC                          ELUGVLSV
01056      END-IF.                                                      ELUGVLSV
01057      SET ADDRESS OF GVL3-OVERLAY-RECORD TO IOP-REC-PTR.           ELUGVLSV
01058                                                                   ELUGVLSV
01059  9045-ALCT-WKFL3-IOP-BLK.                                         ELUGVLSV
01060      SET CIA-ELSWKFL3-DDN TO TRUE.                                ELUGVLSV
01061      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGVLSV
01062      CALL 'ELUSTGMG' USING DFHEIBLK                               ELUGVLSV
01063                           DFHCOMMAREA.                            ELUGVLSV
01064      SET CIA-ELSWKFL3-DDN TO TRUE.                                ELUGVLSV
01065      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
01066           ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                 ELUGVLSV
01067                                                                   ELUGVLSV
01068  9050-DELETE-WRKFL3.                                              ELUGVLSV
01069      SET CIA-ELSWKFL3-DDN TO TRUE.                                ELUGVLSV
01070      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
01071                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.      ELUGVLSV
01072      SET IOP-DEL TO TRUE.                                         ELUGVLSV
01073      SET IOP-FCQ-NONE TO TRUE.                                    ELUGVLSV
01074      SET IOP-KVQ-NONE TO TRUE.                                    ELUGVLSV
01075      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
01076                            DFHCOMMAREA.                           ELUGVLSV
01077                                                                   ELUGVLSV
01078  9055-ALCT-WKFL3-DATA-REC.                                        ELUGVLSV
01079      COMPUTE IOP-MAX-REC-LEN =                                    ELUGVLSV
01080        LENGTH OF GVL3-PRVDR-DTL + (100 * LENGTH OF GVL3-ENTRY).   ELUGVLSV
01081      SET CIA-ELSWKFL3-DDN TO TRUE.                                ELUGVLSV
01082      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGVLSV
01083      SET IOP-GETMAIN-REC TO TRUE.                                 ELUGVLSV
01084      CALL 'ELUSTGMG' USING DFHEIBLK                               ELUGVLSV
01085                            DFHCOMMAREA.                           ELUGVLSV
01086                                                                   ELUGVLSV
01087  9060-ESTBLSH-ADRSS-ALCTE-WKFL4.                                  ELUGVLSV
01088      SET CIA-ELSWKFL4-DDN TO TRUE.                                ELUGVLSV
01089      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
01090            ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                ELUGVLSV
01091      IF CIA-RC-PTR-NULL                                           ELUGVLSV
01092         PERFORM 9065-ALCT-WKFL4-IOP-BLK                           ELUGVLSV
01093      END-IF.                                                      ELUGVLSV
01094      SET WS-ELSWKFL4-PTR TO                                       ELUGVLSV
01095             ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.               ELUGVLSV
01096      MOVE CIA-DDNAME TO WS-ELSWKFL4-DDN.                          ELUGVLSV
01097      PERFORM 9070-DELETE-WRKFL4.                                  ELUGVLSV
01098      IF IOP-REC-PTR = NULL                                        ELUGVLSV
01099         PERFORM 9075-ALCT-WKFL4-DATA-REC                          ELUGVLSV
01100      END-IF.                                                      ELUGVLSV
01101      SET ADDRESS OF GVL4-OVERLAY-RECORD TO IOP-REC-PTR.           ELUGVLSV
01102                                                                   ELUGVLSV
01103  9062-ESTBLSH-ADRS-GVX-FILE-IO.                                   ELUGVLSV
01104      COMPUTE CIA-AREA-LEN = LENGTH OF GVX-NMBR-PRVDR-ENTRS        ELUGVLSV
01105         + (5000 * LENGTH OF GVX-PRVDR-NMBR).                      ELUGVLSV
01106      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELUGVLSV
01107      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGVLSV
01108      CALL 'ELUSTGMG' USING DFHEIBLK                               ELUGVLSV
01109                           DFHCOMMAREA.                            ELUGVLSV
01110      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELUGVLSV
01111      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
01112           ADDRESS OF GVX-PROVIDER-NUMBER-OVERLAY.                 ELUGVLSV
01113                                                                   ELUGVLSV
01114  9065-ALCT-WKFL4-IOP-BLK.                                         ELUGVLSV
01115      SET CIA-ELSWKFL4-DDN TO TRUE.                                ELUGVLSV
01116      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGVLSV
01117      CALL 'ELUSTGMG' USING DFHEIBLK                               ELUGVLSV
01118                           DFHCOMMAREA.                            ELUGVLSV
01119      SET CIA-ELSWKFL4-DDN TO TRUE.                                ELUGVLSV
01120      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
01121           ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                 ELUGVLSV
01122                                                                   ELUGVLSV
01123  9070-DELETE-WRKFL4.                                              ELUGVLSV
01124      SET CIA-ELSWKFL4-DDN TO TRUE.                                ELUGVLSV
01125      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUGVLSV
01126                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.      ELUGVLSV
01127      SET IOP-DEL TO TRUE.                                         ELUGVLSV
01128      SET IOP-FCQ-NONE TO TRUE.                                    ELUGVLSV
01129      SET IOP-KVQ-NONE TO TRUE.                                    ELUGVLSV
01130      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
01131                            DFHCOMMAREA.                           ELUGVLSV
01132                                                                   ELUGVLSV
01133  9075-ALCT-WKFL4-DATA-REC.                                        ELUGVLSV
01134      COMPUTE IOP-MAX-REC-LEN =                                    ELUGVLSV
01135        LENGTH OF GVL4-PRVDR-DTL + (100 * LENGTH OF GVL4-ENTRY).   ELUGVLSV
01136      SET CIA-ELSWKFL4-DDN TO TRUE.                                ELUGVLSV
01137      SET CIA-STG-GETMAIN TO TRUE.                                 ELUGVLSV
01138      SET IOP-GETMAIN-REC TO TRUE.                                 ELUGVLSV
01139      CALL 'ELUSTGMG' USING DFHEIBLK                               ELUGVLSV
01140                            DFHCOMMAREA.                           ELUGVLSV
01141                                                                   ELUGVLSV
01142  9100-SETUP-EXEC-BRSW.                                            ELUGVLSV
01143      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                   ELUGVLSV
01144         TO WS-GCPRVTB2-PTR.                                       ELUGVLSV
01145      SET CIA-GCPRVTB2-DDN TO TRUE.                                ELUGVLSV
01146      SET IOP-ST-BR TO TRUE.                                       ELUGVLSV
01147      SET IOP-KVQ-GTE TO TRUE.                                     ELUGVLSV
01148      SET IOP-FCQ-NONE TO TRUE.                                    ELUGVLSV
01149      SET WS-FIRST-READ TO TRUE.                                   ELUGVLSV
01150      MOVE KWA-GCPRVTB2-KEY TO IOP-FILE-KEY.                       ELUGVLSV
01151      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
01152                            DFHCOMMAREA.                           ELUGVLSV
01153                                                                   ELUGVLSV
01154  9125-SETUP-EXEC-READ.                                            ELUGVLSV
01155      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS                   ELUGVLSV
01156         TO WS-GCPRVTB2-PTR.                                       ELUGVLSV
01157      SET CIA-GCPRVTB2-DDN TO TRUE.                                ELUGVLSV
01158      SET IOP-RD-NXT TO TRUE.                                      ELUGVLSV
01159      SET IOP-FCQ-NONE TO TRUE.                                    ELUGVLSV
01160      SET IOP-KVQ-NONE TO TRUE.                                    ELUGVLSV
01161      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELUGVLSV
01162      CALL 'ELUIOPGM' USING DFHEIBLK                               ELUGVLSV
01163                            DFHCOMMAREA.                           ELUGVLSV
01164      SET ADDRESS OF GVL-GCPS-RECORD TO IOP-REC-PTR.               ELUGVLSV
01165                                                                   ELUGVLSV
01166  9535-CHECK-FOR-EOF.                                              ELUGVLSV
01167      IF WS-FIRST-READ                                             ELUGVLSV
01168         SET CIA-AB-NOTFND-GCPRVTB2 TO TRUE                        ELUGVLSV
01169         PERFORM 9999-ABEND                                        ELUGVLSV
01170      ELSE                                                         ELUGVLSV
01171         SET WS-END-OF-TABULAR TO TRUE                             ELUGVLSV
01172      END-IF.                                                      ELUGVLSV
01173                                                                   ELUGVLSV
01174  9550-SIGNAL-CRITICAL-IO-ERROR.                                   ELUGVLSV
01175      SET CIA-AB-CRITIO TO TRUE.                                   ELUGVLSV
01176      PERFORM 9999-ABEND.                                          ELUGVLSV
01177                                                                   ELUGVLSV
01178  9555-SIGNAL-GCPRVTB2-ERROR.                                      ELUGVLSV
01179      SET CIA-AB-NOTFND-GCPRVTB2 TO TRUE.                          ELUGVLSV
01180      PERFORM 9999-ABEND.                                          ELUGVLSV
01181                                                                   ELUGVLSV
01182  9626-SET-PRVDR-CNTRL1.                                           ELUGVLSV
01183      IF WS-PROCESSING-INST                                        ELUGVLSV
01184         MOVE 'F' TO GVL1B-PVDR-CTL-1                              ELUGVLSV
01185         IF GVL1-PVDR-CTL-1 (GVL1-INDEX) > ZERO AND NOT = 'ZZ'     ELUGVLSV
01186             MOVE 'F' TO GVL1B-PVDR-CTL-2                          ELUGVLSV
01187         END-IF                                                    ELUGVLSV
01188      ELSE                                                         ELUGVLSV
01189         MOVE 'P' TO GVL1B-PVDR-CTL-1                              ELUGVLSV
01190         IF GVL1-PVDR-CTL-2 (GVL1-INDEX) > ZERO AND NOT = 'ZZ'     ELUGVLSV
01191             MOVE 'P' TO GVL1B-PVDR-CTL-2                          ELUGVLSV
01192         END-IF                                                    ELUGVLSV
01193      END-IF.                                                      ELUGVLSV
01194                                                                   ELUGVLSV
01195  9627-SET-PRVDR-CNTRL2.                                           ELUGVLSV
01196      IF WS-PROCESSING-INST                                        ELUGVLSV
01197         MOVE 'G' TO GVL2B-PVDR-CTL-1                              ELUGVLSV
01198         IF GVL2-PVDR-CTL-1 (GVL2-INDEX) > ZERO AND NOT = 'ZZ'     ELUGVLSV
01199             MOVE 'G' TO GVL2B-PVDR-CTL-2                          ELUGVLSV
01200         END-IF                                                    ELUGVLSV
01201      ELSE                                                         ELUGVLSV
01202         MOVE 'Q' TO GVL2B-PVDR-CTL-1                              ELUGVLSV
01203         IF GVL2-PVDR-CTL-2 (GVL2-INDEX) > ZERO AND NOT = 'ZZ'     ELUGVLSV
01204             MOVE 'Q' TO GVL2B-PVDR-CTL-2                          ELUGVLSV
01205         END-IF                                                    ELUGVLSV
01206      END-IF.                                                      ELUGVLSV
01207                                                                   ELUGVLSV
01208  9628-SET-PRVDR-CNTRL3.                                           ELUGVLSV
01209      IF WS-PROCESSING-INST                                        ELUGVLSV
01210         MOVE 'H' TO GVL3B-PVDR-CTL-1                              ELUGVLSV
01211         IF GVL3-PVDR-CTL-1 (GVL3-INDEX) > ZERO AND NOT = 'ZZ'     ELUGVLSV
01212             MOVE 'H' TO GVL3B-PVDR-CTL-2                          ELUGVLSV
01213         END-IF                                                    ELUGVLSV
01214      ELSE                                                         ELUGVLSV
01215         MOVE 'R' TO GVL3B-PVDR-CTL-1                              ELUGVLSV
01216         IF GVL3-PVDR-CTL-2 (GVL3-INDEX) > ZERO AND NOT = 'ZZ'     ELUGVLSV
01217             MOVE 'R' TO GVL3B-PVDR-CTL-2                          ELUGVLSV
01218         END-IF                                                    ELUGVLSV
01219      END-IF.                                                      ELUGVLSV
01220                                                                   ELUGVLSV
       9900-CHECK-MAX-ENTRIES.                                                  
           IF GVX-NMBR-PRVDR-ENTRS > 5000                                       
              SET CIA-AB-CRITIO TO TRUE                                         
              PERFORM 9999-ABEND.                                               
                                                                                
01221  9999-ABEND.                                                      ELUGVLSV
01222      EXEC CICS ABEND                                              ELUGVLSV
01223                ABCODE (CIA-ABCODE)                                ELUGVLSV
01224      END-EXEC.                                                    ELUGVLSV
