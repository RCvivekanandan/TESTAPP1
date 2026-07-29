00001  IDENTIFICATION DIVISION.                                         09/04/03
00002                                                                   ELSGVLM2
00003  PROGRAM-ID.         ELSGVLM2.                                       LV002
00004                                                                   ELSGVLM2
00005  AUTHOR.             JOHN BEIRNE                                  ELSGVLM2
00006                                                                   ELSGVLM2
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSGVLM2
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSGVLM2
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSGVLM2
00010                      233 N. MICHIGAN AVE                          ELSGVLM2
00011                      CHICAGO, ILLINOIS 60601                      ELSGVLM2
00012                                                                   ELSGVLM2
00013  DATE-WRITTEN.       09-14-1993.                                  ELSGVLM2
00014                                                                   ELSGVLM2
00015  DATE-COMPILED.                                                   ELSGVLM2
00016                                                                   ELSGVLM2
00017  SECURITY.           COPYRIGHT 1993,                              ELSGVLM2
00018                      HEALTH CARE SERVICE CORPORATION              ELSGVLM2
00019      SKIP3                                                        ELSGVLM2
00020  ENVIRONMENT DIVISION.                                            ELSGVLM2
00021                                                                   ELSGVLM2
00022  CONFIGURATION SECTION.                                           ELSGVLM2
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELSGVLM2
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELSGVLM2
00025  TITLE 'ELS GENERIC VARIABLE LEVEL PROCESSOR              '.      ELSGVLM2
00026 ****************************************************************  ELSGVLM2
00027 *                                                              *  ELSGVLM2
00028 *    PROGRAM:    ELSGVLM1                                      *  ELSGVLM2
00029 *    DATE:       14-SEP-1993                                   *  ELSGVLM2
00030 *    AUTHOR:     JOHN BEIRNE                                   *  ELSGVLM2
00031 *    FUNCTION:                                                 *  ELSGVLM2
00032 *                                                              *  ELSGVLM2
00033 ****************************************************************  ELSGVLM2
00034 *                                                              *  ELSGVLM2
00035 *                   MAINTENANCE HISTORY                        *  ELSGVLM2
00036 *                                                              *  ELSGVLM2
00037 *  MOD     DATE     BY  DRPT              ACTION               *  ELSGVLM2
00038 * ----- ----------- --- ---- --------------------------------- *  ELSGVLM2
00039 * 01.00 22-DEC-1993 AKK      CREATED-AS A STUB FOR USE TO      *  ELSGVLM2
00040 *                            THAT THIS TOPIC NOT AVAILABLE.    *  ELSGVLM2
00041 *                                                                 ELSGVLM2
00042 * 02.00 18-AUG-1998 AKK      ADDED THIS COMMENT THAT NO        *  ELSGVLM2
00043 *                            CHANGES MADE TO THIS PROGRAM FOR  *  ELSGVLM2
00044 *                            MILLENIUM, RE-COMPILE ONLY.       *  ELSGVLM2
00045 *                                                                 ELSGVLM2
00046 *       13-AUG-2003 AKK      GEN TO TEST ORDER OF COMPILES     *  ELSGVLM2
00047 ****************************************************************  ELSGVLM2
00048      EJECT                                                        ELSGVLM2
00049  DATA DIVISION.                                                   ELSGVLM2
00050  WORKING-STORAGE SECTION.                                         ELSGVLM2
00051  01  WS-BEGIN.                                                    ELSGVLM2
00052                                                                   ELSGVLM2
00053      05  WS-SELECTED-KEYS-COUNTER    PIC  S9(04) COMP.            ELSGVLM2
00054                                                                   ELSGVLM2
00055      05  WS-INPUT-FIELDS-FOUND       PIC  S9(04) COMP.            ELSGVLM2
00056                                                                   ELSGVLM2
00057      05  WS-NUM-HEADINGS             PIC  S9(04) COMP.            ELSGVLM2
00058                                                                   ELSGVLM2
00059      05  WS-SAVE-KTG-IDX             USAGE IS INDEX.              ELSGVLM2
00060                                                                   ELSGVLM2
00061      05  WS-MAX-GCG-IDX              USAGE IS INDEX.              ELSGVLM2
00062                                                                   ELSGVLM2
00063      05  INPUTL                      PIC  S9(04) COMP.            ELSGVLM2
00064                                                                   ELSGVLM2
00065      05  WS-GVL-SLOT-NMBRS.                                       ELSGVLM2
00066          10  WS-GVLF-ID              PIC  X(06).                  ELSGVLM2
00067          10  WS-GVLF-SLOT-NO         PIC  S9(07) COMP-3.          ELSGVLM2
00068                                                                   ELSGVLM2
00069          10  WS-GVLG-ID              PIC  X(06).                  ELSGVLM2
00070          10  WS-GVLG-SLOT-NO         PIC  S9(07) COMP-3.          ELSGVLM2
00071                                                                   ELSGVLM2
00072          10  WS-GVLH-ID              PIC  X(06).                  ELSGVLM2
00073          10  WS-GVLH-SLOT-NO         PIC  S9(07) COMP-3.          ELSGVLM2
00074                                                                   ELSGVLM2
00075          10  WS-GVLP-ID              PIC  X(06).                  ELSGVLM2
00076          10  WS-GVLP-SLOT-NO         PIC  S9(07) COMP-3.          ELSGVLM2
00077                                                                   ELSGVLM2
00078          10  WS-GVLQ-ID              PIC  X(06).                  ELSGVLM2
00079          10  WS-GVLQ-SLOT-NO         PIC  S9(07) COMP-3.          ELSGVLM2
00080                                                                   ELSGVLM2
00081          10  WS-GVLR-ID              PIC  X(06).                  ELSGVLM2
00082          10  WS-GVLR-SLOT-NO         PIC  S9(07) COMP-3.          ELSGVLM2
00083                                                                   ELSGVLM2
00084      05  WS-GVL-FOUND-SWITCH         PIC X.                       ELSGVLM2
00085          88 NO-VALID-GVL-FOUND              VALUE 'N'.            ELSGVLM2
00086          88 VALID-GVL-FOUND                 VALUE 'Y'.            ELSGVLM2
00087                                                                   ELSGVLM2
00088      05  WS-PROV-NUM-ENTERED-SWITCH  PIC X.                       ELSGVLM2
00089          88 PROV-NUM-NOT-ENTERED            VALUE 'N'.            ELSGVLM2
00090          88 PROV-NUM-ENTERED                VALUE 'Y'.            ELSGVLM2
00091                                                                   ELSGVLM2
00092      05  WS-EMBEDDED-BLANK-SWITCH    PIC X.                       ELSGVLM2
00093          88 EMBEDDED-BLANK-FOUND            VALUE 'Y'.            ELSGVLM2
00094          88 NO-EMBEDDED-BLANK               VALUE 'N'.            ELSGVLM2
00095                                                                   ELSGVLM2
00096      05  WS-ERROR-SWITCH             PIC X.                       ELSGVLM2
00097          88  PROVIDER-NUMBER-OK             VALUE '0'.            ELSGVLM2
00098          88  ERROR-FOUND                    VALUE '1'.            ELSGVLM2
00099                                                                   ELSGVLM2
00100      05  WS-DATE-OUT                 PIC 99/99/99.                ELSGVLM2
00101                                                                   ELSGVLM2
00102      05  WS-FROM-DATE                PIC 99/99/99.                ELSGVLM2
00103                                                                   ELSGVLM2
00104      05  WS-TO-DATE                  PIC 99/99/99.                ELSGVLM2
00105                                                                   ELSGVLM2
00106      05  WS-TIME                     PIC 9(07) VALUE ZEROS.       ELSGVLM2
00107      05  WS-TIME-RED REDEFINES WS-TIME.                           ELSGVLM2
00108          10  WS-1                    PIC 9.                       ELSGVLM2
00109          10  WS-HH                   PIC 99.                      ELSGVLM2
00110          10  WS-MM                   PIC 99.                      ELSGVLM2
00111          10  WS-SS                   PIC 99.                      ELSGVLM2
00112                                                                   ELSGVLM2
00113      05  WS-TIME-OUT.                                             ELSGVLM2
00114          10  WS-HH-OUT               PIC 99.                      ELSGVLM2
00115          10  FILLER                  PIC X VALUE ':'.             ELSGVLM2
00116          10  WS-MM-OUT               PIC 99.                      ELSGVLM2
00117          10  FILLER                  PIC X VALUE ':'.             ELSGVLM2
00118          10  WS-SS-OUT               PIC 99.                      ELSGVLM2
00119                                                                   ELSGVLM2
00120      05  WS-NUMB-CHECK.                                           ELSGVLM2
00121          10  WS-NUMB-CHAR            OCCURS 10 TIMES              ELSGVLM2
00122                                      INDEXED BY NUMB-IDX          ELSGVLM2
00123                                      PIC X.                       ELSGVLM2
00124                                                                   ELSGVLM2
00125      05  WS-INVALID-PROV-NUM         PIC X(50) VALUE              ELSGVLM2
00126          'SPACES ARE NOT ALLOWED CHECK THE NUMBER AND RETRY.'.    ELSGVLM2
00127      05  FILLER                      PIC X(49).                   ELSGVLM2
00128                                                                   ELSGVLM2
00129      05  WS-SPC-MNU-TITLE            PIC X(31) VALUE              ELSGVLM2
00130              'SPECIAL PROVIDER CONSIDERATIONS'.                   ELSGVLM2
00131                                                                   ELSGVLM2
00132      05  WS-NO-GVL-AVAILABLE-MSG.                                 ELSGVLM2
00133          10  WS-NO-GVL-AVAILABLE-MSG-1   PIC X(79) VALUE          ELSGVLM2
00134              'THIS SELECTION IS NOT CURRENTLY AVAILABLE.  PLEASE UELSGVLM2
00135 -            'SE THE LIST OPTION.  USE'.                          ELSGVLM2
00136          10  WS-NO-GVL-AVAILABLE-MSG-2   PIC X(79) VALUE          ELSGVLM2
00137              '<PF3> OR <PF9> TO RETURN.'.                         ELSGVLM2
00138                                                                   ELSGVLM2
00139      05  WS-NO-INPUT-MSG             PIC X(49)     VALUE          ELSGVLM2
00140          'PLEASE MAKE A SELECTION OR PRESS PF3 TO RETURN.'.       ELSGVLM2
00141      05  FILLER                          PIC X(30).               ELSGVLM2
00142                                                                   ELSGVLM2
00143      05  WS-ONE-SELECTION-ONLY-MSG   PIC X(40)     VALUE          ELSGVLM2
00144          'ONLY ONE SELECTION IS ALLOWED AT A TIME.'.              ELSGVLM2
00145      05  FILLER                          PIC X(39).               ELSGVLM2
00146 /                                                                 ELSGVLM2
00147 ****************************************************************  ELSGVLM2
00148 *  M A P   C O B O L   S C R E E N   D S E C T                 *  ELSGVLM2
00149 ****************************************************************  ELSGVLM2
00150  01  WS-IO-MAP-AREA-04           PIC  X(24) VALUE                 ELSGVLM2
00151          '*WS MAP I/O AREA EL04 *'.                               ELSGVLM2
00152      COPY EL04SETC.                                               ELSGVLM2
00153 /                                                                 ELSGVLM2
00154  01  WS-IO-MAP-AREA-00           PIC  X(24) VALUE                 ELSGVLM2
00155          '*WS MAP I/O AREA EL00 *'.                               ELSGVLM2
00156      COPY EL00SETC.                                               ELSGVLM2
00157 /                                                                 ELSGVLM2
00158  01  HGADATES-PARM-LIST.                                          ELSGVLM2
00159      COPY HGCDAT01.                                               ELSGVLM2
00160 /                                                                 ELSGVLM2
00161      COPY DFHBMSCA.                                               ELSGVLM2
00162 /                                                                 ELSGVLM2
00163  LINKAGE SECTION.                                                 ELSGVLM2
00164  01  DFHCOMMAREA.                                                 ELSGVLM2
00165  COPY ELSCOMMC.                                                   ELSGVLM2
00166      EJECT                                                        ELSGVLM2
00167  COPY ELSIOPMC.                                                   ELSGVLM2
00168      EJECT                                                        ELSGVLM2
00169  COPY ELSKTBGC.                                                   ELSGVLM2
00170      EJECT                                                        ELSGVLM2
00171  COPY ELSKEYSC.                                                   ELSGVLM2
00172      EJECT                                                        ELSGVLM2
00173  COPY ELSSLNRC.                                                   ELSGVLM2
00174      EJECT                                                        ELSGVLM2
00175  COPY ELSCIA2C.                                                   ELSGVLM2
00176      EJECT                                                        ELSGVLM2
00177  COPY ELSMENUC.                                                   ELSGVLM2
00178      EJECT                                                        ELSGVLM2
00179  COPY ELSMHDGC.                                                   ELSGVLM2
00180      EJECT                                                        ELSGVLM2
00181  COPY ELSMOPTC.                                                   ELSGVLM2
00182      EJECT                                                        ELSGVLM2
00183  COPY ELSSSCBC.                                                   ELSGVLM2
00184      EJECT                                                        ELSGVLM2
00185      EJECT                                                        ELSGVLM2
00186  PROCEDURE DIVISION.                                              ELSGVLM2
00187 ************************************************************      ELSGVLM2
00188 *                                                          *      ELSGVLM2
00189 *        ELSGVLM1 MAINLINE                                 *      ELSGVLM2
00190 *                                                          *      ELSGVLM2
00191 ************************************************************      ELSGVLM2
00192  0000-ELSGVLM1-MAINLINE.                                          ELSGVLM2
00193      PERFORM 0100-INITIALIZE.                                     ELSGVLM2
00194      PERFORM 1000-PROCESS-GVLX-FILE.                              ELSGVLM2
00195      GOBACK.                                                      ELSGVLM2
00196                                                                   ELSGVLM2
00197 ************************************************************      ELSGVLM2
00198 *                                                          *      ELSGVLM2
00199 *        INITIALIZATION                                    *      ELSGVLM2
00200 *                                                          *      ELSGVLM2
00201 ************************************************************      ELSGVLM2
00202  0100-INITIALIZE.                                                 ELSGVLM2
00203      PERFORM 0200-ESTABLISH-ENVIRONMENT.                          ELSGVLM2
00204      PERFORM 0300-EST-ADDRS-TO-SSB.                               ELSGVLM2
00205      PERFORM 0400-EST-ADDRS-TO-KEY-WRK.                           ELSGVLM2
00206      PERFORM 0500-EST-ADDRS-TO-GRP-KEY-TAB.                       ELSGVLM2
00207      PERFORM 0600-EST-ADDRS-TO-SLN.                               ELSGVLM2
00208      PERFORM 0700-EST-ADDRS-TO-MSO.                               ELSGVLM2
00209      PERFORM 0800-OBTAIN-DATE.                                    ELSGVLM2
00210      PERFORM 0900-OBTAIN-TIME.                                    ELSGVLM2
00211                                                                   ELSGVLM2
00212 ************************************************************      ELSGVLM2
00213 *                                                              *  ELSGVLM2
00214 *             ESTABLISH ENVIRONMENT                            *  ELSGVLM2
00215 *                                                              *  ELSGVLM2
00216 ****************************************************************  ELSGVLM2
00217  0200-ESTABLISH-ENVIRONMENT.                                      ELSGVLM2
00218      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELSGVLM2
00219         EXEC CICS ABEND                                           ELSGVLM2
00220                   ABCODE ('EL01')                                 ELSGVLM2
00221         END-EXEC                                                  ELSGVLM2
00222         ELSE IF ECA-CIA-PTR = NULL                                ELSGVLM2
00223                 EXEC CICS ABEND                                   ELSGVLM2
00224                           ABCODE ('EL02')                         ELSGVLM2
00225                 END-EXEC                                          ELSGVLM2
00226              ELSE                                                 ELSGVLM2
00227              CALL 'ELUINISM' USING DFHCOMMAREA                    ELSGVLM2
00228                         ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA. ELSGVLM2
00229                                                                   ELSGVLM2
00230 ****************************************************************  ELSGVLM2
00231 *                                                          *      ELSGVLM2
00232 *        ESTABLISH ADDRESSABILITY TO SELECTOR STATUS BLOCK *      ELSGVLM2
00233 *                                                          *      ELSGVLM2
00234 ************************************************************      ELSGVLM2
00235  0300-EST-ADDRS-TO-SSB.                                           ELSGVLM2
00236      SET CIA-ELSSSCB-DDN TO TRUE                                  ELSGVLM2
00237      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM2
00238                            ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK ELSGVLM2
00239      END-CALL                                                     ELSGVLM2
00240      IF CIA-RC-PTR-NULL                                           ELSGVLM2
00241         SET CIA-AB-PARM-MISSING TO TRUE                           ELSGVLM2
00242         EXEC CICS ABEND                                           ELSGVLM2
00243                   ABCODE (CIA-ABCODE)                             ELSGVLM2
00244         END-EXEC.                                                 ELSGVLM2
00245                                                                   ELSGVLM2
00246 ************************************************************      ELSGVLM2
00247 *                                                          *      ELSGVLM2
00248 *        ESTABLISH ADDRESSABILITY TO KEY WORK AREA         *      ELSGVLM2
00249 *                                                          *      ELSGVLM2
00250 ************************************************************      ELSGVLM2
00251  0400-EST-ADDRS-TO-KEY-WRK.                                       ELSGVLM2
00252      SET CIA-ELSKEYS-DDN TO TRUE                                  ELSGVLM2
00253      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM2
00254                            ADDRESS OF KWA-FILE-KEY-WORK-AREA      ELSGVLM2
00255      END-CALL                                                     ELSGVLM2
00256      IF CIA-RC-PTR-NULL                                           ELSGVLM2
00257         SET CIA-ELSKEYS-DDN TO TRUE                               ELSGVLM2
00258         SET CIA-STG-GETMAIN TO TRUE                               ELSGVLM2
00259         PERFORM 4300-CALL-STORAGE-MANAGER                         ELSGVLM2
00260         SET CIA-ELSKEYS-DDN TO TRUE                               ELSGVLM2
00261         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELSGVLM2
00262                               ADDRESS OF KWA-FILE-KEY-WORK-AREA   ELSGVLM2
00263         END-CALL.                                                 ELSGVLM2
00264                                                                   ELSGVLM2
00265 ************************************************************      ELSGVLM2
00266 *                                                          *      ELSGVLM2
00267 *    ESTABLISH ADDRESSABILITY TO GROUP SPECIFIC KEY TABLE  *      ELSGVLM2
00268 *                                                          *      ELSGVLM2
00269 ************************************************************      ELSGVLM2
00270  0500-EST-ADDRS-TO-GRP-KEY-TAB.                                   ELSGVLM2
00271      SET CIA-ELSKTBG-DDN TO TRUE                                  ELSGVLM2
00272      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM2
00273                            ADDRESS OF KTG-GCGRPSPC-KEY-TABLE      ELSGVLM2
00274      END-CALL                                                     ELSGVLM2
00275      IF CIA-RC-PTR-NULL                                           ELSGVLM2
00276         SET CIA-ELSKTBG-DDN TO TRUE                               ELSGVLM2
00277         SET CIA-STG-RETRIEVE TO TRUE                              ELSGVLM2
00278         PERFORM 4300-CALL-STORAGE-MANAGER                         ELSGVLM2
00279         SET CIA-ELSKTBG-DDN TO TRUE                               ELSGVLM2
00280         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELSGVLM2
00281                               ADDRESS OF KTG-GCGRPSPC-KEY-TABLE   ELSGVLM2
00282         END-CALL.                                                 ELSGVLM2
00283                                                                   ELSGVLM2
00284 ************************************************************      ELSGVLM2
00285 *                                                          *      ELSGVLM2
00286 *    ESTABLISH ADDRESSABILITY TO SELECTION SLOT NUMBERS    *      ELSGVLM2
00287 *                                                          *      ELSGVLM2
00288 ************************************************************      ELSGVLM2
00289  0600-EST-ADDRS-TO-SLN.                                           ELSGVLM2
00290      SET CIA-ELSSLNR-DDN TO TRUE                                  ELSGVLM2
00291      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM2
00292                            ADDRESS OF SLN-SELECTION-SLOT-NUMBERS  ELSGVLM2
00293      END-CALL                                                     ELSGVLM2
00294      IF CIA-RC-PTR-NULL                                           ELSGVLM2
00295         SET CIA-ELSSLNR-DDN TO TRUE                               ELSGVLM2
00296         SET CIA-STG-GETMAIN  TO TRUE                              ELSGVLM2
00297         PERFORM 4300-CALL-STORAGE-MANAGER                         ELSGVLM2
00298         SET CIA-ELSSLNR-DDN TO TRUE                               ELSGVLM2
00299         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELSGVLM2
00300                            ADDRESS OF SLN-SELECTION-SLOT-NUMBERS  ELSGVLM2
00301         END-CALL.                                                 ELSGVLM2
00302                                                                   ELSGVLM2
00303 ************************************************************      ELSGVLM2
00304 *                                                          *      ELSGVLM2
00305 *    ESTABLISH ADDRESSABILITY TO MENU OPTIONS AREA         *      ELSGVLM2
00306 *                                                          *      ELSGVLM2
00307 ************************************************************      ELSGVLM2
00308  0700-EST-ADDRS-TO-MSO.                                           ELSGVLM2
00309      SET CIA-ELSMOPT-DDN TO TRUE                                  ELSGVLM2
00310      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +      ELSGVLM2
00311              (LENGTH OF MSO-MENU-OPT * 1).                        ELSGVLM2
00312      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM2
00313                            ADDRESS OF MSO-MENU-SELECTION-VALUES   ELSGVLM2
00314      END-CALL                                                     ELSGVLM2
00315      IF CIA-RC-PTR-NULL                                           ELSGVLM2
00316         SET CIA-ELSMOPT-DDN TO TRUE                               ELSGVLM2
00317         COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +   ELSGVLM2
00318              (LENGTH OF MSO-MENU-OPT * 1)                         ELSGVLM2
00319         SET CIA-STG-GETMAIN  TO TRUE                              ELSGVLM2
00320         PERFORM 4300-CALL-STORAGE-MANAGER                         ELSGVLM2
00321         SET CIA-ELSMOPT-DDN TO TRUE                               ELSGVLM2
00322         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELSGVLM2
00323                            ADDRESS OF MSO-MENU-SELECTION-VALUES   ELSGVLM2
00324         END-CALL.                                                 ELSGVLM2
00325                                                                   ELSGVLM2
00326 ************************************************************      ELSGVLM2
00327 *                                                          *      ELSGVLM2
00328 *        OBTAIN DATE                                       *      ELSGVLM2
00329 *                                                          *      ELSGVLM2
00330 ************************************************************      ELSGVLM2
00331  0800-OBTAIN-DATE.                                                ELSGVLM2
00332      MOVE EIBDATE           TO  HGADATE-JULIAN1.                  ELSGVLM2
00333      PERFORM 0850-CNVRT-DTE-FROM-JUL-TO-GR.                       ELSGVLM2
00334      MOVE HGADATE-DATE2     TO  WS-DATE-OUT.                      ELSGVLM2
00335      EJECT                                                        ELSGVLM2
00336                                                                   ELSGVLM2
00337 ************************************************************      ELSGVLM2
00338 *                                                          *      ELSGVLM2
00339 *        CONVERT DATE FROM JULIAN TO GREG                  *      ELSGVLM2
00340 *                                                          *      ELSGVLM2
00341 ************************************************************      ELSGVLM2
00342  0850-CNVRT-DTE-FROM-JUL-TO-GR.                                   ELSGVLM2
00343      MOVE 'CNV'  TO  HGADATE-FUNC.                                ELSGVLM2
00344      MOVE 'J'    TO  HGADATE-FORM1.                               ELSGVLM2
00345      MOVE 'M'    TO  HGADATE-FORM2.                               ELSGVLM2
00346      MOVE ZEROS  TO  HGADATE-RETURN, HGADATE-AMOUNT,              ELSGVLM2
00347          HGADATE-DATE2.                                           ELSGVLM2
00348      EXEC CICS LINK PROGRAM ('HGADATES')                          ELSGVLM2
00349                     COMMAREA (HGADATES-PARM-LIST)                 ELSGVLM2
00350                     END-EXEC.                                     ELSGVLM2
00351                                                                   ELSGVLM2
00352 ************************************************************      ELSGVLM2
00353 *                                                          *      ELSGVLM2
00354 *        OBTAIN CURRENT TIME                               *      ELSGVLM2
00355 *                                                          *      ELSGVLM2
00356 ************************************************************      ELSGVLM2
00357  0900-OBTAIN-TIME.                                                ELSGVLM2
00358      MOVE EIBTIME      TO  WS-TIME.                               ELSGVLM2
00359      MOVE WS-HH        TO  WS-HH-OUT.                             ELSGVLM2
00360      MOVE WS-MM        TO  WS-MM-OUT.                             ELSGVLM2
00361      MOVE WS-SS        TO  WS-SS-OUT.                             ELSGVLM2
00362                                                                   ELSGVLM2
00363 ************************************************************      ELSGVLM2
00364 *                                                          *      ELSGVLM2
00365 *        PROCESS GVLX FILE                                 *      ELSGVLM2
00366 *                                                          *      ELSGVLM2
00367 ************************************************************      ELSGVLM2
00368  1000-PROCESS-GVLX-FILE.                                          ELSGVLM2
00369      PERFORM 9999-SEND-SIMPLE-NO-GVL.                             ELSGVLM2
00370                                                                   ELSGVLM2
00371  9999-SEND-SIMPLE-NO-GVL.                                         ELSGVLM2
00372      MOVE WS-SPC-MNU-TITLE TO SSB-MNU-TITLE.                      ELSGVLM2
00373      PERFORM 2800-DELETE-MENU-FILE.                               ELSGVLM2
00374      PERFORM 2900-PREP-AREA-FOR-MESSAGE.                          ELSGVLM2
00375      PERFORM 3100-ACQR-HDNG-STG-AREA.                             ELSGVLM2
00376      PERFORM 3200-GET-DESCR-LINE-AREA.                            ELSGVLM2
00377      MOVE 2 TO MHD-NBR-HDG-LINES.                                 ELSGVLM2
00378      MOVE SPACES TO MHD-HDG-LINES.                                ELSGVLM2
00379      SET MHD-IDX TO 1.                                            ELSGVLM2
00380      MOVE WS-NO-GVL-AVAILABLE-MSG-1 TO                            ELSGVLM2
00381           MHD-HDG-LINE (MHD-IDX).                                 ELSGVLM2
00382      SET SSB-START-MENU (SSB-SELECTOR-STATE)                      ELSGVLM2
00383           TO TRUE.                                                ELSGVLM2
00384      SET MHD-IDX UP BY 1.                                         ELSGVLM2
00385      MOVE WS-NO-GVL-AVAILABLE-MSG-2 TO                            ELSGVLM2
00386           MHD-HDG-LINE (MHD-IDX).                                 ELSGVLM2
00387      SET SSB-START-MENU (SSB-SELECTOR-STATE)                      ELSGVLM2
00388           TO TRUE.                                                ELSGVLM2
00389                                                                   ELSGVLM2
00390 ************************************************************      ELSGVLM2
00391 *                                                          *      ELSGVLM2
00392 *        DELETE MENU FILE                                  *      ELSGVLM2
00393 *                                                          *      ELSGVLM2
00394 ************************************************************      ELSGVLM2
00395  2800-DELETE-MENU-FILE.                                           ELSGVLM2
00396      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGVLM2
00397      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM2
00398                            ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.ELSGVLM2
00399      IF CIA-RC-PTR-NULL                                           ELSGVLM2
00400         PERFORM 3000-ACQUIRE-MENU-STG-AREA.                       ELSGVLM2
00401      SET CIA-ELSMENU-DDN                                          ELSGVLM2
00402          IOP-DEL                                                  ELSGVLM2
00403          IOP-FCQ-NONE                                             ELSGVLM2
00404          IOP-KVQ-NONE TO TRUE.                                    ELSGVLM2
00405      CALL 'ELUIOPGM' USING DFHEIBLK                               ELSGVLM2
00406                            DFHCOMMAREA.                           ELSGVLM2
00407                                                                   ELSGVLM2
00408 ************************************************************      ELSGVLM2
00409 *                                                          *      ELSGVLM2
00410 *        PREPARE AREA FOR MESSAGE                          *      ELSGVLM2
00411 *                                                          *      ELSGVLM2
00412 ************************************************************      ELSGVLM2
00413  2900-PREP-AREA-FOR-MESSAGE.                                      ELSGVLM2
00414      INITIALIZE SSB-MNU-CHOICE (1).                               ELSGVLM2
00415      MOVE 0              TO MSO-NBR-MENU-OPTS.                    ELSGVLM2
00416      MOVE 1              TO MSO-MIN-CHOICES                       ELSGVLM2
00417                             MSO-MAX-CHOICES.                      ELSGVLM2
00418      MOVE LENGTH OF WS-NO-GVL-AVAILABLE-MSG                       ELSGVLM2
00419                          TO MSO-OPT-LEN.                          ELSGVLM2
00420      SET MSO-OPT-TYP-AN  TO TRUE.                                 ELSGVLM2
00421      SET MSO-IDX         TO 1.                                    ELSGVLM2
00422      SET IOP-ADD         TO TRUE.                                 ELSGVLM2
00423      SET IOP-FCQ-NONE    TO TRUE.                                 ELSGVLM2
00424      SET IOP-KVQ-NONE    TO TRUE.                                 ELSGVLM2
00425      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGVLM2
00426                                                                   ELSGVLM2
00427 ************************************************************      ELSGVLM2
00428 *                                                          *      ELSGVLM2
00429 *        ACQUIRE MENU STORAGE AREA                         *      ELSGVLM2
00430 *                                                          *      ELSGVLM2
00431 ************************************************************      ELSGVLM2
00432  3000-ACQUIRE-MENU-STG-AREA.                                      ELSGVLM2
00433      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGVLM2
00434      SET CIA-STG-GETMAIN TO TRUE.                                 ELSGVLM2
00435      PERFORM 4300-CALL-STORAGE-MANAGER.                           ELSGVLM2
00436      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGVLM2
00437      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM2
00438                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.      ELSGVLM2
00439                                                                   ELSGVLM2
00440 ************************************************************      ELSGVLM2
00441 *                                                          *      ELSGVLM2
00442 *        ACQUIRE HEADING STORAGE AREA                      *      ELSGVLM2
00443 *                                                          *      ELSGVLM2
00444 ************************************************************      ELSGVLM2
00445  3100-ACQR-HDNG-STG-AREA.                                         ELSGVLM2
00446      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSGVLM2
00447      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM2
00448                            ADDRESS OF MHD-MENU-HEADINGS.          ELSGVLM2
00449      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES +         ELSGVLM2
00450             (LENGTH OF MHD-HDG-LINE * CIA-MVO).                   ELSGVLM2
00451      SET CIA-STG-GETMAIN TO TRUE.                                 ELSGVLM2
00452      PERFORM 4300-CALL-STORAGE-MANAGER.                           ELSGVLM2
00453      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSGVLM2
00454      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSGVLM2
00455                            ADDRESS OF MHD-MENU-HEADINGS.          ELSGVLM2
00456                                                                   ELSGVLM2
00457 ************************************************************      ELSGVLM2
00458 *                                                          *      ELSGVLM2
00459 *        GET DESCRIPTION LINE AREA                         *      ELSGVLM2
00460 *                                                          *      ELSGVLM2
00461 ************************************************************      ELSGVLM2
00462  3200-GET-DESCR-LINE-AREA.                                        ELSGVLM2
00463      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSGVLM2
00464      SET CIA-STG-GETMAIN TO TRUE.                                 ELSGVLM2
00465      SET IOP-GETMAIN-REC TO TRUE.                                 ELSGVLM2
00466      COMPUTE IOP-REC-LEN = LENGTH OF MSD-NBR-DESCR-LINES +        ELSGVLM2
00467             (LENGTH OF MSD-DESCR-LINE * 1).                       ELSGVLM2
00468      PERFORM 4300-CALL-STORAGE-MANAGER.                           ELSGVLM2
00469      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS                    ELSGVLM2
00470       TO IOP-REC-PTR.                                             ELSGVLM2
00471                                                                   ELSGVLM2
00472                                                                   ELSGVLM2
00473  4300-CALL-STORAGE-MANAGER.                                       ELSGVLM2
00474      CALL 'ELUSTGMG' USING DFHEIBLK                               ELSGVLM2
00475                            DFHCOMMAREA                            ELSGVLM2
00476      END-CALL.                                                    ELSGVLM2
