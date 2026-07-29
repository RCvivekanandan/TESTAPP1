00001  IDENTIFICATION DIVISION.                                         09/03/03
00002 *                                                                 ELGIPGP 
00003  PROGRAM-ID.         ELGIPGP.                                        LV002
00004 *                                                                 ELGIPGP 
00005  AUTHOR.             ANNE KEFFER KING.                            ELGIPGP 
00006                                                                   ELGIPGP 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGIPGP 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELGIPGP 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGIPGP 
00010                      233 N. MICHIGAN AVE                          ELGIPGP 
00011                      CHICAGO, ILLINOIS 60601                      ELGIPGP 
00012 *                                                                 ELGIPGP 
00013  DATE-WRITTEN.       20-DEC-1990.                                 ELGIPGP 
00014 *                                                                 ELGIPGP 
00015  DATE-COMPILED.                                                   ELGIPGP 
00016 *                                                                 ELGIPGP 
00017  SECURITY.           COPYRIGHT 1986,                              ELGIPGP 
00018                      HEALTH CARE SERVICE CORPORATION              ELGIPGP 
00019 *                                                                 ELGIPGP 
00020 ******************************************************************ELGIPGP 
00021 *   ELGIPGP                                                      *ELGIPGP 
00022 *                                                                *ELGIPGP 
00023 *                        PROGRAM ABSTRACT                        *ELGIPGP 
00024 *                                                                *ELGIPGP 
00025 *   PROGRAM NAME:   E.L.S. INTERNAL TABULAR TRANSLATOR           *ELGIPGP 
00026 *                   SUBROUTINE                                   *ELGIPGP 
00027 *                                                                *ELGIPGP 
00028 *   PROGRAM I.D.:   ELGIPGP                                      *ELGIPGP 
00029 *                                                                *ELGIPGP 
00030 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE FIELDS *ELGIPGP 
00031 *              IN THE #IPGP INTERNAL TABULAR                     *ELGIPGP 
00032 *                                                                *ELGIPGP 
00033 *   RECORDS                                                      *ELGIPGP 
00034 *   ACCESSED:  #IPGP INTERNAL TABULAR                            *ELGIPGP 
00035 *                                                                *ELGIPGP 
00036 ******************************************************************ELGIPGP 
00037 *                                                                *ELGIPGP 
00038 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGIPGP 
00039 *       *-*         U P D A T E   H I S T O R Y         *-*      *ELGIPGP 
00040 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGIPGP 
00041 *                                                                *ELGIPGP 
00042 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELGIPGP 
00043 *                                                                *ELGIPGP 
00044 *  ELS 2.0   12/20/90  AKK  CLONED FROM ELGIPGN.                 *ELGIPGP 
00045 *                                                                *ELGIPGP 
00046 *  ELS 2.1   03-12-91  JPB  ADDED CODE TO MOVE C OR H TO SERVICE-*ELGIPGP 
00047 *                           CODE-SYSTEM-ID.                      *ELGIPGP 
00048 *                                                                *ELGIPGP 
00049 *  ELS 3.0   02/06/92  GEM  ACCUM DISPLAY FIX ISSR# 12010.       *ELGIPGP 
00050 *                                                                *ELGIPGP 
00051 *  ELS 3.1   04/17/95  AKK  CORRECTED DISCREPANCY D15432.        *ELGIPGP 
00052 *                           DIAGNOSIS CODES DISPLAYING           *ELGIPGP 
00053 *                           INCORRECTLY.  FIRST DIGIT WAS BEING  *ELGIPGP 
00054 *                           OVERLAID.                            *ELGIPGP 
00055 *                                                                *ELGIPGP 
00056 *  ELS 4.0   10/30/98  AKK  ADD SUPPORT FOR ACP ACCUM.           *ELGIPGP 
00057 *                                                                *ELGIPGP 
00058 *            07/18/03  AKK  REGENNED IN QWIK AS PREPROD COMPILE  *ELGIPGP 
00059 *                           FAILED WHEN ANGELO TRIED MAP FIX.    *ELGIPGP 
00060 **                                                                ELGIPGP 
00061 *            07/30/03  AKK  ADDED -ACP AS DESCRIPTION LINE NOT   *ELGIPGP 
00062 *                           DISPLAYING PROPERLY. IN SINGLE LEVEL  ELGIPGP 
00063 *                           DISPLAY AREA.                         ELGIPGP 
ED0624*                                                                *        
ED0624* BBDA-58217 06/14/24  ED   RECOMPILE FOR PEAQ COPYBOOK          *        
ED0624*                           EXPANSION:                           *        
ED0624*                                 COPYBOOK ELSACUMC              *        
ED0624*                                                                *        
00064 ******************************************************************ELGIPGP 
00065 /                                                                 ELGIPGP 
00066  ENVIRONMENT DIVISION.                                            ELGIPGP 
00067  CONFIGURATION SECTION.                                           ELGIPGP 
00068  SOURCE-COMPUTER.    IBM-3033.                                    ELGIPGP 
00069  OBJECT-COMPUTER.    IBM-3033.                                    ELGIPGP 
00070 /                                                                 ELGIPGP 
00071  DATA DIVISION.                                                   ELGIPGP 
00072  WORKING-STORAGE SECTION.                                         ELGIPGP 
00073  01  WS-BEGIN                    PIC X(24)  VALUE                 ELGIPGP 
00074      'ELGIPGP WORKING STORAGE*'.                                  ELGIPGP 
00075 /     W O R K F I E L D S   A N D   S W I T C H E S               ELGIPGP 
00076  01  WS-MISC-WORK.                                                ELGIPGP 
00077       05  WS-PROCEDURE-CODE.                                      ELGIPGP 
00078           10 WS-PROC-CODE-6     PIC X(06).                        ELGIPGP 
00079           10 WS-PROC-CODE-1     PIC X(01).                        ELGIPGP 
00080                                                                   ELGIPGP 
00081      05  WS-SUB                PIC S9(4)  COMP SYNC VALUE ZEROES. ELGIPGP 
00082      05  WS-ACM-SUB            PIC S9(4)  COMP SYNC VALUE ZEROES. ELGIPGP 
00083      05  WS-CUR-SUB            PIC S9(4)  COMP SYNC VALUE ZEROES. ELGIPGP 
00084      05  WS-NXT-SUB            PIC S9(4)  COMP SYNC VALUE ZEROES. ELGIPGP 
00085      05  WS-TAG-SUB            PIC S9(4)  COMP SYNC VALUE ZEROES. ELGIPGP 
00086      05  WS-LVL-SUB            PIC S9(4)  COMP SYNC VALUE ZEROES. ELGIPGP 
00087      05  WS-BLANK-CNT          PIC S9(4)  COMP.                   ELGIPGP 
00088      05  WS-IPGP               PIC X(5)   VALUE '#IPGP'.          ELGIPGP 
00089      05  WS-PVE                PIC X(4)   VALUE '#PVE'.           ELGIPGP 
00090      05  WS-BEN-ID.                                               ELGIPGP 
00091          10  WS-BEN-ID-5       PIC X(5)   VALUE SPACES.           ELGIPGP 
00092          10  WS-BEN-ID-1       PIC X      VALUE SPACES.           ELGIPGP 
00093      05  WS-GETMAIN-SWITCH     PIC X      VALUE 'N'.              ELGIPGP 
00094          88 GETMAIN-DONE                  VALUE 'Y'.              ELGIPGP 
00095      05  WS-EDIT-COMP          PIC X      VALUE 'N'.              ELGIPGP 
00096          88 WS-EDIT-COMPLETE              VALUE 'Y'.              ELGIPGP 
00097  01  FILLER.                                                      ELGIPGP 
00098    02  WS-ACCUMULATOR-TYPE.                                       ELGIPGP 
00099      03  FILLER   PIC X(21)  VALUE                                ELGIPGP 
00100         '#ACL    COINSURANCE  '.                                  ELGIPGP 
00101      03  FILLER   PIC X(21)  VALUE                                ELGIPGP 
00102         '#ADL    DEDUCTIBLE   '.                                  ELGIPGP 
00103      03  FILLER   PIC X(21)  VALUE                                ELGIPGP 
00104         '#ABM    MAXIMUM      '.                                  ELGIPGP 
00105      03  FILLER   PIC X(21)  VALUE                                ELGIPGP 
00106         '#AOL    OUT-OF-POCKET'.                                  ELGIPGP 
00107      03  FILLER   PIC X(21)  VALUE                                ELGIPGP 
00108         '#ACP    CO-PAY       '.                                  ELGIPGP 
00109    02  WS-ACCUMULATOR-TYPE-TABLE REDEFINES  WS-ACCUMULATOR-TYPE.  ELGIPGP 
00110      03  WS-ACCUMULATOR-TYPE-TBL  OCCURS 5 TIMES                  ELGIPGP 
00111                                   INDEXED BY ACUM-IDX.            ELGIPGP 
00112          05  WS-ACCUM-TYPE       PIC X(08).                       ELGIPGP 
00113          05  WS-ACCUM-NAME       PIC X(13).                       ELGIPGP 
00114                                                                   ELGIPGP 
00115  01  WS-IPGP-VARIABLE-AREA.                                       ELGIPGP 
00116    02  WS-IPGP-COUNT   COMP-3    PIC S9(03)  VALUE ZEROES.        ELGIPGP 
00117    02  WS-IPGP-VARIABLES                                          ELGIPGP 
00118           OCCURS  46  TIMES  DEPENDING  ON  WS-IPGP-COUNT.        ELGIPGP 
00119        05  WS-IPGP-SLOT-NBR      PIC S9(07) COMP-3.               ELGIPGP 
00120        05  WS-IPGP-LEVEL         PIC X(04).                       ELGIPGP 
00121        05  WS-IPGP-PERCENT       PIC S9(03) COMP-3.               ELGIPGP 
00122        05  WS-IPGP-STATUS        PIC X(01).                       ELGIPGP 
00123          88  WS-IPGP-PROCESSED       VALUE 'Y'.                   ELGIPGP 
00124          88  WS-IPGP-NOT-PROCESSED   VALUE 'N'.                   ELGIPGP 
00125                                                                   ELGIPGP 
00126  01  WS-LEVEL-AREA.                                               ELGIPGP 
00127    02  WS-LEVEL-COUNT  COMP-3    PIC S9(03) VALUE ZEROES.         ELGIPGP 
00128    02  WS-LEVEL-TABLE.                                            ELGIPGP 
00129      03  WS-LEVEL-TABLE-ENTRIES                                   ELGIPGP 
00130            OCCURS 46 TIMES DEPENDING ON WS-LEVEL-COUNT.           ELGIPGP 
00131        05  WS-LEVEL-TAG          PIC X(04).                       ELGIPGP 
00132        05  WS-LEVEL-PERCENT      PIC ZZ9.                         ELGIPGP 
00133        05  WS-LEVEL-EDITOR       PIC X(06).                       ELGIPGP 
00134          88  WS-LEVEL-PCT            VALUE '%     '.              ELGIPGP 
00135          88  WS-LEVEL-PCT-COMMA      VALUE '%,    '.              ELGIPGP 
00136          88  WS-LEVEL-PCT-AND        VALUE '% AND '.              ELGIPGP 
00137                                                                   ELGIPGP 
00138  01  PROGRAM-CONSTANTS.                                           ELGIPGP 
00139      05  PC-ABM                  PIC X(08)  VALUE '#ABM    '.     ELGIPGP 
00140      05  PC-ADL                  PIC X(08)  VALUE '#ADL    '.     ELGIPGP 
00141                                                                   ELGIPGP 
00142  01  WS-PROC-NOT-ON-FILE.                                         ELGIPGP 
00143      05  FILLER                  PIC X(26)  VALUE                 ELGIPGP 
00144          '(PROCEDURE IS NOT ON FILE)'.                            ELGIPGP 
00145                                                                   ELGIPGP 
00146  01  WS-PROCS-PAID-PHRASE.                                        ELGIPGP 
00147      05  FILLER                  PIC X(23)  VALUE                 ELGIPGP 
00148          'FOR PROCEDURES PAID AT '.                               ELGIPGP 
00149                                                                   ELGIPGP 
00150  01  WS-FLAG-LINE.                                                ELGIPGP 
00151      05  FILLER                  PIC X(79)  VALUE '*****'.        ELGIPGP 
00152 /                                                                 ELGIPGP 
00153      COPY ELSVLTGC.                                               ELGIPGP 
00154 /                                                                 ELGIPGP 
00155  01  WS-END                      PIC X(24)  VALUE                 ELGIPGP 
00156      '*** ELGIPGP W/S ENDS ***'.                                  ELGIPGP 
00157 /             L I N K A G E   S E C T I O N                       ELGIPGP 
00158  LINKAGE SECTION.                                                 ELGIPGP 
00159  01  DFHCOMMAREA.                                                 ELGIPGP 
00160      COPY ELSCOMMC.                                               ELGIPGP 
00161 /             C I A                                               ELGIPGP 
00162      COPY ELSCIA2C.                                               ELGIPGP 
00163 /                                                                 ELGIPGP 
00164      COPY ELSCMDSC.                                               ELGIPGP 
00165 /                                                                 ELGIPGP 
00166      COPY ELSCMIFC.                                               ELGIPGP 
00167 /                                                                 ELGIPGP 
00168      COPY ELSIOPMC.                                               ELGIPGP 
00169 /                                                                 ELGIPGP 
00170      COPY ELSKEYSC.                                               ELGIPGP 
00171 /                                                                 ELGIPGP 
00172      COPY ELSOUTPC.                                               ELGIPGP 
00173 /                                                                 ELGIPGP 
00174      COPY ELSTCWAC.                                               ELGIPGP 
00175 /                                                                 ELGIPGP 
00176      COPY ELSSRTPC.                                               ELGIPGP 
00177 /                                                                 ELGIPGP 
00178      COPY ELSACUMC.                                               ELGIPGP 
00179 /                                                                 ELGIPGP 
00180  01  GXA-TABULAR-REC-AREA.                                        ELGIPGP 
00181      COPY GCTIPGPC.                                               ELGIPGP 
00182 /    P R O V I D E R   N B R   T O   N A M E   D B   P A R M S    ELGIPGP 
00183  01  PDB-IO-AREA.                                                 ELGIPGP 
00184      COPY DBPIOPMC.                                               ELGIPGP 
00185 /    P R O C E D U R E  M A S T E R   R E C O R D   D E S C R .   ELGIPGP 
00186  01  PROCEDURE-MSTR-REC.                                          ELGIPGP 
00187      COPY PROCMSTR.                                               ELGIPGP 
00188 /    P R O C E D U R E   D I V I S I O N .                        ELGIPGP 
00189 ***************************************************************   ELGIPGP 
00190 *                                                             *   ELGIPGP 
00191 *  I N T E R N A L   T A B U L A R   D I S P L A Y   I P G P  *   ELGIPGP 
00192 *                                                             *   ELGIPGP 
00193 ***************************************************************   ELGIPGP 
00194  PROCEDURE DIVISION.                                              ELGIPGP 
00195  INTERNAL-TABULAR-DISPLAY-IPGN.                                   ELGIPGP 
00196      PERFORM INITIALIZATION-ROUTINE.                              ELGIPGP 
00197                                                                   ELGIPGP 
00198      IF ACCUM-ASCEND-DESCEND-COUNT = 1 AND                        ELGIPGP 
00199        (ACCUM-ABM OR ACCUM-ACL OR ACCUM-ADL OR ACCUM-AOL          ELGIPGP 
00200             OR ACCUM-ACP)                                         ELGIPGP 
00201           PERFORM SINGLE-LEVEL-DISPLAY                            ELGIPGP 
00202      ELSE                                                         ELGIPGP 
00203         IF ACCUM-ASCEND-DESCEND-COUNT > 1 AND                     ELGIPGP 
00204           (ACCUM-ACL OR ACCUM-AOL)                                ELGIPGP 
00205              PERFORM VARIABLE-LEVEL-DISPLAY                       ELGIPGP 
00206         ELSE                                                      ELGIPGP 
00207            EXEC CICS ABEND  ABCODE('EL99')  END-EXEC.             ELGIPGP 
00208                                                                   ELGIPGP 
00209      GOBACK.                                                      ELGIPGP 
00210                                                                   ELGIPGP 
00211 ***************************************************************   ELGIPGP 
00212 *                                                             *   ELGIPGP 
00213 *  I N I T I A L Z A T I O N   R O U T I N E                  *   ELGIPGP 
00214 *                                                             *   ELGIPGP 
00215 ***************************************************************   ELGIPGP 
00216  INITIALIZATION-ROUTINE.                                          ELGIPGP 
00217      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELGIPGP 
00218          EXEC CICS ABEND  ABCODE('EL01')  END-EXEC.               ELGIPGP 
00219                                                                   ELGIPGP 
00220      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGIPGP 
00221          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELGIPGP 
00222      IF CIA-RC-PTR-NULL                                           ELGIPGP 
00223         EXEC CICS ABEND  ABCODE('EL02')  END-EXEC.                ELGIPGP 
00224                                                                   ELGIPGP 
00225      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGIPGP 
00226      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGP 
00227          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELGIPGP 
00228      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIPGP 
00229                                                                   ELGIPGP 
00230      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGIPGP 
00231      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGP 
00232          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELGIPGP 
00233      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIPGP 
00234                                                                   ELGIPGP 
00235      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGIPGP 
00236      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGP 
00237          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELGIPGP 
00238      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIPGP 
00239                                                                   ELGIPGP 
00240      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGIPGP 
00241      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGP 
00242          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELGIPGP 
00243      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIPGP 
00244                                                                   ELGIPGP 
00245      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGIPGP 
00246      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGP 
00247          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELGIPGP 
00248      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIPGP 
00249                                                                   ELGIPGP 
00250      INITIALIZE CMF-CODES-MANUAL-INTERFACE                        ELGIPGP 
00251                 TCAR-FROM-AREA.                                   ELGIPGP 
00252                                                                   ELGIPGP 
00253      SET  CIA-DBPIOPM-DDN TO TRUE.                                ELGIPGP 
00254      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGP 
00255          ADDRESS OF PDB-IO-AREA.                                  ELGIPGP 
00256                                                                   ELGIPGP 
00257      IF CIA-RC-PTR-NULL                                           ELGIPGP 
00258          PERFORM GETMAIN-IO-PARM-AREA.                            ELGIPGP 
00259      IF GETMAIN-DONE                                              ELGIPGP 
00260         SET  CIA-DBPIOPM-DDN TO TRUE                              ELGIPGP 
00261         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGIPGP 
00262            ADDRESS OF PDB-IO-AREA.                                ELGIPGP 
00263      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIPGP 
00264                                                                   ELGIPGP 
00265      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGIPGP 
00266      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGP 
00267         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                   ELGIPGP 
00268      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIPGP 
00269                                                                   ELGIPGP 
00270      SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR.       ELGIPGP 
00271                                                                   ELGIPGP 
00272  CHECK-IF-CIA-RC-PTR-NULL.                                        ELGIPGP 
00273      IF CIA-RC-PTR-NULL                                           ELGIPGP 
00274         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGIPGP 
00275         EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.            ELGIPGP 
00276                                                                   ELGIPGP 
00277                                                                   ELGIPGP 
00278 ***************************************************************   ELGIPGP 
00279 *                                                             *   ELGIPGP 
00280 *  S I N G L E   L E V E L   D I S P L A Y                    *   ELGIPGP 
00281 *                                                             *   ELGIPGP 
00282 ***************************************************************   ELGIPGP 
00283  SINGLE-LEVEL-DISPLAY.                                            ELGIPGP 
00284      PERFORM READ-INTERNAL-TABULAR-RECORD.                        ELGIPGP 
00285      PERFORM LIST-TABULAR-HEADING.                                ELGIPGP 
00286      PERFORM LIST-TABULAR-CONTENTS                                ELGIPGP 
00287        VARYING GXA-INDEX FROM 1 BY 1                              ELGIPGP 
00288          UNTIL GXA-INDEX = GXA-ENTRY-COUNT OR                     ELGIPGP 
00289          GXA-PROCEDURE-ARGUMENT (GXA-INDEX) = HIGH-VALUES.        ELGIPGP 
00290      PERFORM INSERT-BLANK-LINE.                                   ELGIPGP 
00291      INITIALIZE TCAR-FROM-SUB.                                    ELGIPGP 
00292                                                                   ELGIPGP 
00293 ***************************************************************   ELGIPGP 
00294 *                                                             *   ELGIPGP 
00295 *  V A R I A B L E   L E V E L   D I S P L A Y                *   ELGIPGP 
00296 *                                                             *   ELGIPGP 
00297 ***************************************************************   ELGIPGP 
00298  VARIABLE-LEVEL-DISPLAY.                                          ELGIPGP 
00299      MOVE 1 TO WS-CUR-SUB.                                        ELGIPGP 
00300      SET VLT-INDEX TO 1.                                          ELGIPGP 
00301      PERFORM EXTRACT-PERCENT-N-SLOT-NBR                           ELGIPGP 
00302        VARYING ASC-DES-INDEX FROM 1 BY 1                          ELGIPGP 
00303          UNTIL ASC-DES-INDEX > ACCUM-ASCEND-DESCEND-COUNT.        ELGIPGP 
00304      MOVE WS-IPGP-COUNT TO WS-CUR-SUB.                            ELGIPGP 
00305      ADD 1 TO WS-CUR-SUB.                                         ELGIPGP 
00306      MOVE HIGH-VALUES TO WS-IPGP-STATUS (WS-CUR-SUB).             ELGIPGP 
00307      PERFORM IPGP-DISPLAY-PROCESSING                              ELGIPGP 
00308        VARYING WS-CUR-SUB FROM 1 BY 1                             ELGIPGP 
00309          UNTIL WS-IPGP-STATUS (WS-CUR-SUB) = HIGH-VALUES.         ELGIPGP 
00310                                                                   ELGIPGP 
00311 ***************************************************************   ELGIPGP 
00312 *  E X T R A C T   P E R C E N T   &   S L O T   N B R        *   ELGIPGP 
00313 ***************************************************************   ELGIPGP 
00314  EXTRACT-PERCENT-N-SLOT-NBR.                                      ELGIPGP 
00315      IF ACCUM-IPGP-SLOT-NBR (ASC-DES-INDEX) > ZERO                ELGIPGP 
00316         MOVE ACCUM-PERCENT-LEVEL (ASC-DES-INDEX)                  ELGIPGP 
00317           TO WS-IPGP-PERCENT (WS-CUR-SUB)                         ELGIPGP 
00318         MOVE ACCUM-IPGP-SLOT-NBR (ASC-DES-INDEX)                  ELGIPGP 
00319           TO WS-IPGP-SLOT-NBR (WS-CUR-SUB)                        ELGIPGP 
00320         MOVE VLT-VARIABLE-LEVEL-TAG (VLT-INDEX)                   ELGIPGP 
00321           TO WS-IPGP-LEVEL (WS-CUR-SUB)                           ELGIPGP 
00322         SET WS-IPGP-NOT-PROCESSED (WS-CUR-SUB) TO TRUE            ELGIPGP 
00323         ADD 1 TO WS-IPGP-COUNT.                                   ELGIPGP 
00324      ADD 1 TO WS-CUR-SUB.                                         ELGIPGP 
00325      SET VLT-INDEX UP BY 1.                                       ELGIPGP 
00326                                                                   ELGIPGP 
00327 ***************************************************************   ELGIPGP 
00328 *  I P G P   D I S P L A Y   P R O C E S S I N G              *   ELGIPGP 
00329 ***************************************************************   ELGIPGP 
00330  IPGP-DISPLAY-PROCESSING.                                         ELGIPGP 
00331      IF WS-IPGP-NOT-PROCESSED (WS-CUR-SUB)                        ELGIPGP 
00332         MOVE 1 TO WS-LVL-SUB                                      ELGIPGP 
00333         MOVE ZEROES TO WS-LEVEL-COUNT                             ELGIPGP 
00334         PERFORM PUT-EQUAL-SLOT-NBR-TO-LVL-TBL                     ELGIPGP 
00335           VARYING WS-NXT-SUB FROM WS-CUR-SUB BY 1                 ELGIPGP 
00336             UNTIL WS-IPGP-STATUS (WS-NXT-SUB) = HIGH-VALUES       ELGIPGP 
00337         SET ASC-DES-INDEX TO WS-CUR-SUB                           ELGIPGP 
00338         PERFORM READ-INTERNAL-TABULAR-RECORD                      ELGIPGP 
00339         PERFORM EDIT-CONNECT-PERCENT-LEVELS                       ELGIPGP 
00340           VARYING WS-LVL-SUB FROM 1 BY 1                          ELGIPGP 
00341             UNTIL WS-EDIT-COMPLETE                                ELGIPGP 
00342         PERFORM LIST-TABULAR-HEADING                              ELGIPGP 
00343         PERFORM LIST-TABULAR-CONTENTS                             ELGIPGP 
00344          VARYING GXA-INDEX FROM 1 BY 1                            ELGIPGP 
00345            UNTIL GXA-INDEX =   GXA-ENTRY-COUNT OR                 ELGIPGP 
00346            GXA-PROCEDURE-ARGUMENT (GXA-INDEX) = HIGH-VALUES       ELGIPGP 
00347         PERFORM INSERT-BLANK-LINE.                                ELGIPGP 
00348                                                                   ELGIPGP 
00349 ***************************************************************   ELGIPGP 
00350 *  P U T   E Q U A L   S L O T   N B R   T O   L V L   T B L  *   ELGIPGP 
00351 ***************************************************************   ELGIPGP 
00352  PUT-EQUAL-SLOT-NBR-TO-LVL-TBL.                                   ELGIPGP 
00353      IF WS-IPGP-NOT-PROCESSED (WS-NXT-SUB) AND                    ELGIPGP 
00354         WS-IPGP-SLOT-NBR (WS-CUR-SUB) =                           ELGIPGP 
00355                           WS-IPGP-SLOT-NBR (WS-NXT-SUB)           ELGIPGP 
00356            MOVE WS-IPGP-PERCENT (WS-NXT-SUB) TO                   ELGIPGP 
00357                              WS-LEVEL-PERCENT (WS-LVL-SUB)        ELGIPGP 
00358            MOVE WS-IPGP-LEVEL   (WS-NXT-SUB) TO                   ELGIPGP 
00359                              WS-LEVEL-TAG (WS-LVL-SUB)            ELGIPGP 
00360            SET WS-IPGP-PROCESSED (WS-NXT-SUB) TO TRUE             ELGIPGP 
00361            ADD 1 TO WS-LVL-SUB                                    ELGIPGP 
00362            ADD 1 TO WS-LEVEL-COUNT.                               ELGIPGP 
00363                                                                   ELGIPGP 
00364 ***************************************************************   ELGIPGP 
00365 *  R E A D   I N T E R N A L   T A B U L A R   R E C O R D    *   ELGIPGP 
00366 ***************************************************************   ELGIPGP 
00367  READ-INTERNAL-TABULAR-RECORD.                                    ELGIPGP 
00368      SET CIA-GCTABULR-DDN TO TRUE.                                ELGIPGP 
00369      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGP 
00370          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGIPGP 
00371      IF CIA-RC-PTR-NULL                                           ELGIPGP 
00372         PERFORM GETMAIN-IO-PARM-AREA                              ELGIPGP 
00373         SET CIA-GCTABULR-DDN TO TRUE                              ELGIPGP 
00374         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGIPGP 
00375               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELGIPGP 
00376      MOVE '#IPGP ' TO KWA-PROVISION-ID.                           ELGIPGP 
00377      MOVE ACCUM-IPGP-SLOT-NBR (ASC-DES-INDEX)                     ELGIPGP 
00378                    TO KWA-PROVISION-SLOT-NO.                      ELGIPGP 
00379      SET IOP-RD TO TRUE.                                          ELGIPGP 
00380      SET IOP-FCQ-NONE TO TRUE.                                    ELGIPGP 
00381      SET IOP-KVQ-EQ TO TRUE.                                      ELGIPGP 
00382      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGIPGP 
00383      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGIPGP 
00384      SET IOP-REC-PTR TO NULL.                                     ELGIPGP 
00385      SET IOP-STG-MODE-MOVE TO TRUE.                               ELGIPGP 
00386      EXEC CICS LINK  PROGRAM('ELUIOPGM')                          ELGIPGP 
00387                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELGIPGP 
00388      IF IOP-RC-OK                                                 ELGIPGP 
00389         SET ADDRESS OF GXA-TABULAR-REC-AREA TO IOP-REC-PTR        ELGIPGP 
00390      ELSE                                                         ELGIPGP 
00391         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELGIPGP 
00392         EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.            ELGIPGP 
00393                                                                   ELGIPGP 
00394 ***************************************************************   ELGIPGP 
00395 *  G E T M A I N   I O   P A R M   A R E A                    *   ELGIPGP 
00396 ***************************************************************   ELGIPGP 
00397  GETMAIN-IO-PARM-AREA.                                            ELGIPGP 
00398      SET CIA-STG-GETMAIN TO TRUE.                                 ELGIPGP 
00399      EXEC CICS LINK  PROGRAM('ELUSTGMG')                          ELGIPGP 
00400                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELGIPGP 
00401      SET GETMAIN-DONE TO TRUE.                                    ELGIPGP 
00402                                                                   ELGIPGP 
00403 ***************************************************************   ELGIPGP 
00404 *   E D I T   C O N N E C T   P E R C E N T   L E V E L S     *   ELGIPGP 
00405 ***************************************************************   ELGIPGP 
00406  EDIT-CONNECT-PERCENT-LEVELS.                                     ELGIPGP 
00407      IF WS-LVL-SUB = 1 AND                                        ELGIPGP 
00408         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIPGP 
00409         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIPGP 
00410         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIPGP 
00411                                                                   ELGIPGP 
00412      IF WS-LVL-SUB = 2 AND                                        ELGIPGP 
00413         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIPGP 
00414         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIPGP 
00415         SUBTRACT 1 FROM WS-LVL-SUB                                ELGIPGP 
00416         SET WS-LEVEL-PCT-AND (WS-LVL-SUB) TO TRUE                 ELGIPGP 
00417         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIPGP 
00418                                                                   ELGIPGP 
00419      IF WS-LVL-SUB > 2 AND                                        ELGIPGP 
00420         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIPGP 
00421         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIPGP 
00422         SUBTRACT 1 FROM WS-LVL-SUB                                ELGIPGP 
00423         SET WS-LEVEL-PCT-AND (WS-LVL-SUB) TO TRUE                 ELGIPGP 
00424         SET WS-EDIT-COMPLETE     TO TRUE                          ELGIPGP 
00425      ELSE                                                         ELGIPGP 
00426      IF WS-LVL-SUB > 2 AND                                        ELGIPGP 
00427         WS-LVL-SUB NOT EQUAL WS-LEVEL-COUNT                       ELGIPGP 
00428            SET WS-LEVEL-PCT-COMMA (WS-LVL-SUB) TO TRUE.           ELGIPGP 
00429                                                                   ELGIPGP 
00430 ***************************************************************   ELGIPGP 
00431 *  L I S T   T A B U L A R   H E A D I N G                    *   ELGIPGP 
00432 ***************************************************************   ELGIPGP 
00433  LIST-TABULAR-HEADING.                                            ELGIPGP 
00434      INITIALIZE TCAR-FROM-AREA.                                   ELGIPGP 
00435      MOVE WS-IPGP TO CMF-RECORD-PREFIX.                           ELGIPGP 
00436      MOVE 'INCLUDE-EXCLUDE-IND' TO                                ELGIPGP 
00437          CMF-ELEMENT-SYSTEM-NAME.                                 ELGIPGP 
00438      MOVE GXA-INCLUDE-EXCLUDE-IND TO CMF-CODE-VALUE.              ELGIPGP 
00439      EXEC CICS LINK  PROGRAM('ELUCMIF')                           ELGIPGP 
00440                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELGIPGP 
00441      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGIPGP 
00442      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGP 
00443          ADDRESS OF CMF-DESCR.                                    ELGIPGP 
00444      EVALUATE TRUE                                                ELGIPGP 
00445         WHEN ACCUM-ACL                                            ELGIPGP 
00446              MOVE 1 TO WS-ACM-SUB                                 ELGIPGP 
00447         WHEN ACCUM-ADL                                            ELGIPGP 
00448              MOVE 2 TO WS-ACM-SUB                                 ELGIPGP 
00449         WHEN ACCUM-ABM                                            ELGIPGP 
00450              MOVE 3 TO WS-ACM-SUB                                 ELGIPGP 
00451         WHEN ACCUM-AOL                                            ELGIPGP 
00452              MOVE 4 TO WS-ACM-SUB                                 ELGIPGP 
00453         WHEN ACCUM-ACP                                            ELGIPGP 
00454              MOVE 5 TO WS-ACM-SUB                                 ELGIPGP 
00455      END-EVALUATE.                                                ELGIPGP 
00456      IF (ACCUM-ADL OR ACCUM-ABM OR ACCUM-ACP)                     ELGIPGP 
00457          PERFORM STRING-FIRST-LINE-FOR-MAXIMUMX                   ELGIPGP 
00458      ELSE                                                         ELGIPGP 
00459         IF (ACCUM-ACL OR ACCUM-AOL)                               ELGIPGP 
00460            PERFORM STRING-FIRST-LINE-FOR-COINSURA.                ELGIPGP 
00461      PERFORM DO-TEXT-COMPRESSION.                                 ELGIPGP 
00462      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELGIPGP 
00463      MOVE +04 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGIPGP 
00464      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                          ELGIPGP 
00465                  TCAR-OUTPUT-FIELD-2-LEN                          ELGIPGP 
00466                  TCAR-OUTPUT-FIELD-3-LEN                          ELGIPGP 
00467                  TCAR-OUTPUT-FIELD-4-LEN.                         ELGIPGP 
00468      PERFORM DO-TEXT-UNSTRING.                                    ELGIPGP 
00469      PERFORM MOVE-ASTERICKS-TO-FIRST-OUTPUT.                      ELGIPGP 
00470      PERFORM MOVE-COMPRESSED-PHRASE                               ELGIPGP 
00471          VARYING TCAR-FROM-SUB FROM 1 BY 1 UNTIL                  ELGIPGP 
00472              TCAR-FROM-SUB GREATER THAN                           ELGIPGP 
00473             TCAR-OUTPUT-FIELDS-USED.                              ELGIPGP 
00474      PERFORM CALL-OUTPUT.                                         ELGIPGP 
00475                                                                   ELGIPGP 
00476 ***************************************************************   ELGIPGP 
00477 *  S T R I N G   F I R S T   L I N E   M A X I M U M          *   ELGIPGP 
00478 ***************************************************************   ELGIPGP 
00479  STRING-FIRST-LINE-FOR-MAXIMUMX.                                  ELGIPGP 
00480      STRING 'THIS '                       DELIMITED BY SIZE       ELGIPGP 
00481             WS-ACCUM-NAME (WS-ACM-SUB)    DELIMITED BY ' '        ELGIPGP 
00482             ' ACCUMULATOR '               DELIMITED BY SIZE       ELGIPGP 
00483             CMF-DESCR-LINE(1)             DELIMITED BY '  '       ELGIPGP 
00484             ' THE FOLLOWING PROCEDURES:'  DELIMITED BY SIZE       ELGIPGP 
00485             INTO TCAR-FROM-AREA.                                  ELGIPGP 
00486                                                                   ELGIPGP 
00487 ***************************************************************   ELGIPGP 
00488 *  S T R I N G   F I R S T   L I N E   C O I N S U R A N C E  *   ELGIPGP 
00489 ***************************************************************   ELGIPGP 
00490  STRING-FIRST-LINE-FOR-COINSURA.                                  ELGIPGP 
00491      IF ACCUM-ASCEND-DESCEND-COUNT = 1                            ELGIPGP 
00492         MOVE 1 TO WS-LEVEL-COUNT                                  ELGIPGP 
00493         MOVE SPACES TO WS-LEVEL-TAG (1)                           ELGIPGP 
00494         MOVE ACCUM-PERCENT-LEVEL (1)  TO WS-LEVEL-PERCENT (1)     ELGIPGP 
00495         SET WS-LEVEL-PCT (1) TO TRUE.                             ELGIPGP 
00496                                                                   ELGIPGP 
00497      STRING 'THE '                           DELIMITED BY SIZE    ELGIPGP 
00498         WS-ACCUM-NAME (WS-ACM-SUB)           DELIMITED BY ' '     ELGIPGP 
00499         ' ACCUMULATOR '                      DELIMITED BY SIZE    ELGIPGP 
00500         WS-PROCS-PAID-PHRASE                 DELIMITED BY SIZE    ELGIPGP 
00501         WS-LEVEL-TABLE                       DELIMITED BY SIZE    ELGIPGP 
00502         CMF-DESCR-LINE(1)                    DELIMITED BY '  '    ELGIPGP 
00503         ' THE FOLLOWING PROCEDURES:'         DELIMITED BY SIZE    ELGIPGP 
00504             INTO TCAR-FROM-AREA.                                  ELGIPGP 
00505                                                                   ELGIPGP 
00506 ***************************************************************   ELGIPGP 
00507 *  L I S T   T A B U L A R   C O N T E N T S                  *   ELGIPGP 
00508 ***************************************************************   ELGIPGP 
00509  LIST-TABULAR-CONTENTS.                                           ELGIPGP 
00510      SET  CIA-DBPIOPM-DDN TO TRUE.                                ELGIPGP 
00511       MOVE SPACES TO WS-PROCEDURE-CODE.                           ELGIPGP 
00512       MOVE GXA-PROCEDURE-ARGUMENT (GXA-INDEX)  TO                 ELGIPGP 
00513           WS-PROCEDURE-CODE.                                      ELGIPGP 
00514       MOVE WS-PROC-CODE-6 TO PDB-I-SERVICE-CODE.                  ELGIPGP 
00515       MOVE ZERO TO WS-BLANK-CNT.                                  ELGIPGP 
00516 *     INSPECT GXA-PROCEDURE-ARGUMENT (GXA-INDEX) TALLYING         ELGIPGP 
00517       INSPECT WS-PROC-CODE-6                     TALLYING         ELGIPGP 
00518         WS-BLANK-CNT                                              ELGIPGP 
00519           FOR ALL SPACES.                                         ELGIPGP 
00520      IF WS-BLANK-CNT = 1                                          ELGIPGP 
00521         MOVE 'C' TO PDB-I-SERVICE-CODE-SYSTEM-ID                  ELGIPGP 
00522      ELSE                                                         ELGIPGP 
00523         MOVE 'H' TO PDB-I-SERVICE-CODE-SYSTEM-ID.                 ELGIPGP 
00524      MOVE '02' TO PDB-I-VERSION.                                  ELGIPGP 
00525      MOVE 'PRCDR03 '  TO  PDB-I-REQUEST-TYPE.                     ELGIPGP 
00526      SET PDB-I-READ-DIRECT TO TRUE.                               ELGIPGP 
00527      SET PDB-I-RQN-PROCEDURE TO TRUE.                             ELGIPGP 
00528      EXEC CICS  LINK  PROGRAM('DBPIOC')                           ELGIPGP 
00529                       COMMAREA(PDB-IO-AREA)  END-EXEC.            ELGIPGP 
00530      IF NOT PDB-O-RC-SUCCESSFUL                                   ELGIPGP 
00531         PERFORM PROCEDURE-FILE-PROBLEM                            ELGIPGP 
00532      ELSE                                                         ELGIPGP 
00533         SET ADDRESS OF PROCEDURE-MSTR-REC                         ELGIPGP 
00534             TO  ADDRESS OF PDB-O-RECORD-AREA                      ELGIPGP 
00535         IF PM-DESCRIPTION1 NOT =  SPACE AND                       ELGIPGP 
00536              PM-DESCRIPTION2 NOT =  SPACE                         ELGIPGP 
00537            PERFORM STRING-PROC-NO-AND-NAMES                       ELGIPGP 
00538         ELSE                                                      ELGIPGP 
00539            PERFORM STRING-PROC-NO-AND-NAME.                       ELGIPGP 
00540      PERFORM CALL-OUTPUT.                                         ELGIPGP 
00541                                                                   ELGIPGP 
00542 ***************************************************************   ELGIPGP 
00543 *  S T R I N G   P R O C   N O   A N D   N A M E S            *   ELGIPGP 
00544 ***************************************************************   ELGIPGP 
00545  STRING-PROC-NO-AND-NAMES.                                        ELGIPGP 
00546      MOVE SPACES TO TCAR-FROM-LINE(1),                            ELGIPGP 
00547                     TCAR-FROM-LINE(2).                            ELGIPGP 
00548      STRING GXA-PROCEDURE-ARGUMENT(GXA-INDEX),                    ELGIPGP 
00549             '  ',  DELIMITED BY SIZE,                             ELGIPGP 
00550             PM-DESCRIPTION1,  DELIMITED BY '  ',                  ELGIPGP 
00551             ', ',  DELIMITED BY SIZE,                             ELGIPGP 
00552             PM-DESCRIPTION2,  DELIMITED BY '  ',                  ELGIPGP 
00553             INTO  TCAR-FROM-AREA.                                 ELGIPGP 
00554      PERFORM DO-TEXT-COMPRESSION.                                 ELGIPGP 
00555      MOVE +74  TO  TCAR-OUTPUT-FIELD-1-LEN.                       ELGIPGP 
00556      MOVE +63  TO  TCAR-OUTPUT-FIELD-2-LEN.                       ELGIPGP 
00557      MOVE +2  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELGIPGP 
00558      PERFORM DO-TEXT-UNSTRING.                                    ELGIPGP 
00559      PERFORM MOVE-COMPRESSED-PHRASE                               ELGIPGP 
00560         VARYING TCAR-FROM-SUB FROM 1 BY 1 UNTIL TCAR-FROM-SUB     ELGIPGP 
00561            GREATER THAN TCAR-OUTPUT-FIELDS-USED.                  ELGIPGP 
00562                                                                   ELGIPGP 
00563 ***************************************************************   ELGIPGP 
00564 *  S T R I N G   P R O C   A N D   N A M E                    *   ELGIPGP 
00565 ***************************************************************   ELGIPGP 
00566  STRING-PROC-NO-AND-NAME.                                         ELGIPGP 
00567      MOVE SPACES  TO  COF-DTL-LINE(1).                            ELGIPGP 
00568      MOVE 1  TO  COF-NBR-DTL-LINES.                               ELGIPGP 
00569      STRING GXA-PROCEDURE-ARGUMENT(GXA-INDEX),                    ELGIPGP 
00570             '  ',  DELIMITED BY SIZE,                             ELGIPGP 
00571             PM-DESCRIPTION1,   DELIMITED BY '  ',                 ELGIPGP 
00572             ' ',  DELIMITED BY SIZE,                              ELGIPGP 
00573             PM-DESCRIPTION2,   DELIMITED BY '  ',                 ELGIPGP 
00574             INTO  COF-DTL-LINE(1).                                ELGIPGP 
00575                                                                   ELGIPGP 
00576 ***************************************************************   ELGIPGP 
00577 *  I N S E R T   B L A N K   L I N E                          *   ELGIPGP 
00578 ***************************************************************   ELGIPGP 
00579  INSERT-BLANK-LINE.                                               ELGIPGP 
00580      ADD  1       TO  COF-NBR-DTL-LINES.                          ELGIPGP 
00581      MOVE SPACES  TO  COF-DTL-LINE (COF-NBR-DTL-LINES).           ELGIPGP 
00582      PERFORM CALL-OUTPUT.                                         ELGIPGP 
00583                                                                   ELGIPGP 
00584 ***************************************************************   ELGIPGP 
00585 *  M O V E   C O M P R E S S E D   P H R A S E                *   ELGIPGP 
00586 ***************************************************************   ELGIPGP 
00587  MOVE-COMPRESSED-PHRASE.                                          ELGIPGP 
00588      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGIPGP 
00589      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB)                           ELGIPGP 
00590         TO COF-DTL-LINE (COF-NBR-DTL-LINES).                      ELGIPGP 
00591                                                                   ELGIPGP 
00592 ***************************************************************   ELGIPGP 
00593 *  M O V E   A S T E R I C K S   F I R S T   L I N E          *   ELGIPGP 
00594 ***************************************************************   ELGIPGP 
00595  MOVE-ASTERICKS-TO-FIRST-OUTPUT.                                  ELGIPGP 
00596      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGIPGP 
00597      MOVE WS-FLAG-LINE TO COF-DTL-LINE (COF-NBR-DTL-LINES).       ELGIPGP 
00598                                                                   ELGIPGP 
00599 ***************************************************************   ELGIPGP 
00600 *  D O   T E X T   C O M P R E S S I O N                      *   ELGIPGP 
00601 ***************************************************************   ELGIPGP 
00602  DO-TEXT-COMPRESSION.                                             ELGIPGP 
00603      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGIPGP 
00604                                                                   ELGIPGP 
00605 ***************************************************************   ELGIPGP 
00606 *  D O   T E X T   U N S T R I N G                            *   ELGIPGP 
00607 ***************************************************************   ELGIPGP 
00608  DO-TEXT-UNSTRING.                                                ELGIPGP 
00609      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGIPGP 
00610                                                                   ELGIPGP 
00611 ***************************************************************   ELGIPGP 
00612 *  C A L L   O U T P U T                                      *   ELGIPGP 
00613 ***************************************************************   ELGIPGP 
00614  CALL-OUTPUT.                                                     ELGIPGP 
00615      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELGIPGP 
00616                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGIPGP 
00617                                                                   ELGIPGP 
00618 ***************************************************************   ELGIPGP 
00619 *  P R O C E D U R E   F I L E   P R O B L E M                *   ELGIPGP 
00620 ***************************************************************   ELGIPGP 
00621  PROCEDURE-FILE-PROBLEM.                                          ELGIPGP 
00622      MOVE SPACES  TO  COF-DTL-LINE(1).                            ELGIPGP 
00623      ADD  1       TO  COF-NBR-DTL-LINES.                          ELGIPGP 
00624      STRING GXA-PROCEDURE-ARGUMENT(GXA-INDEX),                    ELGIPGP 
00625             '  ',  DELIMITED BY SIZE,                             ELGIPGP 
00626             WS-PROC-NOT-ON-FILE,  DELIMITED BY ' ',               ELGIPGP 
00627             ' ',  DELIMITED BY SIZE,                              ELGIPGP 
00628             INTO  COF-DTL-LINE(1).                                ELGIPGP 
