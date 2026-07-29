00001  IDENTIFICATION DIVISION.                                         09/03/03
00002 *                                                                 ELGIPGN 
00003  PROGRAM-ID.         ELGIPGN.                                        LV002
00004 *                                                                 ELGIPGN 
00005  AUTHOR.             ALIDA JATICH OF T. M. FLOYD, INC.            ELGIPGN 
00006 *                                                                 ELGIPGN 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGIPGN 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELGIPGN 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGIPGN 
00010                      233 N. MICHIGAN AVE                          ELGIPGN 
00011                      CHICAGO, ILLINOIS 60601                      ELGIPGN 
00012 *                                                                 ELGIPGN 
00013  DATE-WRITTEN.       31-OCT-1986.                                 ELGIPGN 
00014 *                                                                 ELGIPGN 
00015  DATE-COMPILED.                                                   ELGIPGN 
00016 *                                                                 ELGIPGN 
00017  SECURITY.           COPYRIGHT 1986,                              ELGIPGN 
00018                      HEALTH CARE SERVICE CORPORATION              ELGIPGN 
00019 *                                                                 ELGIPGN 
00020 ******************************************************************ELGIPGN 
00021 *   ELGIPGN                                                      *ELGIPGN 
00022 *                                                                *ELGIPGN 
00023 *                        PROGRAM ABSTRACT                        *ELGIPGN 
00024 *                                                                *ELGIPGN 
00025 *   PROGRAM NAME:   E.L.S. INTERNAL TABULAR TRANSLATOR           *ELGIPGN 
00026 *                   SUBROUTINE                                   *ELGIPGN 
00027 *                                                                *ELGIPGN 
00028 *   PROGRAM I.D.:   ELGIPGN                                      *ELGIPGN 
00029 *                                                                *ELGIPGN 
00030 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE FIELDS *ELGIPGN 
00031 *              IN THE #IPGN INTERNAL TABULAR                     *ELGIPGN 
00032 *                                                                *ELGIPGN 
00033 *   RECORDS                                                      *ELGIPGN 
00034 *   ACCESSED:  #IPGN INTERNAL TABULAR                            *ELGIPGN 
00035 *                                                                *ELGIPGN 
00036 ******************************************************************ELGIPGN 
00037 *                                                                *ELGIPGN 
00038 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGIPGN 
00039 *       *-*         U P D A T E   H I S T O R Y         *-*      *ELGIPGN 
00040 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGIPGN 
00041 *                                                                *ELGIPGN 
00042 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELGIPGN 
00043 *                                                                *ELGIPGN 
00044 *  ELS 2.0   10/31/86  AMJ  ORIGINAL MODULE WRITTEN.             *ELGIPGN 
00045 *                                                                *ELGIPGN 
00046 *             02/04/87 LET  CHANGED VERBAGE 'COVERAGE' TO        *ELGIPGN 
00047 *                           'THIS ACCUMULATOR'.                  *ELGIPGN 
00048 *            03/11/87  NAC  RESTRUCTURE OUTPUT SENTENCE TO       *ELGIPGN 
00049 *                           INCLUDE ACCUMULATOR TYPE.            *ELGIPGN 
00050 *            07/15/87  NAC  CORRECT LOGIC OF LINE COUNT TO       *ELGIPGN 
00051 *                           OUTPUT.                              *ELGIPGN 
00052 *            07/21/87  DES  ADDED PROVIDER NUMBER CONVERSION TO  *ELGIPGN 
00053 *                           PROVIDER NAME AND ADDRESS.           *ELGIPGN 
00054 *                                                                *ELGIPGN 
00055 *  01.01     11/09/87  REB  CHANGED VERBAGE OF FIRST LINE OF     *ELGIPGN 
00056 *                           OUTPUT, WILL INSERT PERCENT LEVEL    *ELGIPGN 
00057 *                           PASSED IN THE SRP BLOCK.             *ELGIPGN 
00058 *                                                                *ELGIPGN 
00059 *  01.02     11/12/87  REB  CHANGED CODE TO ALLOW PERCENT LEVEL  *ELGIPGN 
00060 *                           TO PRINT ON FIRST LINE.              *ELGIPGN 
00061 *                                                                *ELGIPGN 
00062 *  01.03     11/18/87  REB  DISPLAYING A DIFFERENT OUTPUT LINE   *ELGIPGN 
00063 *                           FOR ABM AND ADL.                     *ELGIPGN 
00064 *                                                                *ELGIPGN 
00065 *  01.04     11/23/87  REB  DISPLAY ASTERISKS ON FIRST LINE TO   *ELGIPGN 
00066 *                           DISTINGUISH INFO FROM DIFFERENT TABS.*ELGIPGN 
00067 *                           PER AUGGIE.                          *ELGIPGN 
00068 *                                                                *ELGIPGN 
00069 *  02.00     03/31/89  NAC  DESTRUCT CONVERSION USING STRUCTURES *ELGIPGN 
00070 *                           VER: 3.5.                            *ELGIPGN 
00071 *                                                                *ELGIPGN 
00072 *  03.00     04/05/89  GEM  STORAGE MANAGEMENT ENHANCEMENTS      *ELGIPGN 
00073 *                                                                *ELGIPGN 
00074 *  04.00     08/29/89  AKK  A CIA-AREA-LEN INSERTED IN EXEC CICS *ELGIPGN 
00075 *                                                                *ELGIPGN 
00076 *  04.00     01/23/92  GEM  ACCUM DISPLAY FIX ISSR# 12010.       *ELGIPGN 
00077 *                                                                *ELGIPGN 
00078 *  05.00     10/30/98  AKK  ADD SUPPORT FOR ACP TABULAR.         *ELGIPGN 
00079 *                                                                *ELGIPGN 
00080 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGIPGN 
00081 *                                                                *ELGIPGN 
ED0624* BBDA-58217 06/14/24  ED   RECOMPILE FOR PEAQ COPYBOOK          *        
ED0624*                           EXPANSION:                           *        
ED0624*                                 COPYBOOK ELSACUMC              *        
ED0624*                                                                *        
00082 ******************************************************************ELGIPGN 
00083 /                                                                 ELGIPGN 
00084  ENVIRONMENT DIVISION.                                            ELGIPGN 
00085  CONFIGURATION SECTION.                                           ELGIPGN 
00086  SOURCE-COMPUTER.    IBM-3033.                                    ELGIPGN 
00087  OBJECT-COMPUTER.    IBM-3033.                                    ELGIPGN 
00088 /                                                                 ELGIPGN 
00089  DATA DIVISION.                                                   ELGIPGN 
00090  WORKING-STORAGE SECTION.                                         ELGIPGN 
00091  01  WS-BEGIN                    PIC X(24)  VALUE                 ELGIPGN 
00092      'ELGIPGN WORKING STORAGE*'.                                  ELGIPGN 
00093 /     W O R K F I E L D S   A N D   S W I T C H E S               ELGIPGN 
00094  01  WS-MISC-WORK.                                                ELGIPGN 
00095      05  WS-SUB                PIC S9(4)  COMP SYNC VALUE ZEROES. ELGIPGN 
00096      05  WS-ACM-SUB            PIC S9(4)  COMP SYNC VALUE ZEROES. ELGIPGN 
00097      05  WS-CUR-SUB            PIC S9(4)  COMP SYNC VALUE ZEROES. ELGIPGN 
00098      05  WS-NXT-SUB            PIC S9(4)  COMP SYNC VALUE ZEROES. ELGIPGN 
00099      05  WS-TAG-SUB            PIC S9(4)  COMP SYNC VALUE ZEROES. ELGIPGN 
00100      05  WS-LVL-SUB            PIC S9(4)  COMP SYNC VALUE ZEROES. ELGIPGN 
00101      05  WS-IPGN               PIC X(5)   VALUE '#IPGN'.          ELGIPGN 
00102      05  WS-PVE                PIC X(4)   VALUE '#PVE'.           ELGIPGN 
00103      05  WS-BEN-ID.                                               ELGIPGN 
00104          10  WS-BEN-ID-5       PIC X(5)   VALUE SPACES.           ELGIPGN 
00105          10  WS-BEN-ID-1       PIC X      VALUE SPACES.           ELGIPGN 
00106      05  WS-GETMAIN-SWITCH     PIC X      VALUE 'N'.              ELGIPGN 
00107          88 GETMAIN-DONE                    VALUE 'Y'.            ELGIPGN 
00108      05  WS-EDIT-COMP          PIC X      VALUE 'N'.              ELGIPGN 
00109          88 WS-EDIT-COMPLETE                VALUE 'Y'.            ELGIPGN 
00110                                                                   ELGIPGN 
00111  01  FILLER.                                                      ELGIPGN 
00112    02  WS-ACCUMULATOR-TYPE.                                       ELGIPGN 
00113      03  FILLER   PIC X(21)  VALUE                                ELGIPGN 
00114         '#ACL    COINSURANCE  '.                                  ELGIPGN 
00115      03  FILLER   PIC X(21)  VALUE                                ELGIPGN 
00116         '#ADL    DEDUCTIBLE   '.                                  ELGIPGN 
00117      03  FILLER   PIC X(21)  VALUE                                ELGIPGN 
00118         '#ABM    MAXIMUM      '.                                  ELGIPGN 
00119      03  FILLER   PIC X(21)  VALUE                                ELGIPGN 
00120         '#AOL    OUT-OF-POCKET'.                                  ELGIPGN 
00121      03  FILLER   PIC X(21)  VALUE                                ELGIPGN 
00122         '#ACP    CO-PAY       '.                                  ELGIPGN 
00123    02  WS-ACCUMULATOR-TYPE-TABLE REDEFINES  WS-ACCUMULATOR-TYPE.  ELGIPGN 
00124      03  WS-ACCUMULATOR-TYPE-TBL  OCCURS 5 TIMES                  ELGIPGN 
00125                                   INDEXED BY ACUM-IDX.            ELGIPGN 
00126          05  WS-ACCUM-TYPE        PIC X(08).                      ELGIPGN 
00127          05  WS-ACCUM-NAME        PIC X(13).                      ELGIPGN 
00128                                                                   ELGIPGN 
00129  01  WS-IPGN-VARIABLE-AREA.                                       ELGIPGN 
00130    02  WS-IPGN-COUNT     COMP-3       PIC S9(03)  VALUE ZEROES.   ELGIPGN 
00131    02  WS-IPGN-VARIABLES                                          ELGIPGN 
00132            OCCURS 46 TIMES DEPENDING  ON  WS-IPGN-COUNT.          ELGIPGN 
00133        05  WS-IPGN-SLOT-NBR           PIC S9(07) COMP-3.          ELGIPGN 
00134        05  WS-IPGN-LEVEL              PIC  X(04).                 ELGIPGN 
00135        05  WS-IPGN-PERCENT            PIC S9(03) COMP-3.          ELGIPGN 
00136        05  WS-IPGN-STATUS             PIC X(01).                  ELGIPGN 
00137          88  WS-IPGN-PROCESSED            VALUE 'Y'.              ELGIPGN 
00138          88  WS-IPGN-NOT-PROCESSED        VALUE 'N'.              ELGIPGN 
00139                                                                   ELGIPGN 
00140  01  WS-LEVEL-AREA.                                               ELGIPGN 
00141    02  WS-LEVEL-COUNT    COMP-3    PIC S9(03) VALUE ZEROES.       ELGIPGN 
00142    02  WS-LEVEL-TABLE.                                            ELGIPGN 
00143      03  WS-LEVEL-TABLE-ENTRIES                                   ELGIPGN 
00144            OCCURS 46 TIMES DEPENDING ON WS-LEVEL-COUNT.           ELGIPGN 
00145        05  WS-LEVEL-TAG            PIC X(04).                     ELGIPGN 
00146        05  WS-LEVEL-PERCENT        PIC ZZ9.                       ELGIPGN 
00147        05  WS-LEVEL-EDITOR         PIC X(06).                     ELGIPGN 
00148          88  WS-LEVEL-PCT           VALUE '%     '.               ELGIPGN 
00149          88  WS-LEVEL-PCT-COMMA     VALUE '%,    '.               ELGIPGN 
00150          88  WS-LEVEL-PCT-AND       VALUE '% AND '.               ELGIPGN 
00151                                                                   ELGIPGN 
00152  01  PROGRAM-CONSTANTS.                                           ELGIPGN 
00153      05  PC-ABM                  PIC X(08)  VALUE '#ABM    '.     ELGIPGN 
00154      05  PC-ADL                  PIC X(08)  VALUE '#ADL    '.     ELGIPGN 
00155                                                                   ELGIPGN 
00156  01  WS-SERVICES-PAID-PHRASE.                                     ELGIPGN 
00157      05  FILLER                  PIC X(21)  VALUE                 ELGIPGN 
00158          'FOR SERVICES PAID AT '.                                 ELGIPGN 
00159                                                                   ELGIPGN 
00160  01  WS-FLAG-LINE.                                                ELGIPGN 
00161      05  FILLER                  PIC X(79)  VALUE '*****'.        ELGIPGN 
00162 /                                                                 ELGIPGN 
00163      COPY ELSVLTGC.                                               ELGIPGN 
00164                                                                   ELGIPGN 
00165  01  WS-END                      PIC X(24)  VALUE                 ELGIPGN 
00166      '*** ELGIPGN W/S ENDS ***'.                                  ELGIPGN 
00167 /             L I N K A G E   S E C T I O N                       ELGIPGN 
00168  LINKAGE SECTION.                                                 ELGIPGN 
00169  01  DFHCOMMAREA.                                                 ELGIPGN 
00170      COPY ELSCOMMC.                                               ELGIPGN 
00171 /             C I A                                               ELGIPGN 
00172      COPY ELSCIA2C.                                               ELGIPGN 
00173 /                                                                 ELGIPGN 
00174      COPY ELSCMDSC.                                               ELGIPGN 
00175 /                                                                 ELGIPGN 
00176      COPY ELSCMIFC.                                               ELGIPGN 
00177 /                                                                 ELGIPGN 
00178      COPY ELSIOPMC.                                               ELGIPGN 
00179 /                                                                 ELGIPGN 
00180      COPY ELSKEYSC.                                               ELGIPGN 
00181 /                                                                 ELGIPGN 
00182      COPY ELSOUTPC.                                               ELGIPGN 
00183 /                                                                 ELGIPGN 
00184      COPY ELSTCWAC.                                               ELGIPGN 
00185 /                                                                 ELGIPGN 
00186      COPY ELSSRTPC.                                               ELGIPGN 
00187 /                                                                 ELGIPGN 
00188      COPY ELSACUMC.                                               ELGIPGN 
00189 /                                                                 ELGIPGN 
00190  01  GX2-TABULAR-REC-AREA.                                        ELGIPGN 
00191      COPY GCTIPGNC.                                               ELGIPGN 
00192 /    P R O V I D E R   N O .   T O   N A M E   D B   P A R M S    ELGIPGN 
00193  01  PDB-IO-AREA.                                                 ELGIPGN 
00194      COPY DBPIOPMC.                                               ELGIPGN 
00195 /    P R O V I D E R   M A S T E R   R E C O R D   D E S C R .    ELGIPGN 
00196  01  PROVIDER-MSTR-REC.                                           ELGIPGN 
00197      COPY PROVMSTR.                                               ELGIPGN 
00198 /                                                                 ELGIPGN 
00199  PROCEDURE DIVISION.                                              ELGIPGN 
00200 ***************************************************************   ELGIPGN 
00201 *  I N T E R N A L   T A B U L A R   D I S P L A Y   I P G N  *   ELGIPGN 
00202 ***************************************************************   ELGIPGN 
00203  INTERNAL-TABULAR-DISPLAY-IPGN.                                   ELGIPGN 
00204                                                                   ELGIPGN 
00205      PERFORM INITIALIZATION-ROUTINE.                              ELGIPGN 
00206                                                                   ELGIPGN 
00207      IF ACCUM-ASCEND-DESCEND-COUNT = 1 AND                        ELGIPGN 
00208        (ACCUM-ABM OR ACCUM-ACL OR ACCUM-ADL OR ACCUM-AOL          ELGIPGN 
00209           OR ACCUM-ACP)                                           ELGIPGN 
00210           PERFORM SINGLE-LEVEL-DISPLAY                            ELGIPGN 
00211      ELSE                                                         ELGIPGN 
00212         IF ACCUM-ASCEND-DESCEND-COUNT > 1 AND                     ELGIPGN 
00213           (ACCUM-ACL OR ACCUM-AOL)                                ELGIPGN 
00214              PERFORM VARIABLE-LEVEL-DISPLAY                       ELGIPGN 
00215         ELSE                                                      ELGIPGN 
00216            EXEC CICS ABEND  ABCODE('EL99')  END-EXEC.             ELGIPGN 
00217                                                                   ELGIPGN 
00218      GOBACK.                                                      ELGIPGN 
00219                                                                   ELGIPGN 
00220 ************************************************************      ELGIPGN 
00221 *  I N I T I A L I Z A T I O N   R O U T I N E             *      ELGIPGN 
00222 ************************************************************      ELGIPGN 
00223  INITIALIZATION-ROUTINE.                                          ELGIPGN 
00224      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELGIPGN 
00225          EXEC CICS ABEND  ABCODE('EL01')  END-EXEC.               ELGIPGN 
00226                                                                   ELGIPGN 
00227      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGIPGN 
00228          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELGIPGN 
00229      IF CIA-RC-PTR-NULL                                           ELGIPGN 
00230          EXEC CICS ABEND  ABCODE('EL02')  END-EXEC.               ELGIPGN 
00231                                                                   ELGIPGN 
00232      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGIPGN 
00233      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGN 
00234          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELGIPGN 
00235      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIPGN 
00236                                                                   ELGIPGN 
00237      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGIPGN 
00238      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGN 
00239          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELGIPGN 
00240      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIPGN 
00241                                                                   ELGIPGN 
00242      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGIPGN 
00243      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGN 
00244          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELGIPGN 
00245      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIPGN 
00246                                                                   ELGIPGN 
00247      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGIPGN 
00248      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGN 
00249          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELGIPGN 
00250      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIPGN 
00251                                                                   ELGIPGN 
00252      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGIPGN 
00253      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGN 
00254          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELGIPGN 
00255      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIPGN 
00256                                                                   ELGIPGN 
00257      SET  CIA-DBPIOPM-DDN TO TRUE.                                ELGIPGN 
00258      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGN 
00259          ADDRESS OF PDB-IO-AREA.                                  ELGIPGN 
00260                                                                   ELGIPGN 
00261      IF CIA-RC-PTR-NULL                                           ELGIPGN 
00262          PERFORM GETMAIN-IO-PARM-AREA.                            ELGIPGN 
00263      IF GETMAIN-DONE                                              ELGIPGN 
00264         SET  CIA-DBPIOPM-DDN TO TRUE                              ELGIPGN 
00265         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGIPGN 
00266            ADDRESS OF PDB-IO-AREA.                                ELGIPGN 
00267                                                                   ELGIPGN 
00268      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGIPGN 
00269      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGN 
00270         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                   ELGIPGN 
00271      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIPGN 
00272      SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR.       ELGIPGN 
00273                                                                   ELGIPGN 
00274      INITIALIZE CMF-CODES-MANUAL-INTERFACE                        ELGIPGN 
00275                 TCAR-FROM-AREA.                                   ELGIPGN 
00276                                                                   ELGIPGN 
00277  CHECK-IF-CIA-RC-PTR-NULL.                                        ELGIPGN 
00278      IF CIA-RC-PTR-NULL                                           ELGIPGN 
00279          SET CIA-AB-UNALLOC-AREA TO TRUE                          ELGIPGN 
00280          EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.           ELGIPGN 
00281                                                                   ELGIPGN 
00282 ************************************************************      ELGIPGN 
00283 *  S I N G L E   L E V E L   D I S P L A Y                 *      ELGIPGN 
00284 ************************************************************      ELGIPGN 
00285  SINGLE-LEVEL-DISPLAY.                                            ELGIPGN 
00286      PERFORM READ-INTERNAL-TABULAR-RECORD.                        ELGIPGN 
00287      PERFORM LIST-TABULAR-HEADING.                                ELGIPGN 
00288      PERFORM LIST-TABULAR-CONTENTS                                ELGIPGN 
00289        VARYING GX2-INDEX FROM 1 BY 1                              ELGIPGN 
00290          UNTIL GX2-INDEX = GX2-ENTRY-COUNT OR                     ELGIPGN 
00291          GX2-PROVIDER-NO-ARGUMENT (GX2-INDEX) = HIGH-VALUES.      ELGIPGN 
00292      PERFORM INSERT-BLANK-LINE.                                   ELGIPGN 
00293                                                                   ELGIPGN 
00294 ************************************************************      ELGIPGN 
00295 *  V A R I A B L E   L E V E L   D I S P L A Y             *      ELGIPGN 
00296 ************************************************************      ELGIPGN 
00297  VARIABLE-LEVEL-DISPLAY.                                          ELGIPGN 
00298      MOVE 1 TO WS-CUR-SUB.                                        ELGIPGN 
00299      SET VLT-INDEX TO 1.                                          ELGIPGN 
00300      PERFORM EXTRACT-PERCENT-N-SLOT-NBR                           ELGIPGN 
00301        VARYING ASC-DES-INDEX FROM 1 BY 1                          ELGIPGN 
00302          UNTIL ASC-DES-INDEX > ACCUM-ASCEND-DESCEND-COUNT.        ELGIPGN 
00303      MOVE WS-IPGN-COUNT TO WS-CUR-SUB.                            ELGIPGN 
00304      ADD 1 TO WS-CUR-SUB.                                         ELGIPGN 
00305      MOVE HIGH-VALUES TO WS-IPGN-STATUS (WS-CUR-SUB).             ELGIPGN 
00306      PERFORM IPGN-DISPLAY-PROCESSING                              ELGIPGN 
00307        VARYING WS-CUR-SUB FROM 1 BY 1                             ELGIPGN 
00308          UNTIL WS-IPGN-STATUS (WS-CUR-SUB) = HIGH-VALUES.         ELGIPGN 
00309                                                                   ELGIPGN 
00310 ************************************************************      ELGIPGN 
00311 *  E X T R A C T   P E R C E N T   &   S L O T   N B R     *      ELGIPGN 
00312 ************************************************************      ELGIPGN 
00313  EXTRACT-PERCENT-N-SLOT-NBR.                                      ELGIPGN 
00314      IF ACCUM-IPGN-SLOT-NBR (ASC-DES-INDEX) > ZERO                ELGIPGN 
00315         MOVE ACCUM-PERCENT-LEVEL (ASC-DES-INDEX)                  ELGIPGN 
00316           TO WS-IPGN-PERCENT (WS-CUR-SUB)                         ELGIPGN 
00317         MOVE ACCUM-IPGN-SLOT-NBR (ASC-DES-INDEX)                  ELGIPGN 
00318           TO WS-IPGN-SLOT-NBR (WS-CUR-SUB)                        ELGIPGN 
00319         MOVE VLT-VARIABLE-LEVEL-TAG (VLT-INDEX)                   ELGIPGN 
00320           TO WS-IPGN-LEVEL (WS-CUR-SUB)                           ELGIPGN 
00321         SET WS-IPGN-NOT-PROCESSED (WS-CUR-SUB) TO TRUE            ELGIPGN 
00322         ADD 1 TO WS-IPGN-COUNT.                                   ELGIPGN 
00323      ADD 1 TO WS-CUR-SUB.                                         ELGIPGN 
00324      SET VLT-INDEX UP BY 1.                                       ELGIPGN 
00325                                                                   ELGIPGN 
00326 ************************************************************      ELGIPGN 
00327 *  I P G N   D I S P L A Y   P R O C E S S I N G           *      ELGIPGN 
00328 ************************************************************      ELGIPGN 
00329  IPGN-DISPLAY-PROCESSING.                                         ELGIPGN 
00330      IF WS-IPGN-NOT-PROCESSED (WS-CUR-SUB)                        ELGIPGN 
00331         MOVE 1 TO WS-LVL-SUB                                      ELGIPGN 
00332         MOVE ZEROES TO WS-LEVEL-COUNT                             ELGIPGN 
00333         PERFORM PUT-EQUAL-SLOT-NBR-TO-LVL-TBL                     ELGIPGN 
00334           VARYING WS-NXT-SUB FROM WS-CUR-SUB BY 1                 ELGIPGN 
00335             UNTIL WS-IPGN-STATUS (WS-NXT-SUB) = HIGH-VALUES       ELGIPGN 
00336         SET ASC-DES-INDEX TO WS-CUR-SUB                           ELGIPGN 
00337         PERFORM READ-INTERNAL-TABULAR-RECORD                      ELGIPGN 
00338         PERFORM EDIT-CONNECT-PERCENT-LEVELS                       ELGIPGN 
00339           VARYING WS-LVL-SUB FROM 1 BY 1                          ELGIPGN 
00340             UNTIL WS-EDIT-COMPLETE                                ELGIPGN 
00341         PERFORM LIST-TABULAR-HEADING                              ELGIPGN 
00342         PERFORM LIST-TABULAR-CONTENTS                             ELGIPGN 
00343           VARYING GX2-INDEX FROM 1 BY 1                           ELGIPGN 
00344             UNTIL GX2-INDEX = GX2-ENTRY-COUNT OR                  ELGIPGN 
00345             GX2-PROVIDER-NO-ARGUMENT (GX2-INDEX) = HIGH-VALUES    ELGIPGN 
00346         PERFORM INSERT-BLANK-LINE.                                ELGIPGN 
00347                                                                   ELGIPGN 
00348 **************************************************************    ELGIPGN 
00349 *  P U T   E Q U A L   S L O T   N B R   T O   L V L   T B L *    ELGIPGN 
00350 **************************************************************    ELGIPGN 
00351  PUT-EQUAL-SLOT-NBR-TO-LVL-TBL.                                   ELGIPGN 
00352      IF WS-IPGN-NOT-PROCESSED (WS-NXT-SUB) AND                    ELGIPGN 
00353         WS-IPGN-SLOT-NBR (WS-CUR-SUB) =                           ELGIPGN 
00354                           WS-IPGN-SLOT-NBR (WS-NXT-SUB)           ELGIPGN 
00355            MOVE WS-IPGN-PERCENT (WS-NXT-SUB) TO                   ELGIPGN 
00356                              WS-LEVEL-PERCENT (WS-LVL-SUB)        ELGIPGN 
00357            MOVE WS-IPGN-LEVEL   (WS-NXT-SUB) TO                   ELGIPGN 
00358                              WS-LEVEL-TAG (WS-LVL-SUB)            ELGIPGN 
00359            SET WS-IPGN-PROCESSED (WS-NXT-SUB) TO TRUE             ELGIPGN 
00360            ADD 1 TO WS-LVL-SUB                                    ELGIPGN 
00361            ADD 1 TO WS-LEVEL-COUNT.                               ELGIPGN 
00362                                                                   ELGIPGN 
00363 ************************************************************      ELGIPGN 
00364 *        READ INTERNAL TABULAR RECORD                      *      ELGIPGN 
00365 ************************************************************      ELGIPGN 
00366  READ-INTERNAL-TABULAR-RECORD.                                    ELGIPGN 
00367      SET CIA-GCTABULR-DDN TO TRUE.                                ELGIPGN 
00368      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGN 
00369          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGIPGN 
00370      IF CIA-RC-PTR-NULL                                           ELGIPGN 
00371         PERFORM GETMAIN-IO-PARM-AREA                              ELGIPGN 
00372      ELSE                                                         ELGIPGN 
00373         SET CIA-GCTABULR-DDN TO TRUE                              ELGIPGN 
00374         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGIPGN 
00375               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELGIPGN 
00376      MOVE '#IPGN' TO KWA-PROVISION-ID.                            ELGIPGN 
00377      MOVE ACCUM-IPGN-SLOT-NBR (ASC-DES-INDEX)                     ELGIPGN 
00378                   TO KWA-PROVISION-SLOT-NO.                       ELGIPGN 
00379      SET IOP-RD TO TRUE.                                          ELGIPGN 
00380      SET IOP-FCQ-NONE TO TRUE.                                    ELGIPGN 
00381      SET IOP-KVQ-EQ TO TRUE.                                      ELGIPGN 
00382      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGIPGN 
00383      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGIPGN 
00384      SET IOP-REC-PTR TO NULL.                                     ELGIPGN 
00385      SET IOP-STG-MODE-MOVE TO TRUE.                               ELGIPGN 
00386      EXEC CICS LINK  PROGRAM('ELUIOPGM')                          ELGIPGN 
00387                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELGIPGN 
00388      IF IOP-RC-OK                                                 ELGIPGN 
00389         SET ADDRESS OF GX2-TABULAR-REC-AREA TO IOP-REC-PTR        ELGIPGN 
00390      ELSE                                                         ELGIPGN 
00391         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELGIPGN 
00392         EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.            ELGIPGN 
00393                                                                   ELGIPGN 
00394 **************************************************************    ELGIPGN 
00395 *  G E T M A I N   I O   P A R M   A R E A                   *    ELGIPGN 
00396 **************************************************************    ELGIPGN 
00397  GETMAIN-IO-PARM-AREA.                                            ELGIPGN 
00398      SET CIA-STG-GETMAIN TO TRUE.                                 ELGIPGN 
00399      EXEC CICS LINK  PROGRAM('ELUSTGMG')                          ELGIPGN 
00400                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELGIPGN 
00401      SET GETMAIN-DONE TO TRUE.                                    ELGIPGN 
00402                                                                   ELGIPGN 
00403 ***************************************************************   ELGIPGN 
00404 *   E D I T   C O N N E C T   P E R C E N T   L E V E L S     *   ELGIPGN 
00405 ***************************************************************   ELGIPGN 
00406  EDIT-CONNECT-PERCENT-LEVELS.                                     ELGIPGN 
00407      IF WS-LVL-SUB = 1 AND                                        ELGIPGN 
00408         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIPGN 
00409         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIPGN 
00410         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIPGN 
00411                                                                   ELGIPGN 
00412      IF WS-LVL-SUB = 2 AND                                        ELGIPGN 
00413         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIPGN 
00414         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIPGN 
00415         SUBTRACT 1 FROM WS-LVL-SUB                                ELGIPGN 
00416         SET WS-LEVEL-PCT-AND (WS-LVL-SUB) TO TRUE                 ELGIPGN 
00417         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIPGN 
00418                                                                   ELGIPGN 
00419      IF WS-LVL-SUB > 2 AND                                        ELGIPGN 
00420         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIPGN 
00421         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIPGN 
00422         SUBTRACT 1 FROM WS-LVL-SUB                                ELGIPGN 
00423         SET WS-LEVEL-PCT-AND (WS-LVL-SUB) TO TRUE                 ELGIPGN 
00424         SET WS-EDIT-COMPLETE     TO TRUE                          ELGIPGN 
00425      ELSE                                                         ELGIPGN 
00426      IF WS-LVL-SUB > 2 AND                                        ELGIPGN 
00427         WS-LVL-SUB NOT EQUAL WS-LEVEL-COUNT                       ELGIPGN 
00428            SET WS-LEVEL-PCT-COMMA (WS-LVL-SUB) TO TRUE.           ELGIPGN 
00429                                                                   ELGIPGN 
00430 **************************************************************    ELGIPGN 
00431 *  L I S T   T A B U L A R   H E A D I N G                   *    ELGIPGN 
00432 **************************************************************    ELGIPGN 
00433  LIST-TABULAR-HEADING.                                            ELGIPGN 
00434      INITIALIZE TCAR-FROM-AREA.                                   ELGIPGN 
00435      MOVE WS-IPGN TO CMF-RECORD-PREFIX.                           ELGIPGN 
00436      MOVE 'INCLUDE-EXCLUDE-IND' TO                                ELGIPGN 
00437          CMF-ELEMENT-SYSTEM-NAME.                                 ELGIPGN 
00438      MOVE GX2-INCLUDE-EXCLUDE-IND TO CMF-CODE-VALUE.              ELGIPGN 
00439      EXEC CICS LINK  PROGRAM('ELUCMIF')                           ELGIPGN 
00440                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELGIPGN 
00441      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGIPGN 
00442      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGN 
00443          ADDRESS OF CMF-DESCR.                                    ELGIPGN 
00444      EVALUATE TRUE                                                ELGIPGN 
00445         WHEN ACCUM-ACL                                            ELGIPGN 
00446              MOVE 1 TO WS-ACM-SUB                                 ELGIPGN 
00447         WHEN ACCUM-ADL                                            ELGIPGN 
00448              MOVE 2 TO WS-ACM-SUB                                 ELGIPGN 
00449         WHEN ACCUM-ABM                                            ELGIPGN 
00450              MOVE 3 TO WS-ACM-SUB                                 ELGIPGN 
00451         WHEN ACCUM-AOL                                            ELGIPGN 
00452              MOVE 4 TO WS-ACM-SUB                                 ELGIPGN 
00453         WHEN ACCUM-AOL                                            ELGIPGN 
00454              MOVE 5 TO WS-ACM-SUB                                 ELGIPGN 
00455      END-EVALUATE.                                                ELGIPGN 
00456      IF (ACCUM-ADL OR ACCUM-ABM OR ACCUM-ADL)                     ELGIPGN 
00457          PERFORM STRING-FIRST-LINE-FOR-MAXIMUMX                   ELGIPGN 
00458      ELSE                                                         ELGIPGN 
00459         IF (ACCUM-ACL OR ACCUM-AOL)                               ELGIPGN 
00460            PERFORM STRING-FIRST-LINE-FOR-COINSURA.                ELGIPGN 
00461      PERFORM DO-TEXT-COMPRESSION.                                 ELGIPGN 
00462      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELGIPGN 
00463      MOVE +04 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGIPGN 
00464      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                          ELGIPGN 
00465                  TCAR-OUTPUT-FIELD-2-LEN                          ELGIPGN 
00466                  TCAR-OUTPUT-FIELD-3-LEN                          ELGIPGN 
00467                  TCAR-OUTPUT-FIELD-4-LEN.                         ELGIPGN 
00468      PERFORM DO-TEXT-UNSTRING.                                    ELGIPGN 
00469      PERFORM MOVE-ASTERISKS-TO-FIRST-OUTPUT.                      ELGIPGN 
00470      PERFORM MOVE-COMPRESSED-PHRASE                               ELGIPGN 
00471        VARYING TCAR-FROM-SUB FROM 1 BY 1 UNTIL                    ELGIPGN 
00472          TCAR-FROM-SUB GREATER THAN TCAR-OUTPUT-FIELDS-USED.      ELGIPGN 
00473      PERFORM CALL-OUTPUT.                                         ELGIPGN 
00474                                                                   ELGIPGN 
00475 **************************************************************    ELGIPGN 
00476 *  S T R I N G   F I R S T   L I N E   M A X                 *    ELGIPGN 
00477 **************************************************************    ELGIPGN 
00478  STRING-FIRST-LINE-FOR-MAXIMUMX.                                  ELGIPGN 
00479      STRING 'THIS '                       DELIMITED BY SIZE       ELGIPGN 
00480             WS-ACCUM-NAME (WS-ACM-SUB)    DELIMITED BY ' '        ELGIPGN 
00481             ' ACCUMULATOR '               DELIMITED BY SIZE       ELGIPGN 
00482             CMF-DESCR-LINE(1)             DELIMITED BY '  '       ELGIPGN 
00483             ' THE FOLLOWING PROVIDERS:'   DELIMITED BY SIZE       ELGIPGN 
00484             INTO TCAR-FROM-AREA.                                  ELGIPGN 
00485                                                                   ELGIPGN 
00486 ***************************************************************   ELGIPGN 
00487 *  S T R I N G   F I R S T   L I N E   C O I N S U R A N C E  *   ELGIPGN 
00488 ***************************************************************   ELGIPGN 
00489  STRING-FIRST-LINE-FOR-COINSURA.                                  ELGIPGN 
00490      IF ACCUM-ASCEND-DESCEND-COUNT = 1                            ELGIPGN 
00491         MOVE 1 TO WS-LEVEL-COUNT                                  ELGIPGN 
00492         MOVE SPACES TO WS-LEVEL-TAG (1)                           ELGIPGN 
00493         MOVE ACCUM-PERCENT-LEVEL (1)  TO WS-LEVEL-PERCENT (1)     ELGIPGN 
00494         SET WS-LEVEL-PCT (1) TO TRUE.                             ELGIPGN 
00495                                                                   ELGIPGN 
00496      STRING 'THE '                           DELIMITED BY SIZE    ELGIPGN 
00497         WS-ACCUM-NAME (WS-ACM-SUB)           DELIMITED BY ' '     ELGIPGN 
00498         ' ACCUMULATOR '                      DELIMITED BY SIZE    ELGIPGN 
00499         WS-SERVICES-PAID-PHRASE              DELIMITED BY SIZE    ELGIPGN 
00500         WS-LEVEL-TABLE                       DELIMITED BY SIZE    ELGIPGN 
00501         CMF-DESCR-LINE(1)                    DELIMITED BY '  '    ELGIPGN 
00502        ' THE FOLLOWING PROVIDERS:'           DELIMITED BY SIZE    ELGIPGN 
00503             INTO TCAR-FROM-AREA.                                  ELGIPGN 
00504                                                                   ELGIPGN 
00505 ***************************************************************   ELGIPGN 
00506 *  L I S T   T A B U L A R   C O N T E N T S                  *   ELGIPGN 
00507 ***************************************************************   ELGIPGN 
00508  LIST-TABULAR-CONTENTS.                                           ELGIPGN 
00509      MOVE GX2-PROVIDER-NO-ARGUMENT (GX2-INDEX)  TO                ELGIPGN 
00510          PDB-I-PROVIDER-NBR.                                      ELGIPGN 
00511      MOVE 'PRVDR02 '  TO  PDB-I-REQUEST-TYPE.                     ELGIPGN 
00512      EXEC CICS  LINK  PROGRAM('DBPIOC')                           ELGIPGN 
00513                       COMMAREA(PDB-IO-AREA)  END-EXEC.            ELGIPGN 
00514      IF NOT PDB-O-RC-SUCCESSFUL                                   ELGIPGN 
00515         PERFORM PROVIDER-FILE-PROBLEM                             ELGIPGN 
00516      ELSE                                                         ELGIPGN 
00517         SET ADDRESS OF PROVIDER-MSTR-REC TO                       ELGIPGN 
00518             ADDRESS OF PDB-O-RECORD-AREA                          ELGIPGN 
00519         IF PFM-PAYEE-NAME-1 NOT =  SPACES AND                     ELGIPGN 
00520            PFM-PAYEE-NAME-2 NOT =  SPACES                         ELGIPGN 
00521            PERFORM STRING-PROV-NO-AND-NAMES                       ELGIPGN 
00522         ELSE                                                      ELGIPGN 
00523            PERFORM STRING-PROV-NO-AND-NAME.                       ELGIPGN 
00524      PERFORM CALL-OUTPUT.                                         ELGIPGN 
00525                                                                   ELGIPGN 
00526 ***************************************************************   ELGIPGN 
00527 *  S T R I N G   P R O V   N O   A N D   N A M E S            *   ELGIPGN 
00528 ***************************************************************   ELGIPGN 
00529  STRING-PROV-NO-AND-NAMES.                                        ELGIPGN 
00530      MOVE SPACES TO TCAR-FROM-LINE(1),                            ELGIPGN 
00531                     TCAR-FROM-LINE(2).                            ELGIPGN 
00532      STRING GX2-PROVIDER-NO-ARGUMENT(GX2-INDEX),                  ELGIPGN 
00533             '  ',  DELIMITED BY SIZE,                             ELGIPGN 
00534             PFM-PAYEE-NAME-1,  DELIMITED BY ' ',                  ELGIPGN 
00535             ', ',  DELIMITED BY SIZE,                             ELGIPGN 
00536             PFM-PAYEE-NAME-2,  DELIMITED BY ' ',                  ELGIPGN 
00537             INTO  TCAR-FROM-AREA.                                 ELGIPGN 
00538      PERFORM DO-TEXT-COMPRESSION.                                 ELGIPGN 
00539      MOVE +74  TO  TCAR-OUTPUT-FIELD-1-LEN.                       ELGIPGN 
00540      MOVE +63  TO  TCAR-OUTPUT-FIELD-2-LEN.                       ELGIPGN 
00541      MOVE +2  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELGIPGN 
00542      PERFORM DO-TEXT-UNSTRING.                                    ELGIPGN 
00543      PERFORM MOVE-COMPRESSED-PHRASE                               ELGIPGN 
00544         VARYING TCAR-FROM-SUB FROM 1 BY 1 UNTIL TCAR-FROM-SUB     ELGIPGN 
00545            GREATER THAN TCAR-OUTPUT-FIELDS-USED.                  ELGIPGN 
00546                                                                   ELGIPGN 
00547 ***************************************************************   ELGIPGN 
00548 *  S T R I N G   P R O V   A N D   N A M E                    *   ELGIPGN 
00549 ***************************************************************   ELGIPGN 
00550  STRING-PROV-NO-AND-NAME.                                         ELGIPGN 
00551      MOVE SPACES  TO  COF-DTL-LINE(1).                            ELGIPGN 
00552      MOVE 1  TO  COF-NBR-DTL-LINES.                               ELGIPGN 
00553      STRING GX2-PROVIDER-NO-ARGUMENT(GX2-INDEX),                  ELGIPGN 
00554             '  ',  DELIMITED BY SIZE,                             ELGIPGN 
00555             PFM-PAYEE-NAME-1,  DELIMITED BY ' ',                  ELGIPGN 
00556             ' ',  DELIMITED BY SIZE,                              ELGIPGN 
00557             PFM-PAYEE-NAME-2,   ' '    DELIMITED BY ' '           ELGIPGN 
00558             INTO  COF-DTL-LINE(1).                                ELGIPGN 
00559                                                                   ELGIPGN 
00560 ***************************************************************   ELGIPGN 
00561 *  I N S E R T   B L A N K   L I N E                          *   ELGIPGN 
00562 ***************************************************************   ELGIPGN 
00563  INSERT-BLANK-LINE.                                               ELGIPGN 
00564      ADD  1       TO  COF-NBR-DTL-LINES.                          ELGIPGN 
00565      MOVE SPACES  TO  COF-DTL-LINE (COF-NBR-DTL-LINES).           ELGIPGN 
00566      PERFORM CALL-OUTPUT.                                         ELGIPGN 
00567                                                                   ELGIPGN 
00568 ***************************************************************   ELGIPGN 
00569 *  M O V E   C O M P R E S S E D   P H R A S E                *   ELGIPGN 
00570 ***************************************************************   ELGIPGN 
00571  MOVE-COMPRESSED-PHRASE.                                          ELGIPGN 
00572      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGIPGN 
00573      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB)                           ELGIPGN 
00574         TO COF-DTL-LINE (COF-NBR-DTL-LINES).                      ELGIPGN 
00575                                                                   ELGIPGN 
00576 ***************************************************************   ELGIPGN 
00577 *  M O V E   A S T E R I S K S   O U T P U T   L I N E        *   ELGIPGN 
00578 ***************************************************************   ELGIPGN 
00579  MOVE-ASTERISKS-TO-FIRST-OUTPUT.                                  ELGIPGN 
00580      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGIPGN 
00581      MOVE WS-FLAG-LINE TO COF-DTL-LINE                            ELGIPGN 
00582          (COF-NBR-DTL-LINES).                                     ELGIPGN 
00583                                                                   ELGIPGN 
00584 ***************************************************************   ELGIPGN 
00585 *  D O   T E X T   C O M P R E S S I O N                      *   ELGIPGN 
00586 ***************************************************************   ELGIPGN 
00587  DO-TEXT-COMPRESSION.                                             ELGIPGN 
00588      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGIPGN 
00589                                                                   ELGIPGN 
00590 ***************************************************************   ELGIPGN 
00591 *  D O   T E X T   U N S T R I N G                            *   ELGIPGN 
00592 ***************************************************************   ELGIPGN 
00593  DO-TEXT-UNSTRING.                                                ELGIPGN 
00594      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGIPGN 
00595                                                                   ELGIPGN 
00596 ***************************************************************   ELGIPGN 
00597 *  C A L L   O U T P U T                                      *   ELGIPGN 
00598 ***************************************************************   ELGIPGN 
00599  CALL-OUTPUT.                                                     ELGIPGN 
00600      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELGIPGN 
00601                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGIPGN 
00602                                                                   ELGIPGN 
00603 ***************************************************************   ELGIPGN 
00604 *  P R O V I D E R   F I L E   P R O B L E M                  *   ELGIPGN 
00605 ***************************************************************   ELGIPGN 
00606  PROVIDER-FILE-PROBLEM.                                           ELGIPGN 
00607      ADD  1  TO  COF-NBR-DTL-LINES.                               ELGIPGN 
00608      STRING 'PROVIDER NUMBER  '          DELIMITED BY SIZE        ELGIPGN 
00609         GX2-PROVIDER-NO-ARGUMENT (GX2-INDEX) DELIMITED BY ' '     ELGIPGN 
00610             '  IS NOT ON FILE.'          DELIMITED BY SIZE        ELGIPGN 
00611                   INTO COF-DTL-LINE(COF-NBR-DTL-LINES).           ELGIPGN 
00612                                                                   ELGIPGN 
