00001  IDENTIFICATION DIVISION.                                         09/03/03
00002 *                                                                 ELGIBGR 
00003  PROGRAM-ID.         ELGIBGR.                                        LV002
00004 *                                                                 ELGIBGR 
00005  AUTHOR.             ALIDA JATICH OF T. M. FLOYD, INC.            ELGIBGR 
00006 *                                                                 ELGIBGR 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGIBGR 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELGIBGR 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGIBGR 
00010                      233 N. MICHIGAN AVE                          ELGIBGR 
00011                      CHICAGO, ILLINOIS 60601                      ELGIBGR 
00012 *                                                                 ELGIBGR 
00013  DATE-WRITTEN.       31-OCT-1986.                                 ELGIBGR 
00014 *                                                                 ELGIBGR 
00015  DATE-COMPILED.                                                   ELGIBGR 
00016 *                                                                 ELGIBGR 
00017  SECURITY.           COPYRIGHT 1986,                              ELGIBGR 
00018                      HEALTH CARE SERVICE CORPORATION              ELGIBGR 
00019 *                                                                 ELGIBGR 
00020 ******************************************************************ELGIBGR 
00021 *   ELGIBGR                                                      *ELGIBGR 
00022 *                                                                *ELGIBGR 
00023 *                        PROGRAM ABSTRACT                        *ELGIBGR 
00024 *                                                                *ELGIBGR 
00025 *   PROGRAM NAME:   E.L.S. INTERNAL TABULAR TRANSLATOR           *ELGIBGR 
00026 *                   SUBROUTINE                                   *ELGIBGR 
00027 *                                                                *ELGIBGR 
00028 *   PROGRAM I.D.:   ELGIBGR                                      *ELGIBGR 
00029 *                                                                *ELGIBGR 
00030 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE FIELDS *ELGIBGR 
00031 *              IN THE #IBGR INTERNAL TABULAR                     *ELGIBGR 
00032 *                                                                *ELGIBGR 
00033 *   RECORDS                                                      *ELGIBGR 
00034 *   ACCESSED:  #IBGR INTERNAL TABULAR                            *ELGIBGR 
00035 *                                                                *ELGIBGR 
00036 ******************************************************************ELGIBGR 
00037 *                                                                *ELGIBGR 
00038 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGIBGR 
00039 *       *-*         U P D A T E   H I S T O R Y         *-*      *ELGIBGR 
00040 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGIBGR 
00041 *                                                                *ELGIBGR 
00042 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELGIBGR 
00043 *                                                                *ELGIBGR 
00044 *  ELS 2.0   10/31/86  AMJ  ORIGINAL MODULE WRITTEN.             *ELGIBGR 
00045 *                                                                *ELGIBGR 
00046 *            01/20/87  NAC  DISPLAY \
00047 *                           A,B,W; DISPLAY \
00048 *                           FORMATS C,D,E.                       *ELGIBGR 
00049 *                                                                *ELGIBGR 
00050 *            02/04/87  LET  CHANGED VERBAGE 'COVERAGE' TO        *ELGIBGR 
00051 *                           'THIS ACCUMULATOR'.                  *ELGIBGR 
00052 *                                                                *ELGIBGR 
00053 *            03/11/87  NAC  RESTRUCTURE OUTPUT SENTENCE TO       *ELGIBGR 
00054 *                           INCLUDE ACCUMULATOR TYPE.            *ELGIBGR 
00055 *                                                                *ELGIBGR 
00056 *            07/13/87  LET  REMOVED EXTRA BLANK LINES THAT WERE  *ELGIBGR 
00057 *                           BEING PRODUCED.                      *ELGIBGR 
00058 *                                                                *ELGIBGR 
00059 *   01.01    11/09/87  REB  CHANGE VERBAGE ON FIRST LINE OF      *ELGIBGR 
00060 *                           OUTPUT, WILL INSERT THE PERCENT      *ELGIBGR 
00061 *                           LEVEL PASSED TO IT IN THE SRP BLOCK. *ELGIBGR 
00062 *                                                                *ELGIBGR 
00063 *   01.02    11/18/87  REB  SEND A DIFFERENT FIRST OUTPUT LINE   *ELGIBGR 
00064 *                           FOR ABM AND ADL ACCUMS.              *ELGIBGR 
00065 *                                                                *ELGIBGR 
00066 *   01.03    11/23/87  REB  DISPLAYED ASTERISKS BEFORE INFO FROM *ELGIBGR 
00067 *                           INTERNAL TABULARS PER AUGGIE.        *ELGIBGR 
00068 *                                                                *ELGIBGR 
00069 *   02.00    03/31/89  NAC  DESTRUCT CONVERSION USING STRUCTURES *ELGIBGR 
00070 *                           VER: 3.5.                            *ELGIBGR 
00071 *                                                                *ELGIBGR 
00072 *   03.00    04/05/89  GEM  STORAGE MANAGEMENT ENHANCEMENT       *ELGIBGR 
00073 *                                                                *ELGIBGR 
00074 *   03.00    12/12/91  GEM  ACCUM DISPLAY FIX ISSR# 12010.       *ELGIBGR 
00075 *                                                                *ELGIBGR 
00076 *   43.00    10/30/98  AKK  ADDED SUPPORT FOR ACP TABULAR.       *ELGIBGR 
00077 *                                                                *ELGIBGR 
00078 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGIBGR 
00079 *                                                                *ELGIBGR 
ED0624* BBDA-58217 06/14/24  ED   RECOMPILE FOR PEAQ COPYBOOK          *        
ED0624*                           EXPANSION:                           *        
ED0624*                                 COPYBOOK ELSACUMC              *        
ED0624*                                                                *        
00080 ******************************************************************ELGIBGR 
00081 /                                                                 ELGIBGR 
00082  ENVIRONMENT DIVISION.                                            ELGIBGR 
00083  CONFIGURATION SECTION.                                           ELGIBGR 
00084  SOURCE-COMPUTER.    IBM-3033.                                    ELGIBGR 
00085  OBJECT-COMPUTER.    IBM-3033.                                    ELGIBGR 
00086 /                                                                 ELGIBGR 
00087  DATA DIVISION.                                                   ELGIBGR 
00088  WORKING-STORAGE SECTION.                                         ELGIBGR 
00089  01  WS-BEGIN               PIC X(24)  VALUE                      ELGIBGR 
00090      'ELGIBGR WORKING STORAGE*'.                                  ELGIBGR 
00091 /     W O R K F I E L D S   A N D   S W I T C H E S               ELGIBGR 
00092  01  WS-MISC-WORK.                                                ELGIBGR 
00093      05  WS-SUB             PIC S9999  COMP SYNC VALUE ZEROES.    ELGIBGR 
00094      05  WS-ACM-SUB         PIC S9999  COMP SYNC VALUE ZEROES.    ELGIBGR 
00095      05  WS-CUR-SUB         PIC S9999  COMP SYNC VALUE ZEROES.    ELGIBGR 
00096      05  WS-NXT-SUB         PIC S9999  COMP SYNC VALUE ZEROES.    ELGIBGR 
00097      05  WS-TAG-SUB         PIC S9999  COMP SYNC VALUE ZEROES.    ELGIBGR 
00098      05  WS-LVL-SUB         PIC S9999  COMP SYNC VALUE ZEROES.    ELGIBGR 
00099      05  WS-IBGR            PIC X(5)   VALUE '#IBGR'.             ELGIBGR 
00100      05  WS-BEN-ID.                                               ELGIBGR 
00101          10  WS-BEN-ID-5    PIC X(5)   VALUE SPACES.              ELGIBGR 
00102          10  WS-BEN-ID-1    PIC X      VALUE SPACES.              ELGIBGR 
00103      05  WS-EDIT-COMP       PIC X(01)  VALUE SPACES.              ELGIBGR 
00104          88  WS-EDIT-COMPLETE             VALUE 'Y'.              ELGIBGR 
00105  01  FILLER.                                                      ELGIBGR 
00106    02  WS-ACCUMULATOR-TYPE.                                       ELGIBGR 
00107      03  FILLER   PIC X(21)  VALUE                                ELGIBGR 
00108         '#ACL    COINSURANCE  '.                                  ELGIBGR 
00109      03  FILLER   PIC X(21)  VALUE                                ELGIBGR 
00110         '#ADL    DEDUCTIBLE   '.                                  ELGIBGR 
00111      03  FILLER   PIC X(21)  VALUE                                ELGIBGR 
00112         '#ABM    MAXIMUM      '.                                  ELGIBGR 
00113      03  FILLER   PIC X(21)  VALUE                                ELGIBGR 
00114         '#AOL    OUT-OF-POCKET'.                                  ELGIBGR 
00115      03  FILLER   PIC X(21)  VALUE                                ELGIBGR 
00116         '#ACP    CO-PAY       '.                                  ELGIBGR 
00117    02  WS-ACCUMULATOR-TYPE-TABLE REDEFINES  WS-ACCUMULATOR-TYPE.  ELGIBGR 
00118      03  WS-ACCUMULATOR-TYPE-TBL  OCCURS 5 TIMES                  ELGIBGR 
00119                                   INDEXED BY ACUM-IDX.            ELGIBGR 
00120          05  WS-ACCUM-TYPE       PIC X(08).                       ELGIBGR 
00121          05  WS-ACCUM-NAME       PIC X(13).                       ELGIBGR 
00122                                                                   ELGIBGR 
00123  01  WS-IBGR-VARIABLE-AREA.                                       ELGIBGR 
00124    02  WS-IBGR-COUNT      COMP-3      PIC S9(03) VALUE ZEROES.    ELGIBGR 
00125    02  WS-IBGR-VARIABLES                                          ELGIBGR 
00126           OCCURS 46 TIMES DEPENDING  ON  WS-IBGR-COUNT.           ELGIBGR 
00127        05  WS-IBGR-SLOT-NBR           PIC S9(07) COMP-3.          ELGIBGR 
00128        05  WS-IBGR-LEVEL              PIC  X(04).                 ELGIBGR 
00129        05  WS-IBGR-PERCENT            PIC S9(03) COMP-3.          ELGIBGR 
00130        05  WS-IBGR-STATUS             PIC X(01).                  ELGIBGR 
00131          88  WS-IBGR-PROCESSED            VALUE 'Y'.              ELGIBGR 
00132          88  WS-IBGR-NOT-PROCESSED        VALUE 'N'.              ELGIBGR 
00133                                                                   ELGIBGR 
00134  01  WS-LEVEL-AREA.                                               ELGIBGR 
00135    02  WS-LEVEL-COUNT    COMP-3    PIC S9(03) VALUE ZEROES.       ELGIBGR 
00136    02  WS-LEVEL-TABLE.                                            ELGIBGR 
00137      03  WS-LEVEL-ENTRIES                                         ELGIBGR 
00138            OCCURS 46 TIMES DEPENDING ON WS-LEVEL-COUNT.           ELGIBGR 
00139        05  WS-LEVEL-TAG            PIC  X(04).                    ELGIBGR 
00140        05  WS-LEVEL-PERCENT        PIC  ZZ9.                      ELGIBGR 
00141        05  WS-LEVEL-EDITOR         PIC  X(06).                    ELGIBGR 
00142          88  WS-LEVEL-PCT           VALUE '%     '.               ELGIBGR 
00143          88  WS-LEVEL-PCT-COMMA     VALUE '%,    '.               ELGIBGR 
00144          88  WS-LEVEL-PCT-AND       VALUE '% AND '.               ELGIBGR 
00145                                                                   ELGIBGR 
00146  01  PROGRAM-CONSTANTS.                                           ELGIBGR 
00147      05  PC-ABM                  PIC X(08)  VALUE '#ABM    '.     ELGIBGR 
00148      05  PC-ADL                  PIC X(08)  VALUE '#ADL    '.     ELGIBGR 
00149                                                                   ELGIBGR 
00150  01  WS-SERVICES-PAID-PHRASE.                                     ELGIBGR 
00151      05  FILLER                  PIC X(21)  VALUE                 ELGIBGR 
00152          'FOR SERVICES PAID AT '.                                 ELGIBGR 
00153                                                                   ELGIBGR 
00154  01  WS-FLAG-LINE.                                                ELGIBGR 
00155      05  FILLER                  PIC X(79)  VALUE '*****'.        ELGIBGR 
00156 /                                                                 ELGIBGR 
00157      COPY ELSVLTGC.                                               ELGIBGR 
00158                                                                   ELGIBGR 
00159  01  WS-END                 PIC X(16)  VALUE                      ELGIBGR 
00160      '*** W/S ENDS ***'.                                          ELGIBGR 
00161 /             L I N K A G E   S E C T I O N                       ELGIBGR 
00162  LINKAGE SECTION.                                                 ELGIBGR 
00163  01  DFHCOMMAREA.                                                 ELGIBGR 
00164      COPY ELSCOMMC.                                               ELGIBGR 
00165 /                                                                 ELGIBGR 
00166      COPY ELSCIA2C.                                               ELGIBGR 
00167 /                                                                 ELGIBGR 
00168      COPY ELSCMDSC.                                               ELGIBGR 
00169 /                                                                 ELGIBGR 
00170      COPY ELSCMIFC.                                               ELGIBGR 
00171 /                                                                 ELGIBGR 
00172      COPY ELSIOPMC.                                               ELGIBGR 
00173 /                                                                 ELGIBGR 
00174      COPY ELSKEYSC.                                               ELGIBGR 
00175 /                                                                 ELGIBGR 
00176      COPY ELSOUTPC.                                               ELGIBGR 
00177 /                                                                 ELGIBGR 
00178      COPY ELSTCWAC.                                               ELGIBGR 
00179 /                                                                 ELGIBGR 
00180      COPY ELSSRTPC.                                               ELGIBGR 
00181 /                                                                 ELGIBGR 
00182      COPY ELSACUMC.                                               ELGIBGR 
00183 /                                                                 ELGIBGR 
00184  01  GX1-TABULAR-REC-AREA.                                        ELGIBGR 
00185      COPY GCTIBGRC.                                               ELGIBGR 
00186 /                                                                 ELGIBGR 
00187  PROCEDURE DIVISION.                                              ELGIBGR 
00188 ***************************************************************   ELGIBGR 
00189 *                                                             *   ELGIBGR 
00190 *  I N T E R N A L   T A B U L A R   D I S P L A Y   I B G R  *   ELGIBGR 
00191 *                                                             *   ELGIBGR 
00192 ***************************************************************   ELGIBGR 
00193  INTERNAL-TABULAR-DISPLAY-IBGR.                                   ELGIBGR 
00194                                                                   ELGIBGR 
00195      PERFORM INITIALIZATION-ROUTINE.                              ELGIBGR 
00196                                                                   ELGIBGR 
00197      IF ACCUM-ASCEND-DESCEND-COUNT  =  1  AND                     ELGIBGR 
00198        (ACCUM-ABM OR ACCUM-ACL OR  ACCUM-ACP OR ACCUM-ADL         ELGIBGR 
00199                  OR ACCUM-AOL)                                    ELGIBGR 
00200           PERFORM SINGLE-LEVEL-DISPLAY                            ELGIBGR 
00201      ELSE                                                         ELGIBGR 
00202         IF ACCUM-ASCEND-DESCEND-COUNT  >  1  AND                  ELGIBGR 
00203           (ACCUM-ACL OR ACCUM-AOL)                                ELGIBGR 
00204              PERFORM VARIABLE-LEVEL-DISPLAY                       ELGIBGR 
00205         ELSE                                                      ELGIBGR 
00206            EXEC CICS ABEND  ABCODE('EL99')  END-EXEC.             ELGIBGR 
00207                                                                   ELGIBGR 
00208      GOBACK.                                                      ELGIBGR 
00209 ***************************************************************   ELGIBGR 
00210 *                                                             *   ELGIBGR 
00211 *   I N I T I A L I Z A T I O N   R O U T I N E               *   ELGIBGR 
00212 *                                                             *   ELGIBGR 
00213 ***************************************************************   ELGIBGR 
00214  INITIALIZATION-ROUTINE.                                          ELGIBGR 
00215      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELGIBGR 
00216          EXEC CICS ABEND  ABCODE('EL01')  END-EXEC.               ELGIBGR 
00217                                                                   ELGIBGR 
00218      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGIBGR 
00219          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELGIBGR 
00220      IF CIA-RC-PTR-NULL                                           ELGIBGR 
00221          EXEC CICS ABEND  ABCODE('EL02')  END-EXEC.               ELGIBGR 
00222                                                                   ELGIBGR 
00223      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGIBGR 
00224      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIBGR 
00225          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELGIBGR 
00226      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIBGR 
00227                                                                   ELGIBGR 
00228      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGIBGR 
00229      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIBGR 
00230          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELGIBGR 
00231      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIBGR 
00232                                                                   ELGIBGR 
00233      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGIBGR 
00234      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIBGR 
00235          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELGIBGR 
00236      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIBGR 
00237                                                                   ELGIBGR 
00238      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGIBGR 
00239      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIBGR 
00240          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELGIBGR 
00241      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIBGR 
00242                                                                   ELGIBGR 
00243      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGIBGR 
00244      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIBGR 
00245          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELGIBGR 
00246      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIBGR 
00247                                                                   ELGIBGR 
00248      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGIBGR 
00249      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIBGR 
00250          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGIBGR 
00251      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIBGR 
00252      SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR.       ELGIBGR 
00253                                                                   ELGIBGR 
00254      INITIALIZE TCAR-FROM-AREA                                    ELGIBGR 
00255                 CMF-CODES-MANUAL-INTERFACE.                       ELGIBGR 
00256                                                                   ELGIBGR 
00257  CHECK-IF-CIA-RC-PTR-NULL.                                        ELGIBGR 
00258      IF CIA-RC-PTR-NULL                                           ELGIBGR 
00259         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGIBGR 
00260         EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.            ELGIBGR 
00261                                                                   ELGIBGR 
00262 ***************************************************************   ELGIBGR 
00263 *                                                             *   ELGIBGR 
00264 *       S I N G L E   L E V E L   D I S P L A Y               *   ELGIBGR 
00265 *                                                             *   ELGIBGR 
00266 ***************************************************************   ELGIBGR 
00267  SINGLE-LEVEL-DISPLAY.                                            ELGIBGR 
00268      PERFORM READ-INTERNAL-TABULAR-RECORD.                        ELGIBGR 
00269      PERFORM LIST-TABULAR-HEADING.                                ELGIBGR 
00270      PERFORM LIST-TABULAR-CONTENTS                                ELGIBGR 
00271        VARYING GX1-INDEX FROM 1 BY 1                              ELGIBGR 
00272          UNTIL GX1-INDEX = GX1-ENTRY-COUNT OR                     ELGIBGR 
00273          GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) = HIGH-VALUES.     ELGIBGR 
00274      PERFORM INSERT-BLANK-LINE.                                   ELGIBGR 
00275                                                                   ELGIBGR 
00276 ***************************************************************   ELGIBGR 
00277 *                                                             *   ELGIBGR 
00278 *       V A R I A B L E   L E V E L   D I S P L A Y           *   ELGIBGR 
00279 *                                                             *   ELGIBGR 
00280 ***************************************************************   ELGIBGR 
00281  VARIABLE-LEVEL-DISPLAY.                                          ELGIBGR 
00282      MOVE 1 TO WS-CUR-SUB.                                        ELGIBGR 
00283      SET VLT-INDEX TO 1.                                          ELGIBGR 
00284      PERFORM EXTRACT-PERCENT-N-SLOT-NBR                           ELGIBGR 
00285        VARYING ASC-DES-INDEX FROM 1 BY 1                          ELGIBGR 
00286          UNTIL ASC-DES-INDEX > ACCUM-ASCEND-DESCEND-COUNT.        ELGIBGR 
00287      MOVE WS-IBGR-COUNT TO WS-CUR-SUB.                            ELGIBGR 
00288      ADD 1 TO WS-CUR-SUB.                                         ELGIBGR 
00289      MOVE HIGH-VALUES TO WS-IBGR-STATUS (WS-CUR-SUB).             ELGIBGR 
00290      PERFORM IBGR-DISPLAY-PROCESSING                              ELGIBGR 
00291        VARYING WS-CUR-SUB FROM 1 BY 1                             ELGIBGR 
00292          UNTIL WS-IBGR-STATUS (WS-CUR-SUB) = HIGH-VALUES.         ELGIBGR 
00293                                                                   ELGIBGR 
00294 ************************************************************      ELGIBGR 
00295 *        READ INTERNAL TABULAR RECORD                      *      ELGIBGR 
00296 ************************************************************      ELGIBGR 
00297  READ-INTERNAL-TABULAR-RECORD.                                    ELGIBGR 
00298      SET CIA-GCTABULR-DDN TO TRUE.                                ELGIBGR 
00299      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIBGR 
00300          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGIBGR 
00301      IF CIA-RC-PTR-NULL                                           ELGIBGR 
00302         SET CIA-STG-GETMAIN TO TRUE                               ELGIBGR 
00303         EXEC CICS LINK  PROGRAM('ELUSTGMG')                       ELGIBGR 
00304                         COMMAREA(DFHCOMMAREA)  END-EXEC           ELGIBGR 
00305         SET CIA-GCTABULR-DDN TO TRUE                              ELGIBGR 
00306         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGIBGR 
00307                  ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.          ELGIBGR 
00308      MOVE '#IBGR ' TO KWA-PROVISION-ID.                           ELGIBGR 
00309      MOVE ACCUM-IBGR-SLOT-NBR (ASC-DES-INDEX)                     ELGIBGR 
00310                           TO KWA-PROVISION-SLOT-NO.               ELGIBGR 
00311      SET IOP-RD TO TRUE.                                          ELGIBGR 
00312      SET IOP-FCQ-NONE TO TRUE.                                    ELGIBGR 
00313      SET IOP-KVQ-EQ TO TRUE.                                      ELGIBGR 
00314      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGIBGR 
00315      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGIBGR 
00316      SET IOP-REC-PTR TO NULL.                                     ELGIBGR 
00317      SET IOP-STG-MODE-MOVE TO TRUE.                               ELGIBGR 
00318      EXEC CICS LINK  PROGRAM('ELUIOPGM')                          ELGIBGR 
00319                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELGIBGR 
00320      IF IOP-RC-OK                                                 ELGIBGR 
00321         SET ADDRESS OF GX1-TABULAR-REC-AREA TO IOP-REC-PTR        ELGIBGR 
00322      ELSE                                                         ELGIBGR 
00323         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELGIBGR 
00324         EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.            ELGIBGR 
00325                                                                   ELGIBGR 
00326 ****************************************************************  ELGIBGR 
00327 *  E X T R A C T   P E R C E N T   &   S L O T   N B R         *  ELGIBGR 
00328 ****************************************************************  ELGIBGR 
00329  EXTRACT-PERCENT-N-SLOT-NBR.                                      ELGIBGR 
00330      IF ACCUM-IBGR-SLOT-NBR (ASC-DES-INDEX) > ZERO                ELGIBGR 
00331         MOVE ACCUM-PERCENT-LEVEL (ASC-DES-INDEX)                  ELGIBGR 
00332           TO WS-IBGR-PERCENT (WS-CUR-SUB)                         ELGIBGR 
00333         MOVE ACCUM-IBGR-SLOT-NBR (ASC-DES-INDEX)                  ELGIBGR 
00334           TO WS-IBGR-SLOT-NBR (WS-CUR-SUB)                        ELGIBGR 
00335         MOVE VLT-VARIABLE-LEVEL-TAG (VLT-INDEX)                   ELGIBGR 
00336           TO WS-IBGR-LEVEL (WS-CUR-SUB)                           ELGIBGR 
00337         SET WS-IBGR-NOT-PROCESSED (WS-CUR-SUB) TO TRUE            ELGIBGR 
00338         ADD 1 TO WS-IBGR-COUNT.                                   ELGIBGR 
00339      ADD 1 TO WS-CUR-SUB.                                         ELGIBGR 
00340      SET VLT-INDEX UP BY 1.                                       ELGIBGR 
00341                                                                   ELGIBGR 
00342 ****************************************************************  ELGIBGR 
00343 *  I B G R   D I S P L A Y   P R O C E S S I N G               *  ELGIBGR 
00344 ****************************************************************  ELGIBGR 
00345  IBGR-DISPLAY-PROCESSING.                                         ELGIBGR 
00346      IF WS-IBGR-NOT-PROCESSED (WS-CUR-SUB)                        ELGIBGR 
00347         MOVE 1 TO WS-LVL-SUB                                      ELGIBGR 
00348         MOVE ZEROES TO WS-LEVEL-COUNT                             ELGIBGR 
00349         PERFORM PUT-EQUAL-SLOT-NBR-TO-LVL-TBL                     ELGIBGR 
00350           VARYING WS-NXT-SUB FROM WS-CUR-SUB BY 1                 ELGIBGR 
00351             UNTIL WS-IBGR-STATUS (WS-NXT-SUB) = HIGH-VALUES       ELGIBGR 
00352         SET ASC-DES-INDEX TO WS-CUR-SUB                           ELGIBGR 
00353         PERFORM READ-INTERNAL-TABULAR-RECORD                      ELGIBGR 
00354         PERFORM EDIT-CONNECT-PERCENT-LEVELS                       ELGIBGR 
00355           VARYING WS-LVL-SUB FROM 1 BY 1                          ELGIBGR 
00356             UNTIL WS-EDIT-COMPLETE                                ELGIBGR 
00357         PERFORM LIST-TABULAR-HEADING                              ELGIBGR 
00358         PERFORM LIST-TABULAR-CONTENTS                             ELGIBGR 
00359           VARYING GX1-INDEX FROM 1 BY 1                           ELGIBGR 
00360             UNTIL GX1-INDEX = GX1-ENTRY-COUNT OR                  ELGIBGR 
00361             GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) = HIGH-VALUES   ELGIBGR 
00362         PERFORM INSERT-BLANK-LINE.                                ELGIBGR 
00363                                                                   ELGIBGR 
00364 ****************************************************************  ELGIBGR 
00365 *  P U T   E Q U A L   S L O T   N B R   T O   L V L   T B L   *  ELGIBGR 
00366 ****************************************************************  ELGIBGR 
00367  PUT-EQUAL-SLOT-NBR-TO-LVL-TBL.                                   ELGIBGR 
00368      IF WS-IBGR-NOT-PROCESSED (WS-NXT-SUB) AND                    ELGIBGR 
00369         WS-IBGR-SLOT-NBR (WS-CUR-SUB) =                           ELGIBGR 
00370                           WS-IBGR-SLOT-NBR (WS-NXT-SUB)           ELGIBGR 
00371            MOVE WS-IBGR-PERCENT (WS-NXT-SUB) TO                   ELGIBGR 
00372                           WS-LEVEL-PERCENT (WS-LVL-SUB)           ELGIBGR 
00373            MOVE WS-IBGR-LEVEL   (WS-NXT-SUB) TO                   ELGIBGR 
00374                           WS-LEVEL-TAG (WS-LVL-SUB)               ELGIBGR 
00375            SET WS-IBGR-PROCESSED (WS-NXT-SUB) TO TRUE             ELGIBGR 
00376            ADD 1 TO WS-LVL-SUB                                    ELGIBGR 
00377            ADD 1 TO WS-LEVEL-COUNT.                               ELGIBGR 
00378                                                                   ELGIBGR 
00379 ***************************************************************   ELGIBGR 
00380 *   E D I T   C O N N E C T   P E R C E N T   L E V E L S     *   ELGIBGR 
00381 ***************************************************************   ELGIBGR 
00382  EDIT-CONNECT-PERCENT-LEVELS.                                     ELGIBGR 
00383      IF WS-LVL-SUB = 1 AND                                        ELGIBGR 
00384         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIBGR 
00385         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIBGR 
00386         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIBGR 
00387                                                                   ELGIBGR 
00388      IF WS-LVL-SUB = 2 AND                                        ELGIBGR 
00389         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIBGR 
00390         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIBGR 
00391         SUBTRACT 1 FROM WS-LVL-SUB                                ELGIBGR 
00392         SET WS-LEVEL-PCT-AND (WS-LVL-SUB) TO TRUE                 ELGIBGR 
00393         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIBGR 
00394                                                                   ELGIBGR 
00395      IF WS-LVL-SUB > 2 AND                                        ELGIBGR 
00396         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIBGR 
00397         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIBGR 
00398         SUBTRACT 1 FROM WS-LVL-SUB                                ELGIBGR 
00399         SET WS-LEVEL-PCT-AND (WS-LVL-SUB) TO TRUE                 ELGIBGR 
00400         SET WS-EDIT-COMPLETE     TO TRUE                          ELGIBGR 
00401      ELSE                                                         ELGIBGR 
00402      IF WS-LVL-SUB > 2 AND                                        ELGIBGR 
00403         WS-LVL-SUB NOT EQUAL WS-LEVEL-COUNT                       ELGIBGR 
00404            SET WS-LEVEL-PCT-COMMA (WS-LVL-SUB) TO TRUE.           ELGIBGR 
00405                                                                   ELGIBGR 
00406 ***************************************************************   ELGIBGR 
00407 *  L I S T   T A B U L A R   H E A D I N G                    *   ELGIBGR 
00408 ***************************************************************   ELGIBGR 
00409  LIST-TABULAR-HEADING.                                            ELGIBGR 
00410      INITIALIZE TCAR-FROM-AREA.                                   ELGIBGR 
00411      MOVE WS-IBGR TO CMF-RECORD-PREFIX.                           ELGIBGR 
00412      MOVE 'INCLUDE-EXCLUDE-IND'   TO CMF-ELEMENT-SYSTEM-NAME.     ELGIBGR 
00413      MOVE GX1-INCLUDE-EXCLUDE-IND TO CMF-CODE-VALUE.              ELGIBGR 
00414      EXEC CICS LINK  PROGRAM ('ELUCMIF')                          ELGIBGR 
00415                      COMMAREA (DFHCOMMAREA)  END-EXEC.            ELGIBGR 
00416      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGIBGR 
00417      CALL 'ELUSETAD' USING DFHCOMMAREA ADDRESS OF CMF-DESCR.      ELGIBGR 
00418      EVALUATE TRUE                                                ELGIBGR 
00419         WHEN ACCUM-ACL                                            ELGIBGR 
00420              MOVE 1 TO WS-ACM-SUB                                 ELGIBGR 
00421         WHEN ACCUM-ADL                                            ELGIBGR 
00422              MOVE 2 TO WS-ACM-SUB                                 ELGIBGR 
00423         WHEN ACCUM-ABM                                            ELGIBGR 
00424              MOVE 3 TO WS-ACM-SUB                                 ELGIBGR 
00425         WHEN ACCUM-AOL                                            ELGIBGR 
00426              MOVE 4 TO WS-ACM-SUB                                 ELGIBGR 
00427         WHEN ACCUM-ACP                                            ELGIBGR 
00428              MOVE 5 TO WS-ACM-SUB                                 ELGIBGR 
00429      END-EVALUATE.                                                ELGIBGR 
00430      IF (ACCUM-ADL OR ACCUM-ABM OR ACCUM-ACP)                     ELGIBGR 
00431          PERFORM STRING-FIRST-LINE-FOR-DEDUCTIB                   ELGIBGR 
00432      ELSE                                                         ELGIBGR 
00433         IF (ACCUM-ACL OR ACCUM-AOL)                               ELGIBGR 
00434            PERFORM STRING-FIRST-LINE-FOR-COINSURA.                ELGIBGR 
00435      PERFORM DO-TEXT-COMPRESSION.                                 ELGIBGR 
00436      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELGIBGR 
00437      MOVE +04 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGIBGR 
00438      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                          ELGIBGR 
00439                  TCAR-OUTPUT-FIELD-2-LEN                          ELGIBGR 
00440                  TCAR-OUTPUT-FIELD-3-LEN                          ELGIBGR 
00441                  TCAR-OUTPUT-FIELD-4-LEN.                         ELGIBGR 
00442      PERFORM DO-TEXT-UNSTRING.                                    ELGIBGR 
00443      PERFORM MOVE-ASTERISK-LINE-TO-OUTPUT-A.                      ELGIBGR 
00444      PERFORM MOVE-COMPRESSED-PHRASE                               ELGIBGR 
00445        VARYING WS-SUB FROM 1 BY 1                                 ELGIBGR 
00446          UNTIL WS-SUB GREATER THAN TCAR-OUTPUT-FIELDS-USED.       ELGIBGR 
00447      PERFORM CALL-OUTPUT.                                         ELGIBGR 
00448                                                                   ELGIBGR 
00449 ****************************************************************  ELGIBGR 
00450 *  S T R I N G   F I R S T   L I N E   F O R   D E D           *  ELGIBGR 
00451 ****************************************************************  ELGIBGR 
00452  STRING-FIRST-LINE-FOR-DEDUCTIB.                                  ELGIBGR 
00453      STRING 'THIS '                   DELIMITED BY SIZE           ELGIBGR 
00454          WS-ACCUM-NAME (WS-ACM-SUB)   DELIMITED BY ' '            ELGIBGR 
00455             ' ACCUMULATOR '           DELIMITED BY SIZE           ELGIBGR 
00456             CMF-DESCR-LINE(1)         DELIMITED BY '  '           ELGIBGR 
00457             ' THE FOLLOWING BENEFIT PROVISIONS:'                  ELGIBGR 
00458                                       DELIMITED BY SIZE           ELGIBGR 
00459                                       INTO TCAR-FROM-AREA.        ELGIBGR 
00460                                                                   ELGIBGR 
00461 ****************************************************************  ELGIBGR 
00462 *  S T R I N G   F I R S T   L I N E   F O R   C O I N S U R A *  ELGIBGR 
00463 ****************************************************************  ELGIBGR 
00464  STRING-FIRST-LINE-FOR-COINSURA.                                  ELGIBGR 
00465      IF ACCUM-ASCEND-DESCEND-COUNT = 1                            ELGIBGR 
00466         MOVE 1 TO WS-LEVEL-COUNT                                  ELGIBGR 
00467         MOVE SPACES TO WS-LEVEL-TAG (1)                           ELGIBGR 
00468         MOVE ACCUM-PERCENT-LEVEL (1) TO WS-LEVEL-PERCENT (1)      ELGIBGR 
00469         SET WS-LEVEL-PCT (1) TO TRUE.                             ELGIBGR 
00470                                                                   ELGIBGR 
00471      STRING 'THE '                    DELIMITED BY SIZE           ELGIBGR 
00472          WS-ACCUM-NAME (WS-ACM-SUB)   DELIMITED BY ' '            ELGIBGR 
00473             ' ACCUMULATOR '           DELIMITED BY SIZE           ELGIBGR 
00474             WS-SERVICES-PAID-PHRASE   DELIMITED BY SIZE           ELGIBGR 
00475             WS-LEVEL-TABLE            DELIMITED BY SIZE           ELGIBGR 
00476             CMF-DESCR-LINE(1)         DELIMITED BY '  '           ELGIBGR 
00477             ' THE FOLLOWING BENEFIT PROVISIONS:'                  ELGIBGR 
00478                                       DELIMITED BY SIZE           ELGIBGR 
00479                                       INTO TCAR-FROM-AREA.        ELGIBGR 
00480                                                                   ELGIBGR 
00481 ****************************************************************  ELGIBGR 
00482 *  L I S T   T A B U L A R   C O N T E N T S                   *  ELGIBGR 
00483 ****************************************************************  ELGIBGR 
00484  LIST-TABULAR-CONTENTS.                                           ELGIBGR 
00485      PERFORM TRANSLATE-BEN-PROV-ID.                               ELGIBGR 
00486      PERFORM TRANSLATE-BEN-PROV-SUFFIX.                           ELGIBGR 
00487      PERFORM FORMAT-BEN-PROV-DESCRIPTION.                         ELGIBGR 
00488                                                                   ELGIBGR 
00489 ****************************************************************  ELGIBGR 
00490 *  T R A N S L A T E   B E N   P R O V   I D                   *  ELGIBGR 
00491 ****************************************************************  ELGIBGR 
00492  TRANSLATE-BEN-PROV-ID.                                           ELGIBGR 
00493      MOVE 0 TO TCAR-FROM-SUB.                                     ELGIBGR 
00494      MOVE 'BP' TO CMF-RECORD-PREFIX.                              ELGIBGR 
00495      MOVE GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) TO WS-BEN-ID.     ELGIBGR 
00496      MOVE 'BEN-PR-ID' TO CMF-ELEMENT-SYSTEM-NAME.                 ELGIBGR 
00497      MOVE WS-BEN-ID-5 TO CMF-CODE-VALUE.                          ELGIBGR 
00498      EXEC CICS LINK  PROGRAM ('ELUCMIF')                          ELGIBGR 
00499                      COMMAREA (DFHCOMMAREA)  END-EXEC.            ELGIBGR 
00500      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGIBGR 
00501      CALL 'ELUSETAD' USING DFHCOMMAREA ADDRESS OF CMF-DESCR.      ELGIBGR 
00502      PERFORM WITH TEST BEFORE                                     ELGIBGR 
00503        VARYING TCAR-FROM-SUB FROM 1 BY 1                          ELGIBGR 
00504        UNTIL TCAR-FROM-SUB > CMF-NBR-DESCR-LINES                  ELGIBGR 
00505                                                                   ELGIBGR 
00506        MOVE CMF-DESCR-LINE (TCAR-FROM-SUB)                        ELGIBGR 
00507                          TO TCAR-FROM-LINE (TCAR-FROM-SUB)        ELGIBGR 
00508      END-PERFORM.                                                 ELGIBGR 
00509                                                                   ELGIBGR 
00510 ****************************************************************  ELGIBGR 
00511 *  T R A N S L A T E   B E N   P R O V   S U F F I X           *  ELGIBGR 
00512 ****************************************************************  ELGIBGR 
00513  TRANSLATE-BEN-PROV-SUFFIX.                                       ELGIBGR 
00514      IF WS-BEN-ID-1  = 'A' OR 'B' OR 'W'                          ELGIBGR 
00515         STRING TCAR-FROM-AREA  DELIMITED BY '  '                  ELGIBGR 
00516           ' INSTITUTIONAL ' DELIMITED BY SIZE                     ELGIBGR 
00517                             INTO TCAR-FROM-AREA                   ELGIBGR 
00518      ELSE                                                         ELGIBGR 
00519         IF WS-BEN-ID-1 = 'C' OR 'D' OR 'E'                        ELGIBGR 
00520         STRING TCAR-FROM-AREA DELIMITED BY '  '                   ELGIBGR 
00521           ' PROFESSIONAL ' DELIMITED BY SIZE                      ELGIBGR 
00522                            INTO TCAR-FROM-AREA.                   ELGIBGR 
00523                                                                   ELGIBGR 
00524 ****************************************************************  ELGIBGR 
00525 *  F O R M A T   B E N   P R O V   D E S C R I P T I O N       *  ELGIBGR 
00526 ****************************************************************  ELGIBGR 
00527  FORMAT-BEN-PROV-DESCRIPTION.                                     ELGIBGR 
00528      PERFORM DO-TEXT-COMPRESSION.                                 ELGIBGR 
00529      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELGIBGR 
00530      MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGIBGR 
00531      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                          ELGIBGR 
00532                  TCAR-OUTPUT-FIELD-2-LEN.                         ELGIBGR 
00533      PERFORM DO-TEXT-UNSTRING.                                    ELGIBGR 
00534      PERFORM MOVE-COMPRESSED-PHRASE                               ELGIBGR 
00535        VARYING WS-SUB FROM 1 BY 1 UNTIL                           ELGIBGR 
00536          WS-SUB GREATER THAN TCAR-OUTPUT-FIELDS-USED.             ELGIBGR 
00537      PERFORM CALL-OUTPUT.                                         ELGIBGR 
00538                                                                   ELGIBGR 
00539 ****************************************************************  ELGIBGR 
00540 *  I N S E R T   B L A N K   L I N E                           *  ELGIBGR 
00541 ****************************************************************  ELGIBGR 
00542  INSERT-BLANK-LINE.                                               ELGIBGR 
00543      ADD  1       TO  COF-NBR-DTL-LINES.                          ELGIBGR 
00544      MOVE SPACES  TO  COF-DTL-LINE (COF-NBR-DTL-LINES).           ELGIBGR 
00545      PERFORM CALL-OUTPUT.                                         ELGIBGR 
00546                                                                   ELGIBGR 
00547 ****************************************************************  ELGIBGR 
00548 *  M O V E   C O M P R E S S E D   P H R A S E                 *  ELGIBGR 
00549 ****************************************************************  ELGIBGR 
00550  MOVE-COMPRESSED-PHRASE.                                          ELGIBGR 
00551      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGIBGR 
00552      MOVE TCAR-OPF-DATA (WS-SUB) TO COF-DTL-LINE                  ELGIBGR 
00553          (COF-NBR-DTL-LINES).                                     ELGIBGR 
00554                                                                   ELGIBGR 
00555 ****************************************************************  ELGIBGR 
00556 *  M O V E   A S T E R I S K   L I N E   T O   O U T P U T     *  ELGIBGR 
00557 ****************************************************************  ELGIBGR 
00558  MOVE-ASTERISK-LINE-TO-OUTPUT-A.                                  ELGIBGR 
00559      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGIBGR 
00560      MOVE WS-FLAG-LINE  TO COF-DTL-LINE (COF-NBR-DTL-LINES).      ELGIBGR 
00561                                                                   ELGIBGR 
00562 ****************************************************************  ELGIBGR 
00563 *  D O   T E X T   C O M P R E S S I O N                       *  ELGIBGR 
00564 ****************************************************************  ELGIBGR 
00565  DO-TEXT-COMPRESSION.                                             ELGIBGR 
00566      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGIBGR 
00567                                                                   ELGIBGR 
00568 ****************************************************************  ELGIBGR 
00569 *  D O   T E X T   U N S T R I N G                             *  ELGIBGR 
00570 ****************************************************************  ELGIBGR 
00571  DO-TEXT-UNSTRING.                                                ELGIBGR 
00572      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGIBGR 
00573                                                                   ELGIBGR 
00574 ****************************************************************  ELGIBGR 
00575 *  C A L L   O U T P U T                                       *  ELGIBGR 
00576 ****************************************************************  ELGIBGR 
00577  CALL-OUTPUT.                                                     ELGIBGR 
00578      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELGIBGR 
00579                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGIBGR 
00580      INITIALIZE TCAR-FROM-AREA.                                   ELGIBGR 
