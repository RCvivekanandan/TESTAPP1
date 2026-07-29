00001  IDENTIFICATION DIVISION.                                         09/03/03
00002 *                                                                 ELGIPGT 
00003  PROGRAM-ID.         ELGIPGT.                                        LV002
00004 *                                                                 ELGIPGT 
00005  AUTHOR.             ALIDA JATICH OF T. M. FLOYD, INC.            ELGIPGT 
00006 *                                                                 ELGIPGT 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGIPGT 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELGIPGT 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGIPGT 
00010                      233 N. MICHIGAN AVE                          ELGIPGT 
00011                      CHICAGO, ILLINOIS 60601                      ELGIPGT 
00012 *                                                                 ELGIPGT 
00013  DATE-WRITTEN.       31-OCT-1986.                                 ELGIPGT 
00014 *                                                                 ELGIPGT 
00015  DATE-COMPILED.                                                   ELGIPGT 
00016 *                                                                 ELGIPGT 
00017  SECURITY.           COPYRIGHT 1986,                              ELGIPGT 
00018                      HEALTH CARE SERVICE CORPORATION              ELGIPGT 
00019 *                                                                 ELGIPGT 
00020 ******************************************************************ELGIPGT 
00021 *   ELGIPGT                                                      *ELGIPGT 
00022 *                                                                *ELGIPGT 
00023 *                        PROGRAM ABSTRACT                        *ELGIPGT 
00024 *                                                                *ELGIPGT 
00025 *   PROGRAM NAME:   E.L.S. INTERNAL TABULAR TRANSLATOR           *ELGIPGT 
00026 *                   SUBROUTINE                                   *ELGIPGT 
00027 *                                                                *ELGIPGT 
00028 *   PROGRAM I.D.:   ELGIPGT                                      *ELGIPGT 
00029 *                                                                *ELGIPGT 
00030 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE FIELDS *ELGIPGT 
00031 *              IN THE #IPGT INTERNAL TABULAR                     *ELGIPGT 
00032 *                                                                *ELGIPGT 
00033 *   RECORDS                                                      *ELGIPGT 
00034 *   ACCESSED:  #IPGT INTERNAL TABULAR                            *ELGIPGT 
00035 *                                                                *ELGIPGT 
00036 ******************************************************************ELGIPGT 
00037 *                                                                *ELGIPGT 
00038 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGIPGT 
00039 *       *-*         U P D A T E   H I S T O R Y         *-*      *ELGIPGT 
00040 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGIPGT 
00041 *                                                                *ELGIPGT 
00042 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELGIPGT 
00043 *                                                                *ELGIPGT 
00044 *  ELS 2.0   10/31/86  AMJ  ORIGINAL MODULE WRITTEN.             *ELGIPGT 
00045 *                                                                *ELGIPGT 
00046 *            01/23/87  NAC  TRANSLATE PROVIDER CODE FROM #PVE    *ELGIPGT 
00047 *                           INSTEAD OF #IPGT.                    *ELGIPGT 
00048 *                                                                *ELGIPGT 
00049 *            02/04/87  LET  CHANGED VERBAGE 'COVERAGE' TO        *ELGIPGT 
00050 *                           'THIS ACCUMULATOR'.                  *ELGIPGT 
00051 *                                                                *ELGIPGT 
00052 *            03/11/87  NAC  RESTRUCTURE OUTPUT SENTENCE TO       *ELGIPGT 
00053 *                           INCLUDE ACCUMULATOR TYPE.            *ELGIPGT 
00054 *            07/15/87  NAC  CORRECT LOGIC OF LINE COUNT TO       *ELGIPGT 
00055 *                           OUTPUT.                              *ELGIPGT 
00056 *                                                                *ELGIPGT 
00057 *  01.01     11/09/87  REB  CHANGED VERBAGE OF FIRST LINE OF     *ELGIPGT 
00058 *                           OUTPUT TO INCLUDE THE PERCENT LEVEL  *ELGIPGT 
00059 *                           PASSED IN THE SRP BLOCK.             *ELGIPGT 
00060 *                                                                *ELGIPGT 
00061 *  01.02     11/12/87  REB  CHANGED CODE TO ALLOW PERCENT LEVEL  *ELGIPGT 
00062 *                           TO PRINT ON FIRST OUTPUT LINE.       *ELGIPGT 
00063 *                                                                *ELGIPGT 
00064 *  01.03     11/18/87  REB  DISPLAY A DIFFERENT OUTPUT LINE FOR  *ELGIPGT 
00065 *                           ABM AND ADL ACCUMS.                  *ELGIPGT 
00066 *                                                                *ELGIPGT 
00067 *  01.04     11/23/87  REB  DISPLAY ASTERICKS ON FIRST OUTPUT    *ELGIPGT 
00068 *                           LINE TO DISTINGUISH INFO FROM OTHER  *ELGIPGT 
00069 *                           TABS. PER AUGGIE.                    *ELGIPGT 
00070 *                                                                *ELGIPGT 
00071 *  02.00     03/31/89  NAC  DESTRUCT CONVERSION USING STRUCTURES *ELGIPGT 
00072 *                           VER: 3.5.                            *ELGIPGT 
00073 *                                                                *ELGIPGT 
00074 *  03.00     04/04/89  GEM  STORAGE MANAGEMENT ENHANCEMENTS      *ELGIPGT 
00075 *                                                                *ELGIPGT 
00076 *  04.00     02/07/92  GEM  ACCUM DISPLAY FIX ISSR# 12010.       *ELGIPGT 
00077 *                                                                *ELGIPGT 
00078 *  05.00     10/30/98  AKK  ADD SUPPORT FOR ACP TABULAR.         *ELGIPGT 
00079 *            08/12/03  AKK  REGEN'D TO CHECK ORDER OF THE COMPILE*ELGIPGT 
00080 *                                                                *ELGIPGT 
ED0624* BBDA-58217 06/14/24  ED   RECOMPILE FOR PEAQ COPYBOOK          *        
ED0624*                           EXPANSION:                           *        
ED0624*                                 COPYBOOK ELSACUMC              *        
ED0624*                                                                *        
00081 ******************************************************************ELGIPGT 
00082 /                                                                 ELGIPGT 
00083  ENVIRONMENT DIVISION.                                            ELGIPGT 
00084  CONFIGURATION SECTION.                                           ELGIPGT 
00085  SOURCE-COMPUTER.    IBM-3090.                                    ELGIPGT 
00086  OBJECT-COMPUTER.    IBM-3090.                                    ELGIPGT 
00087 /                                                                 ELGIPGT 
00088  DATA DIVISION.                                                   ELGIPGT 
00089  WORKING-STORAGE SECTION.                                         ELGIPGT 
00090  01  WS-BEGIN               PIC X(24)  VALUE                      ELGIPGT 
00091      'ELGIPGT WORKING STORAGE*'.                                  ELGIPGT 
00092 /     W O R K F I E L D S   A N D   S W I T C H E S               ELGIPGT 
00093  01  WS-MISC-WORK.                                                ELGIPGT 
00094      05  WS-SUB             PIC S9(4) COMP SYNC VALUE ZEROES.     ELGIPGT 
00095      05  WS-ACM-SUB         PIC S9(4) COMP SYNC VALUE ZEROES.     ELGIPGT 
00096      05  WS-CUR-SUB         PIC S9(4) COMP SYNC VALUE ZEROES.     ELGIPGT 
00097      05  WS-NXT-SUB         PIC S9(4) COMP SYNC VALUE ZEROES.     ELGIPGT 
00098      05  WS-TAG-SUB         PIC S9(4) COMP SYNC VALUE ZEROES.     ELGIPGT 
00099      05  WS-LVL-SUB         PIC S9(4) COMP SYNC VALUE ZEROES.     ELGIPGT 
00100      05  WS-IPGT            PIC X(5)   VALUE '#IPGT'.             ELGIPGT 
00101      05  WS-PVE             PIC X(5)   VALUE '#PVE '.             ELGIPGT 
00102      05  WS-BEN-ID.                                               ELGIPGT 
00103          10  WS-BEN-ID-5    PIC X(5)   VALUE SPACES.              ELGIPGT 
00104          10  WS-BEN-ID-1    PIC X      VALUE SPACES.              ELGIPGT 
00105      05  WS-GETMAIN-SWITCH  PIC X      VALUE 'N'.                 ELGIPGT 
00106          88  GETMAIN-DONE              VALUE 'Y'.                 ELGIPGT 
00107      05  WS-EDIT-COMP       PIC X      VALUE 'N'.                 ELGIPGT 
00108          88  WS-EDIT-COMPLETE          VALUE 'Y'.                 ELGIPGT 
00109  01  FILLER.                                                      ELGIPGT 
00110    02  WS-ACCUMULATOR-TYPE.                                       ELGIPGT 
00111      03  FILLER   PIC X(21)  VALUE                                ELGIPGT 
00112         '#ACL    COINSURANCE  '.                                  ELGIPGT 
00113      03  FILLER   PIC X(21)  VALUE                                ELGIPGT 
00114         '#ADL    DEDUCTIBLE   '.                                  ELGIPGT 
00115      03  FILLER   PIC X(21)  VALUE                                ELGIPGT 
00116         '#ABM    MAXIMUM      '.                                  ELGIPGT 
00117      03  FILLER   PIC X(21)  VALUE                                ELGIPGT 
00118         '#AOL    OUT-OF-POCKET'.                                  ELGIPGT 
00119      03  FILLER   PIC X(21)  VALUE                                ELGIPGT 
00120         '#ACP    CO-PAY       '.                                  ELGIPGT 
00121    02  WS-ACCUMULATOR-TYPE-TABLE REDEFINES  WS-ACCUMULATOR-TYPE.  ELGIPGT 
00122      03  WS-ACCUMULATOR-TYPE-TBL  OCCURS 5 TIMES                  ELGIPGT 
00123                                   INDEXED BY ACUM-IDX.            ELGIPGT 
00124          05  WS-ACCUM-TYPE       PIC X(08).                       ELGIPGT 
00125          05  WS-ACCUM-NAME       PIC X(13).                       ELGIPGT 
00126                                                                   ELGIPGT 
00127  01  WS-IPGT-VARIABLE-AREA.                                       ELGIPGT 
00128    02  WS-IPGT-COUNT   COMP-3    PIC S9(03) VALUE ZEROES.         ELGIPGT 
00129    02  WS-IPGT-VARIABLES                                          ELGIPGT 
00130           OCCURS  46  TIMES  DEPENDING  ON  WS-IPGT-COUNT.        ELGIPGT 
00131      03  WS-IPGT-VARIABLE.                                        ELGIPGT 
00132        05  WS-IPGT-SLOT-NBR      PIC S9(07) COMP-3.               ELGIPGT 
00133        05  WS-IPGT-LEVEL         PIC X(04).                       ELGIPGT 
00134        05  WS-IPGT-PERCENT       PIC S9(03) COMP-3.               ELGIPGT 
00135        05  WS-IPGT-STATUS        PIC X(01).                       ELGIPGT 
00136          88  WS-IPGT-PROCESSED       VALUE 'Y'.                   ELGIPGT 
00137          88  WS-IPGT-NOT-PROCESSED   VALUE 'N'.                   ELGIPGT 
00138                                                                   ELGIPGT 
00139  01  WS-LEVEL-AREA.                                               ELGIPGT 
00140    02  WS-LEVEL-COUNT  COMP-3    PIC S9(03) VALUE ZEROES.         ELGIPGT 
00141    02  WS-LEVEL-TABLE.                                            ELGIPGT 
00142      03  WS-LEVEL-TABLE-ENTRIES                                   ELGIPGT 
00143            OCCURS 46 TIMES DEPENDING ON WS-LEVEL-COUNT.           ELGIPGT 
00144        05  WS-LEVEL-TAG          PIC X(04).                       ELGIPGT 
00145        05  WS-LEVEL-PERCENT      PIC ZZ9.                         ELGIPGT 
00146        05  WS-LEVEL-EDITOR       PIC X(06).                       ELGIPGT 
00147          88  WS-LEVEL-PCT            VALUE '%     '.              ELGIPGT 
00148          88  WS-LEVEL-PCT-COMMA      VALUE '%,    '.              ELGIPGT 
00149          88  WS-LEVEL-PCT-AND        VALUE '% AND '.              ELGIPGT 
00150                                                                   ELGIPGT 
00151  01  PROGRAM-CONSTANTS.                                           ELGIPGT 
00152      05  PC-ABM                  PIC X(08)  VALUE '#ABM    '.     ELGIPGT 
00153      05  PC-ADL                  PIC X(08)  VALUE '#ADL    '.     ELGIPGT 
00154                                                                   ELGIPGT 
00155  01  WS-SERVICES-PAID-PHRASE.                                     ELGIPGT 
00156      05  FILLER                  PIC X(21)  VALUE                 ELGIPGT 
00157          'FOR SERVICES PAID AT '.                                 ELGIPGT 
00158                                                                   ELGIPGT 
00159  01  WS-PERCENT.                                                  ELGIPGT 
00160      05  WS-PERCENT-LEVEL        PIC ZZ9.                         ELGIPGT 
00161      05  FILLER                  PIC X(01)  VALUE '%'.            ELGIPGT 
00162      05  FILLER                  PIC X(01)  VALUE SPACE.          ELGIPGT 
00163                                                                   ELGIPGT 
00164  01  WS-FLAG-LINE.                                                ELGIPGT 
00165      05  FILLER                  PIC X(79)  VALUE '*****'.        ELGIPGT 
00166 /                                                                 ELGIPGT 
00167      COPY ELSVLTGC.                                               ELGIPGT 
00168                                                                   ELGIPGT 
00169  01  WS-END                 PIC X(16)  VALUE                      ELGIPGT 
00170      '*** W/S ENDS ***'.                                          ELGIPGT 
00171 /             L I N K A G E   S E C T I O N                       ELGIPGT 
00172  LINKAGE SECTION.                                                 ELGIPGT 
00173  01  DFHCOMMAREA.                                                 ELGIPGT 
00174      COPY ELSCOMMC.                                               ELGIPGT 
00175 /                                                                 ELGIPGT 
00176      COPY ELSCIA2C.                                               ELGIPGT 
00177 /                                                                 ELGIPGT 
00178      COPY ELSCMDSC.                                               ELGIPGT 
00179 /                                                                 ELGIPGT 
00180      COPY ELSCMIFC.                                               ELGIPGT 
00181 /                                                                 ELGIPGT 
00182      COPY ELSIOPMC.                                               ELGIPGT 
00183 /                                                                 ELGIPGT 
00184      COPY ELSKEYSC.                                               ELGIPGT 
00185 /                                                                 ELGIPGT 
00186      COPY ELSOUTPC.                                               ELGIPGT 
00187 /                                                                 ELGIPGT 
00188      COPY ELSTCWAC.                                               ELGIPGT 
00189 /                                                                 ELGIPGT 
00190      COPY ELSSRTPC.                                               ELGIPGT 
00191 /                                                                 ELGIPGT 
00192      COPY ELSACUMC.                                               ELGIPGT 
00193 /                                                                 ELGIPGT 
00194  01  GX3-TABULAR-REC-AREA.                                        ELGIPGT 
00195      COPY GCTIPGTC.                                               ELGIPGT 
00196 /  P R O C E D U R E   D I V I S I O N .                          ELGIPGT 
00197 ***************************************************************   ELGIPGT 
00198 *                                                             *   ELGIPGT 
00199 *  I N T E R N A L   T A B U L A R   D I S P L A Y   I P G T  *   ELGIPGT 
00200 *                                                             *   ELGIPGT 
00201 ***************************************************************   ELGIPGT 
00202  PROCEDURE DIVISION.                                              ELGIPGT 
00203  INTERNAL-TABULAR-DISPLAY-IPGT.                                   ELGIPGT 
00204      PERFORM INITIALIZATION-ROUTINE.                              ELGIPGT 
00205                                                                   ELGIPGT 
00206      IF ACCUM-ASCEND-DESCEND-COUNT = 1 AND                        ELGIPGT 
00207       (ACCUM-ABM OR ACCUM-ACL OR ACCUM-ADL OR ACCUM-AOL           ELGIPGT 
00208           OR ACCUM-ACP)                                           ELGIPGT 
00209          PERFORM SINGLE-LEVEL-DISPLAY                             ELGIPGT 
00210      ELSE                                                         ELGIPGT 
00211         IF ACCUM-ASCEND-DESCEND-COUNT > 1 AND                     ELGIPGT 
00212          (ACCUM-ACL OR ACCUM-AOL)                                 ELGIPGT 
00213             PERFORM VARIABLE-LEVEL-DISPLAY                        ELGIPGT 
00214         ELSE                                                      ELGIPGT 
00215            EXEC CICS ABEND  ABCODE('EL99')  END-EXEC.             ELGIPGT 
00216                                                                   ELGIPGT 
00217      GOBACK.                                                      ELGIPGT 
00218                                                                   ELGIPGT 
00219 ***************************************************************   ELGIPGT 
00220 *                                                             *   ELGIPGT 
00221 *  I N I T I A L I Z A T I O N   R O U T I N E                *   ELGIPGT 
00222 *                                                             *   ELGIPGT 
00223 ***************************************************************   ELGIPGT 
00224  INITIALIZATION-ROUTINE.                                          ELGIPGT 
00225      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELGIPGT 
00226         EXEC CICS ABEND  ABCODE('EL01')  END-EXEC.                ELGIPGT 
00227                                                                   ELGIPGT 
00228      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGIPGT 
00229          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELGIPGT 
00230      IF CIA-RC-PTR-NULL                                           ELGIPGT 
00231         EXEC CICS ABEND  ABCODE('EL02')  END-EXEC.                ELGIPGT 
00232                                                                   ELGIPGT 
00233      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGIPGT 
00234      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGT 
00235          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELGIPGT 
00236      PERFORM CHECK-CIA-RC-PTR-NULL.                               ELGIPGT 
00237                                                                   ELGIPGT 
00238      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGIPGT 
00239      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGT 
00240          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELGIPGT 
00241      PERFORM CHECK-CIA-RC-PTR-NULL.                               ELGIPGT 
00242                                                                   ELGIPGT 
00243      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGIPGT 
00244      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGT 
00245          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELGIPGT 
00246      PERFORM CHECK-CIA-RC-PTR-NULL.                               ELGIPGT 
00247                                                                   ELGIPGT 
00248      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGIPGT 
00249      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGT 
00250          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELGIPGT 
00251      PERFORM CHECK-CIA-RC-PTR-NULL.                               ELGIPGT 
00252                                                                   ELGIPGT 
00253      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGIPGT 
00254      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGT 
00255          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELGIPGT 
00256      PERFORM CHECK-CIA-RC-PTR-NULL.                               ELGIPGT 
00257                                                                   ELGIPGT 
00258      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGIPGT 
00259      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGT 
00260          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGIPGT 
00261      PERFORM CHECK-CIA-RC-PTR-NULL.                               ELGIPGT 
00262                                                                   ELGIPGT 
00263      SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR.       ELGIPGT 
00264                                                                   ELGIPGT 
00265      INITIALIZE CMF-CODES-MANUAL-INTERFACE                        ELGIPGT 
00266                 TCAR-FROM-AREA.                                   ELGIPGT 
00267                                                                   ELGIPGT 
00268  CHECK-CIA-RC-PTR-NULL.                                           ELGIPGT 
00269      IF CIA-RC-PTR-NULL                                           ELGIPGT 
00270         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGIPGT 
00271         EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.            ELGIPGT 
00272                                                                   ELGIPGT 
00273 ***************************************************************   ELGIPGT 
00274 *                                                             *   ELGIPGT 
00275 *  S I N G L E   L E V E L   D I S P L A Y                    *   ELGIPGT 
00276 *                                                             *   ELGIPGT 
00277 ***************************************************************   ELGIPGT 
00278  SINGLE-LEVEL-DISPLAY.                                            ELGIPGT 
00279      PERFORM READ-INTERNAL-TABULAR-RECORD.                        ELGIPGT 
00280      PERFORM LIST-TABULAR-HEADING.                                ELGIPGT 
00281      PERFORM LIST-TABULAR-CONTENTS                                ELGIPGT 
00282        VARYING GX3-INDEX FROM 1 BY 1                              ELGIPGT 
00283          UNTIL GX3-INDEX = GX3-ENTRY-COUNT OR                     ELGIPGT 
00284          GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX) = HIGH-VALUES.    ELGIPGT 
00285      PERFORM INSERT-BLANK-LINE.                                   ELGIPGT 
00286                                                                   ELGIPGT 
00287 ***************************************************************   ELGIPGT 
00288 *                                                             *   ELGIPGT 
00289 *  V A R I A B L E   L E V E L   D I S P L A Y                *   ELGIPGT 
00290 *                                                             *   ELGIPGT 
00291 ***************************************************************   ELGIPGT 
00292  VARIABLE-LEVEL-DISPLAY.                                          ELGIPGT 
00293      MOVE 1 TO WS-CUR-SUB.                                        ELGIPGT 
00294      SET VLT-INDEX TO 1.                                          ELGIPGT 
00295      PERFORM INITIALIZE-IPGT-VAR-TBLE                             ELGIPGT 
00296        VARYING WS-IPGT-COUNT FROM 1 BY 1                          ELGIPGT 
00297          UNTIL WS-IPGT-COUNT > 46.                                ELGIPGT 
00298      MOVE ZEROES TO WS-IPGT-COUNT.                                ELGIPGT 
00299      PERFORM EXTRACT-PERCENT-N-SLOT-NBR                           ELGIPGT 
00300        VARYING ASC-DES-INDEX FROM 1 BY 1                          ELGIPGT 
00301          UNTIL ASC-DES-INDEX > ACCUM-ASCEND-DESCEND-COUNT.        ELGIPGT 
00302      MOVE WS-IPGT-COUNT TO WS-CUR-SUB.                            ELGIPGT 
00303      ADD 1 TO WS-CUR-SUB.                                         ELGIPGT 
00304      MOVE HIGH-VALUES TO WS-IPGT-STATUS (WS-CUR-SUB).             ELGIPGT 
00305      PERFORM IPGT-DISPLAY-PROCESSING                              ELGIPGT 
00306        VARYING WS-CUR-SUB FROM 1 BY 1                             ELGIPGT 
00307          UNTIL WS-IPGT-STATUS (WS-CUR-SUB) = HIGH-VALUES.         ELGIPGT 
00308                                                                   ELGIPGT 
00309 ***************************************************************   ELGIPGT 
00310 *  I N I T I A L I Z E   I P G T   V A R   T B L E            *   ELGIPGT 
00311 ***************************************************************   ELGIPGT 
00312  INITIALIZE-IPGT-VAR-TBLE.                                        ELGIPGT 
00313      INITIALIZE WS-IPGT-VARIABLE (WS-IPGT-COUNT).                 ELGIPGT 
00314                                                                   ELGIPGT 
00315 ***************************************************************   ELGIPGT 
00316 *  E X T R A C T   P E R C E N T   &   S L O T   N B R        *   ELGIPGT 
00317 ***************************************************************   ELGIPGT 
00318  EXTRACT-PERCENT-N-SLOT-NBR.                                      ELGIPGT 
00319      IF ACCUM-IPGT-SLOT-NBR (ASC-DES-INDEX) > ZERO                ELGIPGT 
00320         MOVE ACCUM-PERCENT-LEVEL (ASC-DES-INDEX)                  ELGIPGT 
00321           TO WS-IPGT-PERCENT (WS-CUR-SUB)                         ELGIPGT 
00322         MOVE ACCUM-IPGT-SLOT-NBR (ASC-DES-INDEX)                  ELGIPGT 
00323           TO WS-IPGT-SLOT-NBR (WS-CUR-SUB)                        ELGIPGT 
00324         MOVE VLT-VARIABLE-LEVEL-TAG (VLT-INDEX)                   ELGIPGT 
00325           TO WS-IPGT-LEVEL (WS-CUR-SUB)                           ELGIPGT 
00326         SET WS-IPGT-NOT-PROCESSED (WS-CUR-SUB) TO TRUE            ELGIPGT 
00327         ADD 1 TO WS-IPGT-COUNT.                                   ELGIPGT 
00328      ADD 1 TO WS-CUR-SUB WS-TAG-SUB.                              ELGIPGT 
00329      SET VLT-INDEX UP BY 1.                                       ELGIPGT 
00330                                                                   ELGIPGT 
00331 ***************************************************************   ELGIPGT 
00332 *  I P G T   D I S P L A Y   P R O C E S S I N G              *   ELGIPGT 
00333 ***************************************************************   ELGIPGT 
00334  IPGT-DISPLAY-PROCESSING.                                         ELGIPGT 
00335      IF WS-IPGT-NOT-PROCESSED (WS-CUR-SUB)                        ELGIPGT 
00336         MOVE 1 TO WS-LVL-SUB                                      ELGIPGT 
00337         MOVE ZEROES TO WS-LEVEL-COUNT                             ELGIPGT 
00338         PERFORM PUT-EQUAL-SLOT-NBR-TO-LVL-TBL                     ELGIPGT 
00339           VARYING WS-NXT-SUB FROM WS-CUR-SUB BY 1                 ELGIPGT 
00340             UNTIL WS-IPGT-STATUS (WS-NXT-SUB) = HIGH-VALUES       ELGIPGT 
00341         SET ASC-DES-INDEX TO WS-CUR-SUB                           ELGIPGT 
00342         PERFORM READ-INTERNAL-TABULAR-RECORD                      ELGIPGT 
00343         PERFORM EDIT-CONNECT-PERCENT-LEVELS                       ELGIPGT 
00344           VARYING WS-LVL-SUB FROM 1 BY 1                          ELGIPGT 
00345             UNTIL WS-EDIT-COMPLETE                                ELGIPGT 
00346         PERFORM LIST-TABULAR-HEADING                              ELGIPGT 
00347         PERFORM LIST-TABULAR-CONTENTS                             ELGIPGT 
00348           VARYING GX3-INDEX FROM 1 BY 1                           ELGIPGT 
00349           UNTIL GX3-INDEX =  GX3-ENTRY-COUNT OR                   ELGIPGT 
00350           GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX) = HIGH-VALUES.   ELGIPGT 
00351         PERFORM INSERT-BLANK-LINE.                                ELGIPGT 
00352                                                                   ELGIPGT 
00353 ***************************************************************   ELGIPGT 
00354 *  P U T   E Q U A L   S L O T   N B R   T O   L V L   T B L  *   ELGIPGT 
00355 ***************************************************************   ELGIPGT 
00356  PUT-EQUAL-SLOT-NBR-TO-LVL-TBL.                                   ELGIPGT 
00357      IF WS-IPGT-NOT-PROCESSED (WS-NXT-SUB) AND                    ELGIPGT 
00358         WS-IPGT-SLOT-NBR (WS-CUR-SUB) =                           ELGIPGT 
00359                           WS-IPGT-SLOT-NBR (WS-NXT-SUB)           ELGIPGT 
00360            MOVE WS-IPGT-PERCENT (WS-NXT-SUB) TO                   ELGIPGT 
00361                              WS-LEVEL-PERCENT (WS-LVL-SUB)        ELGIPGT 
00362            MOVE WS-IPGT-LEVEL   (WS-NXT-SUB) TO                   ELGIPGT 
00363                              WS-LEVEL-TAG (WS-LVL-SUB)            ELGIPGT 
00364            SET WS-IPGT-PROCESSED (WS-NXT-SUB) TO TRUE             ELGIPGT 
00365            ADD 1 TO WS-LVL-SUB                                    ELGIPGT 
00366            ADD 1 TO WS-LEVEL-COUNT.                               ELGIPGT 
00367                                                                   ELGIPGT 
00368 ***************************************************************   ELGIPGT 
00369 *  R E A D   I N T E R N A L   T A B U L A R   R E C O R D    *   ELGIPGT 
00370 ***************************************************************   ELGIPGT 
00371  READ-INTERNAL-TABULAR-RECORD.                                    ELGIPGT 
00372      SET CIA-GCTABULR-DDN TO TRUE.                                ELGIPGT 
00373      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGT 
00374          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGIPGT 
00375      IF CIA-RC-PTR-NULL                                           ELGIPGT 
00376         SET CIA-STG-GETMAIN TO TRUE                               ELGIPGT 
00377         EXEC CICS LINK  PROGRAM ('ELUSTGMG')                      ELGIPGT 
00378                         COMMAREA(DFHCOMMAREA)  END-EXEC.          ELGIPGT 
00379      SET CIA-GCTABULR-DDN TO TRUE.                                ELGIPGT 
00380      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGT 
00381          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGIPGT 
00382      MOVE '#IPGT ' TO KWA-PROVISION-ID.                           ELGIPGT 
00383      MOVE ACCUM-IPGT-SLOT-NBR (ASC-DES-INDEX)                     ELGIPGT 
00384                    TO KWA-PROVISION-SLOT-NO.                      ELGIPGT 
00385      SET IOP-RD TO TRUE.                                          ELGIPGT 
00386      SET IOP-FCQ-NONE TO TRUE.                                    ELGIPGT 
00387      SET IOP-KVQ-EQ TO TRUE.                                      ELGIPGT 
00388      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGIPGT 
00389      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGIPGT 
00390      SET IOP-REC-PTR TO NULL.                                     ELGIPGT 
00391 ******************************************************************ELGIPGT 
00392 * WE ARE USING MOVE MODE BECAUSE THE CALLING PROGRAM IS ALSO     *ELGIPGT 
00393 * READING FROM THE SAME TABULAR FILE.  WHEN USING LOCATE MODE,   *ELGIPGT 
00394 * THE OLD RECORD IN THE DATA AREA IS FREED UP WHEN THE NEXT      *ELGIPGT 
00395 * RECORD IS READ FROM THE SAME FILE.  THIS WILL MESS UP THE      *ELGIPGT 
00396 * LOGIC IN THE CALLING PROGRAM, WHICH STILL NEEDS TO SEE THE     *ELGIPGT 
00397 * TABULAR RECORD.                                                *ELGIPGT 
00398 ******************************************************************ELGIPGT 
00399      SET IOP-STG-MODE-MOVE TO TRUE.                               ELGIPGT 
00400      EXEC CICS LINK  PROGRAM('ELUIOPGM')                          ELGIPGT 
00401                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELGIPGT 
00402      IF IOP-RC-OK                                                 ELGIPGT 
00403         SET ADDRESS OF GX3-TABULAR-REC-AREA TO IOP-REC-PTR        ELGIPGT 
00404      ELSE                                                         ELGIPGT 
00405         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELGIPGT 
00406         EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.            ELGIPGT 
00407                                                                   ELGIPGT 
00408 ***************************************************************   ELGIPGT 
00409 *   E D I T   C O N N E C T   P E R C E N T   L E V E L S     *   ELGIPGT 
00410 ***************************************************************   ELGIPGT 
00411  EDIT-CONNECT-PERCENT-LEVELS.                                     ELGIPGT 
00412      IF WS-LVL-SUB = 1 AND                                        ELGIPGT 
00413         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIPGT 
00414         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIPGT 
00415         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIPGT 
00416                                                                   ELGIPGT 
00417      IF WS-LVL-SUB = 2 AND                                        ELGIPGT 
00418         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIPGT 
00419         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIPGT 
00420         SUBTRACT 1 FROM WS-LVL-SUB                                ELGIPGT 
00421         SET WS-LEVEL-PCT-AND (WS-LVL-SUB) TO TRUE                 ELGIPGT 
00422         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIPGT 
00423                                                                   ELGIPGT 
00424      IF WS-LVL-SUB > 2 AND                                        ELGIPGT 
00425         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIPGT 
00426         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIPGT 
00427         SUBTRACT 1 FROM WS-LVL-SUB                                ELGIPGT 
00428         SET WS-LEVEL-PCT-AND (WS-LVL-SUB) TO TRUE                 ELGIPGT 
00429         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIPGT 
00430                                                                   ELGIPGT 
00431      IF WS-LVL-SUB > 2 AND                                        ELGIPGT 
00432         WS-LVL-SUB NOT EQUAL WS-LEVEL-COUNT                       ELGIPGT 
00433            SET WS-LEVEL-PCT-COMMA (WS-LVL-SUB) TO TRUE.           ELGIPGT 
00434                                                                   ELGIPGT 
00435 ***************************************************************   ELGIPGT 
00436 *  L I S T   T A B U L A R   H E A DI N G                     *   ELGIPGT 
00437 ***************************************************************   ELGIPGT 
00438  LIST-TABULAR-HEADING.                                            ELGIPGT 
00439      INITIALIZE TCAR-FROM-AREA.                                   ELGIPGT 
00440      MOVE WS-IPGT TO CMF-RECORD-PREFIX.                           ELGIPGT 
00441      MOVE 'INCLUDE-EXCLUDE-IND' TO                                ELGIPGT 
00442          CMF-ELEMENT-SYSTEM-NAME.                                 ELGIPGT 
00443      MOVE GX3-INCLUDE-EXCLUDE-IND TO CMF-CODE-VALUE.              ELGIPGT 
00444      EXEC CICS LINK  PROGRAM('ELUCMIF')                           ELGIPGT 
00445                      COMMAREA (DFHCOMMAREA)  END-EXEC.            ELGIPGT 
00446      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGIPGT 
00447      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGT 
00448          ADDRESS OF CMF-DESCR.                                    ELGIPGT 
00449      EVALUATE TRUE                                                ELGIPGT 
00450         WHEN ACCUM-ACL                                            ELGIPGT 
00451              MOVE 1 TO WS-ACM-SUB                                 ELGIPGT 
00452         WHEN ACCUM-ADL                                            ELGIPGT 
00453              MOVE 2 TO WS-ACM-SUB                                 ELGIPGT 
00454         WHEN ACCUM-ABM                                            ELGIPGT 
00455              MOVE 3 TO WS-ACM-SUB                                 ELGIPGT 
00456         WHEN ACCUM-AOL                                            ELGIPGT 
00457              MOVE 4 TO WS-ACM-SUB                                 ELGIPGT 
00458         WHEN ACCUM-ACP                                            ELGIPGT 
00459              MOVE 5 TO WS-ACM-SUB                                 ELGIPGT 
00460      END-EVALUATE.                                                ELGIPGT 
00461      IF (ACCUM-ADL OR ACCUM-ABM OR ACCUM-ACP)                     ELGIPGT 
00462          PERFORM STRING-FIRST-LINE-FOR-MAXIMUMX                   ELGIPGT 
00463      ELSE                                                         ELGIPGT 
00464         IF (ACCUM-ACL OR ACCUM-AOL)                               ELGIPGT 
00465            PERFORM STRING-FIRST-LINE-FOR-COINSURA.                ELGIPGT 
00466      PERFORM DO-TEXT-COMPRESSION.                                 ELGIPGT 
00467      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELGIPGT 
00468      MOVE +04 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGIPGT 
00469      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                          ELGIPGT 
00470                  TCAR-OUTPUT-FIELD-2-LEN                          ELGIPGT 
00471                  TCAR-OUTPUT-FIELD-3-LEN                          ELGIPGT 
00472                  TCAR-OUTPUT-FIELD-4-LEN.                         ELGIPGT 
00473      PERFORM DO-TEXT-UNSTRING.                                    ELGIPGT 
00474      PERFORM MOVE-ASTERICKS-TO-FIRST-OUTPUT.                      ELGIPGT 
00475      PERFORM MOVE-COMPRESSED-PHRASE                               ELGIPGT 
00476          VARYING TCAR-FROM-SUB FROM 1 BY 1 UNTIL                  ELGIPGT 
00477              TCAR-FROM-SUB GREATER THAN                           ELGIPGT 
00478             TCAR-OUTPUT-FIELDS-USED.                              ELGIPGT 
00479      PERFORM CALL-OUTPUT.                                         ELGIPGT 
00480                                                                   ELGIPGT 
00481                                                                   ELGIPGT 
00482 ***************************************************************   ELGIPGT 
00483 *  S T R I N G  F I R S T   L I N E   M A X I M U M           *   ELGIPGT 
00484 ***************************************************************   ELGIPGT 
00485  STRING-FIRST-LINE-FOR-MAXIMUMX.                                  ELGIPGT 
00486      STRING 'THIS '                        DELIMITED BY SIZE      ELGIPGT 
00487             WS-ACCUM-NAME (WS-ACM-SUB)     DELIMITED BY ' '       ELGIPGT 
00488             ' ACCUMULATOR '                DELIMITED BY SIZE      ELGIPGT 
00489             CMF-DESCR-LINE(1)              DELIMITED BY '  '      ELGIPGT 
00490           ' THE FOLLOWING PROVIDER TYPES:' DELIMITED BY SIZE      ELGIPGT 
00491             INTO TCAR-FROM-AREA.                                  ELGIPGT 
00492                                                                   ELGIPGT 
00493 ***************************************************************   ELGIPGT 
00494 *  S T R I N G   F I R S T   L I N E   C O I N S U R A N C E  *   ELGIPGT 
00495 ***************************************************************   ELGIPGT 
00496  STRING-FIRST-LINE-FOR-COINSURA.                                  ELGIPGT 
00497      IF ACCUM-ASCEND-DESCEND-COUNT = 1                            ELGIPGT 
00498         MOVE 1 TO WS-LEVEL-COUNT                                  ELGIPGT 
00499         MOVE SPACES TO WS-LEVEL-TAG (1)                           ELGIPGT 
00500         MOVE ACCUM-PERCENT-LEVEL (1) TO WS-LEVEL-PERCENT (1)      ELGIPGT 
00501         SET WS-LEVEL-PCT (1) TO TRUE.                             ELGIPGT 
00502                                                                   ELGIPGT 
00503      STRING 'THE '                          DELIMITED BY SIZE     ELGIPGT 
00504         WS-ACCUM-NAME (WS-ACM-SUB)          DELIMITED BY ' '      ELGIPGT 
00505         ' ACCUMULATOR '                     DELIMITED BY SIZE     ELGIPGT 
00506         WS-SERVICES-PAID-PHRASE             DELIMITED BY SIZE     ELGIPGT 
00507         WS-LEVEL-TABLE                      DELIMITED BY SIZE     ELGIPGT 
00508         CMF-DESCR-LINE(1)                   DELIMITED BY '  '     ELGIPGT 
00509        ' THE FOLLOWING PROVIDER TYPES:'     DELIMITED BY SIZE     ELGIPGT 
00510             INTO TCAR-FROM-AREA.                                  ELGIPGT 
00511                                                                   ELGIPGT 
00512 ***************************************************************   ELGIPGT 
00513 *  L I S T   T A B U L A R   C O N T E N T S                  *   ELGIPGT 
00514 ***************************************************************   ELGIPGT 
00515  LIST-TABULAR-CONTENTS.                                           ELGIPGT 
00516      MOVE WS-PVE  TO CMF-RECORD-PREFIX.                           ELGIPGT 
00517      MOVE 'PROVIDER-CODE'          TO                             ELGIPGT 
00518          CMF-ELEMENT-SYSTEM-NAME.                                 ELGIPGT 
00519      MOVE GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX) TO               ELGIPGT 
00520          CMF-CODE-VALUE.                                          ELGIPGT 
00521      EXEC CICS LINK  PROGRAM('ELUCMIF')                           ELGIPGT 
00522                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELGIPGT 
00523      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGIPGT 
00524                                                                   ELGIPGT 
00525      PERFORM WITH TEST BEFORE                                     ELGIPGT 
00526        VARYING TCAR-FROM-SUB FROM 1 BY 1                          ELGIPGT 
00527        UNTIL TCAR-FROM-SUB > CMF-NBR-DESCR-LINES                  ELGIPGT 
00528                                                                   ELGIPGT 
00529        MOVE CMF-DESCR-LINE (TCAR-FROM-SUB)                        ELGIPGT 
00530                          TO TCAR-FROM-LINE (TCAR-FROM-SUB)        ELGIPGT 
00531      END-PERFORM.                                                 ELGIPGT 
00532                                                                   ELGIPGT 
00533      PERFORM DO-TEXT-COMPRESSION.                                 ELGIPGT 
00534      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELGIPGT 
00535      MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGIPGT 
00536      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                          ELGIPGT 
00537                  TCAR-OUTPUT-FIELD-2-LEN.                         ELGIPGT 
00538      PERFORM DO-TEXT-UNSTRING.                                    ELGIPGT 
00539      PERFORM MOVE-COMPRESSED-PHRASE                               ELGIPGT 
00540          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELGIPGT 
00541                UNTIL TCAR-FROM-SUB >                              ELGIPGT 
00542              TCAR-OUTPUT-FIELDS-USED.                             ELGIPGT 
00543      PERFORM CALL-OUTPUT.                                         ELGIPGT 
00544                                                                   ELGIPGT 
00545 ***************************************************************   ELGIPGT 
00546 *  I N S E R T   B L A N K   L I N E                          *   ELGIPGT 
00547 ***************************************************************   ELGIPGT 
00548  INSERT-BLANK-LINE.                                               ELGIPGT 
00549      ADD  1       TO  COF-NBR-DTL-LINES.                          ELGIPGT 
00550      MOVE SPACES  TO  COF-DTL-LINE (COF-NBR-DTL-LINES).           ELGIPGT 
00551      PERFORM CALL-OUTPUT.                                         ELGIPGT 
00552                                                                   ELGIPGT 
00553 ***************************************************************   ELGIPGT 
00554 *  M O V E   C O M P R E S S E D   P H R A S E                *   ELGIPGT 
00555 ***************************************************************   ELGIPGT 
00556  MOVE-COMPRESSED-PHRASE.                                          ELGIPGT 
00557      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGIPGT 
00558      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB)                           ELGIPGT 
00559         TO COF-DTL-LINE (COF-NBR-DTL-LINES).                      ELGIPGT 
00560                                                                   ELGIPGT 
00561 ***************************************************************   ELGIPGT 
00562 *  M O V E   A S T E R I C K S   O U T P U T   L I N E        *   ELGIPGT 
00563 ***************************************************************   ELGIPGT 
00564  MOVE-ASTERICKS-TO-FIRST-OUTPUT.                                  ELGIPGT 
00565      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGIPGT 
00566      MOVE WS-FLAG-LINE TO COF-DTL-LINE (COF-NBR-DTL-LINES).       ELGIPGT 
00567                                                                   ELGIPGT 
00568 ***************************************************************   ELGIPGT 
00569 *  D O   T E X T   C O M P R E S S I O                        *   ELGIPGT 
00570 ***************************************************************   ELGIPGT 
00571  DO-TEXT-COMPRESSION.                                             ELGIPGT 
00572      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGIPGT 
00573                                                                   ELGIPGT 
00574 ***************************************************************   ELGIPGT 
00575 *  D O   T E X T   U N S T R I N G                            *   ELGIPGT 
00576 ***************************************************************   ELGIPGT 
00577  DO-TEXT-UNSTRING.                                                ELGIPGT 
00578      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGIPGT 
00579                                                                   ELGIPGT 
00580 ***************************************************************   ELGIPGT 
00581 *  C A L L   O U T P U T                                      *   ELGIPGT 
00582 ***************************************************************   ELGIPGT 
00583  CALL-OUTPUT.                                                     ELGIPGT 
00584      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELGIPGT 
00585                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGIPGT 
00586      MOVE SPACES TO TCAR-FROM-AREA.                               ELGIPGT 
