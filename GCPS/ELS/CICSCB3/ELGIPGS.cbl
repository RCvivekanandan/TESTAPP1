00001  IDENTIFICATION DIVISION.                                         09/03/03
00002 *                                                                 ELGIPGS 
00003  PROGRAM-ID.         ELGIPGS.                                        LV002
00004 *                                                                 ELGIPGS 
00005  AUTHOR.             ANNE KEFFER KING.                            ELGIPGS 
00006 *                    CLONED FROM ELGIPGT                          ELGIPGS 
00007 *                                                                 ELGIPGS 
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGIPGS 
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELGIPGS 
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGIPGS 
00011                      233 N. MICHIGAN AVE                          ELGIPGS 
00012                      CHICAGO, ILLINOIS 60601                      ELGIPGS 
00013 *                                                                 ELGIPGS 
00014  DATE-WRITTEN.       21-AUG-2000.                                 ELGIPGS 
00015 *                                                                 ELGIPGS 
00016  DATE-COMPILED.                                                   ELGIPGS 
00017 *                                                                 ELGIPGS 
00018  SECURITY.           COPYRIGHT 1986,                              ELGIPGS 
00019                      HEALTH CARE SERVICE CORPORATION              ELGIPGS 
00020 *                                                                 ELGIPGS 
00021 ******************************************************************ELGIPGS 
00022 *   ELGIPGS                                                      *ELGIPGS 
00023 *                        PROGRAM ABSTRACT                        *ELGIPGS 
00024 *                                                                *ELGIPGS 
00025 *   PROGRAM NAME:   E.L.S. INTERNAL TABULAR TRANSLATOR           *ELGIPGS 
00026 *                   SUBROUTINE (PROVIDER SPECIALTY)              *ELGIPGS 
00027 *                                                                *ELGIPGS 
00028 *   PROGRAM I.D.:   ELGIPGS                                      *ELGIPGS 
00029 *                                                                *ELGIPGS 
00030 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE FIELDS *ELGIPGS 
00031 *              IN THE #IPGS INTERNAL TABULAR                     *ELGIPGS 
00032 *                                                                *ELGIPGS 
00033 *   RECORDS                                                      *ELGIPGS 
00034 *   ACCESSED:  #IPGS INTERNAL TABULAR                            *ELGIPGS 
00035 *                                                                *ELGIPGS 
00036 ******************************************************************ELGIPGS 
00037 *                                                                *ELGIPGS 
00038 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGIPGS 
00039 *       *-*         U P D A T E   H I S T O R Y         *-*      *ELGIPGS 
00040 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGIPGS 
00041 *                                                                *ELGIPGS 
00042 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELGIPGS 
00043 *                                                                *ELGIPGS 
00044 *  ELS 2.1   08/21/00  AKK  CREATED.                              ELGIPGS 
00045 *                                                                *ELGIPGS 
ED0624* BBDA-58217 06/14/24  ED   RECOMPILE FOR PEAQ COPYBOOK          *        
ED0624*                           EXPANSION:                           *        
ED0624*                                 COPYBOOK ELSACUMC              *        
00046 *                                                                *ELGIPGS 
00047 ******************************************************************ELGIPGS 
00048 /                                                                 ELGIPGS 
00049  ENVIRONMENT DIVISION.                                            ELGIPGS 
00050  CONFIGURATION SECTION.                                           ELGIPGS 
00051  SOURCE-COMPUTER.    IBM-3090.                                    ELGIPGS 
00052  OBJECT-COMPUTER.    IBM-3090.                                    ELGIPGS 
00053 /                                                                 ELGIPGS 
00054  DATA DIVISION.                                                   ELGIPGS 
00055  WORKING-STORAGE SECTION.                                         ELGIPGS 
00056  01  WS-BEGIN               PIC X(24)  VALUE                      ELGIPGS 
00057      'ELGIPGS WORKING STORAGE*'.                                  ELGIPGS 
00058 /     W O R K F I E L D S   A N D   S W I T C H E S               ELGIPGS 
00059  01  WS-MISC-WORK.                                                ELGIPGS 
00060      05  WS-SUB             PIC S9(4) COMP SYNC VALUE ZEROES.     ELGIPGS 
00061      05  WS-ACM-SUB         PIC S9(4) COMP SYNC VALUE ZEROES.     ELGIPGS 
00062      05  WS-CUR-SUB         PIC S9(4) COMP SYNC VALUE ZEROES.     ELGIPGS 
00063      05  WS-NXT-SUB         PIC S9(4) COMP SYNC VALUE ZEROES.     ELGIPGS 
00064      05  WS-TAG-SUB         PIC S9(4) COMP SYNC VALUE ZEROES.     ELGIPGS 
00065      05  WS-LVL-SUB         PIC S9(4) COMP SYNC VALUE ZEROES.     ELGIPGS 
00066      05  WS-IPGS            PIC X(5)   VALUE '#IPGS'.             ELGIPGS 
00067      05  WS-PVE             PIC X(5)   VALUE '#PVE '.             ELGIPGS 
00068      05  WS-BEN-ID.                                               ELGIPGS 
00069          10  WS-BEN-ID-5    PIC X(5)   VALUE SPACES.              ELGIPGS 
00070          10  WS-BEN-ID-1    PIC X      VALUE SPACES.              ELGIPGS 
00071      05  WS-GETMAIN-SWITCH  PIC X      VALUE 'N'.                 ELGIPGS 
00072          88  GETMAIN-DONE              VALUE 'Y'.                 ELGIPGS 
00073      05  WS-EDIT-COMP       PIC X      VALUE 'N'.                 ELGIPGS 
00074          88  WS-EDIT-COMPLETE          VALUE 'Y'.                 ELGIPGS 
00075  01  FILLER.                                                      ELGIPGS 
00076    02  WS-ACCUMULATOR-TYPE.                                       ELGIPGS 
00077      03  FILLER   PIC X(21)  VALUE                                ELGIPGS 
00078         '#ACL    COINSURANCE  '.                                  ELGIPGS 
00079      03  FILLER   PIC X(21)  VALUE                                ELGIPGS 
00080         '#ADL    DEDUCTIBLE   '.                                  ELGIPGS 
00081      03  FILLER   PIC X(21)  VALUE                                ELGIPGS 
00082         '#ABM    MAXIMUM      '.                                  ELGIPGS 
00083      03  FILLER   PIC X(21)  VALUE                                ELGIPGS 
00084         '#AOL    OUT-OF-POCKET'.                                  ELGIPGS 
00085      03  FILLER   PIC X(21)  VALUE                                ELGIPGS 
00086         '#ACP    CO-PAY       '.                                  ELGIPGS 
00087    02  WS-ACCUMULATOR-TYPE-TABLE REDEFINES  WS-ACCUMULATOR-TYPE.  ELGIPGS 
00088      03  WS-ACCUMULATOR-TYPE-TBL  OCCURS 5 TIMES                  ELGIPGS 
00089                                   INDEXED BY ACUM-IDX.            ELGIPGS 
00090          05  WS-ACCUM-TYPE       PIC X(08).                       ELGIPGS 
00091          05  WS-ACCUM-NAME       PIC X(13).                       ELGIPGS 
00092                                                                   ELGIPGS 
00093  01  WS-IPGS-VARIABLE-AREA.                                       ELGIPGS 
00094    02  WS-IPGS-COUNT   COMP-3    PIC S9(03) VALUE ZEROES.         ELGIPGS 
00095    02  WS-IPGS-VARIABLES                                          ELGIPGS 
00096           OCCURS  46  TIMES  DEPENDING  ON  WS-IPGS-COUNT.        ELGIPGS 
00097      03  WS-IPGS-VARIABLE.                                        ELGIPGS 
00098        05  WS-IPGS-SLOT-NBR      PIC S9(07) COMP-3.               ELGIPGS 
00099        05  WS-IPGS-LEVEL         PIC X(04).                       ELGIPGS 
00100        05  WS-IPGS-PERCENT       PIC S9(03) COMP-3.               ELGIPGS 
00101        05  WS-IPGS-STATUS        PIC X(01).                       ELGIPGS 
00102          88  WS-IPGS-PROCESSED       VALUE 'Y'.                   ELGIPGS 
00103          88  WS-IPGS-NOT-PROCESSED   VALUE 'N'.                   ELGIPGS 
00104                                                                   ELGIPGS 
00105  01  WS-LEVEL-AREA.                                               ELGIPGS 
00106    02  WS-LEVEL-COUNT  COMP-3    PIC S9(03) VALUE ZEROES.         ELGIPGS 
00107    02  WS-LEVEL-TABLE.                                            ELGIPGS 
00108      03  WS-LEVEL-TABLE-ENTRIES                                   ELGIPGS 
00109            OCCURS 46 TIMES DEPENDING ON WS-LEVEL-COUNT.           ELGIPGS 
00110        05  WS-LEVEL-TAG          PIC X(04).                       ELGIPGS 
00111        05  WS-LEVEL-PERCENT      PIC ZZ9.                         ELGIPGS 
00112        05  WS-LEVEL-EDITOR       PIC X(06).                       ELGIPGS 
00113          88  WS-LEVEL-PCT            VALUE '%     '.              ELGIPGS 
00114          88  WS-LEVEL-PCT-COMMA      VALUE '%,    '.              ELGIPGS 
00115          88  WS-LEVEL-PCT-AND        VALUE '% AND '.              ELGIPGS 
00116                                                                   ELGIPGS 
00117  01  PROGRAM-CONSTANTS.                                           ELGIPGS 
00118      05  PC-ABM                  PIC X(08)  VALUE '#ABM    '.     ELGIPGS 
00119      05  PC-ADL                  PIC X(08)  VALUE '#ADL    '.     ELGIPGS 
00120                                                                   ELGIPGS 
00121  01  WS-SERVICES-PAID-PHRASE.                                     ELGIPGS 
00122      05  FILLER                  PIC X(21)  VALUE                 ELGIPGS 
00123          'FOR SERVICES PAID AT '.                                 ELGIPGS 
00124                                                                   ELGIPGS 
00125  01  WS-PERCENT.                                                  ELGIPGS 
00126      05  WS-PERCENT-LEVEL        PIC ZZ9.                         ELGIPGS 
00127      05  FILLER                  PIC X(01)  VALUE '%'.            ELGIPGS 
00128      05  FILLER                  PIC X(01)  VALUE SPACE.          ELGIPGS 
00129                                                                   ELGIPGS 
00130  01  WS-FLAG-LINE.                                                ELGIPGS 
00131      05  FILLER                  PIC X(79)  VALUE '*****'.        ELGIPGS 
00132 /                                                                 ELGIPGS 
00133      COPY ELSVLTGC.                                               ELGIPGS 
00134                                                                   ELGIPGS 
00135  01  WS-END                 PIC X(16)  VALUE                      ELGIPGS 
00136      '*** W/S ENDS ***'.                                          ELGIPGS 
00137 /             L I N K A G E   S E C T I O N                       ELGIPGS 
00138  LINKAGE SECTION.                                                 ELGIPGS 
00139  01  DFHCOMMAREA.                                                 ELGIPGS 
00140      COPY ELSCOMMC.                                               ELGIPGS 
00141 /                                                                 ELGIPGS 
00142      COPY ELSCIA2C.                                               ELGIPGS 
00143 /                                                                 ELGIPGS 
00144      COPY ELSCMDSC.                                               ELGIPGS 
00145 /                                                                 ELGIPGS 
00146      COPY ELSCMIFC.                                               ELGIPGS 
00147 /                                                                 ELGIPGS 
00148      COPY ELSIOPMC.                                               ELGIPGS 
00149 /                                                                 ELGIPGS 
00150      COPY ELSKEYSC.                                               ELGIPGS 
00151 /                                                                 ELGIPGS 
00152      COPY ELSOUTPC.                                               ELGIPGS 
00153 /                                                                 ELGIPGS 
00154      COPY ELSTCWAC.                                               ELGIPGS 
00155 /                                                                 ELGIPGS 
00156      COPY ELSSRTPC.                                               ELGIPGS 
00157 /                                                                 ELGIPGS 
00158      COPY ELSACUMC.                                               ELGIPGS 
00159 /                                                                 ELGIPGS 
00160  01  GXS-TABULAR-REC-AREA.                                        ELGIPGS 
00161      COPY GCTIPGSC.                                               ELGIPGS 
00162 /  P R O C E D U R E   D I V I S I O N .                          ELGIPGS 
00163 ***************************************************************   ELGIPGS 
00164 *                                                             *   ELGIPGS 
00165 *  I N T E R N A L   T A B U L A R   D I S P L A Y   I P G T  *   ELGIPGS 
00166 *                                                             *   ELGIPGS 
00167 ***************************************************************   ELGIPGS 
00168  PROCEDURE DIVISION.                                              ELGIPGS 
00169  INTERNAL-TABULAR-DISPLAY-IPGS.                                   ELGIPGS 
00170      PERFORM INITIALIZATION-ROUTINE.                              ELGIPGS 
00171                                                                   ELGIPGS 
00172      IF ACCUM-ASCEND-DESCEND-COUNT = 1 AND                        ELGIPGS 
00173       (ACCUM-ABM OR ACCUM-ACL OR ACCUM-ADL OR ACCUM-AOL           ELGIPGS 
00174           OR ACCUM-ACP)                                           ELGIPGS 
00175          PERFORM SINGLE-LEVEL-DISPLAY                             ELGIPGS 
00176      ELSE                                                         ELGIPGS 
00177         IF ACCUM-ASCEND-DESCEND-COUNT > 1 AND                     ELGIPGS 
00178          (ACCUM-ACL OR ACCUM-AOL)                                 ELGIPGS 
00179             PERFORM VARIABLE-LEVEL-DISPLAY                        ELGIPGS 
00180         ELSE                                                      ELGIPGS 
00181            EXEC CICS ABEND  ABCODE('EL99')  END-EXEC.             ELGIPGS 
00182                                                                   ELGIPGS 
00183      GOBACK.                                                      ELGIPGS 
00184                                                                   ELGIPGS 
00185 ***************************************************************   ELGIPGS 
00186 *                                                             *   ELGIPGS 
00187 *  I N I T I A L I Z A T I O N   R O U T I N E                *   ELGIPGS 
00188 *                                                             *   ELGIPGS 
00189 ***************************************************************   ELGIPGS 
00190  INITIALIZATION-ROUTINE.                                          ELGIPGS 
00191      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELGIPGS 
00192         EXEC CICS ABEND  ABCODE('EL01')  END-EXEC.                ELGIPGS 
00193                                                                   ELGIPGS 
00194      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGIPGS 
00195          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELGIPGS 
00196      IF CIA-RC-PTR-NULL                                           ELGIPGS 
00197         EXEC CICS ABEND  ABCODE('EL02')  END-EXEC.                ELGIPGS 
00198                                                                   ELGIPGS 
00199      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGIPGS 
00200      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGS 
00201          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELGIPGS 
00202      PERFORM CHECK-CIA-RC-PTR-NULL.                               ELGIPGS 
00203                                                                   ELGIPGS 
00204      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGIPGS 
00205      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGS 
00206          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELGIPGS 
00207      PERFORM CHECK-CIA-RC-PTR-NULL.                               ELGIPGS 
00208                                                                   ELGIPGS 
00209      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGIPGS 
00210      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGS 
00211          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELGIPGS 
00212      PERFORM CHECK-CIA-RC-PTR-NULL.                               ELGIPGS 
00213                                                                   ELGIPGS 
00214      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGIPGS 
00215      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGS 
00216          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELGIPGS 
00217      PERFORM CHECK-CIA-RC-PTR-NULL.                               ELGIPGS 
00218                                                                   ELGIPGS 
00219      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGIPGS 
00220      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGS 
00221          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELGIPGS 
00222      PERFORM CHECK-CIA-RC-PTR-NULL.                               ELGIPGS 
00223                                                                   ELGIPGS 
00224      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGIPGS 
00225      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGS 
00226          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGIPGS 
00227      PERFORM CHECK-CIA-RC-PTR-NULL.                               ELGIPGS 
00228                                                                   ELGIPGS 
00229      SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR.       ELGIPGS 
00230                                                                   ELGIPGS 
00231      INITIALIZE CMF-CODES-MANUAL-INTERFACE                        ELGIPGS 
00232                 TCAR-FROM-AREA.                                   ELGIPGS 
00233                                                                   ELGIPGS 
00234  CHECK-CIA-RC-PTR-NULL.                                           ELGIPGS 
00235      IF CIA-RC-PTR-NULL                                           ELGIPGS 
00236         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGIPGS 
00237         EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.            ELGIPGS 
00238                                                                   ELGIPGS 
00239 ***************************************************************   ELGIPGS 
00240 *                                                             *   ELGIPGS 
00241 *  S I N G L E   L E V E L   D I S P L A Y                    *   ELGIPGS 
00242 *                                                             *   ELGIPGS 
00243 ***************************************************************   ELGIPGS 
00244  SINGLE-LEVEL-DISPLAY.                                            ELGIPGS 
00245      PERFORM READ-INTERNAL-TABULAR-RECORD.                        ELGIPGS 
00246      PERFORM LIST-TABULAR-HEADING.                                ELGIPGS 
00247      PERFORM LIST-TABULAR-CONTENTS                                ELGIPGS 
00248        VARYING GXS-INDEX FROM 1 BY 1                              ELGIPGS 
00249          UNTIL GXS-INDEX = GXS-ENTRY-COUNT OR                     ELGIPGS 
00250          GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX) = HIGH-VALUES.    ELGIPGS 
00251      PERFORM INSERT-BLANK-LINE.                                   ELGIPGS 
00252                                                                   ELGIPGS 
00253 ***************************************************************   ELGIPGS 
00254 *                                                             *   ELGIPGS 
00255 *  V A R I A B L E   L E V E L   D I S P L A Y                *   ELGIPGS 
00256 *                                                             *   ELGIPGS 
00257 ***************************************************************   ELGIPGS 
00258  VARIABLE-LEVEL-DISPLAY.                                          ELGIPGS 
00259      MOVE 1 TO WS-CUR-SUB.                                        ELGIPGS 
00260      SET VLT-INDEX TO 1.                                          ELGIPGS 
00261      PERFORM INITIALIZE-IPGS-VAR-TBLE                             ELGIPGS 
00262        VARYING WS-IPGS-COUNT FROM 1 BY 1                          ELGIPGS 
00263          UNTIL WS-IPGS-COUNT > 46.                                ELGIPGS 
00264      MOVE ZEROES TO WS-IPGS-COUNT.                                ELGIPGS 
00265      PERFORM EXTRACT-PERCENT-N-SLOT-NBR                           ELGIPGS 
00266        VARYING ASC-DES-INDEX FROM 1 BY 1                          ELGIPGS 
00267          UNTIL ASC-DES-INDEX > ACCUM-ASCEND-DESCEND-COUNT.        ELGIPGS 
00268      MOVE WS-IPGS-COUNT TO WS-CUR-SUB.                            ELGIPGS 
00269      ADD 1 TO WS-CUR-SUB.                                         ELGIPGS 
00270      MOVE HIGH-VALUES TO WS-IPGS-STATUS (WS-CUR-SUB).             ELGIPGS 
00271      PERFORM IPGS-DISPLAY-PROCESSING                              ELGIPGS 
00272        VARYING WS-CUR-SUB FROM 1 BY 1                             ELGIPGS 
00273          UNTIL WS-IPGS-STATUS (WS-CUR-SUB) = HIGH-VALUES.         ELGIPGS 
00274                                                                   ELGIPGS 
00275 ***************************************************************   ELGIPGS 
00276 *  I N I T I A L I Z E   I P G S   V A R   T B L E            *   ELGIPGS 
00277 ***************************************************************   ELGIPGS 
00278  INITIALIZE-IPGS-VAR-TBLE.                                        ELGIPGS 
00279      INITIALIZE WS-IPGS-VARIABLE (WS-IPGS-COUNT).                 ELGIPGS 
00280                                                                   ELGIPGS 
00281 ***************************************************************   ELGIPGS 
00282 *  E X T R A C T   P E R C E N T   &   S L O T   N B R        *   ELGIPGS 
00283 ***************************************************************   ELGIPGS 
00284  EXTRACT-PERCENT-N-SLOT-NBR.                                      ELGIPGS 
00285      IF ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX) > ZERO                ELGIPGS 
00286         MOVE ACCUM-PERCENT-LEVEL (ASC-DES-INDEX)                  ELGIPGS 
00287           TO WS-IPGS-PERCENT (WS-CUR-SUB)                         ELGIPGS 
00288         MOVE ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX)                  ELGIPGS 
00289           TO WS-IPGS-SLOT-NBR (WS-CUR-SUB)                        ELGIPGS 
00290         MOVE VLT-VARIABLE-LEVEL-TAG (VLT-INDEX)                   ELGIPGS 
00291           TO WS-IPGS-LEVEL (WS-CUR-SUB)                           ELGIPGS 
00292         SET WS-IPGS-NOT-PROCESSED (WS-CUR-SUB) TO TRUE            ELGIPGS 
00293         ADD 1 TO WS-IPGS-COUNT.                                   ELGIPGS 
00294      ADD 1 TO WS-CUR-SUB WS-TAG-SUB.                              ELGIPGS 
00295      SET VLT-INDEX UP BY 1.                                       ELGIPGS 
00296                                                                   ELGIPGS 
00297 ***************************************************************   ELGIPGS 
00298 *  I P G S   D I S P L A Y   P R O C E S S I N G              *   ELGIPGS 
00299 ***************************************************************   ELGIPGS 
00300  IPGS-DISPLAY-PROCESSING.                                         ELGIPGS 
00301      IF WS-IPGS-NOT-PROCESSED (WS-CUR-SUB)                        ELGIPGS 
00302         MOVE 1 TO WS-LVL-SUB                                      ELGIPGS 
00303         MOVE ZEROES TO WS-LEVEL-COUNT                             ELGIPGS 
00304         PERFORM PUT-EQUAL-SLOT-NBR-TO-LVL-TBL                     ELGIPGS 
00305           VARYING WS-NXT-SUB FROM WS-CUR-SUB BY 1                 ELGIPGS 
00306             UNTIL WS-IPGS-STATUS (WS-NXT-SUB) = HIGH-VALUES       ELGIPGS 
00307         SET ASC-DES-INDEX TO WS-CUR-SUB                           ELGIPGS 
00308         PERFORM READ-INTERNAL-TABULAR-RECORD                      ELGIPGS 
00309         PERFORM EDIT-CONNECT-PERCENT-LEVELS                       ELGIPGS 
00310           VARYING WS-LVL-SUB FROM 1 BY 1                          ELGIPGS 
00311             UNTIL WS-EDIT-COMPLETE                                ELGIPGS 
00312         PERFORM LIST-TABULAR-HEADING                              ELGIPGS 
00313         PERFORM LIST-TABULAR-CONTENTS                             ELGIPGS 
00314           VARYING GXS-INDEX FROM 1 BY 1                           ELGIPGS 
00315           UNTIL GXS-INDEX =  GXS-ENTRY-COUNT OR                   ELGIPGS 
00316           GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX) = HIGH-VALUES.   ELGIPGS 
00317         PERFORM INSERT-BLANK-LINE.                                ELGIPGS 
00318                                                                   ELGIPGS 
00319 ***************************************************************   ELGIPGS 
00320 *  P U T   E Q U A L   S L O T   N B R   T O   L V L   T B L  *   ELGIPGS 
00321 ***************************************************************   ELGIPGS 
00322  PUT-EQUAL-SLOT-NBR-TO-LVL-TBL.                                   ELGIPGS 
00323      IF WS-IPGS-NOT-PROCESSED (WS-NXT-SUB) AND                    ELGIPGS 
00324         WS-IPGS-SLOT-NBR (WS-CUR-SUB) =                           ELGIPGS 
00325                           WS-IPGS-SLOT-NBR (WS-NXT-SUB)           ELGIPGS 
00326            MOVE WS-IPGS-PERCENT (WS-NXT-SUB) TO                   ELGIPGS 
00327                              WS-LEVEL-PERCENT (WS-LVL-SUB)        ELGIPGS 
00328            MOVE WS-IPGS-LEVEL   (WS-NXT-SUB) TO                   ELGIPGS 
00329                              WS-LEVEL-TAG (WS-LVL-SUB)            ELGIPGS 
00330            SET WS-IPGS-PROCESSED (WS-NXT-SUB) TO TRUE             ELGIPGS 
00331            ADD 1 TO WS-LVL-SUB                                    ELGIPGS 
00332            ADD 1 TO WS-LEVEL-COUNT.                               ELGIPGS 
00333                                                                   ELGIPGS 
00334 ***************************************************************   ELGIPGS 
00335 *  R E A D   I N T E R N A L   T A B U L A R   R E C O R D    *   ELGIPGS 
00336 ***************************************************************   ELGIPGS 
00337  READ-INTERNAL-TABULAR-RECORD.                                    ELGIPGS 
00338      SET CIA-GCTABULR-DDN TO TRUE.                                ELGIPGS 
00339      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGS 
00340          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGIPGS 
00341      IF CIA-RC-PTR-NULL                                           ELGIPGS 
00342         SET CIA-STG-GETMAIN TO TRUE                               ELGIPGS 
00343         EXEC CICS LINK  PROGRAM ('ELUSTGMG')                      ELGIPGS 
00344                         COMMAREA(DFHCOMMAREA)  END-EXEC.          ELGIPGS 
00345      SET CIA-GCTABULR-DDN TO TRUE.                                ELGIPGS 
00346      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGS 
00347          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGIPGS 
00348      MOVE '#IPGS ' TO KWA-PROVISION-ID.                           ELGIPGS 
00349      MOVE ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX)                     ELGIPGS 
00350                    TO KWA-PROVISION-SLOT-NO.                      ELGIPGS 
00351      SET IOP-RD TO TRUE.                                          ELGIPGS 
00352      SET IOP-FCQ-NONE TO TRUE.                                    ELGIPGS 
00353      SET IOP-KVQ-EQ TO TRUE.                                      ELGIPGS 
00354      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGIPGS 
00355      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGIPGS 
00356      SET IOP-REC-PTR TO NULL.                                     ELGIPGS 
00357 ******************************************************************ELGIPGS 
00358 * WE ARE USING MOVE MODE BECAUSE THE CALLING PROGRAM IS ALSO     *ELGIPGS 
00359 * READING FROM THE SAME TABULAR FILE.  WHEN USING LOCATE MODE,   *ELGIPGS 
00360 * THE OLD RECORD IN THE DATA AREA IS FREED UP WHEN THE NEXT      *ELGIPGS 
00361 * RECORD IS READ FROM THE SAME FILE.  THIS WILL MESS UP THE      *ELGIPGS 
00362 * LOGIC IN THE CALLING PROGRAM, WHICH STILL NEEDS TO SEE THE     *ELGIPGS 
00363 * TABULAR RECORD.                                                *ELGIPGS 
00364 ******************************************************************ELGIPGS 
00365      SET IOP-STG-MODE-MOVE TO TRUE.                               ELGIPGS 
00366      EXEC CICS LINK  PROGRAM('ELUIOPGM')                          ELGIPGS 
00367                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELGIPGS 
00368      IF IOP-RC-OK                                                 ELGIPGS 
00369         SET ADDRESS OF GXS-TABULAR-REC-AREA TO IOP-REC-PTR        ELGIPGS 
00370      ELSE                                                         ELGIPGS 
00371         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELGIPGS 
00372         EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.            ELGIPGS 
00373                                                                   ELGIPGS 
00374 ***************************************************************   ELGIPGS 
00375 *   E D I T   C O N N E C T   P E R C E N T   L E V E L S     *   ELGIPGS 
00376 ***************************************************************   ELGIPGS 
00377  EDIT-CONNECT-PERCENT-LEVELS.                                     ELGIPGS 
00378      IF WS-LVL-SUB = 1 AND                                        ELGIPGS 
00379         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIPGS 
00380         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIPGS 
00381         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIPGS 
00382                                                                   ELGIPGS 
00383      IF WS-LVL-SUB = 2 AND                                        ELGIPGS 
00384         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIPGS 
00385         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIPGS 
00386         SUBTRACT 1 FROM WS-LVL-SUB                                ELGIPGS 
00387         SET WS-LEVEL-PCT-AND (WS-LVL-SUB) TO TRUE                 ELGIPGS 
00388         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIPGS 
00389                                                                   ELGIPGS 
00390      IF WS-LVL-SUB > 2 AND                                        ELGIPGS 
00391         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIPGS 
00392         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIPGS 
00393         SUBTRACT 1 FROM WS-LVL-SUB                                ELGIPGS 
00394         SET WS-LEVEL-PCT-AND (WS-LVL-SUB) TO TRUE                 ELGIPGS 
00395         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIPGS 
00396                                                                   ELGIPGS 
00397      IF WS-LVL-SUB > 2 AND                                        ELGIPGS 
00398         WS-LVL-SUB NOT EQUAL WS-LEVEL-COUNT                       ELGIPGS 
00399            SET WS-LEVEL-PCT-COMMA (WS-LVL-SUB) TO TRUE.           ELGIPGS 
00400                                                                   ELGIPGS 
00401 ***************************************************************   ELGIPGS 
00402 *  L I S T   T A B U L A R   H E A DI N G                     *   ELGIPGS 
00403 ***************************************************************   ELGIPGS 
00404  LIST-TABULAR-HEADING.                                            ELGIPGS 
00405      INITIALIZE TCAR-FROM-AREA.                                   ELGIPGS 
00406      MOVE WS-IPGS TO CMF-RECORD-PREFIX.                           ELGIPGS 
00407      MOVE 'INCLUDE-EXCLUDE-IND' TO                                ELGIPGS 
00408          CMF-ELEMENT-SYSTEM-NAME.                                 ELGIPGS 
00409      MOVE GXS-INCLUDE-EXCLUDE-IND TO CMF-CODE-VALUE.              ELGIPGS 
00410      EXEC CICS LINK  PROGRAM('ELUCMIF')                           ELGIPGS 
00411                      COMMAREA (DFHCOMMAREA)  END-EXEC.            ELGIPGS 
00412      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGIPGS 
00413      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIPGS 
00414          ADDRESS OF CMF-DESCR.                                    ELGIPGS 
00415      EVALUATE TRUE                                                ELGIPGS 
00416         WHEN ACCUM-ACL                                            ELGIPGS 
00417              MOVE 1 TO WS-ACM-SUB                                 ELGIPGS 
00418         WHEN ACCUM-ADL                                            ELGIPGS 
00419              MOVE 2 TO WS-ACM-SUB                                 ELGIPGS 
00420         WHEN ACCUM-ABM                                            ELGIPGS 
00421              MOVE 3 TO WS-ACM-SUB                                 ELGIPGS 
00422         WHEN ACCUM-AOL                                            ELGIPGS 
00423              MOVE 4 TO WS-ACM-SUB                                 ELGIPGS 
00424         WHEN ACCUM-ACP                                            ELGIPGS 
00425              MOVE 5 TO WS-ACM-SUB                                 ELGIPGS 
00426      END-EVALUATE.                                                ELGIPGS 
00427      IF (ACCUM-ADL OR ACCUM-ABM OR ACCUM-ACP)                     ELGIPGS 
00428          PERFORM STRING-FIRST-LINE-FOR-MAXIMUMX                   ELGIPGS 
00429      ELSE                                                         ELGIPGS 
00430         IF (ACCUM-ACL OR ACCUM-AOL)                               ELGIPGS 
00431            PERFORM STRING-FIRST-LINE-FOR-COINSURA.                ELGIPGS 
00432      PERFORM DO-TEXT-COMPRESSION.                                 ELGIPGS 
00433      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELGIPGS 
00434      MOVE +04 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGIPGS 
00435      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                          ELGIPGS 
00436                  TCAR-OUTPUT-FIELD-2-LEN                          ELGIPGS 
00437                  TCAR-OUTPUT-FIELD-3-LEN                          ELGIPGS 
00438                  TCAR-OUTPUT-FIELD-4-LEN.                         ELGIPGS 
00439      PERFORM DO-TEXT-UNSTRING.                                    ELGIPGS 
00440      PERFORM MOVE-ASTERICKS-TO-FIRST-OUTPUT.                      ELGIPGS 
00441      PERFORM MOVE-COMPRESSED-PHRASE                               ELGIPGS 
00442          VARYING TCAR-FROM-SUB FROM 1 BY 1 UNTIL                  ELGIPGS 
00443              TCAR-FROM-SUB GREATER THAN                           ELGIPGS 
00444             TCAR-OUTPUT-FIELDS-USED.                              ELGIPGS 
00445      PERFORM CALL-OUTPUT.                                         ELGIPGS 
00446                                                                   ELGIPGS 
00447                                                                   ELGIPGS 
00448 ***************************************************************   ELGIPGS 
00449 *  S T R I N G  F I R S T   L I N E   M A X I M U M           *   ELGIPGS 
00450 ***************************************************************   ELGIPGS 
00451  STRING-FIRST-LINE-FOR-MAXIMUMX.                                  ELGIPGS 
00452      STRING 'THIS '                        DELIMITED BY SIZE      ELGIPGS 
00453             WS-ACCUM-NAME (WS-ACM-SUB)     DELIMITED BY ' '       ELGIPGS 
00454             ' ACCUMULATOR '                DELIMITED BY SIZE      ELGIPGS 
00455             CMF-DESCR-LINE(1)              DELIMITED BY '  '      ELGIPGS 
00456           ' THE FOLLOWING PROVIDER SPECIALTIES:'                  ELGIPGS 
00457              DELIMITED BY SIZE                                    ELGIPGS 
00458             INTO TCAR-FROM-AREA.                                  ELGIPGS 
00459                                                                   ELGIPGS 
00460 ***************************************************************   ELGIPGS 
00461 *  S T R I N G   F I R S T   L I N E   C O I N S U R A N C E  *   ELGIPGS 
00462 ***************************************************************   ELGIPGS 
00463  STRING-FIRST-LINE-FOR-COINSURA.                                  ELGIPGS 
00464      IF ACCUM-ASCEND-DESCEND-COUNT = 1                            ELGIPGS 
00465         MOVE 1 TO WS-LEVEL-COUNT                                  ELGIPGS 
00466         MOVE SPACES TO WS-LEVEL-TAG (1)                           ELGIPGS 
00467         MOVE ACCUM-PERCENT-LEVEL (1) TO WS-LEVEL-PERCENT (1)      ELGIPGS 
00468         SET WS-LEVEL-PCT (1) TO TRUE.                             ELGIPGS 
00469                                                                   ELGIPGS 
00470      STRING 'THE '                          DELIMITED BY SIZE     ELGIPGS 
00471         WS-ACCUM-NAME (WS-ACM-SUB)          DELIMITED BY ' '      ELGIPGS 
00472         ' ACCUMULATOR '                     DELIMITED BY SIZE     ELGIPGS 
00473         WS-SERVICES-PAID-PHRASE             DELIMITED BY SIZE     ELGIPGS 
00474         WS-LEVEL-TABLE                      DELIMITED BY SIZE     ELGIPGS 
00475         CMF-DESCR-LINE(1)                   DELIMITED BY '  '     ELGIPGS 
00476           ' THE FOLLOWING PROVIDER SPECIALTIES:'                  ELGIPGS 
00477              DELIMITED BY SIZE                                    ELGIPGS 
00478             INTO TCAR-FROM-AREA.                                  ELGIPGS 
00479                                                                   ELGIPGS 
00480 ***************************************************************   ELGIPGS 
00481 *  L I S T   T A B U L A R   C O N T E N T S                  *   ELGIPGS 
00482 ***************************************************************   ELGIPGS 
00483  LIST-TABULAR-CONTENTS.                                           ELGIPGS 
00484      MOVE WS-PVE  TO CMF-RECORD-PREFIX.                           ELGIPGS 
00485      MOVE 'PROVIDER-CODE'          TO                             ELGIPGS 
00486          CMF-ELEMENT-SYSTEM-NAME.                                 ELGIPGS 
00487      MOVE GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX) TO               ELGIPGS 
00488          CMF-CODE-VALUE.                                          ELGIPGS 
00489      EXEC CICS LINK  PROGRAM('ELUCMIF')                           ELGIPGS 
00490                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELGIPGS 
00491      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGIPGS 
00492                                                                   ELGIPGS 
00493      PERFORM WITH TEST BEFORE                                     ELGIPGS 
00494        VARYING TCAR-FROM-SUB FROM 1 BY 1                          ELGIPGS 
00495        UNTIL TCAR-FROM-SUB > CMF-NBR-DESCR-LINES                  ELGIPGS 
00496                                                                   ELGIPGS 
00497        MOVE CMF-DESCR-LINE (TCAR-FROM-SUB)                        ELGIPGS 
00498                          TO TCAR-FROM-LINE (TCAR-FROM-SUB)        ELGIPGS 
00499      END-PERFORM.                                                 ELGIPGS 
00500                                                                   ELGIPGS 
00501      PERFORM DO-TEXT-COMPRESSION.                                 ELGIPGS 
00502      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELGIPGS 
00503      MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGIPGS 
00504      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                          ELGIPGS 
00505                  TCAR-OUTPUT-FIELD-2-LEN.                         ELGIPGS 
00506      PERFORM DO-TEXT-UNSTRING.                                    ELGIPGS 
00507      PERFORM MOVE-COMPRESSED-PHRASE                               ELGIPGS 
00508          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELGIPGS 
00509                UNTIL TCAR-FROM-SUB >                              ELGIPGS 
00510              TCAR-OUTPUT-FIELDS-USED.                             ELGIPGS 
00511      PERFORM CALL-OUTPUT.                                         ELGIPGS 
00512                                                                   ELGIPGS 
00513 ***************************************************************   ELGIPGS 
00514 *  I N S E R T   B L A N K   L I N E                          *   ELGIPGS 
00515 ***************************************************************   ELGIPGS 
00516  INSERT-BLANK-LINE.                                               ELGIPGS 
00517      ADD  1       TO  COF-NBR-DTL-LINES.                          ELGIPGS 
00518      MOVE SPACES  TO  COF-DTL-LINE (COF-NBR-DTL-LINES).           ELGIPGS 
00519      PERFORM CALL-OUTPUT.                                         ELGIPGS 
00520                                                                   ELGIPGS 
00521 ***************************************************************   ELGIPGS 
00522 *  M O V E   C O M P R E S S E D   P H R A S E                *   ELGIPGS 
00523 ***************************************************************   ELGIPGS 
00524  MOVE-COMPRESSED-PHRASE.                                          ELGIPGS 
00525      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGIPGS 
00526      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB)                           ELGIPGS 
00527         TO COF-DTL-LINE (COF-NBR-DTL-LINES).                      ELGIPGS 
00528                                                                   ELGIPGS 
00529 ***************************************************************   ELGIPGS 
00530 *  M O V E   A S T E R I C K S   O U T P U T   L I N E        *   ELGIPGS 
00531 ***************************************************************   ELGIPGS 
00532  MOVE-ASTERICKS-TO-FIRST-OUTPUT.                                  ELGIPGS 
00533      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGIPGS 
00534      MOVE WS-FLAG-LINE TO COF-DTL-LINE (COF-NBR-DTL-LINES).       ELGIPGS 
00535                                                                   ELGIPGS 
00536 ***************************************************************   ELGIPGS 
00537 *  D O   T E X T   C O M P R E S S I O                        *   ELGIPGS 
00538 ***************************************************************   ELGIPGS 
00539  DO-TEXT-COMPRESSION.                                             ELGIPGS 
00540      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGIPGS 
00541                                                                   ELGIPGS 
00542 ***************************************************************   ELGIPGS 
00543 *  D O   T E X T   U N S T R I N G                            *   ELGIPGS 
00544 ***************************************************************   ELGIPGS 
00545  DO-TEXT-UNSTRING.                                                ELGIPGS 
00546      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGIPGS 
00547                                                                   ELGIPGS 
00548 ***************************************************************   ELGIPGS 
00549 *  C A L L   O U T P U T                                      *   ELGIPGS 
00550 ***************************************************************   ELGIPGS 
00551  CALL-OUTPUT.                                                     ELGIPGS 
00552      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELGIPGS 
00553                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGIPGS 
00554      MOVE SPACES TO TCAR-FROM-AREA.                               ELGIPGS 
