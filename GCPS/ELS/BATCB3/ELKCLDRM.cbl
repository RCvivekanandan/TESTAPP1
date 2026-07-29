00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKCLDRM
00003  PROGRAM-ID.           ELKCLDRM.                                     LV004
00004                                                                   ELKCLDRM
00005  AUTHOR.               BARBARA KEIB.                              ELKCLDRM
00006                                                                   ELKCLDRM
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKCLDRM
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKCLDRM
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKCLDRM
00010                        233 N. MICHIGAN AVE                        ELKCLDRM
00011                        CHICAGO, ILLINOIS 60601                    ELKCLDRM
00012                                                                   ELKCLDRM
00013  DATE-WRITTEN.         22-SEP-1992.                               ELKCLDRM
00014                                                                   ELKCLDRM
00015  ENVIRONMENT DIVISION.                                            ELKCLDRM
00016                                                                   ELKCLDRM
00017  CONFIGURATION SECTION.                                           ELKCLDRM
00018                                                                   ELKCLDRM
00019  SOURCE-COMPUTER.      IBM-3090.                                  ELKCLDRM
00020  OBJECT-COMPUTER.      IBM-3090.                                  ELKCLDRM
00021                                                                   ELKCLDRM
00022 ******************************************************************ELKCLDRM
00023 *AKK 12/06/05 REGEN FOR TEST                                     *ELKCLDRM
00024 *                                                                *ELKCLDRM
00025 *  ELKCLDRM - COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE           *ELKCLDRM
00026 *             FACTOR FOR CLDR TABULAR.                           *ELKCLDRM
00027 *                                                                *ELKCLDRM
00028 *              THIS MODULE IS BEING CALLED BY ANY OTHER PROGRAM  *ELKCLDRM
00029 *              FOR THE PURPOSE OF READING THE CLDR TABULAR AND   *ELKCLDRM
00030 *              ADDING ALL THE EXCLUDE/INCLUDED DIAGNOSIS AND     *ELKCLDRM
00031 *              COMPARING THOSE EXCLUDED AGAINST A LIST PASSED    *ELKCLDRM
00032 *              TO THIS MODULE AND COMING UP WITH A COMPUTED      *ELKCLDRM
00033 *              FIGURE OF EXCLUDED DIAGNOSIS CODES VERSUS TOTAL   *ELKCLDRM
00034 *              POSSIBLE.                                         *ELKCLDRM
00035 *                                                                *ELKCLDRM
00036 ******************************************************************ELKCLDRM
00037 *                      MAINTENANCE HISTORY                       *ELKCLDRM
00038 *  MOD     DATE      BY                    ACTION                *ELKCLDRM
00039 * ----- ----------- --- -----------------------------------------*ELKCLDRM
00040 * 01.00 22-SEP-1992 BAK CREATED                                  *ELKCLDRM
00041 *                                                              *  ELKCLDRM
00042 * 01.06 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX       *  ELKCLDRM
00043 *                             ASM RECOMPILES                   *  ELKCLDRM
00044 *                                                              *  ELKCLDRM
00045 * 01.07 24-JUN-2003 AKK       CHANGES DUE TO FILE EXPNASION.   *  ELKCLDRM
00046 *                             MOVING ONLY SIX CHARS FOR DIAG   *  ELKCLDRM
00047 *                            CODE.  ADING WS FIELD.            *  ELKCLDRM
00048 *                                                                *ELKCLDRM
00049 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKCLDRM
00050 *                                                                *ELKCLDRM
00051 * 02.01 09-JAN-2004 AKK INTERTEST S0C7                           *ELKCLDRM
00052 * 02.05 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKCLDRM
00053 *                             CICSCB3 COMPILER FIX             *  ELKCLDRM
00054 *                                                                *ELKCLDRM
00055 ******************************************************************ELKCLDRM
00056  EJECT                                                            ELKCLDRM
00057  DATA DIVISION.                                                   ELKCLDRM
00058                                                                   ELKCLDRM
00059  WORKING-STORAGE SECTION.                                         ELKCLDRM
00060                                                                   ELKCLDRM
00061  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKCLDRM
00062    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKCLDRM
00063    88  WS-INVALID-PARM                 VALUE +8.                  ELKCLDRM
00064    88  WS-MISSING-PARM                 VALUE +12.                 ELKCLDRM
00065    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKCLDRM
00066                                                                   ELKCLDRM
00067  01  WS-HOLD-DIAGNOSIS-CODE.                                      ELKCLDRM
00068      05 WS-HOLD-CODE             PIC X(06).                       ELKCLDRM
00069      05 WS-HOLD-FILLER           PIC X(04).                       ELKCLDRM
00070                                                                   ELKCLDRM
00071  01  WS-ENTRY-COUNT                    COMP-1.                    ELKCLDRM
00072  01  WS-EXCLUDED                       COMP-1.                    ELKCLDRM
00073  01  WS-INCLUDED                       COMP-1.                    ELKCLDRM
00074  01  WS-MATCHED                        COMP-1.                    ELKCLDRM
00075                                                                   ELKCLDRM
00076  01  WS-GTB-MAX-INDEX            INDEX.                           ELKCLDRM
00077  EJECT                                                            ELKCLDRM
00078  LINKAGE SECTION.                                                 ELKCLDRM
00079                                                                   ELKCLDRM
00080                                                                   ELKCLDRM
00081  EJECT                                                            ELKCLDRM
00082                                                                   ELKCLDRM
00083  01  CLDR-RECORD.                                                 ELKCLDRM
00084      COPY GCTCLDRC.                                               ELKCLDRM
00085                                                                   ELKCLDRM
00086                                                                   ELKCLDRM
00087      COPY ELSDXSLC.                                               ELKCLDRM
00088                                                                   ELKCLDRM
00089                                                                   ELKCLDRM
00090      COPY ELSCFDBC.                                               ELKCLDRM
00091  EJECT                                                            ELKCLDRM
00092 ******************************************************************ELKCLDRM
00093 *                                                                *ELKCLDRM
00094 *    PROCEDURE DIVISION                                          *ELKCLDRM
00095 *                                                                *ELKCLDRM
00096 ******************************************************************ELKCLDRM
00097                                                                   ELKCLDRM
00098  PROCEDURE DIVISION USING CLDR-RECORD DXSL-DX-TBL                 ELKCLDRM
00099                                    CFDB-CNFDNC-FCTR-DATA-BLCK.    ELKCLDRM
00100                                                                   ELKCLDRM
00101  0000-DETERMINE-CLDR-CONFIDENCE.                                  ELKCLDRM
00102                                                                   ELKCLDRM
00103      PERFORM 0100-INITIALIZATION.                                 ELKCLDRM
00104      PERFORM 1000-PROCESS-CLDR-TABULAR.                           ELKCLDRM
00105      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKCLDRM
00106      GOBACK.                                                      ELKCLDRM
00107                                                                   ELKCLDRM
00108 ******************************************************************ELKCLDRM
00109 *                                                                *ELKCLDRM
00110 *    INITIALIZATION                                              *ELKCLDRM
00111 *                                                                *ELKCLDRM
00112 ******************************************************************ELKCLDRM
00113                                                                   ELKCLDRM
00114  0100-INITIALIZATION.                                             ELKCLDRM
00115                                                                   ELKCLDRM
00116      MOVE ZEROS TO WS-ENTRY-COUNT.                                ELKCLDRM
00117      MOVE ZEROS TO WS-EXCLUDED.                                   ELKCLDRM
00118      MOVE ZEROS TO WS-INCLUDED.                                   ELKCLDRM
00119      MOVE ZEROS TO WS-MATCHED.                                    ELKCLDRM
00120      INITIALIZE CFDB-CF-CLDR.                                     ELKCLDRM
00121                                                                   ELKCLDRM
00122 ******************************************************************ELKCLDRM
00123 *                                                                *ELKCLDRM
00124 *    PROCESS-CLDR-TABULAR                                        *ELKCLDRM
00125 *                                                                *ELKCLDRM
00126 ******************************************************************ELKCLDRM
00127                                                                   ELKCLDRM
00128  1000-PROCESS-CLDR-TABULAR.                                       ELKCLDRM
00129                                                                   ELKCLDRM
00130      IF ADDRESS OF CLDR-RECORD = NULL                             ELKCLDRM
00131         SET WS-MISSING-PARM TO TRUE                               ELKCLDRM
00132      ELSE                                                         ELKCLDRM
00133      IF ADDRESS OF DXSL-DX-TBL = NULL                             ELKCLDRM
00134         SET WS-MISSING-PARM TO TRUE                               ELKCLDRM
00135      ELSE                                                         ELKCLDRM
00136      IF DXSL-NBR-ENTRS = ZERO                                     ELKCLDRM
00137         SET WS-MISSING-PARM TO TRUE                               ELKCLDRM
00138      ELSE                                                         ELKCLDRM
00139      IF WS-SUCCESSFUL-CALL                                        ELKCLDRM
00140         PERFORM 1100-MATCH-LIST-DIAG-CODES.                       ELKCLDRM
00141                                                                   ELKCLDRM
00142 ******************************************************************ELKCLDRM
00143 *                                                                *ELKCLDRM
00144 *    MATCH LIST OF DIAGNOSIS CODES TO CLDR.                      *ELKCLDRM
00145 *                                                                *ELKCLDRM
00146 ******************************************************************ELKCLDRM
00147                                                                   ELKCLDRM
00148  1100-MATCH-LIST-DIAG-CODES.                                      ELKCLDRM
00149                                                                   ELKCLDRM
00150      SET GTB-INDEX TO GTB-ENTRY-COUNT.                            ELKCLDRM
00151      SET WS-GTB-MAX-INDEX TO GTB-INDEX.                           ELKCLDRM
00152      SET GTB-INDEX TO 1.                                          ELKCLDRM
00153      SET DXSL-IDX TO 1.                                           ELKCLDRM
00154      PERFORM 1200-MATCH-CLDR-TAB-MATCH-LIST                       ELKCLDRM
00155           UNTIL DXSL-IDX > DXSL-NBR-ENTRS OR                      ELKCLDRM
00156                  GTB-INDEX > WS-GTB-MAX-INDEX OR                  ELKCLDRM
00157                   WS-MISSING-PARM.                                ELKCLDRM
00158      IF WS-SUCCESSFUL-CALL                                        ELKCLDRM
00159         MOVE DXSL-NBR-ENTRS TO WS-ENTRY-COUNT                     ELKCLDRM
00160         COMPUTE WS-INCLUDED =                                     ELKCLDRM
00161               DXSL-NBR-ENTRS - WS-EXCLUDED                        ELKCLDRM
00162         COMPUTE WS-MATCHED =                                      ELKCLDRM
00163               ((WS-INCLUDED / WS-ENTRY-COUNT) * 2) - 1            ELKCLDRM
00164         MOVE WS-MATCHED TO CFDB-CF-CLDR-UNWGHTD-LST-MTCH.         ELKCLDRM
00165                                                                   ELKCLDRM
00166 ******************************************************************ELKCLDRM
00167 *                                                                *ELKCLDRM
00168 *    MATCH CLDR TABULAR TO DIAGNOSIS CODE MATCH LIST             *ELKCLDRM
00169 *                                                                *ELKCLDRM
00170 ******************************************************************ELKCLDRM
00171                                                                   ELKCLDRM
00172  1200-MATCH-CLDR-TAB-MATCH-LIST.                                  ELKCLDRM
00173                                                                   ELKCLDRM
00174      MOVE GTB-DIAGNOSIS (GTB-INDEX) TO WS-HOLD-DIAGNOSIS-CODE.    ELKCLDRM
00175 *    IF GTB-DIAGNOSIS (GTB-INDEX) = DXSL-DX (DXSL-IDX)            ELKCLDRM
00176      IF WS-HOLD-CODE = DXSL-DX (DXSL-IDX)                         ELKCLDRM
00177         PERFORM 1300-UPDATE-MATCH-TOTALS                          ELKCLDRM
00178      ELSE                                                         ELKCLDRM
00179         IF WS-HOLD-CODE  > DXSL-DX (DXSL-IDX)                     ELKCLDRM
00180 *       IF GTB-DIAGNOSIS (GTB-INDEX) > DXSL-DX (DXSL-IDX)         ELKCLDRM
00181            SET DXSL-IDX UP BY 1                                   ELKCLDRM
00182      ELSE                                                         ELKCLDRM
00183            SET GTB-INDEX UP BY 1.                                 ELKCLDRM
00184                                                                   ELKCLDRM
00185 ******************************************************************ELKCLDRM
00186 *                                                                *ELKCLDRM
00187 *    UPDATE MATCH TOTALS.                                        *ELKCLDRM
00188 *                                                                *ELKCLDRM
00189 ******************************************************************ELKCLDRM
00190                                                                   ELKCLDRM
00191  1300-UPDATE-MATCH-TOTALS.                                        ELKCLDRM
00192                                                                   ELKCLDRM
00193      IF GTB-DIAG-INCLUDED (GTB-INDEX)                             ELKCLDRM
00194          CONTINUE                                                 ELKCLDRM
00195      ELSE                                                         ELKCLDRM
00196          IF GTB-DIAG-EXCLUDED (GTB-INDEX)                         ELKCLDRM
00197             ADD +1 TO WS-EXCLUDED                                 ELKCLDRM
00198      ELSE                                                         ELKCLDRM
00199          SET WS-MISSING-PARM TO TRUE.                             ELKCLDRM
00200      SET GTB-INDEX UP BY 1.                                       ELKCLDRM
00201      SET DXSL-IDX UP BY 1.                                        ELKCLDRM
00202                                                                   ELKCLDRM
