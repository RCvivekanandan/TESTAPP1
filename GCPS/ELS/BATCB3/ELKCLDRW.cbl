00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKCLDRW
00003  PROGRAM-ID.           ELKCLDRW.                                     LV003
00004                                                                   ELKCLDRW
00005  AUTHOR.               BARBARA KEIB.                              ELKCLDRW
00006                                                                   ELKCLDRW
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKCLDRW
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKCLDRW
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKCLDRW
00010                        233 N. MICHIGAN AVE                        ELKCLDRW
00011                        CHICAGO, ILLINOIS 60601                    ELKCLDRW
00012                                                                   ELKCLDRW
00013  DATE-WRITTEN.         23-SEP-1992.                               ELKCLDRW
00014                                                                   ELKCLDRW
00015  ENVIRONMENT DIVISION.                                            ELKCLDRW
00016                                                                   ELKCLDRW
00017  CONFIGURATION SECTION.                                           ELKCLDRW
00018                                                                   ELKCLDRW
00019  SOURCE-COMPUTER.      IBM-3090.                                  ELKCLDRW
00020  OBJECT-COMPUTER.      IBM-3090.                                  ELKCLDRW
00021                                                                   ELKCLDRW
00022 ******************************************************************ELKCLDRW
00023 *                                                                *ELKCLDRW
00024 *  ELKCLDRW - COMPUTE WEIGHTED LIST MATCH CONFIDENCE             *ELKCLDRW
00025 *             FACTOR FOR CLDR TABULAR.                           *ELKCLDRW
00026 *                                                                *ELKCLDRW
00027 *              THIS MODULE IS BEING CALLED BY ANY OTHER PROGRAM  *ELKCLDRW
00028 *              FOR THE PURPOSE OF READING THE CLDR TABULAR AND   *ELKCLDRW
00029 *              ADDING ALL THE INCLUDED/EXCLUDED DIAGNOSIS AND    *ELKCLDRW
00030 *              COMPARING THOSE AGAINST A LIST PASSED             *ELKCLDRW
00031 *              TO THIS MODULE AND COMING UP WITH A COMPUTED      *ELKCLDRW
00032 *              FIGURE OF INCLUDED DIAGNOSIS CODES VERSUS TOTAL   *ELKCLDRW
00033 *              OF LIST FILE.                                     *ELKCLDRW
00034 *                                                                *ELKCLDRW
00035 ******************************************************************ELKCLDRW
00036 *                      MAINTENANCE HISTORY                       *ELKCLDRW
00037 *                                                                *ELKCLDRW
00038 *  MOD     DATE      BY                    ACTION                *ELKCLDRW
00039 * ----- ----------- --- -----------------------------------------*ELKCLDRW
00040 * 01.00 22-SEP-1992 BAK CREATED                                  *ELKCLDRW
00041 *                                                                *ELKCLDRW
00042 * 01.06 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX         *ELKCLDRW
00043 *                             ASM RECOMPILES                     *ELKCLDRW
00044 *                                                                *ELKCLDRW
00045 * 02.00 24-JUN-2003 AKK       CHANGES TO SPPORT FILE EXPANSION   *ELKCLDRW
00046 *                             NOT USING 10 CHAR DIAGNOSIS CODE   *ELKCLDRW
00047 *                             USING 6. LESS AFFECT ON PMCI.      *ELKCLDRW
00048 *                                                                *ELKCLDRW
00049 *       19-AUG-2003 AKK       REGENNING AS MOVE DOES NOT RECOG-  *ELKCLDRW
00050 *                             NIZE CHANGES AND WONT' MOVE     -  *ELKCLDRW
00051 *       09-JAN-2004 AKK       S0C7 INTERTEST                     *ELKCLDRW
00052 ******************************************************************ELKCLDRW
00053 /                                                                 ELKCLDRW
00054  DATA DIVISION.                                                   ELKCLDRW
00055  WORKING-STORAGE SECTION.                                         ELKCLDRW
00056                                                                   ELKCLDRW
00057  01  WS-HOLD-DIAGNOSIS-CODE.                                      ELKCLDRW
00058      05 WS-HOLD-CODE             PIC X(06).                       ELKCLDRW
00059      05 WS-HOLD-FILLER           PIC X(04).                       ELKCLDRW
00060                                                                   ELKCLDRW
00061  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKCLDRW
00062    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKCLDRW
00063    88  WS-INVALID-PARM                 VALUE +8.                  ELKCLDRW
00064    88  WS-MISSING-PARM                 VALUE +12.                 ELKCLDRW
00065    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKCLDRW
00066                                                                   ELKCLDRW
00067  01  WS-EXCLUDED-WEIGHT          PIC S9(04) COMP.                 ELKCLDRW
00068  01  WS-INCLUDED-WEIGHT          PIC S9(04) COMP.                 ELKCLDRW
00069  01  WS-MATCHED-WEIGHT           PIC S9(04) COMP.                 ELKCLDRW
00070  01  WS-TOTAL-WEIGHT             PIC S9(04) COMP.                 ELKCLDRW
00071                                                                   ELKCLDRW
00072  01  WS-GTB-MAX-INDEX            INDEX.                           ELKCLDRW
00073 /                                                                 ELKCLDRW
00074  LINKAGE SECTION.                                                 ELKCLDRW
00075                                                                   ELKCLDRW
00076 /                                                                 ELKCLDRW
00077                                                                   ELKCLDRW
00078  01  CLDR-RECORD.                                                 ELKCLDRW
00079      COPY GCTCLDRC.                                               ELKCLDRW
00080                                                                   ELKCLDRW
00081                                                                   ELKCLDRW
00082      COPY ELSDXSWC.                                               ELKCLDRW
00083                                                                   ELKCLDRW
00084                                                                   ELKCLDRW
00085      COPY ELSCFDBC.                                               ELKCLDRW
00086 /*****************************************************************ELKCLDRW
00087 *                                                                *ELKCLDRW
00088 *    PROCEDURE DIVISION                                          *ELKCLDRW
00089 *                                                                *ELKCLDRW
00090 ******************************************************************ELKCLDRW
00091                                                                   ELKCLDRW
00092  PROCEDURE DIVISION USING CLDR-RECORD DXSW-DX-TBL                 ELKCLDRW
00093                                CFDB-CNFDNC-FCTR-DATA-BLCK.        ELKCLDRW
00094                                                                   ELKCLDRW
00095  0000-DETERMINE-CLDR-CONFIDENCE.                                  ELKCLDRW
00096                                                                   ELKCLDRW
00097      PERFORM 0100-INITIALIZATION.                                 ELKCLDRW
00098      PERFORM 1000-PROCESS-CLDR-TABULAR.                           ELKCLDRW
00099      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKCLDRW
00100      GOBACK.                                                      ELKCLDRW
00101                                                                   ELKCLDRW
00102 ******************************************************************ELKCLDRW
00103 *                                                                *ELKCLDRW
00104 *    INITIALIZATION                                              *ELKCLDRW
00105 *                                                                *ELKCLDRW
00106 ******************************************************************ELKCLDRW
00107                                                                   ELKCLDRW
00108  0100-INITIALIZATION.                                             ELKCLDRW
00109                                                                   ELKCLDRW
00110      MOVE ZEROS TO WS-EXCLUDED-WEIGHT.                            ELKCLDRW
00111      MOVE ZEROS TO WS-INCLUDED-WEIGHT.                            ELKCLDRW
00112      MOVE ZEROS TO WS-MATCHED-WEIGHT.                             ELKCLDRW
00113      MOVE ZEROS TO WS-TOTAL-WEIGHT.                               ELKCLDRW
00114      INITIALIZE CFDB-CF-CLDR.                                     ELKCLDRW
00115                                                                   ELKCLDRW
00116 ******************************************************************ELKCLDRW
00117 *                                                                *ELKCLDRW
00118 *    PROCESS-CLDR-TABULAR                                        *ELKCLDRW
00119 *                                                                *ELKCLDRW
00120 ******************************************************************ELKCLDRW
00121                                                                   ELKCLDRW
00122  1000-PROCESS-CLDR-TABULAR.                                       ELKCLDRW
00123                                                                   ELKCLDRW
00124      IF ADDRESS OF CLDR-RECORD = NULL                             ELKCLDRW
00125         SET WS-MISSING-PARM TO TRUE                               ELKCLDRW
00126      ELSE                                                         ELKCLDRW
00127      IF ADDRESS OF DXSW-DX-TBL = NULL                             ELKCLDRW
00128         SET WS-MISSING-PARM TO TRUE                               ELKCLDRW
00129      ELSE                                                         ELKCLDRW
00130      IF DXSW-NBR-ENTRS = ZERO                                     ELKCLDRW
00131         SET WS-MISSING-PARM TO TRUE                               ELKCLDRW
00132      ELSE                                                         ELKCLDRW
00133      IF WS-SUCCESSFUL-CALL                                        ELKCLDRW
00134         PERFORM 1100-MATCH-LIST-DIAG-CODES.                       ELKCLDRW
00135                                                                   ELKCLDRW
00136 ******************************************************************ELKCLDRW
00137 *                                                                *ELKCLDRW
00138 *    MATCH LIST OF DIAGNOSIS CODES TO CLDR.                      *ELKCLDRW
00139 *                                                                *ELKCLDRW
00140 ******************************************************************ELKCLDRW
00141                                                                   ELKCLDRW
00142  1100-MATCH-LIST-DIAG-CODES.                                      ELKCLDRW
00143                                                                   ELKCLDRW
00144      SET GTB-INDEX TO GTB-ENTRY-COUNT.                            ELKCLDRW
00145      SET WS-GTB-MAX-INDEX TO GTB-INDEX.                           ELKCLDRW
00146      SET GTB-INDEX TO 1.                                          ELKCLDRW
00147      SET DXSW-IDX TO 1.                                           ELKCLDRW
00148      PERFORM 1200-MATCH-CLDR-TAB-MATCH-LIST                       ELKCLDRW
00149           UNTIL DXSW-IDX > DXSW-NBR-ENTRS OR                      ELKCLDRW
00150              WS-MISSING-PARM.                                     ELKCLDRW
00151      IF WS-SUCCESSFUL-CALL                                        ELKCLDRW
00152         COMPUTE WS-INCLUDED-WEIGHT =                              ELKCLDRW
00153                    WS-TOTAL-WEIGHT - WS-EXCLUDED-WEIGHT           ELKCLDRW
00154         COMPUTE WS-MATCHED-WEIGHT =                               ELKCLDRW
00155                ((WS-INCLUDED-WEIGHT / WS-TOTAL-WEIGHT) * 2) - 1   ELKCLDRW
00156         MOVE WS-MATCHED-WEIGHT TO CFDB-CF-CLDR-WGHTD-LST-MTCH.    ELKCLDRW
00157                                                                   ELKCLDRW
00158 ******************************************************************ELKCLDRW
00159 *                                                                *ELKCLDRW
00160 *    MATCH CLDR TABULAR TO DIAGNOSIS CODE MATCH LIST             *ELKCLDRW
00161 *                                                                *ELKCLDRW
00162 ******************************************************************ELKCLDRW
00163                                                                   ELKCLDRW
00164  1200-MATCH-CLDR-TAB-MATCH-LIST.                                  ELKCLDRW
00165                                                                   ELKCLDRW
00166      MOVE GTB-DIAGNOSIS (GTB-INDEX) TO WS-HOLD-DIAGNOSIS-CODE.    ELKCLDRW
00167                                                                   ELKCLDRW
00168      IF GTB-INDEX > WS-GTB-MAX-INDEX                              ELKCLDRW
00169         ADD DXSW-WGHT (DXSW-IDX) TO WS-TOTAL-WEIGHT               ELKCLDRW
00170         SET GTB-INDEX UP BY 1.                                    ELKCLDRW
00171      IF WS-HOLD-CODE = DXSW-DX (DXSW-IDX)                         ELKCLDRW
00172 *    IF GTB-DIAGNOSIS (GTB-INDEX) = DXSW-DX (DXSW-IDX)            ELKCLDRW
00173         PERFORM 1300-UPDATE-MATCH-TOTALS                          ELKCLDRW
00174      ELSE                                                         ELKCLDRW
00175      IF WS-HOLD-CODE = DXSW-DX (DXSW-IDX)                         ELKCLDRW
00176 *    IF GTB-DIAGNOSIS (GTB-INDEX) > DXSW-DX (DXSW-IDX)            ELKCLDRW
00177         ADD DXSW-WGHT (DXSW-IDX) TO WS-TOTAL-WEIGHT               ELKCLDRW
00178         SET DXSW-IDX UP BY 1                                      ELKCLDRW
00179      ELSE                                                         ELKCLDRW
00180         SET GTB-INDEX UP BY 1.                                    ELKCLDRW
00181                                                                   ELKCLDRW
00182 ******************************************************************ELKCLDRW
00183 *                                                                *ELKCLDRW
00184 *    UPDATE MATCH TOTALS.                                        *ELKCLDRW
00185 *                                                                *ELKCLDRW
00186 ******************************************************************ELKCLDRW
00187                                                                   ELKCLDRW
00188  1300-UPDATE-MATCH-TOTALS.                                        ELKCLDRW
00189                                                                   ELKCLDRW
00190      IF GTB-DIAG-INCLUDED (GTB-INDEX)                             ELKCLDRW
00191          CONTINUE                                                 ELKCLDRW
00192      ELSE                                                         ELKCLDRW
00193          IF GTB-DIAG-EXCLUDED (GTB-INDEX)                         ELKCLDRW
00194             ADD DXSW-WGHT (DXSW-IDX) TO WS-EXCLUDED-WEIGHT        ELKCLDRW
00195      ELSE                                                         ELKCLDRW
00196          SET WS-MISSING-PARM TO TRUE.                             ELKCLDRW
00197      ADD DXSW-WGHT (DXSW-IDX) TO WS-TOTAL-WEIGHT.                 ELKCLDRW
00198      SET GTB-INDEX UP BY 1.                                       ELKCLDRW
00199      SET DXSW-IDX UP BY 1.                                        ELKCLDRW
00200                                                                   ELKCLDRW
