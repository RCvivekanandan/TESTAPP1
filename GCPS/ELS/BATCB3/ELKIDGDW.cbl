00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIDGDW
00003  PROGRAM-ID.           ELKIDGDW.                                     LV004
00004                                                                   ELKIDGDW
00005  AUTHOR.               BARBARA KEIB.                              ELKIDGDW
00006                                                                   ELKIDGDW
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIDGDW
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIDGDW
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIDGDW
00010                        233 N. MICHIGAN AVE                        ELKIDGDW
00011                        CHICAGO, ILLINOIS 60601                    ELKIDGDW
00012                                                                   ELKIDGDW
00013  DATE-WRITTEN.         09-OCT-1992.                               ELKIDGDW
00014                                                                   ELKIDGDW
00015  ENVIRONMENT DIVISION.                                            ELKIDGDW
00016                                                                   ELKIDGDW
00017  CONFIGURATION SECTION.                                           ELKIDGDW
00018                                                                   ELKIDGDW
00019  SOURCE-COMPUTER.      IBM-3090.                                  ELKIDGDW
00020  OBJECT-COMPUTER.      IBM-3090.                                  ELKIDGDW
00021                                                                   ELKIDGDW
00022 ******************************************************************ELKIDGDW
00023 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIDGDW
00024 *  ELKIDGDW - COMPUTE WEIGHTED LIST MATCH CONFIDENCE             *ELKIDGDW
00025 *             FACTOR FOR IDGD TABULAR.                           *ELKIDGDW
00026 *                                                                *ELKIDGDW
00027 *              THIS PROGRAM MATCHES AN #IDGD(DIAGNOSIS CODE INTRN*ELKIDGDW
00028 *              TAB) TO A WEIGHTED LIST OF DIAGNOSIS CODES AND    *ELKIDGDW
00029 *              PRODUCES A CONFIDENCE FACTOR WHICH REPRESENTS THE *ELKIDGDW
00030 *              DEGREE TO WHICH THE DIAGNOSIS CODES IN THE MATCH  *ELKIDGDW
00031 *              LIST ARE REPRESENTED IN THE #IDGD TABULAR.        *ELKIDGDW
00032 *                                                                *ELKIDGDW
00033 ******************************************************************ELKIDGDW
00034 *                      MAINTENANCE HISTORY                       *ELKIDGDW
00035 *                                                                *ELKIDGDW
00036 *                                                                *ELKIDGDW
00037 *  MOD     DATE      BY                    ACTION                *ELKIDGDW
00038 * ----- ----------- --- -----------------------------------------*ELKIDGDW
00039 * 01.00 09-OCT-1992 BAK CREATED                                  *ELKIDGDW
00040 *                                                                *ELKIDGDW
00041 * 01.06 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX         *ELKIDGDW
00042 *                             ASM RECOMPILES                     *ELKIDGDW
00043 * 02.00 07-MAY-2003 AKK       REGEN'D FOR EXPANSION OF THE       *ELKIDGDW
00044 *                             PROC/DIAG ARGUMENTS                *ELKIDGDW
00045 *                                                                *ELKIDGDW
00046 * 02.01 24-JUN-2003 AKK       USING WS FIELD TO MOVE 6 CHAR      *ELKIDGDW
00047 *                             DIAG CODE TO MOVE ARG FOR CALC     *ELKIDGDW
00048 *                             THIS IS TO AVOID LOTS OF CHANGES   *ELKIDGDW
00049 *                             TO CALC FOR FIELDS.                 ELKIDGDW
00050 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIDGDW
00051 *                                                                *ELKIDGDW
00052 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELKIDGDW
00053 *                                                                *ELKIDGDW
00054 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIDGDW
00055 *                             CICSCB3 COMPILER FIX             *  ELKIDGDW
00056 ******************************************************************ELKIDGDW
00057 /                                                                 ELKIDGDW
00058  DATA DIVISION.                                                   ELKIDGDW
00059                                                                   ELKIDGDW
00060  WORKING-STORAGE SECTION.                                         ELKIDGDW
00061                                                                   ELKIDGDW
00062  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIDGDW
00063    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIDGDW
00064    88  WS-INVALID-PARM                 VALUE +8.                  ELKIDGDW
00065    88  WS-MISSING-PARM                 VALUE +12.                 ELKIDGDW
00066    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIDGDW
00067                                                                   ELKIDGDW
00068  01  WS-HOLD-DIAGNOSIS-ARGUMENT.                                  ELKIDGDW
00069      05 WS-HOLD-DIAG             PIC X(06).                       ELKIDGDW
00070      05 WS-FILLER-4              PIC X(04).                       ELKIDGDW
00071                                                                   ELKIDGDW
00072  01  WS-ELIGIBLE-WEIGHT          PIC S9(04) COMP.                 ELKIDGDW
00073  01  WS-TOTAL-WEIGHT             PIC S9(04) COMP.                 ELKIDGDW
00074                                                                   ELKIDGDW
00075  01  WS-CF-ELIGIBLE              COMP-1.                          ELKIDGDW
00076  01  WS-CF-MATCHED               COMP-1.                          ELKIDGDW
00077  01  WS-CF-TOTAL                 COMP-1.                          ELKIDGDW
00078                                                                   ELKIDGDW
00079  01  WS-CONF-ZERO                 COMP-1   VALUE +0.000000E+00.   ELKIDGDW
00080  01  WS-CONF-NEGATIVE             COMP-1   VALUE -1.000000E+00.   ELKIDGDW
00081                                                                   ELKIDGDW
00082  01  WS-GX9-MAX-INDEX            INDEX.                           ELKIDGDW
00083 /                                                                 ELKIDGDW
00084  LINKAGE SECTION.                                                 ELKIDGDW
00085                                                                   ELKIDGDW
00086 /                                                                 ELKIDGDW
00087                                                                   ELKIDGDW
00088  01  IDGD-RECORD.                                                 ELKIDGDW
00089      COPY GCTIDGDC.                                               ELKIDGDW
00090                                                                   ELKIDGDW
00091                                                                   ELKIDGDW
00092      COPY ELSDXSWC.                                               ELKIDGDW
00093                                                                   ELKIDGDW
00094                                                                   ELKIDGDW
00095      COPY ELSCFDBC.                                               ELKIDGDW
00096 /*****************************************************************ELKIDGDW
00097 *                                                                *ELKIDGDW
00098 *    PROCEDURE DIVISION                                          *ELKIDGDW
00099 *                                                                *ELKIDGDW
00100 ******************************************************************ELKIDGDW
00101                                                                   ELKIDGDW
00102  PROCEDURE DIVISION USING IDGD-RECORD                             ELKIDGDW
00103                              DXSW-DX-TBL                          ELKIDGDW
00104                                CFDB-CNFDNC-FCTR-DATA-BLCK.        ELKIDGDW
00105                                                                   ELKIDGDW
00106  0000-DETERMINE-IDGD-CONFIDENCE.                                  ELKIDGDW
00107                                                                   ELKIDGDW
00108      PERFORM 0100-INITIALIZATION.                                 ELKIDGDW
00109      PERFORM 1000-PROCESS-IDGD-TABULAR.                           ELKIDGDW
00110      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIDGDW
00111      GOBACK.                                                      ELKIDGDW
00112                                                                   ELKIDGDW
00113 ******************************************************************ELKIDGDW
00114 *                                                                *ELKIDGDW
00115 *    INITIALIZATION                                              *ELKIDGDW
00116 *                                                                *ELKIDGDW
00117 ******************************************************************ELKIDGDW
00118                                                                   ELKIDGDW
00119  0100-INITIALIZATION.                                             ELKIDGDW
00120                                                                   ELKIDGDW
00121      MOVE ZEROS TO WS-ELIGIBLE-WEIGHT.                            ELKIDGDW
00122      MOVE ZEROS TO WS-CF-MATCHED.                                 ELKIDGDW
00123      MOVE ZEROS TO WS-TOTAL-WEIGHT.                               ELKIDGDW
00124      INITIALIZE CFDB-CF-IDGD.                                     ELKIDGDW
00125                                                                   ELKIDGDW
00126 ******************************************************************ELKIDGDW
00127 *                                                                *ELKIDGDW
00128 *    PROCESS-IDGD-TABULAR                                        *ELKIDGDW
00129 *                                                                *ELKIDGDW
00130 ******************************************************************ELKIDGDW
00131                                                                   ELKIDGDW
00132  1000-PROCESS-IDGD-TABULAR.                                       ELKIDGDW
00133                                                                   ELKIDGDW
00134      IF ADDRESS OF IDGD-RECORD = NULL OR                          ELKIDGDW
00135         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIDGDW
00136         SET WS-MISSING-PARM TO TRUE                               ELKIDGDW
00137      ELSE                                                         ELKIDGDW
00138      IF ADDRESS OF DXSW-DX-TBL = NULL                             ELKIDGDW
00139         SET WS-MISSING-PARM TO TRUE                               ELKIDGDW
00140      ELSE                                                         ELKIDGDW
00141      IF DXSW-NBR-ENTRS = ZERO                                     ELKIDGDW
00142         SET WS-MISSING-PARM TO TRUE                               ELKIDGDW
00143      ELSE                                                         ELKIDGDW
00144      IF WS-SUCCESSFUL-CALL                                        ELKIDGDW
00145         PERFORM 1100-MATCH-LIST-DXSW-CODES.                       ELKIDGDW
00146                                                                   ELKIDGDW
00147 ******************************************************************ELKIDGDW
00148 *                                                                *ELKIDGDW
00149 *    MATCH LIST OF DIAGNOSIS CODE MATCH LIST TO IDGD.            *ELKIDGDW
00150 *                                                                *ELKIDGDW
00151 ******************************************************************ELKIDGDW
00152                                                                   ELKIDGDW
00153  1100-MATCH-LIST-DXSW-CODES.                                      ELKIDGDW
00154                                                                   ELKIDGDW
00155      SET GX9-INDEX TO GX9-ENTRY-COUNT.                            ELKIDGDW
00156      SET WS-GX9-MAX-INDEX TO GX9-INDEX.                           ELKIDGDW
00157      SET GX9-INDEX TO 1.                                          ELKIDGDW
00158      SET DXSW-IDX TO 1.                                           ELKIDGDW
00159      PERFORM 1200-MATCH-IDGD-TAB-MATCH-LIST                       ELKIDGDW
00160           UNTIL DXSW-IDX > DXSW-NBR-ENTRS OR                      ELKIDGDW
00161              WS-MISSING-PARM.                                     ELKIDGDW
00162                                                                   ELKIDGDW
00163      MOVE WS-ELIGIBLE-WEIGHT TO WS-CF-ELIGIBLE                    ELKIDGDW
00164      MOVE WS-TOTAL-WEIGHT TO WS-CF-TOTAL                          ELKIDGDW
00165                                                                   ELKIDGDW
00166      IF GX9-ID-ARGUMENT-INCLUDED                                  ELKIDGDW
00167         COMPUTE WS-CF-MATCHED =                                   ELKIDGDW
00168                     WS-CF-TOTAL / WS-CF-ELIGIBLE                  ELKIDGDW
00169      ELSE                                                         ELKIDGDW
00170         COMPUTE WS-CF-MATCHED =                                   ELKIDGDW
00171                     WS-CF-TOTAL / WS-CF-ELIGIBLE *                ELKIDGDW
00172                           WS-CONF-NEGATIVE.                       ELKIDGDW
00173                                                                   ELKIDGDW
00174      IF GX9-ID-ARGUMENT-INCLUDED AND                              ELKIDGDW
00175              WS-CF-MATCHED = WS-CONF-ZERO                         ELKIDGDW
00176         MOVE WS-CONF-NEGATIVE TO WS-CF-MATCHED.                   ELKIDGDW
00177                                                                   ELKIDGDW
00178         MOVE WS-CF-MATCHED TO CFDB-CF-IDGD-WGHTD-LST-MTCH.        ELKIDGDW
00179                                                                   ELKIDGDW
00180 ******************************************************************ELKIDGDW
00181 *                                                                *ELKIDGDW
00182 *    MATCH IDGD TABULAR TO DIAGNOSIS CODE MATCH LIST             *ELKIDGDW
00183 *                                                                *ELKIDGDW
00184 ******************************************************************ELKIDGDW
00185                                                                   ELKIDGDW
00186  1200-MATCH-IDGD-TAB-MATCH-LIST.                                  ELKIDGDW
00187                                                                   ELKIDGDW
00188      IF GX9-INDEX > WS-GX9-MAX-INDEX                              ELKIDGDW
00189         ADD DXSW-WGHT (DXSW-IDX) TO WS-TOTAL-WEIGHT               ELKIDGDW
00190         SET DXSW-IDX UP BY 1                                      ELKIDGDW
00191      ELSE                                                         ELKIDGDW
00192 *    IF GX9-DIAGNOSIS-ARGUMENT (GX9-INDEX) =                      ELKIDGDW
00193      IF WS-HOLD-DIAG =                                            ELKIDGDW
00194           DXSW-DX (DXSW-IDX)                                      ELKIDGDW
00195         ADD DXSW-WGHT (DXSW-IDX) TO WS-ELIGIBLE-WEIGHT            ELKIDGDW
00196         SET DXSW-IDX UP BY 1                                      ELKIDGDW
00197         SET GX9-INDEX UP BY 1                                     ELKIDGDW
00198      ELSE                                                         ELKIDGDW
00199 *    IF GX9-DIAGNOSIS-ARGUMENT (GX9-INDEX) >                      ELKIDGDW
00200      IF WS-HOLD-DIAG >                                            ELKIDGDW
00201           DXSW-DX (DXSW-IDX)                                      ELKIDGDW
00202         ADD DXSW-WGHT (DXSW-IDX) TO WS-TOTAL-WEIGHT               ELKIDGDW
00203         SET DXSW-IDX UP BY 1                                      ELKIDGDW
00204      ELSE                                                         ELKIDGDW
00205         SET GX9-INDEX UP BY 1.                                    ELKIDGDW
00206                                                                   ELKIDGDW
