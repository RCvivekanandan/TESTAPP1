00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIBGRW
00003  PROGRAM-ID.           ELKIBGRW.                                     LV004
00004                                                                   ELKIBGRW
00005  AUTHOR.               BARBARA KEIB.                              ELKIBGRW
00006                                                                   ELKIBGRW
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIBGRW
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIBGRW
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIBGRW
00010                        233 N. MICHIGAN AVE                        ELKIBGRW
00011                        CHICAGO, ILLINOIS 60601                    ELKIBGRW
00012                                                                   ELKIBGRW
00013                                                                   ELKIBGRW
00014  DATE-WRITTEN.         08-OCT-1992.                               ELKIBGRW
00015                                                                   ELKIBGRW
00016  ENVIRONMENT DIVISION.                                            ELKIBGRW
00017                                                                   ELKIBGRW
00018  CONFIGURATION SECTION.                                           ELKIBGRW
00019                                                                   ELKIBGRW
00020  SOURCE-COMPUTER.      IBM-3090.                                  ELKIBGRW
00021  OBJECT-COMPUTER.      IBM-3090.                                  ELKIBGRW
00022                                                                   ELKIBGRW
00023 ******************************************************************ELKIBGRW
00024 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIBGRW
00025 *  ELKIBGRW - COMPUTE WEIGHTED LIST MATCH CONFIDENCE             *ELKIBGRW
00026 *             FACTOR FOR IBGR TABULAR.                           *ELKIBGRW
00027 *                                                                *ELKIBGRW
00028 *              THIS PROGRAM MATCHES AN #IBGR(BPV INTERNAL TAB)   *ELKIBGRW
00029 *              TO A WEIGHTED LIST OF BENEFIT PROVISIONS AND      *ELKIBGRW
00030 *              PRODUCES A CONFIDENCE FACTOR WHICH REPRESENTS THE *ELKIBGRW
00031 *              DEGREE TO WHICH THE BENEFIT PROVISIONS IN THE     *ELKIBGRW
00032 *              MATCH LIST ARE REPRESENTED IN THE #IBGR TABULAR.  *ELKIBGRW
00033 *                                                                *ELKIBGRW
00034 ******************************************************************ELKIBGRW
00035 *                      MAINTENANCE HISTORY                       *ELKIBGRW
00036 *  MOD     DATE      BY                    ACTION                *ELKIBGRW
00037 * ----- ----------- --- -----------------------------------------*ELKIBGRW
00038 * 01.00 08-OCT-1992 BAK CREATED                                  *ELKIBGRW
00039 *                                                                *ELKIBGRW
00040 *                                                                *ELKIBGRW
00041 * 01.06 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX         *ELKIBGRW
00042 *                             ASM RECOMPILES                     *ELKIBGRW
00043 *                                                                *ELKIBGRW
00044 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIBGRW
00045 *                                                                *ELKIBGRW
00046 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELKIBGRW
00047 *                                                                *ELKIBGRW
00048 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    * *ELKIBGRW
00049 *                             CICSCB3 COMPILER FIX             * *ELKIBGRW
00050 ******************************************************************ELKIBGRW
00051 /                                                                 ELKIBGRW
00052  DATA DIVISION.                                                   ELKIBGRW
00053                                                                   ELKIBGRW
00054  WORKING-STORAGE SECTION.                                         ELKIBGRW
00055                                                                   ELKIBGRW
00056  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIBGRW
00057    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIBGRW
00058    88  WS-INVALID-PARM                 VALUE +8.                  ELKIBGRW
00059    88  WS-MISSING-PARM                 VALUE +12.                 ELKIBGRW
00060    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIBGRW
00061                                                                   ELKIBGRW
00062  01  WS-ELIGIBLE-WEIGHT          PIC S9(04) COMP.                 ELKIBGRW
00063  01  WS-TOTAL-WEIGHT             PIC S9(04) COMP.                 ELKIBGRW
00064                                                                   ELKIBGRW
00065  01  WS-CF-MATCHED               COMP-1.                          ELKIBGRW
00066  01  WS-CF-ELIGIBLE              COMP-1.                          ELKIBGRW
00067  01  WS-CF-TOTAL                 COMP-1.                          ELKIBGRW
00068                                                                   ELKIBGRW
00069  01  WS-CONF-ZERO            COMP-1  VALUE +0.000000E+00.         ELKIBGRW
00070  01  WS-CONF-NEGATIVE        COMP-1  VALUE -1.000000E+00.         ELKIBGRW
00071                                                                   ELKIBGRW
00072  01  WS-GX1-MAX-INDEX            INDEX.                           ELKIBGRW
00073 /                                                                 ELKIBGRW
00074  LINKAGE SECTION.                                                 ELKIBGRW
00075                                                                   ELKIBGRW
00076 /                                                                 ELKIBGRW
00077                                                                   ELKIBGRW
00078  01  IBGR-RECORD.                                                 ELKIBGRW
00079      COPY GCTIBGRC.                                               ELKIBGRW
00080                                                                   ELKIBGRW
00081                                                                   ELKIBGRW
00082      COPY ELSBPVWC.                                               ELKIBGRW
00083                                                                   ELKIBGRW
00084                                                                   ELKIBGRW
00085      COPY ELSCFDBC.                                               ELKIBGRW
00086 /*****************************************************************ELKIBGRW
00087 *                                                                *ELKIBGRW
00088 *    PROCEDURE DIVISION                                          *ELKIBGRW
00089 *                                                                *ELKIBGRW
00090 ******************************************************************ELKIBGRW
00091                                                                   ELKIBGRW
00092  PROCEDURE DIVISION USING IBGR-RECORD                             ELKIBGRW
00093                              BPVW-BNFT-PRVSN-TBL                  ELKIBGRW
00094                                CFDB-CNFDNC-FCTR-DATA-BLCK.        ELKIBGRW
00095                                                                   ELKIBGRW
00096  0000-DETERMINE-IBGR-CONFIDENCE.                                  ELKIBGRW
00097                                                                   ELKIBGRW
00098      PERFORM 0100-INITIALIZATION.                                 ELKIBGRW
00099      PERFORM 1000-PROCESS-IBGR-TABULAR.                           ELKIBGRW
00100      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIBGRW
00101      GOBACK.                                                      ELKIBGRW
00102                                                                   ELKIBGRW
00103 ******************************************************************ELKIBGRW
00104 *                                                                *ELKIBGRW
00105 *    INITIALIZATION                                              *ELKIBGRW
00106 *                                                                *ELKIBGRW
00107 ******************************************************************ELKIBGRW
00108                                                                   ELKIBGRW
00109  0100-INITIALIZATION.                                             ELKIBGRW
00110                                                                   ELKIBGRW
00111      MOVE ZEROS TO WS-ELIGIBLE-WEIGHT.                            ELKIBGRW
00112      MOVE ZEROS TO WS-TOTAL-WEIGHT.                               ELKIBGRW
00113      INITIALIZE CFDB-CF-IBGR.                                     ELKIBGRW
00114                                                                   ELKIBGRW
00115 ******************************************************************ELKIBGRW
00116 *                                                                *ELKIBGRW
00117 *    PROCESS-IBGR-TABULAR                                        *ELKIBGRW
00118 *                                                                *ELKIBGRW
00119 ******************************************************************ELKIBGRW
00120                                                                   ELKIBGRW
00121  1000-PROCESS-IBGR-TABULAR.                                       ELKIBGRW
00122                                                                   ELKIBGRW
00123      IF ADDRESS OF IBGR-RECORD = NULL OR                          ELKIBGRW
00124         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIBGRW
00125         SET WS-MISSING-PARM TO TRUE                               ELKIBGRW
00126      ELSE                                                         ELKIBGRW
00127      IF ADDRESS OF BPVW-BNFT-PRVSN-TBL = NULL                     ELKIBGRW
00128         SET WS-MISSING-PARM TO TRUE                               ELKIBGRW
00129      ELSE                                                         ELKIBGRW
00130      IF BPVW-NBR-ENTRS = ZERO                                     ELKIBGRW
00131         SET WS-MISSING-PARM TO TRUE                               ELKIBGRW
00132      ELSE                                                         ELKIBGRW
00133      IF WS-SUCCESSFUL-CALL                                        ELKIBGRW
00134         PERFORM 1100-MATCH-LIST-BPVW-CODES.                       ELKIBGRW
00135                                                                   ELKIBGRW
00136 ******************************************************************ELKIBGRW
00137 *                                                                *ELKIBGRW
00138 *    MATCH LIST OF BENEFIT PROV MATCH LIST TO IBGR.              *ELKIBGRW
00139 *                                                                *ELKIBGRW
00140 ******************************************************************ELKIBGRW
00141                                                                   ELKIBGRW
00142  1100-MATCH-LIST-BPVW-CODES.                                      ELKIBGRW
00143                                                                   ELKIBGRW
00144      SET GX1-INDEX TO GX1-ENTRY-COUNT.                            ELKIBGRW
00145      SET WS-GX1-MAX-INDEX TO GX1-INDEX.                           ELKIBGRW
00146      SET GX1-INDEX TO 1.                                          ELKIBGRW
00147      SET BPVW-IDX TO 1.                                           ELKIBGRW
00148      PERFORM 1200-MATCH-IBGR-TAB-MATCH-LIST                       ELKIBGRW
00149           UNTIL BPVW-IDX > BPVW-NBR-ENTRS.                        ELKIBGRW
00150                                                                   ELKIBGRW
00151      MOVE WS-ELIGIBLE-WEIGHT TO WS-CF-ELIGIBLE                    ELKIBGRW
00152      MOVE WS-TOTAL-WEIGHT TO WS-CF-TOTAL                          ELKIBGRW
00153                                                                   ELKIBGRW
00154      IF GX1-ID-ARGUMENT-INCLUDED                                  ELKIBGRW
00155         COMPUTE WS-CF-MATCHED =                                   ELKIBGRW
00156                    WS-CF-TOTAL / WS-CF-ELIGIBLE                   ELKIBGRW
00157         ELSE                                                      ELKIBGRW
00158            COMPUTE WS-CF-MATCHED =                                ELKIBGRW
00159                 (WS-CF-TOTAL / WS-CF-ELIGIBLE) *                  ELKIBGRW
00160                                  WS-CONF-NEGATIVE.                ELKIBGRW
00161                                                                   ELKIBGRW
00162      IF GX1-ID-ARGUMENT-INCLUDED AND                              ELKIBGRW
00163              WS-CF-MATCHED = WS-CONF-ZERO                         ELKIBGRW
00164         MOVE WS-CONF-NEGATIVE TO WS-CF-MATCHED.                   ELKIBGRW
00165                                                                   ELKIBGRW
00166      MOVE WS-CF-MATCHED TO CFDB-CF-IBGR-WGHTD-LST-MTCH.           ELKIBGRW
00167                                                                   ELKIBGRW
00168 ******************************************************************ELKIBGRW
00169 *                                                                *ELKIBGRW
00170 *    MATCH IBGR TABULAR TO DIAGNOSIS CODE MATCH LIST             *ELKIBGRW
00171 *                                                                *ELKIBGRW
00172 ******************************************************************ELKIBGRW
00173                                                                   ELKIBGRW
00174  1200-MATCH-IBGR-TAB-MATCH-LIST.                                  ELKIBGRW
00175                                                                   ELKIBGRW
00176      IF GX1-INDEX > WS-GX1-MAX-INDEX                              ELKIBGRW
00177         ADD  BPVW-WGHT (BPVW-IDX) TO WS-TOTAL-WEIGHT              ELKIBGRW
00178         SET BPVW-IDX UP BY 1                                      ELKIBGRW
00179      ELSE                                                         ELKIBGRW
00180         IF GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) =                ELKIBGRW
00181                 BPVW-BNFT-PRVSN (BPVW-IDX)                        ELKIBGRW
00182            ADD  BPVW-WGHT (BPVW-IDX) TO WS-ELIGIBLE-WEIGHT        ELKIBGRW
00183            SET GX1-INDEX UP BY 1                                  ELKIBGRW
00184            SET BPVW-IDX UP BY 1                                   ELKIBGRW
00185      ELSE                                                         ELKIBGRW
00186         IF GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) >                ELKIBGRW
00187                  BPVW-BNFT-PRVSN (BPVW-IDX)                       ELKIBGRW
00188            ADD  BPVW-WGHT (BPVW-IDX) TO WS-TOTAL-WEIGHT           ELKIBGRW
00189            SET BPVW-IDX UP BY 1                                   ELKIBGRW
00190      ELSE                                                         ELKIBGRW
00191            SET GX1-INDEX UP BY 1.                                 ELKIBGRW
00192                                                                   ELKIBGRW
