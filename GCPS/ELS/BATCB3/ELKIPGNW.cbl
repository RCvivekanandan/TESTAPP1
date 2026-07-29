00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGNW
00003  PROGRAM-ID.           ELKIPGNW.                                     LV004
00004                                                                   ELKIPGNW
00005  AUTHOR.               BARBARA KEIB.                              ELKIPGNW
00006                                                                   ELKIPGNW
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGNW
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGNW
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGNW
00010                        233 N. MICHIGAN AVE                        ELKIPGNW
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGNW
00012                                                                   ELKIPGNW
00013                                                                   ELKIPGNW
00014  DATE-WRITTEN.         08-OCT-1992.                               ELKIPGNW
00015                                                                   ELKIPGNW
00016  ENVIRONMENT DIVISION.                                            ELKIPGNW
00017                                                                   ELKIPGNW
00018  CONFIGURATION SECTION.                                           ELKIPGNW
00019                                                                   ELKIPGNW
00020  SOURCE-COMPUTER.      IBM-3090.                                  ELKIPGNW
00021  OBJECT-COMPUTER.      IBM-3090.                                  ELKIPGNW
00022                                                                   ELKIPGNW
00023 ******************************************************************ELKIPGNW
00024 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIPGNW
00025 *                                                                *ELKIPGNW
00026 *  ELKIPGNW - COMPUTE WEIGHTED LIST MATCH CONFIDENCE             *ELKIPGNW
00027 *             FACTOR FOR IPGN TABULAR.                           *ELKIPGNW
00028 *                                                                *ELKIPGNW
00029 *              THIS PROGRAM MATCHES AN #IPGN(PROV. NBR INTRNL TAB*ELKIPGNW
00030 *              TO A WEIGHTED LIST OF PROVIDER NUMBERS AND        *ELKIPGNW
00031 *              PRODUCES A CONFIDENCE FACTOR WHICH REPRESENTS THE *ELKIPGNW
00032 *              DEGREE TO WHICH THE PROVIDER NUMBERS IN THE MATCH *ELKIPGNW
00033 *              LIST ARE REPRESENTED IN THE #IPGN TABULAR.        *ELKIPGNW
00034 *                                                                *ELKIPGNW
00035 ******************************************************************ELKIPGNW
00036 *                      MAINTENANCE HISTORY                       *ELKIPGNW
00037 *                                                                *ELKIPGNW
00038 *  MOD     DATE      BY                    ACTION                *ELKIPGNW
00039 * ----- ----------- --- -----------------------------------------*ELKIPGNW
00040 * 01.00 08-OCT-1992 BAK CREATED                                  *ELKIPGNW
00041 *                                                                *ELKIPGNW
00042 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGNW
00043 *                                                                *ELKIPGNW
00044 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGNW
00045 *                                                                *ELKIPGNW
00046 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST COMPILE                   *ELKIPGNW
00047 *                                                                *ELKIPGNW
00048 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGNW
00049 *                             CICSCB3 COMPILER FIX             *  ELKIPGNW
00050 /                                                                 ELKIPGNW
00051  DATA DIVISION.                                                   ELKIPGNW
00052                                                                   ELKIPGNW
00053  WORKING-STORAGE SECTION.                                         ELKIPGNW
00054                                                                   ELKIPGNW
00055  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIPGNW
00056    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIPGNW
00057    88  WS-INVALID-PARM                 VALUE +8.                  ELKIPGNW
00058    88  WS-MISSING-PARM                 VALUE +12.                 ELKIPGNW
00059    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIPGNW
00060                                                                   ELKIPGNW
00061  01  WS-ELIGIBLE-WEIGHT          PIC S9(04) COMP.                 ELKIPGNW
00062  01  WS-MATCHED-WEIGHT           PIC S9(04) COMP.                 ELKIPGNW
00063  01  WS-TOTAL-WEIGHT             PIC S9(04) COMP.                 ELKIPGNW
00064                                                                   ELKIPGNW
00065  01  WS-CF-ELIGIBLE              COMP-1.                          ELKIPGNW
00066  01  WS-CF-MATCHED               COMP-1.                          ELKIPGNW
00067  01  WS-CF-TOTAL                 COMP-1.                          ELKIPGNW
00068                                                                   ELKIPGNW
00069  01  WS-CONF-ZERO           COMP-1  VALUE +0.000000E+00.          ELKIPGNW
00070  01  WS-CONF-NEGATIVE       COMP-1  VALUE -1.000000E+00.          ELKIPGNW
00071                                                                   ELKIPGNW
00072  01  WS-GX2-MAX-INDEX            INDEX.                           ELKIPGNW
00073 /                                                                 ELKIPGNW
00074  LINKAGE SECTION.                                                 ELKIPGNW
00075                                                                   ELKIPGNW
00076 /                                                                 ELKIPGNW
00077                                                                   ELKIPGNW
00078  01  IPGN-RECORD.                                                 ELKIPGNW
00079      COPY GCTIPGNC.                                               ELKIPGNW
00080                                                                   ELKIPGNW
00081                                                                   ELKIPGNW
00082      COPY ELSPVNWC.                                               ELKIPGNW
00083                                                                   ELKIPGNW
00084                                                                   ELKIPGNW
00085      COPY ELSCFDBC.                                               ELKIPGNW
00086 /*****************************************************************ELKIPGNW
00087 *                                                                *ELKIPGNW
00088 *    PROCEDURE DIVISION                                          *ELKIPGNW
00089 *                                                                *ELKIPGNW
00090 ******************************************************************ELKIPGNW
00091                                                                   ELKIPGNW
00092  PROCEDURE DIVISION USING IPGN-RECORD                             ELKIPGNW
00093                              PVNW-PRVDR-NBR-TBL                   ELKIPGNW
00094                                CFDB-CNFDNC-FCTR-DATA-BLCK.        ELKIPGNW
00095                                                                   ELKIPGNW
00096  0000-DETERMINE-IPGN-CONFIDENCE.                                  ELKIPGNW
00097                                                                   ELKIPGNW
00098      PERFORM 0100-INITIALIZATION.                                 ELKIPGNW
00099      PERFORM 1000-PROCESS-IPGN-TABULAR.                           ELKIPGNW
00100      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGNW
00101      GOBACK.                                                      ELKIPGNW
00102                                                                   ELKIPGNW
00103 ******************************************************************ELKIPGNW
00104 *                                                                *ELKIPGNW
00105 *    INITIALIZATION                                              *ELKIPGNW
00106 *                                                                *ELKIPGNW
00107 ******************************************************************ELKIPGNW
00108                                                                   ELKIPGNW
00109  0100-INITIALIZATION.                                             ELKIPGNW
00110                                                                   ELKIPGNW
00111      MOVE ZEROS TO WS-ELIGIBLE-WEIGHT.                            ELKIPGNW
00112      MOVE ZEROS TO WS-MATCHED-WEIGHT.                             ELKIPGNW
00113      MOVE ZEROS TO WS-TOTAL-WEIGHT.                               ELKIPGNW
00114      INITIALIZE CFDB-CF-IPGN.                                     ELKIPGNW
00115                                                                   ELKIPGNW
00116 ******************************************************************ELKIPGNW
00117 *                                                                *ELKIPGNW
00118 *    PROCESS-IPGN-TABULAR                                        *ELKIPGNW
00119 *                                                                *ELKIPGNW
00120 ******************************************************************ELKIPGNW
00121                                                                   ELKIPGNW
00122  1000-PROCESS-IPGN-TABULAR.                                       ELKIPGNW
00123                                                                   ELKIPGNW
00124      IF ADDRESS OF IPGN-RECORD = NULL OR                          ELKIPGNW
00125         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIPGNW
00126         SET WS-MISSING-PARM TO TRUE                               ELKIPGNW
00127      ELSE                                                         ELKIPGNW
00128      IF ADDRESS OF PVNW-PRVDR-NBR-TBL = NULL                      ELKIPGNW
00129         SET WS-MISSING-PARM TO TRUE                               ELKIPGNW
00130      ELSE                                                         ELKIPGNW
00131      IF PVNW-NBR-ENTRS = ZERO                                     ELKIPGNW
00132         SET WS-MISSING-PARM TO TRUE                               ELKIPGNW
00133      ELSE                                                         ELKIPGNW
00134      IF WS-SUCCESSFUL-CALL                                        ELKIPGNW
00135         PERFORM 1100-MATCH-LIST-PVNW-CODES.                       ELKIPGNW
00136                                                                   ELKIPGNW
00137 ******************************************************************ELKIPGNW
00138 *                                                                *ELKIPGNW
00139 *    MATCH LIST OF PROVIDER NUMBERS TO TO IPGN.                  *ELKIPGNW
00140 *                                                                *ELKIPGNW
00141 ******************************************************************ELKIPGNW
00142                                                                   ELKIPGNW
00143  1100-MATCH-LIST-PVNW-CODES.                                      ELKIPGNW
00144                                                                   ELKIPGNW
00145      SET GX2-INDEX TO GX2-ENTRY-COUNT.                            ELKIPGNW
00146      SET WS-GX2-MAX-INDEX TO GX2-INDEX.                           ELKIPGNW
00147      SET GX2-INDEX TO 1.                                          ELKIPGNW
00148      SET PVNW-IDX TO 1.                                           ELKIPGNW
00149      PERFORM 1200-MATCH-IPGN-TAB-MATCH-LIST                       ELKIPGNW
00150           UNTIL PVNW-IDX > PVNW-NBR-ENTRS OR                      ELKIPGNW
00151              WS-MISSING-PARM.                                     ELKIPGNW
00152                                                                   ELKIPGNW
00153      MOVE WS-ELIGIBLE-WEIGHT TO WS-CF-ELIGIBLE                    ELKIPGNW
00154      MOVE WS-TOTAL-WEIGHT TO WS-CF-TOTAL                          ELKIPGNW
00155                                                                   ELKIPGNW
00156      IF GX2-ID-ARGUMENT-INCLUDED                                  ELKIPGNW
00157         COMPUTE WS-CF-MATCHED =                                   ELKIPGNW
00158                 WS-CF-TOTAL / WS-CF-ELIGIBLE                      ELKIPGNW
00159      ELSE                                                         ELKIPGNW
00160         COMPUTE WS-CF-MATCHED =                                   ELKIPGNW
00161                 (WS-CF-TOTAL / WS-CF-ELIGIBLE) *                  ELKIPGNW
00162                                  WS-CONF-NEGATIVE.                ELKIPGNW
00163                                                                   ELKIPGNW
00164      IF GX2-ID-ARGUMENT-INCLUDED AND                              ELKIPGNW
00165                WS-CF-MATCHED = WS-CONF-ZERO                       ELKIPGNW
00166         MOVE WS-CONF-NEGATIVE TO WS-CF-MATCHED                    ELKIPGNW
00167                                                                   ELKIPGNW
00168      MOVE WS-CF-MATCHED TO CFDB-CF-IPGN-WGHTD-LST-MTCH.           ELKIPGNW
00169                                                                   ELKIPGNW
00170 ******************************************************************ELKIPGNW
00171 *                                                                *ELKIPGNW
00172 *    MATCH IPGN TABULAR TO DIAGNOSIS CODE MATCH LIST             *ELKIPGNW
00173 *                                                                *ELKIPGNW
00174 ******************************************************************ELKIPGNW
00175                                                                   ELKIPGNW
00176  1200-MATCH-IPGN-TAB-MATCH-LIST.                                  ELKIPGNW
00177                                                                   ELKIPGNW
00178      IF GX2-INDEX > WS-GX2-MAX-INDEX                              ELKIPGNW
00179         ADD PVNW-WGHT (PVNW-IDX) TO WS-TOTAL-WEIGHT               ELKIPGNW
00180         SET PVNW-IDX UP BY 1                                      ELKIPGNW
00181      IF GX2-PROVIDER-NO-ARGUMENT (GX2-INDEX) =                    ELKIPGNW
00182           PVNW-PRVDR-NBR (PVNW-IDX)                               ELKIPGNW
00183         ADD PVNW-WGHT (PVNW-IDX) TO WS-ELIGIBLE-WEIGHT            ELKIPGNW
00184         SET GX2-INDEX UP BY 1                                     ELKIPGNW
00185         SET PVNW-IDX UP BY 1                                      ELKIPGNW
00186      ELSE                                                         ELKIPGNW
00187      IF GX2-PROVIDER-NO-ARGUMENT (GX2-INDEX) >                    ELKIPGNW
00188           PVNW-PRVDR-NBR (PVNW-IDX)                               ELKIPGNW
00189         ADD PVNW-WGHT (PVNW-IDX) TO WS-TOTAL-WEIGHT               ELKIPGNW
00190         SET PVNW-IDX UP BY 1                                      ELKIPGNW
00191      ELSE                                                         ELKIPGNW
00192         SET GX2-INDEX UP BY 1.                                    ELKIPGNW
00193                                                                   ELKIPGNW
