00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGSW
00003  PROGRAM-ID.           ELKIPGSW.                                     LV004
00004                                                                   ELKIPGSW
00005  AUTHOR.               ANNE KEFFER KING.                          ELKIPGSW
00006                                                                   ELKIPGSW
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGSW
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGSW
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGSW
00010                        233 N. MICHIGAN AVE                        ELKIPGSW
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGSW
00012                                                                   ELKIPGSW
00013                                                                   ELKIPGSW
00014  DATE-WRITTEN.         25-AUG-2000.                               ELKIPGSW
00015                                                                   ELKIPGSW
00016  ENVIRONMENT DIVISION.                                            ELKIPGSW
00017                                                                   ELKIPGSW
00018  CONFIGURATION SECTION.                                           ELKIPGSW
00019                                                                   ELKIPGSW
00020  SOURCE-COMPUTER.      IBM-3090.                                  ELKIPGSW
00021  OBJECT-COMPUTER.      IBM-3090.                                  ELKIPGSW
00022                                                                   ELKIPGSW
00023 *AKK 12/06/05 REGEN FOR TESTING                                   ELKIPGSW
00024 ******************************************************************ELKIPGSW
00025 *                                                                *ELKIPGSW
00026 *  ELKIPGSW - COMPUTE WEIGHTED LIST MATCH CONFIDENCE             *ELKIPGSW
00027 *             FACTOR FOR IPGS TABULAR.                           *ELKIPGSW
00028 *                                                                *ELKIPGSW
00029 *              THIS PROGRAM MATCHES AN #IPGS(PROVIDER SPEC INTRNL*ELKIPGSW
00030 *              TAB) TO A WEIGHTED LIST OF PROVIDER SPECS AND     *ELKIPGSW
00031 *              PRODUCES A CONFIDENCE FACTOR WHICH REPRESENTS THE *ELKIPGSW
00032 *              DEGREE TO WHICH THE PROVIDER SPECS IN THE MATCH   *ELKIPGSW
00033 *              LIST ARE REPRESENTED IN THE #IPGS TABULAR.        *ELKIPGSW
00034 *                                                                *ELKIPGSW
00035 ******************************************************************ELKIPGSW
00036 *                      MAINTENANCE HISTORY                       *ELKIPGSW
00037 *                                                                *ELKIPGSW
00038 *  MOD     DATE      BY                    ACTION                *ELKIPGSW
00039 * ----- ----------- --- -----------------------------------------*ELKIPGSW
00040 * 01.00 25-AUG-2000 AKK CLONED FROM ELKIPGTW.                    *ELKIPGSW
00041 *                                                                *ELKIPGSW
00042 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGSW
00043 *                                                                *ELKIPGSW
00044 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGSW
00045 *                                                                *ELKIPGSW
00046 * 02.01 09-JAN-2004 AKK S0C7                                     *ELKIPGSW
00047 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGSW
00048 *                             CICSCB3 COMPILER FIX             *  ELKIPGSW
00049 *                                                                *ELKIPGSW
00050 ******************************************************************ELKIPGSW
00051 /                                                                 ELKIPGSW
00052  DATA DIVISION.                                                   ELKIPGSW
00053                                                                   ELKIPGSW
00054  WORKING-STORAGE SECTION.                                         ELKIPGSW
00055                                                                   ELKIPGSW
00056  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIPGSW
00057    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIPGSW
00058    88  WS-INVALID-PARM                 VALUE +8.                  ELKIPGSW
00059    88  WS-MISSING-PARM                 VALUE +12.                 ELKIPGSW
00060    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIPGSW
00061                                                                   ELKIPGSW
00062  01  WS-ELIGIBLE-WEIGHT          PIC S9(04) COMP.                 ELKIPGSW
00063  01  WS-MATCHED-WEIGHT           PIC S9(04) COMP.                 ELKIPGSW
00064  01  WS-TOTAL-WEIGHT             PIC S9(04) COMP.                 ELKIPGSW
00065                                                                   ELKIPGSW
00066  01  WS-CF-TOTAL                 COMP-1.                          ELKIPGSW
00067  01  WS-CF-ELIGIBLE              COMP-1.                          ELKIPGSW
00068  01  WS-CF-MATCHED               COMP-1.                          ELKIPGSW
00069                                                                   ELKIPGSW
00070  01  WS-CONF-ZERO            COMP-1  VALUE +0.000000E+00.         ELKIPGSW
00071  01  WS-CONF-NEGATIVE        COMP-1  VALUE -1.000000E+00.         ELKIPGSW
00072                                                                   ELKIPGSW
00073  01  WS-GXS-MAX-INDEX            INDEX.                           ELKIPGSW
00074 /                                                                 ELKIPGSW
00075  LINKAGE SECTION.                                                 ELKIPGSW
00076                                                                   ELKIPGSW
00077 /                                                                 ELKIPGSW
00078                                                                   ELKIPGSW
00079  01  IPGS-RECORD.                                                 ELKIPGSW
00080      COPY GCTIPGSC.                                               ELKIPGSW
00081                                                                   ELKIPGSW
00082                                                                   ELKIPGSW
00083      COPY ELSPVSWC.                                               ELKIPGSW
00084                                                                   ELKIPGSW
00085                                                                   ELKIPGSW
00086      COPY ELSCFDBC.                                               ELKIPGSW
00087 /*****************************************************************ELKIPGSW
00088 *                                                                *ELKIPGSW
00089 *    PROCEDURE DIVISION                                          *ELKIPGSW
00090 *                                                                *ELKIPGSW
00091 ******************************************************************ELKIPGSW
00092                                                                   ELKIPGSW
00093  PROCEDURE DIVISION USING IPGS-RECORD                             ELKIPGSW
00094                              PVSW-PRVDR-SPC-TBL                   ELKIPGSW
00095                                CFDB-CNFDNC-FCTR-DATA-BLCK.        ELKIPGSW
00096                                                                   ELKIPGSW
00097  0000-DETERMINE-IPGS-CONFIDENCE.                                  ELKIPGSW
00098                                                                   ELKIPGSW
00099      PERFORM 0100-INITIALIZATION.                                 ELKIPGSW
00100      PERFORM 1000-PROCESS-IPGS-TABULAR.                           ELKIPGSW
00101      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGSW
00102      GOBACK.                                                      ELKIPGSW
00103                                                                   ELKIPGSW
00104 ******************************************************************ELKIPGSW
00105 *                                                                *ELKIPGSW
00106 *    INITIALIZATION                                              *ELKIPGSW
00107 *                                                                *ELKIPGSW
00108 ******************************************************************ELKIPGSW
00109                                                                   ELKIPGSW
00110  0100-INITIALIZATION.                                             ELKIPGSW
00111                                                                   ELKIPGSW
00112      MOVE ZEROS TO WS-ELIGIBLE-WEIGHT.                            ELKIPGSW
00113      MOVE ZEROS TO WS-MATCHED-WEIGHT.                             ELKIPGSW
00114      MOVE ZEROS TO WS-TOTAL-WEIGHT.                               ELKIPGSW
00115      INITIALIZE CFDB-CF-IPGS.                                     ELKIPGSW
00116                                                                   ELKIPGSW
00117 ******************************************************************ELKIPGSW
00118 *                                                                *ELKIPGSW
00119 *    PROCESS-IPGS-TABULAR                                        *ELKIPGSW
00120 *                                                                *ELKIPGSW
00121 ******************************************************************ELKIPGSW
00122                                                                   ELKIPGSW
00123  1000-PROCESS-IPGS-TABULAR.                                       ELKIPGSW
00124                                                                   ELKIPGSW
00125      IF ADDRESS OF IPGS-RECORD = NULL OR                          ELKIPGSW
00126         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIPGSW
00127         SET WS-MISSING-PARM TO TRUE                               ELKIPGSW
00128      ELSE                                                         ELKIPGSW
00129      IF ADDRESS OF PVSW-PRVDR-SPC-TBL = NULL                      ELKIPGSW
00130         SET WS-MISSING-PARM TO TRUE                               ELKIPGSW
00131      ELSE                                                         ELKIPGSW
00132      IF PVSW-NBR-ENTRS = ZERO                                     ELKIPGSW
00133         SET WS-MISSING-PARM TO TRUE                               ELKIPGSW
00134      ELSE                                                         ELKIPGSW
00135      IF WS-SUCCESSFUL-CALL                                        ELKIPGSW
00136         PERFORM 1100-MATCH-LIST-PVTW-CODES.                       ELKIPGSW
00137                                                                   ELKIPGSW
00138 ******************************************************************ELKIPGSW
00139 *                                                                *ELKIPGSW
00140 *    MATCH LIST OF PROVIDER SPEC MATCH LIST TO IPGS.             *ELKIPGSW
00141 *                                                                *ELKIPGSW
00142 ******************************************************************ELKIPGSW
00143                                                                   ELKIPGSW
00144  1100-MATCH-LIST-PVTW-CODES.                                      ELKIPGSW
00145                                                                   ELKIPGSW
00146      SET GXS-INDEX TO GXS-ENTRY-COUNT.                            ELKIPGSW
00147      SET WS-GXS-MAX-INDEX TO GXS-INDEX.                           ELKIPGSW
00148      SET GXS-INDEX TO 1.                                          ELKIPGSW
00149      SET PVSW-IDX TO 1.                                           ELKIPGSW
00150      PERFORM 1200-MATCH-IPGS-TAB-MATCH-LIST                       ELKIPGSW
00151           UNTIL PVSW-IDX > PVSW-NBR-ENTRS OR                      ELKIPGSW
00152              WS-MISSING-PARM.                                     ELKIPGSW
00153                                                                   ELKIPGSW
00154      MOVE WS-ELIGIBLE-WEIGHT TO WS-CF-ELIGIBLE                    ELKIPGSW
00155      MOVE WS-TOTAL-WEIGHT TO WS-CF-TOTAL                          ELKIPGSW
00156                                                                   ELKIPGSW
00157      IF GXS-ID-ARGUMENT-INCLUDED                                  ELKIPGSW
00158         COMPUTE WS-CF-MATCHED =                                   ELKIPGSW
00159                  WS-CF-TOTAL / WS-CF-ELIGIBLE                     ELKIPGSW
00160      ELSE                                                         ELKIPGSW
00161         COMPUTE WS-CF-MATCHED =                                   ELKIPGSW
00162                 (WS-CF-TOTAL / WS-CF-ELIGIBLE) *                  ELKIPGSW
00163                                   WS-CONF-NEGATIVE.               ELKIPGSW
00164                                                                   ELKIPGSW
00165      IF GXS-ID-ARGUMENT-INCLUDED AND                              ELKIPGSW
00166               WS-CF-MATCHED = WS-CONF-ZERO                        ELKIPGSW
00167         MOVE WS-CONF-NEGATIVE TO WS-CF-MATCHED.                   ELKIPGSW
00168                                                                   ELKIPGSW
00169      MOVE WS-MATCHED-WEIGHT TO CFDB-CF-IPGS-WGHTD-LST-MTCH.       ELKIPGSW
00170                                                                   ELKIPGSW
00171 ******************************************************************ELKIPGSW
00172 *                                                                *ELKIPGSW
00173 *    MATCH IPGS TABULAR TO PROVIDER SPEC MATCH LIST              *ELKIPGSW
00174 *                                                                *ELKIPGSW
00175 ******************************************************************ELKIPGSW
00176                                                                   ELKIPGSW
00177  1200-MATCH-IPGS-TAB-MATCH-LIST.                                  ELKIPGSW
00178                                                                   ELKIPGSW
00179      IF GXS-INDEX > WS-GXS-MAX-INDEX                              ELKIPGSW
00180         ADD PVSW-WGHT (PVSW-IDX) TO WS-TOTAL-WEIGHT               ELKIPGSW
00181         SET PVSW-IDX UP BY 1                                      ELKIPGSW
00182      IF GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX) =                  ELKIPGSW
00183                 PVSW-PRVDR-SPC (PVSW-IDX)                         ELKIPGSW
00184         ADD PVSW-WGHT (PVSW-IDX) TO WS-ELIGIBLE-WEIGHT            ELKIPGSW
00185         SET GXS-INDEX UP BY 1                                     ELKIPGSW
00186         SET PVSW-IDX UP BY 1                                      ELKIPGSW
00187      ELSE                                                         ELKIPGSW
00188      IF GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX) >                  ELKIPGSW
00189           PVSW-PRVDR-SPC (PVSW-IDX)                               ELKIPGSW
00190         ADD PVSW-WGHT (PVSW-IDX) TO WS-TOTAL-WEIGHT               ELKIPGSW
00191         SET PVSW-IDX UP BY 1                                      ELKIPGSW
00192      ELSE                                                         ELKIPGSW
00193         SET GXS-INDEX UP BY 1.                                    ELKIPGSW
00194                                                                   ELKIPGSW
