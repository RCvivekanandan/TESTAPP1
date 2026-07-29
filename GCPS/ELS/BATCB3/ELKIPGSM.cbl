00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGSM
00003  PROGRAM-ID.           ELKIPGSM.                                     LV004
00004                                                                   ELKIPGSM
00005  AUTHOR.               ANNE KEFFER KING.                          ELKIPGSM
00006                                                                   ELKIPGSM
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGSM
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGSM
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGSM
00010                        233 N. MICHIGAN AVE                        ELKIPGSM
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGSM
00012                                                                   ELKIPGSM
00013                                                                   ELKIPGSM
00014  DATE-WRITTEN.         25-AUG-2000.                               ELKIPGSM
00015                                                                   ELKIPGSM
00016  ENVIRONMENT DIVISION.                                            ELKIPGSM
00017                                                                   ELKIPGSM
00018  CONFIGURATION SECTION.                                           ELKIPGSM
00019                                                                   ELKIPGSM
00020  SOURCE-COMPUTER.      IBM-3090.                                  ELKIPGSM
00021  OBJECT-COMPUTER.      IBM-3090.                                  ELKIPGSM
00022                                                                   ELKIPGSM
00023 ******************************************************************ELKIPGSM
00024 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIPGSM
00025 *                                                                *ELKIPGSM
00026 *  ELKIPGSM - COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE           *ELKIPGSM
00027 *             FACTOR FOR IPGS TABULAR.                           *ELKIPGSM
00028 *                                                                *ELKIPGSM
00029 *                                                                *ELKIPGSM
00030 *              THIS PROGRAM MATCHES AN #IPGS(PROVIDER SPEC INTRNL*ELKIPGSM
00031 *              TAB) TO AN UNWEIGHTED LIST OF PROVIDER SPECS AND  *ELKIPGSM
00032 *              PRODUCES A CONFIDENCE FACTOR WHICH REPRESENTS THE *ELKIPGSM
00033 *              DEGREE TO WHICH THE PROVIDER SPECS IN THE MATCH   *ELKIPGSM
00034 *              LIST ARE REPRESENTED IN THE #IPGS TABULAR.        *ELKIPGSM
00035 *                                                                *ELKIPGSM
00036 ******************************************************************ELKIPGSM
00037 *                      MAINTENANCE HISTORY                       *ELKIPGSM
00038 *  MOD     DATE      BY                    ACTION                *ELKIPGSM
00039 * ----  ----------- --- -----------------------------------------*ELKIPGSM
00040 * 01.00 25-AUG-2000 AKK CLONED FROM ELKIPGTM.                    *ELKIPGSM
00041 *                                                                *ELKIPGSM
00042 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGSM
00043 *                                                                *ELKIPGSM
00044 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGSM
00045 *                                                                *ELKIPGSM
00046 * 02.01 09-JAN-2004 AKK S0C7                                     *ELKIPGSM
00047 * 02.04 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGSM
00048 *                             CICSCB3 COMPILER FIX             *  ELKIPGSM
00049 *                                                                *ELKIPGSM
00050 ******************************************************************ELKIPGSM
00051  EJECT                                                            ELKIPGSM
00052  DATA DIVISION.                                                   ELKIPGSM
00053                                                                   ELKIPGSM
00054  WORKING-STORAGE SECTION.                                         ELKIPGSM
00055                                                                   ELKIPGSM
00056  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIPGSM
00057    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIPGSM
00058    88  WS-INVALID-PARM                 VALUE +8.                  ELKIPGSM
00059    88  WS-MISSING-PARM                 VALUE +12.                 ELKIPGSM
00060    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIPGSM
00061                                                                   ELKIPGSM
00062  01  WS-ENTRY-COUNT                    COMP-1.                    ELKIPGSM
00063  01  WS-ELIGIBLE                       COMP-1.                    ELKIPGSM
00064  01  WS-INCLUDED                       COMP-1.                    ELKIPGSM
00065  01  WS-MATCHED                        COMP-1.                    ELKIPGSM
00066                                                                   ELKIPGSM
00067  01  WS-CONF-ZERO             COMP-1   VALUE +0.000000E+00.       ELKIPGSM
00068  01  WS-CONF-NEGATIVE         COMP-1   VALUE -1.000000E+00.       ELKIPGSM
00069                                                                   ELKIPGSM
00070  01  WS-GXS-MAX-INDEX            INDEX.                           ELKIPGSM
00071  EJECT                                                            ELKIPGSM
00072  LINKAGE SECTION.                                                 ELKIPGSM
00073                                                                   ELKIPGSM
00074  EJECT                                                            ELKIPGSM
00075                                                                   ELKIPGSM
00076  01  IPGS-RECORD.                                                 ELKIPGSM
00077      COPY GCTIPGSC.                                               ELKIPGSM
00078                                                                   ELKIPGSM
00079                                                                   ELKIPGSM
00080      COPY ELSPVSLC.                                               ELKIPGSM
00081                                                                   ELKIPGSM
00082                                                                   ELKIPGSM
00083      COPY ELSCFDBC.                                               ELKIPGSM
00084  EJECT                                                            ELKIPGSM
00085 ******************************************************************ELKIPGSM
00086 *                                                                *ELKIPGSM
00087 *    PROCEDURE DIVISION                                          *ELKIPGSM
00088 *                                                                *ELKIPGSM
00089 ******************************************************************ELKIPGSM
00090                                                                   ELKIPGSM
00091  PROCEDURE DIVISION USING IPGS-RECORD                             ELKIPGSM
00092                               PVSL-PRVDR-SPC-TBL                  ELKIPGSM
00093                                    CFDB-CNFDNC-FCTR-DATA-BLCK.    ELKIPGSM
00094                                                                   ELKIPGSM
00095  0000-DETERMINE-IPGS-CONFIDENCE.                                  ELKIPGSM
00096                                                                   ELKIPGSM
00097      PERFORM 0100-INITIALIZATION.                                 ELKIPGSM
00098      PERFORM 1000-PROCESS-IPGS-TABULAR.                           ELKIPGSM
00099      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGSM
00100      GOBACK.                                                      ELKIPGSM
00101                                                                   ELKIPGSM
00102 ******************************************************************ELKIPGSM
00103 *                                                                *ELKIPGSM
00104 *    INITIALIZATION                                              *ELKIPGSM
00105 *                                                                *ELKIPGSM
00106 ******************************************************************ELKIPGSM
00107                                                                   ELKIPGSM
00108  0100-INITIALIZATION.                                             ELKIPGSM
00109                                                                   ELKIPGSM
00110      MOVE ZEROS TO WS-ENTRY-COUNT.                                ELKIPGSM
00111      MOVE ZEROS TO WS-ELIGIBLE.                                   ELKIPGSM
00112      MOVE ZEROS TO WS-INCLUDED.                                   ELKIPGSM
00113      MOVE ZEROS TO WS-MATCHED.                                    ELKIPGSM
00114      INITIALIZE CFDB-CF-IPGS.                                     ELKIPGSM
00115                                                                   ELKIPGSM
00116 ******************************************************************ELKIPGSM
00117 *                                                                *ELKIPGSM
00118 *    PROCESS-IPGS-TABULAR                                        *ELKIPGSM
00119 *                                                                *ELKIPGSM
00120 ******************************************************************ELKIPGSM
00121                                                                   ELKIPGSM
00122  1000-PROCESS-IPGS-TABULAR.                                       ELKIPGSM
00123                                                                   ELKIPGSM
00124      IF ADDRESS OF IPGS-RECORD = NULL OR                          ELKIPGSM
00125         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIPGSM
00126         SET WS-MISSING-PARM TO TRUE                               ELKIPGSM
00127      ELSE                                                         ELKIPGSM
00128      IF ADDRESS OF PVSL-PRVDR-SPC-TBL = NULL                      ELKIPGSM
00129         SET WS-MISSING-PARM TO TRUE                               ELKIPGSM
00130      ELSE                                                         ELKIPGSM
00131      IF PVSL-NBR-ENTRS = ZERO                                     ELKIPGSM
00132         SET WS-MISSING-PARM TO TRUE                               ELKIPGSM
00133      ELSE                                                         ELKIPGSM
00134      IF WS-SUCCESSFUL-CALL                                        ELKIPGSM
00135         PERFORM 1100-MATCH-LIST-PVSL-CODES.                       ELKIPGSM
00136                                                                   ELKIPGSM
00137 ******************************************************************ELKIPGSM
00138 *                                                                *ELKIPGSM
00139 *    MATCH LIST OF PROVIDER-SPEC-ARGUMENT CODES TO IPGS.         *ELKIPGSM
00140 *                                                                *ELKIPGSM
00141 ******************************************************************ELKIPGSM
00142                                                                   ELKIPGSM
00143  1100-MATCH-LIST-PVSL-CODES.                                      ELKIPGSM
00144                                                                   ELKIPGSM
00145      SET GXS-INDEX TO GXS-ENTRY-COUNT.                            ELKIPGSM
00146      SET WS-GXS-MAX-INDEX TO GXS-INDEX.                           ELKIPGSM
00147      SET GXS-INDEX TO 1.                                          ELKIPGSM
00148      SET PVSL-IDX TO 1.                                           ELKIPGSM
00149      PERFORM 1200-MATCH-IPGS-TAB-MATCH-LIST                       ELKIPGSM
00150           UNTIL PVSL-IDX > PVSL-NBR-ENTRS OR                      ELKIPGSM
00151                  GXS-INDEX > WS-GXS-MAX-INDEX.                    ELKIPGSM
00152                                                                   ELKIPGSM
00153      MOVE PVSL-NBR-ENTRS TO WS-ENTRY-COUNT.                       ELKIPGSM
00154      IF GXS-ID-ARGUMENT-INCLUDED                                  ELKIPGSM
00155         COMPUTE WS-MATCHED =                                      ELKIPGSM
00156               (WS-ELIGIBLE / WS-ENTRY-COUNT)                      ELKIPGSM
00157      ELSE                                                         ELKIPGSM
00158         COMPUTE WS-MATCHED =                                      ELKIPGSM
00159               (WS-ELIGIBLE / WS-ENTRY-COUNT) * WS-CONF-NEGATIVE.  ELKIPGSM
00160                                                                   ELKIPGSM
00161      IF GXS-ID-ARGUMENT-INCLUDED AND WS-MATCHED = WS-CONF-ZERO    ELKIPGSM
00162         MOVE WS-CONF-NEGATIVE TO WS-MATCHED.                      ELKIPGSM
00163                                                                   ELKIPGSM
00164      MOVE WS-MATCHED TO CFDB-CF-IPGS-UNWGHTD-LST-MTCH.            ELKIPGSM
00165                                                                   ELKIPGSM
00166 ******************************************************************ELKIPGSM
00167 *                                                                *ELKIPGSM
00168 *   MATCH IPGS TABULAR TO PROVIDER-SPEC-ARGUMENT CODE MATCH LIST *ELKIPGSM
00169 *                                                                *ELKIPGSM
00170 ******************************************************************ELKIPGSM
00171                                                                   ELKIPGSM
00172  1200-MATCH-IPGS-TAB-MATCH-LIST.                                  ELKIPGSM
00173                                                                   ELKIPGSM
00174      IF GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX) =                  ELKIPGSM
00175             PVSL-PRVDR-SPC (PVSL-IDX)                             ELKIPGSM
00176         ADD +1 TO WS-ELIGIBLE                                     ELKIPGSM
00177         SET GXS-INDEX UP BY 1                                     ELKIPGSM
00178         SET PVSL-IDX UP BY 1                                      ELKIPGSM
00179      ELSE                                                         ELKIPGSM
00180         IF GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX) >               ELKIPGSM
00181             PVSL-PRVDR-SPC (PVSL-IDX)                             ELKIPGSM
00182            SET PVSL-IDX UP BY 1                                   ELKIPGSM
00183      ELSE                                                         ELKIPGSM
00184            SET GXS-INDEX UP BY 1.                                 ELKIPGSM
00185                                                                   ELKIPGSM
