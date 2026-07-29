00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGTM
00003  PROGRAM-ID.           ELKIPGTM.                                     LV004
00004                                                                   ELKIPGTM
00005  AUTHOR.               BARBARA KEIB.                              ELKIPGTM
00006                                                                   ELKIPGTM
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGTM
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGTM
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGTM
00010                        233 N. MICHIGAN AVE                        ELKIPGTM
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGTM
00012                                                                   ELKIPGTM
00013  DATE-WRITTEN.         09-OCT-1992.                               ELKIPGTM
00014                                                                   ELKIPGTM
00015  ENVIRONMENT DIVISION.                                            ELKIPGTM
00016                                                                   ELKIPGTM
00017  CONFIGURATION SECTION.                                           ELKIPGTM
00018                                                                   ELKIPGTM
00019  SOURCE-COMPUTER.      IBM-3090.                                  ELKIPGTM
00020  OBJECT-COMPUTER.      IBM-3090.                                  ELKIPGTM
00021                                                                   ELKIPGTM
00022 ******************************************************************ELKIPGTM
00023 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIPGTM
00024 *  ELKIPGTM - COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE           *ELKIPGTM
00025 *             FACTOR FOR IPGT TABULAR.                           *ELKIPGTM
00026 *                                                                *ELKIPGTM
00027 *              THIS PROGRAM MATCHES AN #IPGT(PROVIDER TYPE INTRNL*ELKIPGTM
00028 *              TAB) TO AN UNWEIGHTED LIST OF PROVIDER TYPES AND  *ELKIPGTM
00029 *              PRODUCES A CONFIDENCE FACTOR WHICH REPRESENTS THE *ELKIPGTM
00030 *              DEGREE TO WHICH THE PROVIDER TYPES IN THE MATCH   *ELKIPGTM
00031 *              LIST ARE REPRESENTED IN THE #IPGT TABULAR.        *ELKIPGTM
00032 *                                                                *ELKIPGTM
00033 ******************************************************************ELKIPGTM
00034 *                      MAINTENANCE HISTORY                       *ELKIPGTM
00035 *                                                                *ELKIPGTM
00036 *  MOD     DATE      BY                    ACTION                *ELKIPGTM
00037 * ----  ----------- --- -----------------------------------------*ELKIPGTM
00038 * 01.00 09-OCT-1992 BAK CREATED                                  *ELKIPGTM
00039 *                                                                *ELKIPGTM
00040 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGTM
00041 *                                                                *ELKIPGTM
00042 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGTM
00043 *                                                                *ELKIPGTM
00044 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST TEST                      *ELKIPGTM
00045 *                                                                *ELKIPGTM
00046 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGTM
00047 *                             CICSCB3 COMPILER FIX             *  ELKIPGTM
00048 ******************************************************************ELKIPGTM
00049  EJECT                                                            ELKIPGTM
00050  DATA DIVISION.                                                   ELKIPGTM
00051                                                                   ELKIPGTM
00052  WORKING-STORAGE SECTION.                                         ELKIPGTM
00053                                                                   ELKIPGTM
00054  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIPGTM
00055    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIPGTM
00056    88  WS-INVALID-PARM                 VALUE +8.                  ELKIPGTM
00057    88  WS-MISSING-PARM                 VALUE +12.                 ELKIPGTM
00058    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIPGTM
00059                                                                   ELKIPGTM
00060  01  WS-ENTRY-COUNT                    COMP-1.                    ELKIPGTM
00061  01  WS-ELIGIBLE                       COMP-1.                    ELKIPGTM
00062  01  WS-INCLUDED                       COMP-1.                    ELKIPGTM
00063  01  WS-MATCHED                        COMP-1.                    ELKIPGTM
00064                                                                   ELKIPGTM
00065  01  WS-CONF-ZERO             COMP-1   VALUE +0.000000E+00.       ELKIPGTM
00066  01  WS-CONF-NEGATIVE         COMP-1   VALUE -1.000000E+00.       ELKIPGTM
00067                                                                   ELKIPGTM
00068  01  WS-GX3-MAX-INDEX            INDEX.                           ELKIPGTM
00069  EJECT                                                            ELKIPGTM
00070  LINKAGE SECTION.                                                 ELKIPGTM
00071                                                                   ELKIPGTM
00072  EJECT                                                            ELKIPGTM
00073                                                                   ELKIPGTM
00074  01  IPGT-RECORD.                                                 ELKIPGTM
00075      COPY GCTIPGTC.                                               ELKIPGTM
00076                                                                   ELKIPGTM
00077                                                                   ELKIPGTM
00078      COPY ELSPVTLC.                                               ELKIPGTM
00079                                                                   ELKIPGTM
00080                                                                   ELKIPGTM
00081      COPY ELSCFDBC.                                               ELKIPGTM
00082  EJECT                                                            ELKIPGTM
00083 ******************************************************************ELKIPGTM
00084 *                                                                *ELKIPGTM
00085 *    PROCEDURE DIVISION                                          *ELKIPGTM
00086 *                                                                *ELKIPGTM
00087 ******************************************************************ELKIPGTM
00088                                                                   ELKIPGTM
00089  PROCEDURE DIVISION USING IPGT-RECORD                             ELKIPGTM
00090                               PVTL-PRVDR-TYP-TBL                  ELKIPGTM
00091                                    CFDB-CNFDNC-FCTR-DATA-BLCK.    ELKIPGTM
00092                                                                   ELKIPGTM
00093  0000-DETERMINE-IPGT-CONFIDENCE.                                  ELKIPGTM
00094                                                                   ELKIPGTM
00095      PERFORM 0100-INITIALIZATION.                                 ELKIPGTM
00096      PERFORM 1000-PROCESS-IPGT-TABULAR.                           ELKIPGTM
00097      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGTM
00098      GOBACK.                                                      ELKIPGTM
00099                                                                   ELKIPGTM
00100 ******************************************************************ELKIPGTM
00101 *                                                                *ELKIPGTM
00102 *    INITIALIZATION                                              *ELKIPGTM
00103 *                                                                *ELKIPGTM
00104 ******************************************************************ELKIPGTM
00105                                                                   ELKIPGTM
00106  0100-INITIALIZATION.                                             ELKIPGTM
00107                                                                   ELKIPGTM
00108      MOVE ZEROS TO WS-ENTRY-COUNT.                                ELKIPGTM
00109      MOVE ZEROS TO WS-ELIGIBLE.                                   ELKIPGTM
00110      MOVE ZEROS TO WS-INCLUDED.                                   ELKIPGTM
00111      MOVE ZEROS TO WS-MATCHED.                                    ELKIPGTM
00112      INITIALIZE CFDB-CF-IPGT.                                     ELKIPGTM
00113                                                                   ELKIPGTM
00114 ******************************************************************ELKIPGTM
00115 *                                                                *ELKIPGTM
00116 *    PROCESS-IPGT-TABULAR                                        *ELKIPGTM
00117 *                                                                *ELKIPGTM
00118 ******************************************************************ELKIPGTM
00119                                                                   ELKIPGTM
00120  1000-PROCESS-IPGT-TABULAR.                                       ELKIPGTM
00121                                                                   ELKIPGTM
00122      IF ADDRESS OF IPGT-RECORD = NULL OR                          ELKIPGTM
00123         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIPGTM
00124         SET WS-MISSING-PARM TO TRUE                               ELKIPGTM
00125      ELSE                                                         ELKIPGTM
00126      IF ADDRESS OF PVTL-PRVDR-TYP-TBL = NULL                      ELKIPGTM
00127         SET WS-MISSING-PARM TO TRUE                               ELKIPGTM
00128      ELSE                                                         ELKIPGTM
00129      IF PVTL-NBR-ENTRS = ZERO                                     ELKIPGTM
00130         SET WS-MISSING-PARM TO TRUE                               ELKIPGTM
00131      ELSE                                                         ELKIPGTM
00132      IF WS-SUCCESSFUL-CALL                                        ELKIPGTM
00133         PERFORM 1100-MATCH-LIST-PVTL-CODES.                       ELKIPGTM
00134                                                                   ELKIPGTM
00135 ******************************************************************ELKIPGTM
00136 *                                                                *ELKIPGTM
00137 *    MATCH LIST OF PROVIDER-TYPE-ARGUMENT CODES TO IPGT.         *ELKIPGTM
00138 *                                                                *ELKIPGTM
00139 ******************************************************************ELKIPGTM
00140                                                                   ELKIPGTM
00141  1100-MATCH-LIST-PVTL-CODES.                                      ELKIPGTM
00142                                                                   ELKIPGTM
00143      SET GX3-INDEX TO GX3-ENTRY-COUNT.                            ELKIPGTM
00144      SET WS-GX3-MAX-INDEX TO GX3-INDEX.                           ELKIPGTM
00145      SET GX3-INDEX TO 1.                                          ELKIPGTM
00146      SET PVTL-IDX TO 1.                                           ELKIPGTM
00147      PERFORM 1200-MATCH-IPGT-TAB-MATCH-LIST                       ELKIPGTM
00148           UNTIL PVTL-IDX > PVTL-NBR-ENTRS OR                      ELKIPGTM
00149                  GX3-INDEX > WS-GX3-MAX-INDEX.                    ELKIPGTM
00150                                                                   ELKIPGTM
00151      MOVE PVTL-NBR-ENTRS TO WS-ENTRY-COUNT.                       ELKIPGTM
00152      IF GX3-ID-ARGUMENT-INCLUDED                                  ELKIPGTM
00153         COMPUTE WS-MATCHED =                                      ELKIPGTM
00154               (WS-ELIGIBLE / WS-ENTRY-COUNT)                      ELKIPGTM
00155      ELSE                                                         ELKIPGTM
00156         COMPUTE WS-MATCHED =                                      ELKIPGTM
00157               (WS-ELIGIBLE / WS-ENTRY-COUNT) * WS-CONF-NEGATIVE.  ELKIPGTM
00158                                                                   ELKIPGTM
00159      IF GX3-ID-ARGUMENT-INCLUDED AND WS-MATCHED = WS-CONF-ZERO    ELKIPGTM
00160         MOVE WS-CONF-NEGATIVE TO WS-MATCHED.                      ELKIPGTM
00161                                                                   ELKIPGTM
00162      MOVE WS-MATCHED TO CFDB-CF-IPGT-UNWGHTD-LST-MTCH.            ELKIPGTM
00163                                                                   ELKIPGTM
00164 ******************************************************************ELKIPGTM
00165 *                                                                *ELKIPGTM
00166 *   MATCH IPGT TABULAR TO PROVIDER-TYPE-ARGUMENT CODE MATCH LIST *ELKIPGTM
00167 *                                                                *ELKIPGTM
00168 ******************************************************************ELKIPGTM
00169                                                                   ELKIPGTM
00170  1200-MATCH-IPGT-TAB-MATCH-LIST.                                  ELKIPGTM
00171                                                                   ELKIPGTM
00172      IF GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX) =                  ELKIPGTM
00173             PVTL-PRVDR-TYP (PVTL-IDX)                             ELKIPGTM
00174         ADD +1 TO WS-ELIGIBLE                                     ELKIPGTM
00175         SET GX3-INDEX UP BY 1                                     ELKIPGTM
00176         SET PVTL-IDX UP BY 1                                      ELKIPGTM
00177      ELSE                                                         ELKIPGTM
00178         IF GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX) >               ELKIPGTM
00179             PVTL-PRVDR-TYP (PVTL-IDX)                             ELKIPGTM
00180            SET PVTL-IDX UP BY 1                                   ELKIPGTM
00181      ELSE                                                         ELKIPGTM
00182            SET GX3-INDEX UP BY 1.                                 ELKIPGTM
00183                                                                   ELKIPGTM
