00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGTW
00003  PROGRAM-ID.           ELKIPGTW.                                     LV004
00004                                                                   ELKIPGTW
00005  AUTHOR.               BARBARA KEIB.                              ELKIPGTW
00006                                                                   ELKIPGTW
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGTW
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGTW
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGTW
00010                        233 N. MICHIGAN AVE                        ELKIPGTW
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGTW
00012                                                                   ELKIPGTW
00013  DATE-WRITTEN.         09-OCT-1992.                               ELKIPGTW
00014                                                                   ELKIPGTW
00015  ENVIRONMENT DIVISION.                                            ELKIPGTW
00016                                                                   ELKIPGTW
00017  CONFIGURATION SECTION.                                           ELKIPGTW
00018                                                                   ELKIPGTW
00019  SOURCE-COMPUTER.      IBM-3090.                                  ELKIPGTW
00020  OBJECT-COMPUTER.      IBM-3090.                                  ELKIPGTW
00021                                                                   ELKIPGTW
00022 ******************************************************************ELKIPGTW
00023 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIPGTW
00024 *  ELKIPGTW - COMPUTE WEIGHTED LIST MATCH CONFIDENCE             *ELKIPGTW
00025 *             FACTOR FOR IPGT TABULAR.                           *ELKIPGTW
00026 *                                                                *ELKIPGTW
00027 *              THIS PROGRAM MATCHES AN #IPGT(PROVIDER TYPE INTRNL*ELKIPGTW
00028 *              TAB) TO A WEIGHTED LIST OF PROVIDER TYPES AND     *ELKIPGTW
00029 *              PRODUCES A CONFIDENCE FACTOR WHICH REPRESENTS THE *ELKIPGTW
00030 *              DEGREE TO WHICH THE PROVIDER TYPES IN THE MATCH   *ELKIPGTW
00031 *              LIST ARE REPRESENTED IN THE #IPGT TABULAR.        *ELKIPGTW
00032 *                                                                *ELKIPGTW
00033 ******************************************************************ELKIPGTW
00034 *                      MAINTENANCE HISTORY                       *ELKIPGTW
00035 *                                                                *ELKIPGTW
00036 *  MOD     DATE      BY                    ACTION                *ELKIPGTW
00037 * ----- ----------- --- -----------------------------------------*ELKIPGTW
00038 * 01.00 09-OCT-1992 BAK CREATED                                  *ELKIPGTW
00039 *                                                                *ELKIPGTW
00040 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGTW
00041 *                                                                *ELKIPGTW
00042 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGTW
00043 *                                                                *ELKIPGTW
00044 * 02.01 09-JAN-2004 AKK S0C7 TEST                                *ELKIPGTW
00045 *                                                                *ELKIPGTW
00046 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGTW
00047 *                             CICSCB3 COMPILER FIX             *  ELKIPGTW
00048 ******************************************************************ELKIPGTW
00049 /                                                                 ELKIPGTW
00050  DATA DIVISION.                                                   ELKIPGTW
00051                                                                   ELKIPGTW
00052  WORKING-STORAGE SECTION.                                         ELKIPGTW
00053                                                                   ELKIPGTW
00054  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIPGTW
00055    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIPGTW
00056    88  WS-INVALID-PARM                 VALUE +8.                  ELKIPGTW
00057    88  WS-MISSING-PARM                 VALUE +12.                 ELKIPGTW
00058    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIPGTW
00059                                                                   ELKIPGTW
00060  01  WS-ELIGIBLE-WEIGHT          PIC S9(04) COMP.                 ELKIPGTW
00061  01  WS-MATCHED-WEIGHT           PIC S9(04) COMP.                 ELKIPGTW
00062  01  WS-TOTAL-WEIGHT             PIC S9(04) COMP.                 ELKIPGTW
00063                                                                   ELKIPGTW
00064  01  WS-CF-TOTAL                 COMP-1.                          ELKIPGTW
00065  01  WS-CF-ELIGIBLE              COMP-1.                          ELKIPGTW
00066  01  WS-CF-MATCHED               COMP-1.                          ELKIPGTW
00067                                                                   ELKIPGTW
00068  01  WS-CONF-ZERO            COMP-1  VALUE +0.000000E+00.         ELKIPGTW
00069  01  WS-CONF-NEGATIVE        COMP-1  VALUE -1.000000E+00.         ELKIPGTW
00070                                                                   ELKIPGTW
00071  01  WS-GX3-MAX-INDEX            INDEX.                           ELKIPGTW
00072 /                                                                 ELKIPGTW
00073  LINKAGE SECTION.                                                 ELKIPGTW
00074                                                                   ELKIPGTW
00075 /                                                                 ELKIPGTW
00076                                                                   ELKIPGTW
00077  01  IPGT-RECORD.                                                 ELKIPGTW
00078      COPY GCTIPGTC.                                               ELKIPGTW
00079                                                                   ELKIPGTW
00080                                                                   ELKIPGTW
00081      COPY ELSPVTWC.                                               ELKIPGTW
00082                                                                   ELKIPGTW
00083                                                                   ELKIPGTW
00084      COPY ELSCFDBC.                                               ELKIPGTW
00085 /*****************************************************************ELKIPGTW
00086 *                                                                *ELKIPGTW
00087 *    PROCEDURE DIVISION                                          *ELKIPGTW
00088 *                                                                *ELKIPGTW
00089 ******************************************************************ELKIPGTW
00090                                                                   ELKIPGTW
00091  PROCEDURE DIVISION USING IPGT-RECORD                             ELKIPGTW
00092                              PVTW-PRVDR-TYP-TBL                   ELKIPGTW
00093                                CFDB-CNFDNC-FCTR-DATA-BLCK.        ELKIPGTW
00094                                                                   ELKIPGTW
00095  0000-DETERMINE-IPGT-CONFIDENCE.                                  ELKIPGTW
00096                                                                   ELKIPGTW
00097      PERFORM 0100-INITIALIZATION.                                 ELKIPGTW
00098      PERFORM 1000-PROCESS-IPGT-TABULAR.                           ELKIPGTW
00099      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGTW
00100      GOBACK.                                                      ELKIPGTW
00101                                                                   ELKIPGTW
00102 ******************************************************************ELKIPGTW
00103 *                                                                *ELKIPGTW
00104 *    INITIALIZATION                                              *ELKIPGTW
00105 *                                                                *ELKIPGTW
00106 ******************************************************************ELKIPGTW
00107                                                                   ELKIPGTW
00108  0100-INITIALIZATION.                                             ELKIPGTW
00109                                                                   ELKIPGTW
00110      MOVE ZEROS TO WS-ELIGIBLE-WEIGHT.                            ELKIPGTW
00111      MOVE ZEROS TO WS-MATCHED-WEIGHT.                             ELKIPGTW
00112      MOVE ZEROS TO WS-TOTAL-WEIGHT.                               ELKIPGTW
00113      INITIALIZE CFDB-CF-IPGT.                                     ELKIPGTW
00114                                                                   ELKIPGTW
00115 ******************************************************************ELKIPGTW
00116 *                                                                *ELKIPGTW
00117 *    PROCESS-IPGT-TABULAR                                        *ELKIPGTW
00118 *                                                                *ELKIPGTW
00119 ******************************************************************ELKIPGTW
00120                                                                   ELKIPGTW
00121  1000-PROCESS-IPGT-TABULAR.                                       ELKIPGTW
00122                                                                   ELKIPGTW
00123      IF ADDRESS OF IPGT-RECORD = NULL OR                          ELKIPGTW
00124         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIPGTW
00125         SET WS-MISSING-PARM TO TRUE                               ELKIPGTW
00126      ELSE                                                         ELKIPGTW
00127      IF ADDRESS OF PVTW-PRVDR-TYP-TBL = NULL                      ELKIPGTW
00128         SET WS-MISSING-PARM TO TRUE                               ELKIPGTW
00129      ELSE                                                         ELKIPGTW
00130      IF PVTW-NBR-ENTRS = ZERO                                     ELKIPGTW
00131         SET WS-MISSING-PARM TO TRUE                               ELKIPGTW
00132      ELSE                                                         ELKIPGTW
00133      IF WS-SUCCESSFUL-CALL                                        ELKIPGTW
00134         PERFORM 1100-MATCH-LIST-PVTW-CODES.                       ELKIPGTW
00135                                                                   ELKIPGTW
00136 ******************************************************************ELKIPGTW
00137 *                                                                *ELKIPGTW
00138 *    MATCH LIST OF PROVIDER TYPE MATCH LIST TO IPGT.             *ELKIPGTW
00139 *                                                                *ELKIPGTW
00140 ******************************************************************ELKIPGTW
00141                                                                   ELKIPGTW
00142  1100-MATCH-LIST-PVTW-CODES.                                      ELKIPGTW
00143                                                                   ELKIPGTW
00144      SET GX3-INDEX TO GX3-ENTRY-COUNT.                            ELKIPGTW
00145      SET WS-GX3-MAX-INDEX TO GX3-INDEX.                           ELKIPGTW
00146      SET GX3-INDEX TO 1.                                          ELKIPGTW
00147      SET PVTW-IDX TO 1.                                           ELKIPGTW
00148      PERFORM 1200-MATCH-IPGT-TAB-MATCH-LIST                       ELKIPGTW
00149           UNTIL PVTW-IDX > PVTW-NBR-ENTRS OR                      ELKIPGTW
00150              WS-MISSING-PARM.                                     ELKIPGTW
00151                                                                   ELKIPGTW
00152      MOVE WS-ELIGIBLE-WEIGHT TO WS-CF-ELIGIBLE                    ELKIPGTW
00153      MOVE WS-TOTAL-WEIGHT TO WS-CF-TOTAL                          ELKIPGTW
00154                                                                   ELKIPGTW
00155      IF GX3-ID-ARGUMENT-INCLUDED                                  ELKIPGTW
00156         COMPUTE WS-CF-MATCHED =                                   ELKIPGTW
00157                  WS-CF-TOTAL / WS-CF-ELIGIBLE                     ELKIPGTW
00158      ELSE                                                         ELKIPGTW
00159         COMPUTE WS-CF-MATCHED =                                   ELKIPGTW
00160                 (WS-CF-TOTAL / WS-CF-ELIGIBLE) *                  ELKIPGTW
00161                                   WS-CONF-NEGATIVE.               ELKIPGTW
00162                                                                   ELKIPGTW
00163      IF GX3-ID-ARGUMENT-INCLUDED AND                              ELKIPGTW
00164               WS-CF-MATCHED = WS-CONF-ZERO                        ELKIPGTW
00165         MOVE WS-CONF-NEGATIVE TO WS-CF-MATCHED.                   ELKIPGTW
00166                                                                   ELKIPGTW
00167      MOVE WS-MATCHED-WEIGHT TO CFDB-CF-IPGT-WGHTD-LST-MTCH.       ELKIPGTW
00168                                                                   ELKIPGTW
00169 ******************************************************************ELKIPGTW
00170 *                                                                *ELKIPGTW
00171 *    MATCH IPGT TABULAR TO PROVIDER TYPE MATCH LIST              *ELKIPGTW
00172 *                                                                *ELKIPGTW
00173 ******************************************************************ELKIPGTW
00174                                                                   ELKIPGTW
00175  1200-MATCH-IPGT-TAB-MATCH-LIST.                                  ELKIPGTW
00176                                                                   ELKIPGTW
00177      IF GX3-INDEX > WS-GX3-MAX-INDEX                              ELKIPGTW
00178         ADD PVTW-WGHT (PVTW-IDX) TO WS-TOTAL-WEIGHT               ELKIPGTW
00179         SET PVTW-IDX UP BY 1                                      ELKIPGTW
00180      IF GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX) =                  ELKIPGTW
00181                 PVTW-PRVDR-TYP (PVTW-IDX)                         ELKIPGTW
00182         ADD PVTW-WGHT (PVTW-IDX) TO WS-ELIGIBLE-WEIGHT            ELKIPGTW
00183         SET GX3-INDEX UP BY 1                                     ELKIPGTW
00184         SET PVTW-IDX UP BY 1                                      ELKIPGTW
00185      ELSE                                                         ELKIPGTW
00186      IF GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX) >                  ELKIPGTW
00187           PVTW-PRVDR-TYP (PVTW-IDX)                               ELKIPGTW
00188         ADD PVTW-WGHT (PVTW-IDX) TO WS-TOTAL-WEIGHT               ELKIPGTW
00189         SET PVTW-IDX UP BY 1                                      ELKIPGTW
00190      ELSE                                                         ELKIPGTW
00191         SET GX3-INDEX UP BY 1.                                    ELKIPGTW
00192                                                                   ELKIPGTW
