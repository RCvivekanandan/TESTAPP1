00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGPW
00003  PROGRAM-ID.           ELKIPGPW.                                     LV004
00004                                                                   ELKIPGPW
00005  AUTHOR.               BARBARA KEIB.                              ELKIPGPW
00006                                                                   ELKIPGPW
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGPW
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGPW
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGPW
00010                        233 N. MICHIGAN AVE                        ELKIPGPW
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGPW
00012                                                                   ELKIPGPW
00013  DATE-WRITTEN.         08-OCT-1992.                               ELKIPGPW
00014                                                                   ELKIPGPW
00015  ENVIRONMENT DIVISION.                                            ELKIPGPW
00016                                                                   ELKIPGPW
00017  CONFIGURATION SECTION.                                           ELKIPGPW
00018                                                                   ELKIPGPW
00019  SOURCE-COMPUTER.      IBM-3090.                                  ELKIPGPW
00020  OBJECT-COMPUTER.      IBM-3090.                                  ELKIPGPW
00021                                                                   ELKIPGPW
00022 ******************************************************************ELKIPGPW
00023 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIPGPW
00024 *                                                                *ELKIPGPW
00025 *  ELKIPGPW - COMPUTE WEIGHTED LIST MATCH CONFIDENCE             *ELKIPGPW
00026 *             FACTOR FOR IPGP TABULAR.                           *ELKIPGPW
00027 *                                                                *ELKIPGPW
00028 *              THIS PROGRAM MATCHES AN #IPGP(PROCEDURE CODE INTL *ELKIPGPW
00029 *              TO A WEIGHTED LIST OF PROCEDURE CODES AND         *ELKIPGPW
00030 *              PRODUCES A CONFIDENCE FACTOR WHICH REPRESENTS THE *ELKIPGPW
00031 *              DEGREE TO WHICH THE PROCEDURE CODES IN THE MATCH  *ELKIPGPW
00032 *              LIST ARE REPRESENTED IN THE #IPGP TABULAR.        *ELKIPGPW
00033 *                                                                *ELKIPGPW
00034 ******************************************************************ELKIPGPW
00035 *                      MAINTENANCE HISTORY                       *ELKIPGPW
00036 *  MOD     DATE      BY                    ACTION                *ELKIPGPW
00037 * ----- ----------- --- -----------------------------------------*ELKIPGPW
00038 * 01.00 08-OCT-1992 BAK CREATED                                  *ELKIPGPW
00039 *                                                                *ELKIPGPW
00040 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGPW
00041 *                                                                *ELKIPGPW
00042 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGPW
00043 *                                                                *ELKIPGPW
00044 * 02.01 09-JAN-2004 AKK INTERTEST FOR S0C7                       *ELKIPGPW
00045 *                                                                *ELKIPGPW
00046 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGPW
00047 *                             CICSCB3 COMPILER FIX             *  ELKIPGPW
00048 ******************************************************************ELKIPGPW
00049 /                                                                 ELKIPGPW
00050  DATA DIVISION.                                                   ELKIPGPW
00051                                                                   ELKIPGPW
00052  WORKING-STORAGE SECTION.                                         ELKIPGPW
00053                                                                   ELKIPGPW
00054  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIPGPW
00055    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIPGPW
00056    88  WS-INVALID-PARM                 VALUE +8.                  ELKIPGPW
00057    88  WS-MISSING-PARM                 VALUE +12.                 ELKIPGPW
00058    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIPGPW
00059                                                                   ELKIPGPW
00060  01  WS-ELIGIBLE-WEIGHT          PIC S9(04) COMP.                 ELKIPGPW
00061  01  WS-MATCHED-WEIGHT           PIC S9(04) COMP.                 ELKIPGPW
00062  01  WS-TOTAL-WEIGHT             PIC S9(04) COMP.                 ELKIPGPW
00063                                                                   ELKIPGPW
00064  01  WS-CF-TOTAL                 COMP-1.                          ELKIPGPW
00065  01  WS-CF-ELIGIBLE              COMP-1.                          ELKIPGPW
00066  01  WS-CF-MATCHED               COMP-1.                          ELKIPGPW
00067                                                                   ELKIPGPW
00068  01  WS-CONF-ZERO            COMP-1  VALUE +0.000000E+00.         ELKIPGPW
00069  01  WS-CONF-NEGATIVE        COMP-1  VALUE -1.000000E+00.         ELKIPGPW
00070                                                                   ELKIPGPW
00071  01  WS-GXA-MAX-INDEX            INDEX.                           ELKIPGPW
00072 /                                                                 ELKIPGPW
00073  LINKAGE SECTION.                                                 ELKIPGPW
00074                                                                   ELKIPGPW
00075 /                                                                 ELKIPGPW
00076                                                                   ELKIPGPW
00077  01  IPGP-RECORD.                                                 ELKIPGPW
00078      COPY GCTIPGPC.                                               ELKIPGPW
00079                                                                   ELKIPGPW
00080                                                                   ELKIPGPW
00081      COPY ELSPRCWC.                                               ELKIPGPW
00082                                                                   ELKIPGPW
00083                                                                   ELKIPGPW
00084      COPY ELSCFDBC.                                               ELKIPGPW
00085 /*****************************************************************ELKIPGPW
00086 *                                                                *ELKIPGPW
00087 *    PROCEDURE DIVISION                                          *ELKIPGPW
00088 *                                                                *ELKIPGPW
00089 ******************************************************************ELKIPGPW
00090                                                                   ELKIPGPW
00091  PROCEDURE DIVISION USING IPGP-RECORD                             ELKIPGPW
00092                              PRCW-DX-TBL                          ELKIPGPW
00093                                CFDB-CNFDNC-FCTR-DATA-BLCK.        ELKIPGPW
00094                                                                   ELKIPGPW
00095  0000-DETERMINE-IPGP-CONFIDENCE.                                  ELKIPGPW
00096                                                                   ELKIPGPW
00097      PERFORM 0100-INITIALIZATION.                                 ELKIPGPW
00098      PERFORM 1000-PROCESS-IPGP-TABULAR.                           ELKIPGPW
00099      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGPW
00100      GOBACK.                                                      ELKIPGPW
00101                                                                   ELKIPGPW
00102 ******************************************************************ELKIPGPW
00103 *                                                                *ELKIPGPW
00104 *    INITIALIZATION                                              *ELKIPGPW
00105 *                                                                *ELKIPGPW
00106 ******************************************************************ELKIPGPW
00107                                                                   ELKIPGPW
00108  0100-INITIALIZATION.                                             ELKIPGPW
00109                                                                   ELKIPGPW
00110      MOVE ZEROS TO WS-ELIGIBLE-WEIGHT.                            ELKIPGPW
00111      MOVE ZEROS TO WS-MATCHED-WEIGHT.                             ELKIPGPW
00112      MOVE ZEROS TO WS-TOTAL-WEIGHT.                               ELKIPGPW
00113      INITIALIZE CFDB-CF-IPGP.                                     ELKIPGPW
00114                                                                   ELKIPGPW
00115 ******************************************************************ELKIPGPW
00116 *                                                                *ELKIPGPW
00117 *    PROCESS-IPGP-TABULAR                                        *ELKIPGPW
00118 *                                                                *ELKIPGPW
00119 ******************************************************************ELKIPGPW
00120                                                                   ELKIPGPW
00121  1000-PROCESS-IPGP-TABULAR.                                       ELKIPGPW
00122                                                                   ELKIPGPW
00123      IF ADDRESS OF IPGP-RECORD = NULL OR                          ELKIPGPW
00124         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIPGPW
00125         SET WS-MISSING-PARM TO TRUE                               ELKIPGPW
00126      ELSE                                                         ELKIPGPW
00127      IF ADDRESS OF PRCW-DX-TBL = NULL                             ELKIPGPW
00128         SET WS-MISSING-PARM TO TRUE                               ELKIPGPW
00129      ELSE                                                         ELKIPGPW
00130      IF PRCW-NBR-ENTRS = ZERO                                     ELKIPGPW
00131         SET WS-MISSING-PARM TO TRUE                               ELKIPGPW
00132      ELSE                                                         ELKIPGPW
00133      IF WS-SUCCESSFUL-CALL                                        ELKIPGPW
00134         PERFORM 1100-MATCH-LIST-PRCW-CODES.                       ELKIPGPW
00135                                                                   ELKIPGPW
00136 ******************************************************************ELKIPGPW
00137 *                                                                *ELKIPGPW
00138 *    MATCH LIST OF PROVISION CODES TO IPGP TABULAR.              *ELKIPGPW
00139 *                                                                *ELKIPGPW
00140 ******************************************************************ELKIPGPW
00141                                                                   ELKIPGPW
00142  1100-MATCH-LIST-PRCW-CODES.                                      ELKIPGPW
00143                                                                   ELKIPGPW
00144      SET GXA-INDEX TO GXA-ENTRY-COUNT.                            ELKIPGPW
00145      SET WS-GXA-MAX-INDEX TO GXA-INDEX.                           ELKIPGPW
00146      SET GXA-INDEX TO 1.                                          ELKIPGPW
00147      SET PRCW-IDX TO 1.                                           ELKIPGPW
00148      PERFORM 1200-MATCH-IPGP-TAB-MATCH-LIST                       ELKIPGPW
00149           UNTIL PRCW-IDX > PRCW-NBR-ENTRS OR                      ELKIPGPW
00150              WS-MISSING-PARM.                                     ELKIPGPW
00151                                                                   ELKIPGPW
00152      MOVE WS-ELIGIBLE-WEIGHT TO WS-CF-ELIGIBLE                    ELKIPGPW
00153      MOVE WS-TOTAL-WEIGHT TO WS-CF-TOTAL                          ELKIPGPW
00154                                                                   ELKIPGPW
00155      IF GXA-ID-ARGUMENT-INCLUDED                                  ELKIPGPW
00156         COMPUTE WS-CF-MATCHED =                                   ELKIPGPW
00157                WS-CF-TOTAL / WS-CF-ELIGIBLE                       ELKIPGPW
00158      ELSE                                                         ELKIPGPW
00159         COMPUTE WS-CF-MATCHED =                                   ELKIPGPW
00160                (WS-CF-TOTAL / WS-CF-ELIGIBLE) *                   ELKIPGPW
00161                                 WS-CONF-NEGATIVE.                 ELKIPGPW
00162                                                                   ELKIPGPW
00163      IF GXA-ID-ARGUMENT-INCLUDED AND                              ELKIPGPW
00164             WS-CF-MATCHED = WS-CONF-ZERO                          ELKIPGPW
00165         MOVE WS-CONF-NEGATIVE TO WS-CF-MATCHED.                   ELKIPGPW
00166                                                                   ELKIPGPW
00167      MOVE WS-MATCHED-WEIGHT TO CFDB-CF-IPGP-WGHTD-LST-MTCH.       ELKIPGPW
00168                                                                   ELKIPGPW
00169 ******************************************************************ELKIPGPW
00170 *                                                                *ELKIPGPW
00171 *    MATCH IPGP TABULAR TO PROCEDURE CODE MATCH LIST             *ELKIPGPW
00172 *                                                                *ELKIPGPW
00173 ******************************************************************ELKIPGPW
00174                                                                   ELKIPGPW
00175  1200-MATCH-IPGP-TAB-MATCH-LIST.                                  ELKIPGPW
00176                                                                   ELKIPGPW
00177      IF GXA-INDEX > WS-GXA-MAX-INDEX                              ELKIPGPW
00178         ADD PRCW-WGHT (PRCW-IDX) TO WS-TOTAL-WEIGHT               ELKIPGPW
00179         SET PRCW-IDX UP BY 1                                      ELKIPGPW
00180      IF GXA-PROCEDURE-ARGUMENT (GXA-INDEX) =                      ELKIPGPW
00181                        PRCW-PRCDR (PRCW-IDX)                      ELKIPGPW
00182         ADD PRCW-WGHT (PRCW-IDX) TO WS-ELIGIBLE-WEIGHT            ELKIPGPW
00183         SET GXA-INDEX UP BY 1                                     ELKIPGPW
00184         SET PRCW-IDX UP BY 1                                      ELKIPGPW
00185      ELSE                                                         ELKIPGPW
00186      IF GXA-PROCEDURE-ARGUMENT (GXA-INDEX) >                      ELKIPGPW
00187           PRCW-PRCDR (PRCW-IDX)                                   ELKIPGPW
00188         ADD PRCW-WGHT (PRCW-IDX) TO WS-TOTAL-WEIGHT               ELKIPGPW
00189         SET PRCW-IDX UP BY 1                                      ELKIPGPW
00190      ELSE                                                         ELKIPGPW
00191         SET GXA-INDEX UP BY 1.                                    ELKIPGPW
00192                                                                   ELKIPGPW
