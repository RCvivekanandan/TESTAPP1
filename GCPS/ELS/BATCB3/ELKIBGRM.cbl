00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIBGRM
00003  PROGRAM-ID.           ELKIBGRM.                                     LV004
00004                                                                   ELKIBGRM
00005  AUTHOR.               BARBARA KEIB.                              ELKIBGRM
00006                                                                   ELKIBGRM
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIBGRM
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIBGRM
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIBGRM
00010                        233 N. MICHIGAN AVE                        ELKIBGRM
00011                        CHICAGO, ILLINOIS 60601                    ELKIBGRM
00012                                                                   ELKIBGRM
00013  DATE-WRITTEN.         08-OCT-1992.                               ELKIBGRM
00014                                                                   ELKIBGRM
00015  ENVIRONMENT DIVISION.                                            ELKIBGRM
00016                                                                   ELKIBGRM
00017  CONFIGURATION SECTION.                                           ELKIBGRM
00018                                                                   ELKIBGRM
00019  SOURCE-COMPUTER.      IBM-3090.                                  ELKIBGRM
00020  OBJECT-COMPUTER.      IBM-3090.                                  ELKIBGRM
00021                                                                   ELKIBGRM
00022 ******************************************************************ELKIBGRM
00023 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIBGRM
00024 *  ELKIBGRM - COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE           *ELKIBGRM
00025 *             FACTOR FOR IBGR TABULAR.                           *ELKIBGRM
00026 *                                                                *ELKIBGRM
00027 *              THIS PROGRAM MATCHES AN #IBGR(PBV INTERNAL TAB)   *ELKIBGRM
00028 *              TO AN UNWEIGHTED LIST OF BENEFIT PROVISIONS AND   *ELKIBGRM
00029 *              PRODUCES A CONFIDENCE FACTOR WHICH REPRESENTS THE *ELKIBGRM
00030 *              DEGREE TO WHICH THE BENEFIT PROVISIONS IN THE     *ELKIBGRM
00031 *              MATCH LIST ARE REPRESENTED IN THE #IBGR TABULAR.  *ELKIBGRM
00032 *                                                                *ELKIBGRM
00033 ******************************************************************ELKIBGRM
00034 *                      MAINTENANCE HISTORY                       *ELKIBGRM
00035 *  MOD     DATE      BY                    ACTION                *ELKIBGRM
00036 * ----- ----------- --- -----------------------------------------*ELKIBGRM
00037 * 01.00 08-OCT-1992 BAK CREATED                                  *ELKIBGRM
00038 *                                                                *ELKIBGRM
00039 * 01.06 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX         *ELKIBGRM
00040 *                             ASM RECOMPILES                     *ELKIBGRM
00041 *                                                                *ELKIBGRM
00042 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIBGRM
00043 *                                                                *ELKIBGRM
00044 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELKIBGRM
00045 *                                                                *ELKIBGRM
00046 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIBGRM
00047 *                             CICSCB3 COMPILER FIX             *  ELKIBGRM
00048 ******************************************************************ELKIBGRM
00049  EJECT                                                            ELKIBGRM
00050  DATA DIVISION.                                                   ELKIBGRM
00051                                                                   ELKIBGRM
00052  WORKING-STORAGE SECTION.                                         ELKIBGRM
00053                                                                   ELKIBGRM
00054  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIBGRM
00055    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIBGRM
00056    88  WS-INVALID-PARM                 VALUE +8.                  ELKIBGRM
00057    88  WS-MISSING-PARM                 VALUE +12.                 ELKIBGRM
00058    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIBGRM
00059                                                                   ELKIBGRM
00060  01  WS-ENTRY-COUNT                    COMP-1.                    ELKIBGRM
00061  01  WS-ELIGIBLE                       COMP-1.                    ELKIBGRM
00062  01  WS-INCLUDED                       COMP-1.                    ELKIBGRM
00063  01  WS-MATCHED                        COMP-1.                    ELKIBGRM
00064                                                                   ELKIBGRM
00065  01  WS-CONF-ZERO            COMP-1    VALUE +0.000000E+00.       ELKIBGRM
00066  01  WS-CONF-NEGATIVE        COMP-1    VALUE -1.000000E+00.       ELKIBGRM
00067                                                                   ELKIBGRM
00068  01  WS-GX1-MAX-INDEX            INDEX.                           ELKIBGRM
00069                                                                   ELKIBGRM
00070  EJECT                                                            ELKIBGRM
00071  LINKAGE SECTION.                                                 ELKIBGRM
00072                                                                   ELKIBGRM
00073  EJECT                                                            ELKIBGRM
00074                                                                   ELKIBGRM
00075  01  IBGR-RECORD.                                                 ELKIBGRM
00076      COPY GCTIBGRC.                                               ELKIBGRM
00077                                                                   ELKIBGRM
00078                                                                   ELKIBGRM
00079      COPY ELSBPVLC.                                               ELKIBGRM
00080                                                                   ELKIBGRM
00081                                                                   ELKIBGRM
00082      COPY ELSCFDBC.                                               ELKIBGRM
00083  EJECT                                                            ELKIBGRM
00084 ******************************************************************ELKIBGRM
00085 *                                                                *ELKIBGRM
00086 *    PROCEDURE DIVISION                                          *ELKIBGRM
00087 *                                                                *ELKIBGRM
00088 ******************************************************************ELKIBGRM
00089                                                                   ELKIBGRM
00090  PROCEDURE DIVISION USING IBGR-RECORD                             ELKIBGRM
00091                               BPVL-BNFT-PRVSN-TBL                 ELKIBGRM
00092                                    CFDB-CNFDNC-FCTR-DATA-BLCK.    ELKIBGRM
00093                                                                   ELKIBGRM
00094  0000-DETERMINE-IBGR-CONFIDENCE.                                  ELKIBGRM
00095                                                                   ELKIBGRM
00096      PERFORM 0100-INITIALIZATION.                                 ELKIBGRM
00097      PERFORM 1000-PROCESS-IBGR-TABULAR.                           ELKIBGRM
00098      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIBGRM
00099      GOBACK.                                                      ELKIBGRM
00100                                                                   ELKIBGRM
00101 ******************************************************************ELKIBGRM
00102 *                                                                *ELKIBGRM
00103 *    INITIALIZATION                                              *ELKIBGRM
00104 *                                                                *ELKIBGRM
00105 ******************************************************************ELKIBGRM
00106                                                                   ELKIBGRM
00107  0100-INITIALIZATION.                                             ELKIBGRM
00108                                                                   ELKIBGRM
00109      MOVE ZEROS TO WS-ENTRY-COUNT.                                ELKIBGRM
00110      MOVE ZEROS TO WS-ELIGIBLE.                                   ELKIBGRM
00111      MOVE ZEROS TO WS-INCLUDED.                                   ELKIBGRM
00112      MOVE ZEROS TO WS-MATCHED.                                    ELKIBGRM
00113      INITIALIZE CFDB-CF-IBGR.                                     ELKIBGRM
00114                                                                   ELKIBGRM
00115 ******************************************************************ELKIBGRM
00116 *                                                                *ELKIBGRM
00117 *    PROCESS-IBGR-TABULAR                                        *ELKIBGRM
00118 *                                                                *ELKIBGRM
00119 ******************************************************************ELKIBGRM
00120                                                                   ELKIBGRM
00121  1000-PROCESS-IBGR-TABULAR.                                       ELKIBGRM
00122                                                                   ELKIBGRM
00123      IF ADDRESS OF IBGR-RECORD = NULL OR                          ELKIBGRM
00124         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIBGRM
00125         SET WS-MISSING-PARM TO TRUE                               ELKIBGRM
00126      ELSE                                                         ELKIBGRM
00127      IF ADDRESS OF BPVL-BNFT-PRVSN-TBL = NULL                     ELKIBGRM
00128         SET WS-MISSING-PARM TO TRUE                               ELKIBGRM
00129      ELSE                                                         ELKIBGRM
00130      IF BPVL-NBR-ENTRS = ZERO                                     ELKIBGRM
00131         SET WS-MISSING-PARM TO TRUE                               ELKIBGRM
00132      ELSE                                                         ELKIBGRM
00133      IF WS-SUCCESSFUL-CALL                                        ELKIBGRM
00134         PERFORM 1100-MATCH-LIST-BPVL-CODES.                       ELKIBGRM
00135                                                                   ELKIBGRM
00136 ******************************************************************ELKIBGRM
00137 *                                                                *ELKIBGRM
00138 *    MATCH LIST OF PROVISION-ID-ARGUMENT CODES TO IBGR.          *ELKIBGRM
00139 *                                                                *ELKIBGRM
00140 ******************************************************************ELKIBGRM
00141                                                                   ELKIBGRM
00142  1100-MATCH-LIST-BPVL-CODES.                                      ELKIBGRM
00143                                                                   ELKIBGRM
00144      SET GX1-INDEX TO GX1-ENTRY-COUNT.                            ELKIBGRM
00145      SET WS-GX1-MAX-INDEX TO GX1-INDEX.                           ELKIBGRM
00146      SET GX1-INDEX TO 1.                                          ELKIBGRM
00147      SET BPVL-IDX TO 1.                                           ELKIBGRM
00148      SET BPVL-MAX-IDX TO BPVL-NBR-ENTRS.                          ELKIBGRM
00149      PERFORM 1200-MATCH-IBGR-TAB-MATCH-LIST                       ELKIBGRM
00150           UNTIL BPVL-IDX > BPVL-MAX-IDX OR                        ELKIBGRM
00151                  GX1-INDEX > WS-GX1-MAX-INDEX.                    ELKIBGRM
00152                                                                   ELKIBGRM
00153      MOVE BPVL-NBR-ENTRS TO WS-ENTRY-COUNT.                       ELKIBGRM
00154      IF GX1-ID-ARGUMENT-INCLUDED                                  ELKIBGRM
00155         COMPUTE WS-MATCHED =                                      ELKIBGRM
00156               (WS-ELIGIBLE / WS-ENTRY-COUNT)                      ELKIBGRM
00157         ELSE                                                      ELKIBGRM
00158            COMPUTE WS-MATCHED =                                   ELKIBGRM
00159              (WS-ELIGIBLE / WS-ENTRY-COUNT) * WS-CONF-NEGATIVE.   ELKIBGRM
00160                                                                   ELKIBGRM
00161      IF GX1-ID-ARGUMENT-INCLUDED AND                              ELKIBGRM
00162                 WS-MATCHED = WS-CONF-ZERO                         ELKIBGRM
00163         MOVE WS-CONF-NEGATIVE TO WS-MATCHED.                      ELKIBGRM
00164                                                                   ELKIBGRM
00165      MOVE WS-MATCHED TO CFDB-CF-IBGR-UNWGHTD-LST-MTCH.            ELKIBGRM
00166                                                                   ELKIBGRM
00167 ******************************************************************ELKIBGRM
00168 *                                                                *ELKIBGRM
00169 *    MATCH IBGR TABULAR TO PROVISION-ID-ARGUMENT CODE MATCH LIST *ELKIBGRM
00170 *                                                                *ELKIBGRM
00171 ******************************************************************ELKIBGRM
00172                                                                   ELKIBGRM
00173  1200-MATCH-IBGR-TAB-MATCH-LIST.                                  ELKIBGRM
00174                                                                   ELKIBGRM
00175      IF GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) =                   ELKIBGRM
00176                           BPVL-BNFT-PRVSN (BPVL-IDX)              ELKIBGRM
00177         ADD +1 TO WS-ELIGIBLE                                     ELKIBGRM
00178         SET BPVL-IDX UP BY 1                                      ELKIBGRM
00179         SET GX1-INDEX UP BY 1                                     ELKIBGRM
00180      ELSE                                                         ELKIBGRM
00181         IF GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) >                ELKIBGRM
00182             BPVL-BNFT-PRVSN (BPVL-IDX)                            ELKIBGRM
00183            SET BPVL-IDX UP BY 1                                   ELKIBGRM
00184      ELSE                                                         ELKIBGRM
00185            SET GX1-INDEX UP BY 1.                                 ELKIBGRM
00186                                                                   ELKIBGRM
