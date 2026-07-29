00001  IDENTIFICATION DIVISION.                                         08/26/05
00002  PROGRAM-ID.           ELKIPGNM.                                  ELKIPGNM
00003                                                                      LV003
00004  AUTHOR.               BARBARA KEIB.                              ELKIPGNM
00005                                                                   ELKIPGNM
00006  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGNM
00007                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGNM
00008                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGNM
00009                        233 N. MICHIGAN AVE                        ELKIPGNM
00010                        CHICAGO, ILLINOIS 60601                    ELKIPGNM
00011                                                                   ELKIPGNM
00012  DATE-WRITTEN.         08-OCT-1992.                               ELKIPGNM
00013                                                                   ELKIPGNM
00014  ENVIRONMENT DIVISION.                                            ELKIPGNM
00015                                                                   ELKIPGNM
00016  CONFIGURATION SECTION.                                           ELKIPGNM
00017                                                                   ELKIPGNM
00018  SOURCE-COMPUTER.      IBM-3090.                                  ELKIPGNM
00019  OBJECT-COMPUTER.      IBM-3090.                                  ELKIPGNM
00020                                                                   ELKIPGNM
00021 ******************************************************************ELKIPGNM
00022 *                                                                *ELKIPGNM
00023 *                                                                *ELKIPGNM
00024 *  ELKIPGNM - COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE           *ELKIPGNM
00025 *             FACTOR FOR IPGN TABULAR.                           *ELKIPGNM
00026 *                                                                *ELKIPGNM
00027 *              THIS PROGRAM MATCHES AN #IPGN(PROVIDER INTRNL TAB)*ELKIPGNM
00028 *              TO AN UNWEIGHTED LIST OF PROVIDER NUMBERS AND     *ELKIPGNM
00029 *              PRODUCES A CONFIDENCE FACTOR WHICH REPRESENTS THE *ELKIPGNM
00030 *              DEGREE TO WHICH THE PROVIDER NUMBERS IN THE MATCH *ELKIPGNM
00031 *              LIST ARE REPRESENTED IN THE #IPGN TABULAR.        *ELKIPGNM
00032 *                                                                *ELKIPGNM
00033 ******************************************************************ELKIPGNM
00034 *                      MAINTENANCE HISTORY                       *ELKIPGNM
00035 *  MOD     DATE      BY                    ACTION                *ELKIPGNM
00036 * ----  ----------- --- -----------------------------------------*ELKIPGNM
00037 * 01.00 08-OCT-1992 BAK CREATED                                  *ELKIPGNM
00038 *                                                                *ELKIPGNM
00039 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGNM
00040 *                                                                *ELKIPGNM
00041 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGNM
00042 *                                                                *ELKIPGNM
00043 * 02.01 09-JAN-2004 AKK SOC7 INTERTEST                           *ELKIPGNM
00044 *                                                                *ELKIPGNM
00045 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGNM
00046 *                             CICSCB3 COMPILER FIX             *  ELKIPGNM
00047 *                                                                *ELKIPGNM
00048 ******************************************************************ELKIPGNM
00049  EJECT                                                            ELKIPGNM
00050  DATA DIVISION.                                                   ELKIPGNM
00051                                                                   ELKIPGNM
00052  WORKING-STORAGE SECTION.                                         ELKIPGNM
00053                                                                   ELKIPGNM
00054  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIPGNM
00055    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIPGNM
00056    88  WS-INVALID-PARM                 VALUE +8.                  ELKIPGNM
00057    88  WS-MISSING-PARM                 VALUE +12.                 ELKIPGNM
00058    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIPGNM
00059                                                                   ELKIPGNM
00060  01  WS-ENTRY-COUNT                    COMP-1.                    ELKIPGNM
00061  01  WS-ELIGIBLE                       COMP-1.                    ELKIPGNM
00062  01  WS-INCLUDED                       COMP-1.                    ELKIPGNM
00063  01  WS-MATCHED                        COMP-1.                    ELKIPGNM
00064                                                                   ELKIPGNM
00065  01  WS-CONF-ZERO              COMP-1    VALUE +0.000000E+00.     ELKIPGNM
00066  01  WS-CONF-NEGATIVE          COMP-1    VALUE -1.000000E+00.     ELKIPGNM
00067                                                                   ELKIPGNM
00068  01  WS-GX2-MAX-INDEX            INDEX.                           ELKIPGNM
00069  EJECT                                                            ELKIPGNM
00070  LINKAGE SECTION.                                                 ELKIPGNM
00071                                                                   ELKIPGNM
00072  EJECT                                                            ELKIPGNM
00073                                                                   ELKIPGNM
00074  01  IPGN-RECORD.                                                 ELKIPGNM
00075      COPY GCTIPGNC.                                               ELKIPGNM
00076                                                                   ELKIPGNM
00077                                                                   ELKIPGNM
00078      COPY ELSPVNLC.                                               ELKIPGNM
00079                                                                   ELKIPGNM
00080                                                                   ELKIPGNM
00081      COPY ELSCFDBC.                                               ELKIPGNM
00082  EJECT                                                            ELKIPGNM
00083 ******************************************************************ELKIPGNM
00084 *                                                                *ELKIPGNM
00085 *    PROCEDURE DIVISION                                          *ELKIPGNM
00086 *                                                                *ELKIPGNM
00087 ******************************************************************ELKIPGNM
00088                                                                   ELKIPGNM
00089  PROCEDURE DIVISION USING IPGN-RECORD                             ELKIPGNM
00090                               PVNL-PRVDR-NBR-TBL                  ELKIPGNM
00091                                    CFDB-CNFDNC-FCTR-DATA-BLCK.    ELKIPGNM
00092                                                                   ELKIPGNM
00093  0000-DETERMINE-IPGN-CONFIDENCE.                                  ELKIPGNM
00094                                                                   ELKIPGNM
00095      PERFORM 0100-INITIALIZATION.                                 ELKIPGNM
00096      PERFORM 1000-PROCESS-IPGN-TABULAR.                           ELKIPGNM
00097      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGNM
00098      GOBACK.                                                      ELKIPGNM
00099                                                                   ELKIPGNM
00100 ******************************************************************ELKIPGNM
00101 *                                                                *ELKIPGNM
00102 *    INITIALIZATION                                              *ELKIPGNM
00103 *                                                                *ELKIPGNM
00104 ******************************************************************ELKIPGNM
00105                                                                   ELKIPGNM
00106  0100-INITIALIZATION.                                             ELKIPGNM
00107                                                                   ELKIPGNM
00108      MOVE ZEROS TO WS-ENTRY-COUNT.                                ELKIPGNM
00109      MOVE ZEROS TO WS-ELIGIBLE.                                   ELKIPGNM
00110      MOVE ZEROS TO WS-INCLUDED.                                   ELKIPGNM
00111      MOVE ZEROS TO WS-MATCHED.                                    ELKIPGNM
00112      INITIALIZE CFDB-CF-IPGN.                                     ELKIPGNM
00113                                                                   ELKIPGNM
00114 ******************************************************************ELKIPGNM
00115 *                                                                *ELKIPGNM
00116 *    PROCESS-IPGN-TABULAR                                        *ELKIPGNM
00117 *                                                                *ELKIPGNM
00118 ******************************************************************ELKIPGNM
00119                                                                   ELKIPGNM
00120  1000-PROCESS-IPGN-TABULAR.                                       ELKIPGNM
00121                                                                   ELKIPGNM
00122      IF ADDRESS OF IPGN-RECORD = NULL OR                          ELKIPGNM
00123         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIPGNM
00124         SET WS-MISSING-PARM TO TRUE                               ELKIPGNM
00125      ELSE                                                         ELKIPGNM
00126      IF ADDRESS OF PVNL-PRVDR-NBR-TBL = NULL                      ELKIPGNM
00127         SET WS-MISSING-PARM TO TRUE                               ELKIPGNM
00128      ELSE                                                         ELKIPGNM
00129      IF PVNL-NBR-ENTRS = ZERO                                     ELKIPGNM
00130         SET WS-MISSING-PARM TO TRUE                               ELKIPGNM
00131      ELSE                                                         ELKIPGNM
00132      IF WS-SUCCESSFUL-CALL                                        ELKIPGNM
00133         PERFORM 1100-MATCH-LIST-PVNL-CODES.                       ELKIPGNM
00134                                                                   ELKIPGNM
00135 ******************************************************************ELKIPGNM
00136 *                                                                *ELKIPGNM
00137 *    MATCH LIST OF PROVIDER-NO-ARGUMENT CODES TO IPGN.           *ELKIPGNM
00138 *                                                                *ELKIPGNM
00139 ******************************************************************ELKIPGNM
00140                                                                   ELKIPGNM
00141  1100-MATCH-LIST-PVNL-CODES.                                      ELKIPGNM
00142                                                                   ELKIPGNM
00143      SET GX2-INDEX TO GX2-ENTRY-COUNT.                            ELKIPGNM
00144      SET WS-GX2-MAX-INDEX TO GX2-INDEX.                           ELKIPGNM
00145      SET GX2-INDEX TO 1.                                          ELKIPGNM
00146      SET PVNL-IDX TO 1.                                           ELKIPGNM
00147      PERFORM 1200-MATCH-IPGN-TAB-MATCH-LIST                       ELKIPGNM
00148           UNTIL PVNL-IDX > PVNL-NBR-ENTRS OR                      ELKIPGNM
00149                  GX2-INDEX > WS-GX2-MAX-INDEX.                    ELKIPGNM
00150                                                                   ELKIPGNM
00151      MOVE PVNL-NBR-ENTRS TO WS-ENTRY-COUNT.                       ELKIPGNM
00152      IF GX2-ID-ARGUMENT-INCLUDED                                  ELKIPGNM
00153         COMPUTE WS-MATCHED =                                      ELKIPGNM
00154               (WS-ELIGIBLE / WS-ENTRY-COUNT)                      ELKIPGNM
00155      ELSE                                                         ELKIPGNM
00156         COMPUTE WS-MATCHED =                                      ELKIPGNM
00157               (WS-ELIGIBLE / WS-ENTRY-COUNT) * WS-CONF-NEGATIVE.  ELKIPGNM
00158                                                                   ELKIPGNM
00159      IF GX2-ID-ARGUMENT-INCLUDED AND                              ELKIPGNM
00160                  WS-MATCHED = WS-CONF-ZERO                        ELKIPGNM
00161         MOVE WS-CONF-NEGATIVE TO WS-MATCHED.                      ELKIPGNM
00162                                                                   ELKIPGNM
00163         MOVE WS-MATCHED TO CFDB-CF-IPGN-UNWGHTD-LST-MTCH.         ELKIPGNM
00164                                                                   ELKIPGNM
00165 ******************************************************************ELKIPGNM
00166 *                                                                *ELKIPGNM
00167 *    MATCH IPGN TABULAR TO PROVIDER-NO-ARGUMENT CODE MATCH LIST * ELKIPGNM
00168 *                                                                *ELKIPGNM
00169 ******************************************************************ELKIPGNM
00170                                                                   ELKIPGNM
00171  1200-MATCH-IPGN-TAB-MATCH-LIST.                                  ELKIPGNM
00172                                                                   ELKIPGNM
00173      IF GX2-PROVIDER-NO-ARGUMENT (GX2-INDEX) =                    ELKIPGNM
00174             PVNL-PRVDR-NBR (PVNL-IDX)                             ELKIPGNM
00175         ADD +1 TO WS-ELIGIBLE                                     ELKIPGNM
00176         SET GX2-INDEX UP BY 1                                     ELKIPGNM
00177         SET PVNL-IDX UP BY 1                                      ELKIPGNM
00178      ELSE                                                         ELKIPGNM
00179         IF GX2-PROVIDER-NO-ARGUMENT (GX2-INDEX) >                 ELKIPGNM
00180             PVNL-PRVDR-NBR (PVNL-IDX)                             ELKIPGNM
00181            SET PVNL-IDX UP BY 1                                   ELKIPGNM
00182      ELSE                                                         ELKIPGNM
00183            SET GX2-INDEX UP BY 1.                                 ELKIPGNM
00184                                                                   ELKIPGNM
00185                                                                   ELKIPGNM
