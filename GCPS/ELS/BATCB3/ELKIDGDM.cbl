00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIDGDM
00003  PROGRAM-ID.           ELKIDGDM.                                     LV004
00004                                                                   ELKIDGDM
00005  AUTHOR.               BARBARA KEIB.                              ELKIDGDM
00006                                                                   ELKIDGDM
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIDGDM
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIDGDM
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIDGDM
00010                        233 N. MICHIGAN AVE                        ELKIDGDM
00011                        CHICAGO, ILLINOIS 60601                    ELKIDGDM
00012                                                                   ELKIDGDM
00013  DATE-WRITTEN.         09-OCT-1992.                               ELKIDGDM
00014                                                                   ELKIDGDM
00015  ENVIRONMENT DIVISION.                                            ELKIDGDM
00016                                                                   ELKIDGDM
00017  CONFIGURATION SECTION.                                           ELKIDGDM
00018                                                                   ELKIDGDM
00019  SOURCE-COMPUTER.      IBM-3090.                                  ELKIDGDM
00020  OBJECT-COMPUTER.      IBM-3090.                                  ELKIDGDM
00021                                                                   ELKIDGDM
00022 ******************************************************************ELKIDGDM
00023 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIDGDM
00024 *                                                                *ELKIDGDM
00025 *  ELKIDGDM - COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE           *ELKIDGDM
00026 *             FACTOR FOR IDGD TABULAR.                           *ELKIDGDM
00027 *                                                                *ELKIDGDM
00028 *              THIS PROGRAM MATCHES AN #IDGD(DIAGNOSIS CODE INTRN*ELKIDGDM
00029 *              TAB) TO AN UNWEIGHTED LIST OF DIAGNOSIS CODES AND *ELKIDGDM
00030 *              PRODUCES A CONFIDENCE FACTOR WHICH REPRESENTS THE *ELKIDGDM
00031 *              DEGREE TO WHICH THE DIAGNOSIS CODES IN THE MATCH  *ELKIDGDM
00032 *              LIST ARE REPRESENTED IN THE #IDGD TABULAR.        *ELKIDGDM
00033 *                                                                *ELKIDGDM
00034 ******************************************************************ELKIDGDM
00035 *                      MAINTENANCE HISTORY                       *ELKIDGDM
00036 *                                                                *ELKIDGDM
00037 *  MOD     DATE      BY                    ACTION                *ELKIDGDM
00038 * ----  ----------- --- -----------------------------------------*ELKIDGDM
00039 * 01.00 09-OCT-1992 BAK CREATED                                  *ELKIDGDM
00040 *                                                                *ELKIDGDM
00041 * 01.06 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX         *ELKIDGDM
00042 *                             ASM RECOMPILES                     *ELKIDGDM
00043 *                                                                *ELKIDGDM
00044 * 02.00 07-MAY-2003 AKK       REGEN'D FOR EXPANSION OF THE       *ELKIDGDM
00045 *                             PROC/DIAG CODES.                   *ELKIDGDM
00046 *                                                                *ELKIDGDM
00047 * 02.01 24-JUN-2003 AKK       USING WS FILED TO MOVE ONLY 6 CHAR *ELKIDGDM
00048 *                             DIAG CODE FOR CALC. THIS IS TO     *ELKIDGDM
00049 *                             AVOID MANY CHAGES TO CALC LOGIC.   *ELKIDGDM
00050 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIDGDM
00051 *                                                                *ELKIDGDM
00052 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELKIDGDM
00053 *                                                                *ELKIDGDM
00054 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIDGDM
00055 *                             CICSCB3 COMPILER FIX             *  ELKIDGDM
00056 ******************************************************************ELKIDGDM
00057  EJECT                                                            ELKIDGDM
00058  DATA DIVISION.                                                   ELKIDGDM
00059                                                                   ELKIDGDM
00060  WORKING-STORAGE SECTION.                                         ELKIDGDM
00061                                                                   ELKIDGDM
00062  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIDGDM
00063    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIDGDM
00064    88  WS-INVALID-PARM                 VALUE +8.                  ELKIDGDM
00065    88  WS-MISSING-PARM                 VALUE +12.                 ELKIDGDM
00066    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIDGDM
00067                                                                   ELKIDGDM
00068  01  WS-HOLD-DIAGNOSIS-CODE.                                      ELKIDGDM
00069      05  WS-HOLD-DIAG-6          PIC X(06).                       ELKIDGDM
00070      05  WS-FILLER-4              PIC X(04).                      ELKIDGDM
00071                                                                   ELKIDGDM
00072  01  WS-ENTRY-COUNT                    COMP-1.                    ELKIDGDM
00073  01  WS-ELIGIBLE                       COMP-1.                    ELKIDGDM
00074  01  WS-INCLUDED                       COMP-1.                    ELKIDGDM
00075  01  WS-MATCHED                        COMP-1.                    ELKIDGDM
00076                                                                   ELKIDGDM
00077  01  WS-CONF-ZERO            COMP-1     VALUE +0.000000E+00.      ELKIDGDM
00078  01  WS-CONF-NEGATIVE        COMP-1     VALUE -1.000000E+00.      ELKIDGDM
00079                                                                   ELKIDGDM
00080  01  WS-GX9-MAX-INDEX            INDEX.                           ELKIDGDM
00081  EJECT                                                            ELKIDGDM
00082  LINKAGE SECTION.                                                 ELKIDGDM
00083                                                                   ELKIDGDM
00084  EJECT                                                            ELKIDGDM
00085                                                                   ELKIDGDM
00086  01  IDGD-RECORD.                                                 ELKIDGDM
00087      COPY GCTIDGDC.                                               ELKIDGDM
00088                                                                   ELKIDGDM
00089                                                                   ELKIDGDM
00090      COPY ELSDXSLC.                                               ELKIDGDM
00091                                                                   ELKIDGDM
00092                                                                   ELKIDGDM
00093      COPY ELSCFDBC.                                               ELKIDGDM
00094  EJECT                                                            ELKIDGDM
00095 ******************************************************************ELKIDGDM
00096 *                                                                *ELKIDGDM
00097 *    PROCEDURE DIVISION                                          *ELKIDGDM
00098 *                                                                *ELKIDGDM
00099 ******************************************************************ELKIDGDM
00100                                                                   ELKIDGDM
00101  PROCEDURE DIVISION USING IDGD-RECORD                             ELKIDGDM
00102                               DXSL-DX-TBL                         ELKIDGDM
00103                                    CFDB-CNFDNC-FCTR-DATA-BLCK.    ELKIDGDM
00104                                                                   ELKIDGDM
00105  0000-DETERMINE-IDGD-CONFIDENCE.                                  ELKIDGDM
00106                                                                   ELKIDGDM
00107      PERFORM 0100-INITIALIZATION.                                 ELKIDGDM
00108      PERFORM 1000-PROCESS-IDGD-TABULAR.                           ELKIDGDM
00109      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIDGDM
00110      GOBACK.                                                      ELKIDGDM
00111                                                                   ELKIDGDM
00112 ******************************************************************ELKIDGDM
00113 *                                                                *ELKIDGDM
00114 *    INITIALIZATION                                              *ELKIDGDM
00115 *                                                                *ELKIDGDM
00116 ******************************************************************ELKIDGDM
00117                                                                   ELKIDGDM
00118  0100-INITIALIZATION.                                             ELKIDGDM
00119                                                                   ELKIDGDM
00120      MOVE ZEROS TO WS-ENTRY-COUNT.                                ELKIDGDM
00121      MOVE ZEROS TO WS-ELIGIBLE.                                   ELKIDGDM
00122      MOVE ZEROS TO WS-INCLUDED.                                   ELKIDGDM
00123      MOVE ZEROS TO WS-MATCHED.                                    ELKIDGDM
00124      INITIALIZE CFDB-CF-IDGD.                                     ELKIDGDM
00125                                                                   ELKIDGDM
00126 ******************************************************************ELKIDGDM
00127 *                                                                *ELKIDGDM
00128 *    PROCESS-IDGD-TABULAR                                        *ELKIDGDM
00129 *                                                                *ELKIDGDM
00130 ******************************************************************ELKIDGDM
00131                                                                   ELKIDGDM
00132  1000-PROCESS-IDGD-TABULAR.                                       ELKIDGDM
00133                                                                   ELKIDGDM
00134      IF ADDRESS OF IDGD-RECORD = NULL OR                          ELKIDGDM
00135         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIDGDM
00136         SET WS-MISSING-PARM TO TRUE                               ELKIDGDM
00137      ELSE                                                         ELKIDGDM
00138         IF ADDRESS OF DXSL-DX-TBL = NULL                          ELKIDGDM
00139            SET WS-MISSING-PARM TO TRUE                            ELKIDGDM
00140      ELSE                                                         ELKIDGDM
00141            IF DXSL-NBR-ENTRS = ZERO                               ELKIDGDM
00142               SET WS-MISSING-PARM TO TRUE                         ELKIDGDM
00143      ELSE                                                         ELKIDGDM
00144               IF WS-SUCCESSFUL-CALL                               ELKIDGDM
00145                  PERFORM 1100-MATCH-LIST-DXSL-CODES.              ELKIDGDM
00146                                                                   ELKIDGDM
00147 ******************************************************************ELKIDGDM
00148 *                                                                *ELKIDGDM
00149 *    MATCH LIST OF DIAGNOSIS-ARGUMENT CODES TO IDGD.             *ELKIDGDM
00150 *                                                                *ELKIDGDM
00151 ******************************************************************ELKIDGDM
00152                                                                   ELKIDGDM
00153  1100-MATCH-LIST-DXSL-CODES.                                      ELKIDGDM
00154                                                                   ELKIDGDM
00155      SET GX9-INDEX TO GX9-ENTRY-COUNT.                            ELKIDGDM
00156      SET WS-GX9-MAX-INDEX TO GX9-INDEX.                           ELKIDGDM
00157      SET GX9-INDEX TO 1.                                          ELKIDGDM
00158      SET DXSL-IDX TO 1.                                           ELKIDGDM
00159      PERFORM 1200-MATCH-IDGD-TAB-MATCH-LIST                       ELKIDGDM
00160           UNTIL DXSL-IDX > DXSL-NBR-ENTRS OR                      ELKIDGDM
00161                  GX9-INDEX > WS-GX9-MAX-INDEX.                    ELKIDGDM
00162                                                                   ELKIDGDM
00163         MOVE DXSL-NBR-ENTRS TO WS-ENTRY-COUNT                     ELKIDGDM
00164      IF GX9-ID-ARGUMENT-INCLUDED                                  ELKIDGDM
00165         COMPUTE WS-MATCHED =                                      ELKIDGDM
00166               (WS-ELIGIBLE / WS-ENTRY-COUNT)                      ELKIDGDM
00167      ELSE                                                         ELKIDGDM
00168         COMPUTE WS-MATCHED =                                      ELKIDGDM
00169               (WS-ELIGIBLE / WS-ENTRY-COUNT) * WS-CONF-NEGATIVE.  ELKIDGDM
00170                                                                   ELKIDGDM
00171      IF GX9-ID-ARGUMENT-INCLUDED AND                              ELKIDGDM
00172                     WS-MATCHED = WS-CONF-ZERO                     ELKIDGDM
00173         MOVE WS-CONF-NEGATIVE TO WS-MATCHED.                      ELKIDGDM
00174                                                                   ELKIDGDM
00175      MOVE WS-MATCHED TO CFDB-CF-IDGD-UNWGHTD-LST-MTCH.            ELKIDGDM
00176                                                                   ELKIDGDM
00177 ******************************************************************ELKIDGDM
00178 *                                                                *ELKIDGDM
00179 *    MATCH IDGD TABULAR TO DIAGNOSIS-ARGUMENT CODE MATCH LIST *   ELKIDGDM
00180 *                                                                *ELKIDGDM
00181 ******************************************************************ELKIDGDM
00182                                                                   ELKIDGDM
00183  1200-MATCH-IDGD-TAB-MATCH-LIST.                                  ELKIDGDM
00184      MOVE GX9-DIAGNOSIS-ARGUMENT (GX9-INDEX) TO WS-HOLD-DIAG-6.   ELKIDGDM
00185 *    IF GX9-DIAGNOSIS-ARGUMENT (GX9-INDEX) =                      ELKIDGDM
00186      IF WS-HOLD-DIAG-6 =                                          ELKIDGDM
00187             DXSL-DX (DXSL-IDX)                                    ELKIDGDM
00188         ADD +1 TO WS-ELIGIBLE                                     ELKIDGDM
00189         SET GX9-INDEX UP BY 1                                     ELKIDGDM
00190         SET DXSL-IDX UP BY 1                                      ELKIDGDM
00191      ELSE                                                         ELKIDGDM
00192 *       IF GX9-DIAGNOSIS-ARGUMENT (GX9-INDEX) >                   ELKIDGDM
00193         IF WS-HOLD-DIAG-6 >                                       ELKIDGDM
00194             DXSL-DX (DXSL-IDX)                                    ELKIDGDM
00195            SET DXSL-IDX UP BY 1                                   ELKIDGDM
00196      ELSE                                                         ELKIDGDM
00197            SET GX9-INDEX UP BY 1.                                 ELKIDGDM
00198                                                                   ELKIDGDM
