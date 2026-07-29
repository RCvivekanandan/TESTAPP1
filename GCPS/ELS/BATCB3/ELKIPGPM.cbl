00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGPM
00003  PROGRAM-ID.           ELKIPGPM.                                     LV004
00004                                                                   ELKIPGPM
00005  AUTHOR.               BARBARA KEIB.                              ELKIPGPM
00006                                                                   ELKIPGPM
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGPM
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGPM
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGPM
00010                        233 N. MICHIGAN AVE                        ELKIPGPM
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGPM
00012                                                                   ELKIPGPM
00013  DATE-WRITTEN.         08-OCT-1992.                               ELKIPGPM
00014                                                                   ELKIPGPM
00015  ENVIRONMENT DIVISION.                                            ELKIPGPM
00016                                                                   ELKIPGPM
00017  CONFIGURATION SECTION.                                           ELKIPGPM
00018                                                                   ELKIPGPM
00019  SOURCE-COMPUTER.      IBM-3090.                                  ELKIPGPM
00020  OBJECT-COMPUTER.      IBM-3090.                                  ELKIPGPM
00021                                                                   ELKIPGPM
00022 ******************************************************************ELKIPGPM
00023 *AKK 12/06/05 REGEN FOR TEST                                     *ELKIPGPM
00024 *                                                                *ELKIPGPM
00025 *  ELKIPGPM - COMPUTE UNWEIGHTED LIST MATCH CONFIDENCE           *ELKIPGPM
00026 *             FACTOR FOR IPGP TABULAR.                           *ELKIPGPM
00027 *                                                                *ELKIPGPM
00028 *              THIS PROGRAM MATCHES AN #IPGP(PROCEDURE CODE INTRL*ELKIPGPM
00029 *              TO AN UNWEIGHTED LIST OF PROCEDURE CODES AND      *ELKIPGPM
00030 *              PRODUCES A CONFIDENCE FACTOR WHICH REPRESENTS THE *ELKIPGPM
00031 *              DEGREE TO WHICH THE PROCEDURE CODES IN THE MATCH  *ELKIPGPM
00032 *              LIST ARE REPRESENTED IN THE #IPGP TABULAR.        *ELKIPGPM
00033 *                                                                *ELKIPGPM
00034 ******************************************************************ELKIPGPM
00035 *                      MAINTENANCE HISTORY                       *ELKIPGPM
00036 *                                                                *ELKIPGPM
00037 *  MOD     DATE      BY                    ACTION                *ELKIPGPM
00038 * ----  ----------- --- -----------------------------------------*ELKIPGPM
00039 * 01.00 08-OCT-1992 BAK CREATED                                  *ELKIPGPM
00040 *                                                                *ELKIPGPM
00041 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGPM
00042 *                                                                *ELKIPGPM
00043 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGPM
00044 *                                                                *ELKIPGPM
00045 * 02.01 09-JAN-2003 AKK S0C7 INTERTEST                           *ELKIPGPM
00046 *                                                                *ELKIPGPM
00047 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGPM
00048 *                             CICSCB3 COMPILER FIX             *  ELKIPGPM
00049 ******************************************************************ELKIPGPM
00050  EJECT                                                            ELKIPGPM
00051  DATA DIVISION.                                                   ELKIPGPM
00052                                                                   ELKIPGPM
00053  WORKING-STORAGE SECTION.                                         ELKIPGPM
00054                                                                   ELKIPGPM
00055  01  WS-RETURN-CODE              PIC S9(04) COMP.                 ELKIPGPM
00056    88  WS-SUCCESSFUL-CALL              VALUE ZERO.                ELKIPGPM
00057    88  WS-INVALID-PARM                 VALUE +8.                  ELKIPGPM
00058    88  WS-MISSING-PARM                 VALUE +12.                 ELKIPGPM
00059    88  WS-INTERNAL-ERROR               VALUE +16.                 ELKIPGPM
00060                                                                   ELKIPGPM
00061  01  WS-ENTRY-COUNT                    COMP-1.                    ELKIPGPM
00062  01  WS-ELIGIBLE                       COMP-1.                    ELKIPGPM
00063  01  WS-INCLUDED                       COMP-1.                    ELKIPGPM
00064  01  WS-MATCHED                        COMP-1.                    ELKIPGPM
00065                                                                   ELKIPGPM
00066  01  WS-CONF-ZERO                 COMP-1   VALUE +0.000000E+00.   ELKIPGPM
00067  01  WS-CONF-NEGATIVE             COMP-1   VALUE -1.000000E+00.   ELKIPGPM
00068                                                                   ELKIPGPM
00069  01  WS-GXA-MAX-INDEX            INDEX.                           ELKIPGPM
00070  EJECT                                                            ELKIPGPM
00071  LINKAGE SECTION.                                                 ELKIPGPM
00072                                                                   ELKIPGPM
00073  EJECT                                                            ELKIPGPM
00074                                                                   ELKIPGPM
00075  01  IPGP-RECORD.                                                 ELKIPGPM
00076      COPY GCTIPGPC.                                               ELKIPGPM
00077                                                                   ELKIPGPM
00078                                                                   ELKIPGPM
00079      COPY ELSPRCLC.                                               ELKIPGPM
00080                                                                   ELKIPGPM
00081                                                                   ELKIPGPM
00082      COPY ELSCFDBC.                                               ELKIPGPM
00083  EJECT                                                            ELKIPGPM
00084 ******************************************************************ELKIPGPM
00085 *                                                                *ELKIPGPM
00086 *    PROCEDURE DIVISION                                          *ELKIPGPM
00087 *                                                                *ELKIPGPM
00088 ******************************************************************ELKIPGPM
00089                                                                   ELKIPGPM
00090  PROCEDURE DIVISION USING IPGP-RECORD                             ELKIPGPM
00091                               PRCL-DX-TBL                         ELKIPGPM
00092                                    CFDB-CNFDNC-FCTR-DATA-BLCK.    ELKIPGPM
00093                                                                   ELKIPGPM
00094  0000-DETERMINE-IPGP-CONFIDENCE.                                  ELKIPGPM
00095                                                                   ELKIPGPM
00096      PERFORM 0100-INITIALIZATION.                                 ELKIPGPM
00097      PERFORM 1000-PROCESS-IPGP-TABULAR.                           ELKIPGPM
00098      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGPM
00099      GOBACK.                                                      ELKIPGPM
00100                                                                   ELKIPGPM
00101 ******************************************************************ELKIPGPM
00102 *                                                                *ELKIPGPM
00103 *    INITIALIZATION                                              *ELKIPGPM
00104 *                                                                *ELKIPGPM
00105 ******************************************************************ELKIPGPM
00106                                                                   ELKIPGPM
00107  0100-INITIALIZATION.                                             ELKIPGPM
00108                                                                   ELKIPGPM
00109      MOVE ZEROS TO WS-ENTRY-COUNT.                                ELKIPGPM
00110      MOVE ZEROS TO WS-ELIGIBLE.                                   ELKIPGPM
00111      MOVE ZEROS TO WS-INCLUDED.                                   ELKIPGPM
00112      MOVE ZEROS TO WS-MATCHED.                                    ELKIPGPM
00113      INITIALIZE CFDB-CF-IPGP.                                     ELKIPGPM
00114                                                                   ELKIPGPM
00115 ******************************************************************ELKIPGPM
00116 *                                                                *ELKIPGPM
00117 *    PROCESS-IPGP-TABULAR                                        *ELKIPGPM
00118 *                                                                *ELKIPGPM
00119 ******************************************************************ELKIPGPM
00120                                                                   ELKIPGPM
00121  1000-PROCESS-IPGP-TABULAR.                                       ELKIPGPM
00122                                                                   ELKIPGPM
00123      IF ADDRESS OF IPGP-RECORD = NULL OR                          ELKIPGPM
00124         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIPGPM
00125         SET WS-MISSING-PARM TO TRUE                               ELKIPGPM
00126      ELSE                                                         ELKIPGPM
00127      IF ADDRESS OF PRCL-DX-TBL = NULL                             ELKIPGPM
00128         SET WS-MISSING-PARM TO TRUE                               ELKIPGPM
00129      ELSE                                                         ELKIPGPM
00130      IF PRCL-NBR-ENTRS = ZERO                                     ELKIPGPM
00131         SET WS-MISSING-PARM TO TRUE                               ELKIPGPM
00132      ELSE                                                         ELKIPGPM
00133      IF WS-SUCCESSFUL-CALL                                        ELKIPGPM
00134         PERFORM 1100-MATCH-LIST-PRCL-CODES.                       ELKIPGPM
00135                                                                   ELKIPGPM
00136 ******************************************************************ELKIPGPM
00137 *                                                                *ELKIPGPM
00138 *    MATCH LIST OF PROCEDURE-ARGUMENT CODES TO IPGP.             *ELKIPGPM
00139 *                                                                *ELKIPGPM
00140 ******************************************************************ELKIPGPM
00141                                                                   ELKIPGPM
00142  1100-MATCH-LIST-PRCL-CODES.                                      ELKIPGPM
00143                                                                   ELKIPGPM
00144      SET GXA-INDEX TO GXA-ENTRY-COUNT.                            ELKIPGPM
00145      SET WS-GXA-MAX-INDEX TO GXA-INDEX.                           ELKIPGPM
00146      SET GXA-INDEX TO 1.                                          ELKIPGPM
00147      SET PRCL-IDX TO 1.                                           ELKIPGPM
00148      PERFORM 1200-MATCH-IPGP-TAB-MATCH-LIST                       ELKIPGPM
00149           UNTIL PRCL-IDX > PRCL-NBR-ENTRS OR                      ELKIPGPM
00150                  GXA-INDEX > WS-GXA-MAX-INDEX.                    ELKIPGPM
00151                                                                   ELKIPGPM
00152      MOVE PRCL-NBR-ENTRS TO WS-ENTRY-COUNT.                       ELKIPGPM
00153      IF GXA-ID-ARGUMENT-INCLUDED                                  ELKIPGPM
00154         COMPUTE WS-MATCHED =                                      ELKIPGPM
00155               (WS-ELIGIBLE / WS-ENTRY-COUNT)                      ELKIPGPM
00156      ELSE                                                         ELKIPGPM
00157         COMPUTE WS-MATCHED =                                      ELKIPGPM
00158               (WS-ELIGIBLE / WS-ENTRY-COUNT) * WS-CONF-NEGATIVE.  ELKIPGPM
00159                                                                   ELKIPGPM
00160      IF GXA-ID-ARGUMENT-INCLUDED AND WS-MATCHED = WS-CONF-ZERO    ELKIPGPM
00161         MOVE WS-CONF-NEGATIVE TO WS-MATCHED.                      ELKIPGPM
00162                                                                   ELKIPGPM
00163      MOVE WS-MATCHED TO CFDB-CF-IPGP-UNWGHTD-LST-MTCH.            ELKIPGPM
00164                                                                   ELKIPGPM
00165 ******************************************************************ELKIPGPM
00166 *                                                                *ELKIPGPM
00167 *    MATCH IPGP TABULAR TO PROCEDURE-ARGUMENT CODE MATCH LIST *   ELKIPGPM
00168 *                                                                *ELKIPGPM
00169 ******************************************************************ELKIPGPM
00170                                                                   ELKIPGPM
00171  1200-MATCH-IPGP-TAB-MATCH-LIST.                                  ELKIPGPM
00172                                                                   ELKIPGPM
00173      IF GXA-PROCEDURE-ARGUMENT (GXA-INDEX) =                      ELKIPGPM
00174             PRCL-PRCDR (PRCL-IDX)                                 ELKIPGPM
00175         ADD +1 TO WS-ELIGIBLE                                     ELKIPGPM
00176         SET GXA-INDEX UP BY 1                                     ELKIPGPM
00177         SET PRCL-IDX UP BY 1                                      ELKIPGPM
00178      ELSE                                                         ELKIPGPM
00179         IF GXA-PROCEDURE-ARGUMENT (GXA-INDEX) >                   ELKIPGPM
00180             PRCL-PRCDR (PRCL-IDX)                                 ELKIPGPM
00181            SET PRCL-IDX UP BY 1                                   ELKIPGPM
00182      ELSE                                                         ELKIPGPM
00183            SET GXA-INDEX UP BY 1.                                 ELKIPGPM
00184                                                                   ELKIPGPM
