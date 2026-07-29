00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGPC
00003  PROGRAM-ID.              ELKIPGPC                                   LV004
00004                                                                   ELKIPGPC
00005  AUTHOR.                  BARBARA KEIB                            ELKIPGPC
00006                                                                   ELKIPGPC
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGPC
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGPC
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGPC
00010                        233 N. MICHIGAN AVE                        ELKIPGPC
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGPC
00012                                                                   ELKIPGPC
00013  DATE-WRITTEN.         09-OCT-1992.                               ELKIPGPC
00014                                                                   ELKIPGPC
00015  ENVIRONMENT DIVISION.                                            ELKIPGPC
00016  CONFIGURATION SECTION.                                           ELKIPGPC
00017  SOURCE-COMPUTER. IBM-3090.                                       ELKIPGPC
00018  OBJECT-COMPUTER. IBM-3090.                                       ELKIPGPC
00019                                                                   ELKIPGPC
00020 ****************************************************************  ELKIPGPC
00021 *AKK 12/06/05 REGEN FOR TEST                                   *  ELKIPGPC
00022 *  ELKIPGPC :  THIS PROGRAM COMPUTES THE OVERALL CONFIDENCE    *  ELKIPGPC
00023 *              FACTORS FOR A SINGLE #IPGP TABULAR RECORD.      *  ELKIPGPC
00024 *                                                              *  ELKIPGPC
00025 *              THIS PROGRAM COMPUTES THE OVERALL CONFIDENCE    *  ELKIPGPC
00026 *              FACTORS FOR A SINGLE #IPGP TABULAR.             *  ELKIPGPC
00027 *              THE CONFIDENCE FACTORS COMPUTED ARE:            *  ELKIPGPC
00028 *              INSITUTIONAL, PROFESSIONAL AND OVERALL.         *  ELKIPGPC
00029 *                                                              *  ELKIPGPC
00030 *              VALUES ARE BASED ON AN EXTERNALLY DETERMINED    *  ELKIPGPC
00031 *              NUMBER OF POSSIBLE ICD-9 (INSTITUTIONAL) AND    *  ELKIPGPC
00032 *              CPT-4 (PROFESSIONAL) PROCEDURE CODES.           *  ELKIPGPC
00033 ****************************************************************  ELKIPGPC
00034 *                      MAINTENANCE HISTORY                     *  ELKIPGPC
00035 *                                                              *  ELKIPGPC
00036 *  MOD     DATE      BY  DRPT              ACTION              *  ELKIPGPC
00037 * ----- ----------- --- ----- ---------------------------------*  ELKIPGPC
00038 * 01.00 09-OCT-1992 BAK       CREATED                          *  ELKIPGPC
00039 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELKIPGPC
00040 *                                                                *ELKIPGPC
00041 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGPC
00042 *                                                                *ELKIPGPC
00043 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST.                          *ELKIPGPC
00044 *                                                                *ELKIPGPC
00045 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGPC
00046 *                             CICSCB3 COMPILER FIX             *  ELKIPGPC
00047 ****************************************************************  ELKIPGPC
00048                                                                   ELKIPGPC
00049  DATA DIVISION.                                                   ELKIPGPC
00050                                                                   ELKIPGPC
00051  WORKING-STORAGE SECTION.                                         ELKIPGPC
00052                                                                   ELKIPGPC
00053  01  WS-BEGIN                       PIC X(32)   VALUE             ELKIPGPC
00054      '**WORKING STORAGE FOR ELKIPGPC**'.                          ELKIPGPC
00055                                                                   ELKIPGPC
00056  01  WS-COUNTS.                                                   ELKIPGPC
00057      05  WS-COUNT-INST           PIC S9(04)  COMP  VALUE ZERO.    ELKIPGPC
00058      05  WS-COUNT-PROF           PIC S9(04)  COMP  VALUE ZERO.    ELKIPGPC
00059      05  WS-COUNT-OV             PIC S9(04)  COMP  VALUE ZERO.    ELKIPGPC
00060                                                                   ELKIPGPC
00061  01  WS-ELIGIBLE-FACTORS.                                         ELKIPGPC
00062      05  WS-ELIG-INST            COMP-1 VALUE ZERO.               ELKIPGPC
00063      05  WS-ELIG-PROF            COMP-1 VALUE ZERO.               ELKIPGPC
00064      05  WS-ELIG-OV              COMP-1 VALUE ZERO.               ELKIPGPC
00065                                                                   ELKIPGPC
00066  01  WS-INCLUDED-FACTORS.                                         ELKIPGPC
00067      05  WS-INCL-INST            COMP-1 VALUE ZERO.               ELKIPGPC
00068      05  WS-INCL-PROF            COMP-1 VALUE ZERO.               ELKIPGPC
00069      05  WS-INCL-OV              COMP-1 VALUE ZERO.               ELKIPGPC
00070                                                                   ELKIPGPC
00071  01  WS-RETURN-CODE                 PIC S9(04) COMP  VALUE ZERO.  ELKIPGPC
00072      88  WS-SUCCESSFUL-CALL              VALUE ZERO.              ELKIPGPC
00073      88  WS-INVALID-PARM                 VALUE +8.                ELKIPGPC
00074      88  WS-MISSING-PARM                 VALUE +12.               ELKIPGPC
00075      88  WS-INTERNAL-ERROR               VALUE +16.               ELKIPGPC
00076                                                                   ELKIPGPC
00077  01  WS-PROCEDURE-WORK.                                           ELKIPGPC
00078      05  WS-PROC-1                  PIC X(04).                    ELKIPGPC
00079      05  WS-PROC-2                  PIC X(01).                    ELKIPGPC
00080      05  WS-PROC-3                  PIC X(01).                    ELKIPGPC
00081                                                                   ELKIPGPC
00082  01  WS-GXA-MAX-INDEX             INDEX.                          ELKIPGPC
00083                                                                   ELKIPGPC
00084                                                                   ELKIPGPC
00085      COPY ELSCVG1C.                                               ELKIPGPC
00086                                                                   ELKIPGPC
00087                                                                   ELKIPGPC
00088  01  WS-END                         PIC X(32)   VALUE             ELKIPGPC
00089      '**END WORKING STORAGE-ELKIPGPC**'.                          ELKIPGPC
00090 /                                                                 ELKIPGPC
00091  LINKAGE SECTION.                                                 ELKIPGPC
00092                                                                   ELKIPGPC
00093  01  IPGP-RECORD.                                                 ELKIPGPC
00094      COPY GCTIPGPC.                                               ELKIPGPC
00095                                                                   ELKIPGPC
00096      COPY ELSCFDBC.                                               ELKIPGPC
00097                                                                   ELKIPGPC
00098 /***********************************************************      ELKIPGPC
00099 *                                                          *      ELKIPGPC
00100 *    CREATE IPGP CONFIDENCE FACTORS TABLE                  *      ELKIPGPC
00101 *                                                          *      ELKIPGPC
00102 ************************************************************      ELKIPGPC
00103                                                                   ELKIPGPC
00104  PROCEDURE DIVISION USING IPGP-RECORD                             ELKIPGPC
00105                             CFDB-CNFDNC-FCTR-DATA-BLCK.           ELKIPGPC
00106                                                                   ELKIPGPC
00107  0000-COMPUTE-OVERALL-CONF-FACT.                                  ELKIPGPC
00108      IF ADDRESS OF IPGP-RECORD = NULL OR                          ELKIPGPC
00109         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIPGPC
00110         SET WS-MISSING-PARM TO TRUE                               ELKIPGPC
00111      ELSE                                                         ELKIPGPC
00112         PERFORM 0100-INITIALIZE                                   ELKIPGPC
00113         PERFORM 1000-PROCESS-IPGP-TABULAR.                        ELKIPGPC
00114      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGPC
00115      GOBACK.                                                      ELKIPGPC
00116                                                                   ELKIPGPC
00117 ************************************************************      ELKIPGPC
00118 *                                                          *      ELKIPGPC
00119 *        INITIALIZE                                        *      ELKIPGPC
00120 *                                                          *      ELKIPGPC
00121 ************************************************************      ELKIPGPC
00122                                                                   ELKIPGPC
00123  0100-INITIALIZE.                                                 ELKIPGPC
00124                                                                   ELKIPGPC
00125      INITIALIZE WS-COUNTS.                                        ELKIPGPC
00126      INITIALIZE WS-ELIGIBLE-FACTORS.                              ELKIPGPC
00127      INITIALIZE WS-INCLUDED-FACTORS.                              ELKIPGPC
00128      MOVE ZEROS TO WS-RETURN-CODE.                                ELKIPGPC
00129                                                                   ELKIPGPC
00130 ************************************************************      ELKIPGPC
00131 *                                                          *      ELKIPGPC
00132 *    PROCESS IPGP TABULAR                                  *      ELKIPGPC
00133 *                                                          *      ELKIPGPC
00134 ************************************************************      ELKIPGPC
00135                                                                   ELKIPGPC
00136  1000-PROCESS-IPGP-TABULAR.                                       ELKIPGPC
00137                                                                   ELKIPGPC
00138      MOVE CVG1-NBR-CPT-PRCDR-CDS TO WS-ELIG-PROF.                 ELKIPGPC
00139      MOVE CVG1-NBR-HCPCS-PRCDR-CDS TO WS-ELIG-INST.               ELKIPGPC
00140      COMPUTE WS-ELIG-OV = WS-ELIG-PROF + WS-ELIG-INST.            ELKIPGPC
00141      SET GXA-INDEX TO GXA-ENTRY-COUNT.                            ELKIPGPC
00142      SET WS-GXA-MAX-INDEX TO GXA-INDEX.                           ELKIPGPC
00143      PERFORM 1100-READ-IPGP-TABULAR                               ELKIPGPC
00144               VARYING GXA-INDEX                                   ELKIPGPC
00145                 FROM 1 BY 1 UNTIL                                 ELKIPGPC
00146                   GXA-INDEX > WS-GXA-MAX-INDEX OR                 ELKIPGPC
00147                     GXA-PROCEDURE-ARGUMENT (GXA-INDEX) =          ELKIPGPC
00148                                HIGH-VALUES.                       ELKIPGPC
00149                                                                   ELKIPGPC
00150      PERFORM 1200-COMPUTE-IPGP-CONF-FACT.                         ELKIPGPC
00151 ************************************************************      ELKIPGPC
00152 *                                                          *      ELKIPGPC
00153 *    READ IPGP TABULARS DETERMINE IF PROFESSIONAL OR INSTIT*      ELKIPGPC
00154 *                                                          *      ELKIPGPC
00155 ************************************************************      ELKIPGPC
00156                                                                   ELKIPGPC
00157  1100-READ-IPGP-TABULAR.                                          ELKIPGPC
00158                                                                   ELKIPGPC
00159      ADD 1 TO WS-COUNT-OV.                                        ELKIPGPC
00160      MOVE GXA-PROCEDURE-ARGUMENT (GXA-INDEX) TO                   ELKIPGPC
00161             WS-PROCEDURE-WORK.                                    ELKIPGPC
00162      IF WS-PROC-2 = SPACES                                        ELKIPGPC
00163         ADD 1 TO WS-COUNT-INST                                    ELKIPGPC
00164      ELSE                                                         ELKIPGPC
00165         ADD 1 TO WS-COUNT-PROF.                                   ELKIPGPC
00166                                                                   ELKIPGPC
00167 ************************************************************      ELKIPGPC
00168 *                                                                 ELKIPGPC
00169 *    COMPUTE IPGP CONFIDENCE FACTORS.                             ELKIPGPC
00170 *                                                                 ELKIPGPC
00171 ************************************************************      ELKIPGPC
00172                                                                   ELKIPGPC
00173  1200-COMPUTE-IPGP-CONF-FACT.                                     ELKIPGPC
00174                                                                   ELKIPGPC
00175      IF GXA-ID-ARGUMENT-INCLUDED                                  ELKIPGPC
00176         MOVE WS-COUNT-OV TO WS-INCL-OV                            ELKIPGPC
00177         MOVE WS-COUNT-INST TO WS-INCL-INST                        ELKIPGPC
00178         MOVE WS-COUNT-PROF TO WS-INCL-PROF                        ELKIPGPC
00179         PERFORM 1300-COMPUTE-CONFIDENCE-FACT                      ELKIPGPC
00180      ELSE                                                         ELKIPGPC
00181         IF GXA-ID-ARGUMENT-EXCLUDED                               ELKIPGPC
00182            COMPUTE WS-INCL-OV =                                   ELKIPGPC
00183                 WS-ELIG-OV -  WS-COUNT-OV                         ELKIPGPC
00184            COMPUTE WS-INCL-INST =                                 ELKIPGPC
00185                 WS-ELIG-INST -  WS-COUNT-INST                     ELKIPGPC
00186            COMPUTE WS-INCL-PROF =                                 ELKIPGPC
00187                 WS-ELIG-PROF - WS-COUNT-PROF                      ELKIPGPC
00188            PERFORM 1300-COMPUTE-CONFIDENCE-FACT                   ELKIPGPC
00189         ELSE                                                      ELKIPGPC
00190            SET WS-MISSING-PARM TO TRUE.                           ELKIPGPC
00191                                                                   ELKIPGPC
00192 ************************************************************      ELKIPGPC
00193 *                                                                 ELKIPGPC
00194 *    COMPUTE CONFIDENCE FACTORS                                   ELKIPGPC
00195 *                                                                 ELKIPGPC
00196 ************************************************************      ELKIPGPC
00197                                                                   ELKIPGPC
00198  1300-COMPUTE-CONFIDENCE-FACT.                                    ELKIPGPC
00199                                                                   ELKIPGPC
00200      COMPUTE CFDB-CF-IPGP-INSTTNL =                               ELKIPGPC
00201         ((WS-INCL-INST / WS-ELIG-INST) * 2 ) - 1.                 ELKIPGPC
00202                                                                   ELKIPGPC
00203      COMPUTE CFDB-CF-IPGP-PRFSNL =                                ELKIPGPC
00204         ((WS-INCL-PROF / WS-ELIG-PROF) * 2 ) - 1.                 ELKIPGPC
00205                                                                   ELKIPGPC
00206      COMPUTE CFDB-CF-IPGP-OV =                                    ELKIPGPC
00207         ((WS-INCL-OV / WS-ELIG-OV) * 2 ) - 1.                     ELKIPGPC
00208                                                                   ELKIPGPC
