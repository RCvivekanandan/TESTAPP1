00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIDGDC
00003  PROGRAM-ID.              ELKIDGDC                                   LV004
00004                                                                   ELKIDGDC
00005  AUTHOR.                  BARBARA KEIB                            ELKIDGDC
00006                                                                   ELKIDGDC
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIDGDC
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIDGDC
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIDGDC
00010                        233 N. MICHIGAN AVE                        ELKIDGDC
00011                        CHICAGO, ILLINOIS 60601                    ELKIDGDC
00012                                                                   ELKIDGDC
00013                                                                   ELKIDGDC
00014  DATE-WRITTEN.         09-OCT-1992.                               ELKIDGDC
00015                                                                   ELKIDGDC
00016  ENVIRONMENT DIVISION.                                            ELKIDGDC
00017  CONFIGURATION SECTION.                                           ELKIDGDC
00018  SOURCE-COMPUTER. IBM-3090.                                       ELKIDGDC
00019  OBJECT-COMPUTER. IBM-3090.                                       ELKIDGDC
00020                                                                   ELKIDGDC
00021 ****************************************************************  ELKIDGDC
00022 *AKK 12/06/05 REGEN FOR TEST                                   *  ELKIDGDC
00023 *  ELKIDGDC :  THIS PROGRAM COMPUTES THE OVERALL CONFIDENCE    *  ELKIDGDC
00024 *              FACTORS FOR A SINGLE #IDGD TABULAR RECORD.      *  ELKIDGDC
00025 *                                                              *  ELKIDGDC
00026 *                                                              *  ELKIDGDC
00027 *              THIS PROGRAM COMPUTES THE OVERALL CONFIDENCE    *  ELKIDGDC
00028 *              FACTOR FOR A SINGLE #IDGD TABULAR.  THE VALUE   *  ELKIDGDC
00029 *              IS BASED ON AN EXTERNALLY DETERMINED NUMBER OF  *  ELKIDGDC
00030 *              POSSIBLE DIAGNOSIS CODES.                       *  ELKIDGDC
00031 *                                                              *  ELKIDGDC
00032 ****************************************************************  ELKIDGDC
00033 *                      MAINTENANCE HISTORY                     *  ELKIDGDC
00034 *                                                              *  ELKIDGDC
00035 *                                                              *  ELKIDGDC
00036 *  MOD     DATE      BY  DRPT              ACTION              *  ELKIDGDC
00037 * ----- ----------- --- ----- ---------------------------------*  ELKIDGDC
00038 * ----- ----------- --- ----- ---------------------------------*  ELKIDGDC
00039 * 01.00 09-OCT-1992 BAK       CREATED                          *  ELKIDGDC
00040 *                                                                *ELKIDGDC
00041 * 01.06 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX         *ELKIDGDC
00042 *                             ASM RECOMPILES                     *ELKIDGDC
00043 *                                                                *ELKIDGDC
00044 * 02.00 07-MAY-2003 AKK       REGEN'D FOR EXPANSION OF           *ELKIDGDC
00045 *                             PROC/DIAG CODES.                   *ELKIDGDC
00046 *                                                                *ELKIDGDC
00047 * 02.01 24-JUN-2003 AKK       CHANGED TO USE 6 CHAR DIAG CODE    *ELKIDGDC
00048 *                             SO AS NOT MAKE TOO MANY CHANGES TO *ELKIDGDC
00049 *                             PMCI CALCULATIONS.                 *ELKIDGDC
00050 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIDGDC
00051 *                                                                *ELKIDGDC
00052 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELKIDGDC
00053 *                                                                *ELKIDGDC
00054 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    * *ELKIDGDC
00055 *                             CICSCB3 COMPILER FIX             * *ELKIDGDC
00056 *                                                                *ELKIDGDC
00057 ****************************************************************  ELKIDGDC
00058                                                                   ELKIDGDC
00059  DATA DIVISION.                                                   ELKIDGDC
00060                                                                   ELKIDGDC
00061  WORKING-STORAGE SECTION.                                         ELKIDGDC
00062                                                                   ELKIDGDC
00063  01  WS-BEGIN                       PIC X(32)   VALUE             ELKIDGDC
00064      '**WORKING STORAGE FOR ELKIDGDC**'.                          ELKIDGDC
00065                                                                   ELKIDGDC
00066  01  WS-HOLD-DIAGNOSIS-CODE.                                      ELKIDGDC
00067      05 WS-HOLD-CODE                PIC X(06).                    ELKIDGDC
00068      05 WS-FILLER-HOLD              PIC X(04).                    ELKIDGDC
00069                                                                   ELKIDGDC
00070  01  WS-COUNTS-FACTORS.                                           ELKIDGDC
00071      05  WS-COUNT-OV             PIC S9(04)  COMP  VALUE ZERO.    ELKIDGDC
00072      05  WS-INCL-OV              COMP-1 VALUE ZERO.               ELKIDGDC
00073      05  WS-ELIG-OV              COMP-1 VALUE ZERO.               ELKIDGDC
00074                                                                   ELKIDGDC
00075  01  WS-RETURN-CODE                 PIC S9(04) COMP  VALUE ZERO.  ELKIDGDC
00076      88  WS-SUCCESSFUL-CALL              VALUE ZERO.              ELKIDGDC
00077      88  WS-INVALID-PARM                 VALUE +8.                ELKIDGDC
00078      88  WS-MISSING-PARM                 VALUE +12.               ELKIDGDC
00079      88  WS-INTERNAL-ERROR               VALUE +16.               ELKIDGDC
00080                                                                   ELKIDGDC
00081  01  WS-GX9-MAX-INDEX             INDEX.                          ELKIDGDC
00082                                                                   ELKIDGDC
00083                                                                   ELKIDGDC
00084      COPY ELSCVG1C.                                               ELKIDGDC
00085                                                                   ELKIDGDC
00086                                                                   ELKIDGDC
00087  01  WS-END                         PIC X(32)   VALUE             ELKIDGDC
00088      '**END WORKING STORAGE-ELKIDGDC**'.                          ELKIDGDC
00089 /                                                                 ELKIDGDC
00090  LINKAGE SECTION.                                                 ELKIDGDC
00091                                                                   ELKIDGDC
00092  01  IDGD-RECORD.                                                 ELKIDGDC
00093      COPY GCTIDGDC.                                               ELKIDGDC
00094                                                                   ELKIDGDC
00095      COPY ELSCFDBC.                                               ELKIDGDC
00096                                                                   ELKIDGDC
00097 /***********************************************************      ELKIDGDC
00098 *                                                          *      ELKIDGDC
00099 *    CREATE IDGD CONFIDENCE FACTORS TABLE                  *      ELKIDGDC
00100 *                                                          *      ELKIDGDC
00101 ************************************************************      ELKIDGDC
00102                                                                   ELKIDGDC
00103  PROCEDURE DIVISION USING IDGD-RECORD                             ELKIDGDC
00104                             CFDB-CNFDNC-FCTR-DATA-BLCK.           ELKIDGDC
00105                                                                   ELKIDGDC
00106  0000-COMPUTE-OVERALL-CONF-FACT.                                  ELKIDGDC
00107      IF ADDRESS OF IDGD-RECORD = NULL OR                          ELKIDGDC
00108         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIDGDC
00109         SET WS-MISSING-PARM TO TRUE                               ELKIDGDC
00110      ELSE                                                         ELKIDGDC
00111         PERFORM 0100-INITIALIZE                                   ELKIDGDC
00112         PERFORM 1000-PROCESS-IDGD-TABULAR.                        ELKIDGDC
00113      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIDGDC
00114      GOBACK.                                                      ELKIDGDC
00115                                                                   ELKIDGDC
00116 ************************************************************      ELKIDGDC
00117 *                                                          *      ELKIDGDC
00118 *        INITIALIZE                                        *      ELKIDGDC
00119 *                                                          *      ELKIDGDC
00120 ************************************************************      ELKIDGDC
00121                                                                   ELKIDGDC
00122  0100-INITIALIZE.                                                 ELKIDGDC
00123                                                                   ELKIDGDC
00124      INITIALIZE WS-COUNTS-FACTORS.                                ELKIDGDC
00125      MOVE ZEROS TO WS-RETURN-CODE.                                ELKIDGDC
00126                                                                   ELKIDGDC
00127 ************************************************************      ELKIDGDC
00128 *                                                          *      ELKIDGDC
00129 *    PROCESS IDGD TABULAR                                  *      ELKIDGDC
00130 *                                                          *      ELKIDGDC
00131 ************************************************************      ELKIDGDC
00132                                                                   ELKIDGDC
00133  1000-PROCESS-IDGD-TABULAR.                                       ELKIDGDC
00134                                                                   ELKIDGDC
00135      MOVE CVG1-NBR-ICD9-DX-CDS TO WS-ELIG-OV.                     ELKIDGDC
00136      SET GX9-INDEX TO GX9-ENTRY-COUNT.                            ELKIDGDC
00137                                                                   ELKIDGDC
00138      MOVE GX9-DIAGNOSIS-ARGUMENT (GX9-INDEX) TO                   ELKIDGDC
00139           WS-HOLD-DIAGNOSIS-CODE.                                 ELKIDGDC
00140      IF WS-HOLD-CODE = HIGH-VALUES                                ELKIDGDC
00141 *    IF GX9-DIAGNOSIS-ARGUMENT (GX9-INDEX) = HIGH-VALUES          ELKIDGDC
00142          COMPUTE WS-COUNT-OV = GX9-ENTRY-COUNT - 1                ELKIDGDC
00143      ELSE                                                         ELKIDGDC
00144          MOVE GX9-ENTRY-COUNT TO WS-COUNT-OV.                     ELKIDGDC
00145                                                                   ELKIDGDC
00146      IF GX9-ID-ARGUMENT-INCLUDED                                  ELKIDGDC
00147         MOVE WS-COUNT-OV TO WS-INCL-OV                            ELKIDGDC
00148         PERFORM 1100-COMPUTE-CONFIDENCE-FACT                      ELKIDGDC
00149      ELSE                                                         ELKIDGDC
00150         IF GX9-ID-ARGUMENT-EXCLUDED                               ELKIDGDC
00151            COMPUTE WS-INCL-OV =                                   ELKIDGDC
00152                 WS-ELIG-OV -  WS-COUNT-OV                         ELKIDGDC
00153            PERFORM 1100-COMPUTE-CONFIDENCE-FACT                   ELKIDGDC
00154         ELSE                                                      ELKIDGDC
00155            SET WS-MISSING-PARM TO TRUE.                           ELKIDGDC
00156                                                                   ELKIDGDC
00157 ************************************************************      ELKIDGDC
00158 *                                                                 ELKIDGDC
00159 *    COMPUTE CONFIDENCE FACTORS                                   ELKIDGDC
00160 *                                                                 ELKIDGDC
00161 ************************************************************      ELKIDGDC
00162                                                                   ELKIDGDC
00163  1100-COMPUTE-CONFIDENCE-FACT.                                    ELKIDGDC
00164                                                                   ELKIDGDC
00165      COMPUTE CFDB-CF-IDGD-OV =                                    ELKIDGDC
00166         ((WS-INCL-OV / WS-ELIG-OV) * 2 ) - 1.                     ELKIDGDC
00167                                                                   ELKIDGDC
