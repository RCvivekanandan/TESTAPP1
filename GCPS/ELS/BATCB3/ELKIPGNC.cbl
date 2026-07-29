00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELKIPGNC
00003  PROGRAM-ID.              ELKIPGNC                                   LV004
00004                                                                   ELKIPGNC
00005  AUTHOR.                  BARBARA KEIB                            ELKIPGNC
00006                                                                   ELKIPGNC
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELKIPGNC
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELKIPGNC
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELKIPGNC
00010                        233 N. MICHIGAN AVE                        ELKIPGNC
00011                        CHICAGO, ILLINOIS 60601                    ELKIPGNC
00012                                                                   ELKIPGNC
00013  DATE-WRITTEN.         09-OCT-1992.                               ELKIPGNC
00014                                                                   ELKIPGNC
00015  ENVIRONMENT DIVISION.                                            ELKIPGNC
00016  CONFIGURATION SECTION.                                           ELKIPGNC
00017  SOURCE-COMPUTER. IBM-3090.                                       ELKIPGNC
00018  OBJECT-COMPUTER. IBM-3090.                                       ELKIPGNC
00019                                                                   ELKIPGNC
00020 ****************************************************************  ELKIPGNC
00021 *                                                              *  ELKIPGNC
00022 *  ELKIPGNC :  THIS PROGRAM COMPUTES THE OVERALL CONFIDENCE    *  ELKIPGNC
00023 *              FACTORS FOR A SINGLE #IPGN TABULAR RECORD.      *  ELKIPGNC
00024 *                                                              *  ELKIPGNC
00025 *              THIS PROGRAM COMPUTES THE OVERALL CONFIDENCE    *  ELKIPGNC
00026 *              FACTOR FOR A SINGLE #IPGN TABULAR.  THE VALUE   *  ELKIPGNC
00027 *              IS BASED ON AN EXTERNALLY DETERMINED NUMBER OF  *  ELKIPGNC
00028 *              POSSIBLE PROVIDER NUMBERS.                      *  ELKIPGNC
00029 ****************************************************************  ELKIPGNC
00030 *                      MAINTENANCE HISTORY                     *  ELKIPGNC
00031 *AKK 12/06/05 REGEN FOR TEST                                   *  ELKIPGNC
00032 *                                                              *  ELKIPGNC
00033 *  MOD     DATE      BY  DRPT              ACTION              *  ELKIPGNC
00034 * ----- ----------- --- ----- ---------------------------------*  ELKIPGNC
00035 * 01.00 09-OCT-1992 BAK       CREATED                          *  ELKIPGNC
00036 *                                                                 ELKIPGNC
00037 * 01.01 01-APR-2003 AKK       REGEN'D FOR ASM REGEN               ELKIPGNC
00038 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELKIPGNC
00039 *                                                                *ELKIPGNC
00040 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELKIPGNC
00041 *                                                                *ELKIPGNC
00042 * 02.02 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *  ELKIPGNC
00043 *                             CICSCB3 COMPILER FIX             *  ELKIPGNC
00044 ****************************************************************  ELKIPGNC
00045                                                                   ELKIPGNC
00046  DATA DIVISION.                                                   ELKIPGNC
00047                                                                   ELKIPGNC
00048  WORKING-STORAGE SECTION.                                         ELKIPGNC
00049                                                                   ELKIPGNC
00050  01  WS-BEGIN                       PIC X(32)   VALUE             ELKIPGNC
00051      '**WORKING STORAGE FOR ELKIPGNC**'.                          ELKIPGNC
00052                                                                   ELKIPGNC
00053  01  WS-COUNTS-FACTORS.                                           ELKIPGNC
00054      05  WS-COUNT-OV             PIC S9(04)  COMP  VALUE ZERO.    ELKIPGNC
00055      05  WS-INCL-OV              COMP-1 VALUE ZERO.               ELKIPGNC
00056      05  WS-ELIG-OV              COMP-1 VALUE ZERO.               ELKIPGNC
00057                                                                   ELKIPGNC
00058  01  WS-RETURN-CODE                 PIC S9(04) COMP  VALUE ZERO.  ELKIPGNC
00059      88  WS-SUCCESSFUL-CALL              VALUE ZERO.              ELKIPGNC
00060      88  WS-INVALID-PARM                 VALUE +8.                ELKIPGNC
00061      88  WS-MISSING-PARM                 VALUE +12.               ELKIPGNC
00062      88  WS-INTERNAL-ERROR               VALUE +16.               ELKIPGNC
00063                                                                   ELKIPGNC
00064  01  WS-GX2-MAX-INDEX             INDEX.                          ELKIPGNC
00065                                                                   ELKIPGNC
00066                                                                   ELKIPGNC
00067      COPY ELSCVG1C.                                               ELKIPGNC
00068                                                                   ELKIPGNC
00069                                                                   ELKIPGNC
00070  01  WS-END                         PIC X(32)   VALUE             ELKIPGNC
00071      '**END WORKING STORAGE-ELKIPGNC**'.                          ELKIPGNC
00072 /                                                                 ELKIPGNC
00073  LINKAGE SECTION.                                                 ELKIPGNC
00074                                                                   ELKIPGNC
00075  01  IPGN-RECORD.                                                 ELKIPGNC
00076      COPY GCTIPGNC.                                               ELKIPGNC
00077                                                                   ELKIPGNC
00078      COPY ELSCFDBC.                                               ELKIPGNC
00079                                                                   ELKIPGNC
00080 /***********************************************************      ELKIPGNC
00081 *                                                          *      ELKIPGNC
00082 *    CREATE IPGN CONFIDENCE FACTORS TABLE                  *      ELKIPGNC
00083 *                                                          *      ELKIPGNC
00084 ************************************************************      ELKIPGNC
00085                                                                   ELKIPGNC
00086  PROCEDURE DIVISION USING IPGN-RECORD                             ELKIPGNC
00087                             CFDB-CNFDNC-FCTR-DATA-BLCK.           ELKIPGNC
00088                                                                   ELKIPGNC
00089  0000-COMPUTE-OVERALL-CONF-FACT.                                  ELKIPGNC
00090      IF ADDRESS OF IPGN-RECORD = NULL OR                          ELKIPGNC
00091         ADDRESS OF CFDB-CNFDNC-FCTR-DATA-BLCK = NULL              ELKIPGNC
00092         SET WS-MISSING-PARM TO TRUE                               ELKIPGNC
00093      ELSE                                                         ELKIPGNC
00094         PERFORM 0100-INITIALIZE                                   ELKIPGNC
00095         PERFORM 1000-PROCESS-IPGN-TABULAR.                        ELKIPGNC
00096      MOVE WS-RETURN-CODE TO RETURN-CODE.                          ELKIPGNC
00097      GOBACK.                                                      ELKIPGNC
00098                                                                   ELKIPGNC
00099 ************************************************************      ELKIPGNC
00100 *                                                          *      ELKIPGNC
00101 *        INITIALIZE                                        *      ELKIPGNC
00102 *                                                          *      ELKIPGNC
00103 ************************************************************      ELKIPGNC
00104                                                                   ELKIPGNC
00105  0100-INITIALIZE.                                                 ELKIPGNC
00106                                                                   ELKIPGNC
00107      INITIALIZE WS-COUNTS-FACTORS.                                ELKIPGNC
00108      MOVE ZEROS TO WS-RETURN-CODE.                                ELKIPGNC
00109                                                                   ELKIPGNC
00110 ************************************************************      ELKIPGNC
00111 *                                                          *      ELKIPGNC
00112 *    PROCESS IPGN TABULAR                                  *      ELKIPGNC
00113 *                                                          *      ELKIPGNC
00114 ************************************************************      ELKIPGNC
00115                                                                   ELKIPGNC
00116  1000-PROCESS-IPGN-TABULAR.                                       ELKIPGNC
00117                                                                   ELKIPGNC
00118      MOVE CVG1-NBR-PRVDRS TO WS-ELIG-OV.                          ELKIPGNC
00119      SET GX2-INDEX TO GX2-ENTRY-COUNT.                            ELKIPGNC
00120                                                                   ELKIPGNC
00121      IF GX2-PROVIDER-NO-ARGUMENT (GX2-INDEX) = HIGH-VALUES        ELKIPGNC
00122          COMPUTE WS-COUNT-OV = GX2-ENTRY-COUNT - 1                ELKIPGNC
00123      ELSE                                                         ELKIPGNC
00124          MOVE GX2-ENTRY-COUNT TO WS-COUNT-OV.                     ELKIPGNC
00125                                                                   ELKIPGNC
00126      IF GX2-ID-ARGUMENT-INCLUDED                                  ELKIPGNC
00127         MOVE WS-COUNT-OV TO WS-INCL-OV                            ELKIPGNC
00128         PERFORM 1100-COMPUTE-CONFIDENCE-FACT                      ELKIPGNC
00129      ELSE                                                         ELKIPGNC
00130         IF GX2-ID-ARGUMENT-EXCLUDED                               ELKIPGNC
00131            COMPUTE WS-INCL-OV =                                   ELKIPGNC
00132                 WS-ELIG-OV -  WS-COUNT-OV                         ELKIPGNC
00133            PERFORM 1100-COMPUTE-CONFIDENCE-FACT                   ELKIPGNC
00134         ELSE                                                      ELKIPGNC
00135            SET WS-MISSING-PARM TO TRUE.                           ELKIPGNC
00136                                                                   ELKIPGNC
00137 ************************************************************      ELKIPGNC
00138 *                                                                 ELKIPGNC
00139 *    COMPUTE CONFIDENCE FACTORS                                   ELKIPGNC
00140 *                                                                 ELKIPGNC
00141 ************************************************************      ELKIPGNC
00142                                                                   ELKIPGNC
00143  1100-COMPUTE-CONFIDENCE-FACT.                                    ELKIPGNC
00144                                                                   ELKIPGNC
00145      COMPUTE CFDB-CF-IPGN-OV =                                    ELKIPGNC
00146         ((WS-INCL-OV / WS-ELIG-OV) * 2 ) - 1.                     ELKIPGNC
00147                                                                   ELKIPGNC
