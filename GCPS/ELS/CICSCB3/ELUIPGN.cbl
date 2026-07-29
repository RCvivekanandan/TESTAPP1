00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUIPGN 
00003  PROGRAM-ID.           ELUIPGN.                                      LV001
00004                                                                   ELUIPGN 
00005  AUTHOR.               R. BARILEAU.                               ELUIPGN 
00006                        R. LUKETICH (REWRITTEN 24-OCT-1989).       ELUIPGN 
00007                                                                   ELUIPGN 
00008  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELUIPGN 
00009                        A MUTUAL LEGAL RESERVE COMPANY             ELUIPGN 
00010                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELUIPGN 
00011                        233 N. MICHIGAN AVE                        ELUIPGN 
00012                        CHICAGO, ILLINOIS 60601                    ELUIPGN 
00013                                                                   ELUIPGN 
00014  DATE-WRITTEN.         01-SEP-1988.                               ELUIPGN 
00015                        24-OCT-1989 REWRITTEN.                     ELUIPGN 
00016                                                                   ELUIPGN 
00017  ENVIRONMENT DIVISION.                                            ELUIPGN 
00018                                                                   ELUIPGN 
00019  CONFIGURATION SECTION.                                           ELUIPGN 
00020                                                                   ELUIPGN 
00021  SOURCE-COMPUTER.      IBM-3090.                                  ELUIPGN 
00022  OBJECT-COMPUTER.      IBM-3090.                                  ELUIPGN 
00023                                                                   ELUIPGN 
00024 ******************************************************************ELUIPGN 
00025 *                                                                *ELUIPGN 
00026 *  ELUIPGN :   THIS MODULE IS BEING CALLLED BY 'ELUOVCFD'.       *ELUIPGN 
00027 *              THE PURPOSE IS DETERMINE THE PARTIAL              *ELUIPGN 
00028 *              CONFIDENCE FACTOR FOR THE OVERALL ATTRIBUTE.      *ELUIPGN 
00029 *                                                                *ELUIPGN 
00030 ******************************************************************ELUIPGN 
00031 *                      MAINTENANCE HISTORY                       *ELUIPGN 
00032 *                                                                *ELUIPGN 
00033 *  MOD     DATE      BY                    ACTION                *ELUIPGN 
00034 * ----- ----------- --- -----------------------------------------*ELUIPGN 
00035 * 01.00 01-SEP-1988 REB CREATED                                  *ELUIPGN 
00036 * 02.00 24-OCT-1989 RJL REWRITTEN TO CONVERT FROM STRUCTURE(S)   *ELUIPGN 
00037 *                       CODE GENERATOR TO NATIVE COBOL CODE,     *ELUIPGN 
00038 *                       AND TO CORRECT CALCULATIONS OF           *ELUIPGN 
00039 *                       CONFIDENCE FACTORS.                      *ELUIPGN 
00040 *                                                                *ELUIPGN 
00041 ******************************************************************ELUIPGN 
00042 /                                                                 ELUIPGN 
00043  DATA DIVISION.                                                   ELUIPGN 
00044                                                                   ELUIPGN 
00045  WORKING-STORAGE SECTION.                                         ELUIPGN 
00046                                                                   ELUIPGN 
00047  01  WS-SUB1                     PICTURE S9(04)          COMP.    ELUIPGN 
00048  01  WS-CF-FALSE                 COMP-1                           ELUIPGN 
00049                                  VALUE -1.00E+00.                 ELUIPGN 
00050  01  WS-FP-NEG-ONE               COMP-1                           ELUIPGN 
00051                                  VALUE -1.00E+00.                 ELUIPGN 
00052  01  WS-FP-ONE                   COMP-1                           ELUIPGN 
00053                                  VALUE +1.00E+00.                 ELUIPGN 
00054  01  WS-FP-TWO                   COMP-1                           ELUIPGN 
00055                                  VALUE +2.00E+00.                 ELUIPGN 
00056  01  WS-MAX-IPGN-ENTRY-COUNT     COMP-1                           ELUIPGN 
00057                                  VALUE +3.95E+02.                 ELUIPGN 
00058  01  WS-IPGN-ENTRY-COUNT         COMP-1.                          ELUIPGN 
00059 /                                                                 ELUIPGN 
00060  LINKAGE SECTION.                                                 ELUIPGN 
00061  01  DFHCOMMAREA.                                                 ELUIPGN 
00062      COPY ELSCOMMC.                                               ELUIPGN 
00063 /                                                                 ELUIPGN 
00064      COPY ELSCIA2C.                                               ELUIPGN 
00065 /    COPYBOOK USED FOR OVERALL ACCUM IPGN TABLE                   ELUIPGN 
00066      COPY ELSIPGNC.                                               ELUIPGN 
00067 /    GCPS COPYBOOK USED FOR IBGN RECORD LAYOUT                    ELUIPGN 
00068  01  IPGN-RECORD.                                                 ELUIPGN 
00069      COPY GCTIPGNC.                                               ELUIPGN 
00070 /*****************************************************************ELUIPGN 
00071 *                                                                *ELUIPGN 
00072 *    DETERMINE IPGN CONFIDENCE FACTORS                           *ELUIPGN 
00073 *    PROCEDURE DIVISION                                          *ELUIPGN 
00074 *                                                                *ELUIPGN 
00075 ******************************************************************ELUIPGN 
00076                                                                   ELUIPGN 
00077  PROCEDURE DIVISION.                                              ELUIPGN 
00078                                                                   ELUIPGN 
00079  000-DET-IPGN-CONF-FACTORS.                                       ELUIPGN 
00080      PERFORM 001-INITIALIZE.                                      ELUIPGN 
00081      PERFORM 100-PROCESS.                                         ELUIPGN 
00082      GOBACK.                                                      ELUIPGN 
00083                                                                   ELUIPGN 
00084 ******************************************************************ELUIPGN 
00085 *                                                                *ELUIPGN 
00086 *    INITIALIZE                                                  *ELUIPGN 
00087 *                                                                *ELUIPGN 
00088 ******************************************************************ELUIPGN 
00089                                                                   ELUIPGN 
00090  001-INITIALIZE.                                                  ELUIPGN 
00091      PERFORM 990-ESTAB-ECI-STG-ENVIRON.                           ELUIPGN 
00092      PERFORM 907-ESTAB-ADDR-ELSIPGN.                              ELUIPGN 
00093                                                                   ELUIPGN 
00094 ******************************************************************ELUIPGN 
00095 *                                                                *ELUIPGN 
00096 *    PROCESS                                                     *ELUIPGN 
00097 *                                                                *ELUIPGN 
00098 ******************************************************************ELUIPGN 
00099                                                                   ELUIPGN 
00100  100-PROCESS.                                                     ELUIPGN 
00101      PERFORM 110-SCAN-ALL-IPGN-TABS                               ELUIPGN 
00102         VARYING WS-SUB1 FROM 1 BY 1                               ELUIPGN 
00103           UNTIL WS-SUB1 > IPGN-TBL-CNT.                           ELUIPGN 
00104                                                                   ELUIPGN 
00105 ******************************************************************ELUIPGN 
00106 *                                                                *ELUIPGN 
00107 *    SCAN ALL IPGN RECORDS IN ELSIPGN TABLE                      *ELUIPGN 
00108 *                                                                *ELUIPGN 
00109 ******************************************************************ELUIPGN 
00110                                                                   ELUIPGN 
00111  110-SCAN-ALL-IPGN-TABS.                                          ELUIPGN 
00112      IF IPGN-TABULAR-PTR (WS-SUB1) = NULL                         ELUIPGN 
00113      THEN                                                         ELUIPGN 
00114         CONTINUE                                                  ELUIPGN 
00115      ELSE                                                         ELUIPGN 
00116          PERFORM 120-CALC-IPGN-CONF-FACTORS.                      ELUIPGN 
00117                                                                   ELUIPGN 
00118 ******************************************************************ELUIPGN 
00119 *                                                                *ELUIPGN 
00120 *    CALCULATE THE CONFIDENCE FACTORS FOR THE CURRENT IPGN       *ELUIPGN 
00121 *    TABULAR RECORD                                              *ELUIPGN 
00122 *                                                                *ELUIPGN 
00123 ******************************************************************ELUIPGN 
00124                                                                   ELUIPGN 
00125  120-CALC-IPGN-CONF-FACTORS.                                      ELUIPGN 
00126      SET ADDRESS OF IPGN-RECORD TO IPGN-TABULAR-PTR (WS-SUB1).    ELUIPGN 
00127      IF GX2-ID-ARGUMENT-INCLUDED                                  ELUIPGN 
00128         MOVE WS-CF-FALSE TO IPGN-CF-OV (WS-SUB1)                  ELUIPGN 
00129      ELSE                                                         ELUIPGN 
00130         MOVE GX2-ENTRY-COUNT TO WS-IPGN-ENTRY-COUNT               ELUIPGN 
00131         COMPUTE IPGN-CF-OV (WS-SUB1) =                            ELUIPGN 
00132              WS-FP-ONE                                            ELUIPGN 
00133            - (  WS-FP-TWO                                         ELUIPGN 
00134               * (  WS-IPGN-ENTRY-COUNT                            ELUIPGN 
00135                  / WS-MAX-IPGN-ENTRY-COUNT) ).                    ELUIPGN 
00136                                                                   ELUIPGN 
00137 /*****************************************************************ELUIPGN 
00138 *                                                                *ELUIPGN 
00139 *    ESTABLISH ADDRESSABILITY OF IPGN TABLE                      *ELUIPGN 
00140 *                                                                *ELUIPGN 
00141 ******************************************************************ELUIPGN 
00142                                                                   ELUIPGN 
00143  907-ESTAB-ADDR-ELSIPGN.                                          ELUIPGN 
00144      SET CIA-ELSIPGN-DDN TO TRUE.                                 ELUIPGN 
00145      CALL 'ELUSETAD'                                              ELUIPGN 
00146         USING DFHCOMMAREA                                         ELUIPGN 
00147               ADDRESS OF IPGN-INTERNAL-TABS-TABLE.                ELUIPGN 
00148      IF CIA-RC-PTR-NULL                                           ELUIPGN 
00149      THEN                                                         ELUIPGN 
00150          PERFORM 991-SIGNAL-UNALLOC-AREA                          ELUIPGN 
00151      ELSE                                                         ELUIPGN 
00152         CONTINUE.                                                 ELUIPGN 
00153                                                                   ELUIPGN 
00154 /*****************************************************************ELUIPGN 
00155 *                                                                *ELUIPGN 
00156 *    ESTABLISH THE ENGLISH CONTRACT INQUIRY STORAGE              *ELUIPGN 
00157 *    MANAGEMENT ENVIRONMENT                                      *ELUIPGN 
00158 *                                                                *ELUIPGN 
00159 *  - CHECK VALIDITY OF COMMAREA, ABEND IF NOT VALID              *ELUIPGN 
00160 *  - ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA,          *ELUIPGN 
00161 *    ABEND IF THE POINTER IS NULL                                *ELUIPGN 
00162 *  - INITIALIZE THE STORAGE MANAGEMENT SYSTEM                    *ELUIPGN 
00163 *                                                                *ELUIPGN 
00164 ******************************************************************ELUIPGN 
00165                                                                   ELUIPGN 
00166  990-ESTAB-ECI-STG-ENVIRON.                                       ELUIPGN 
00167                                                                   ELUIPGN 
00168      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELUIPGN 
00169      THEN                                                         ELUIPGN 
00170         EXEC CICS ABEND ABCODE ('EL01') END-EXEC                  ELUIPGN 
00171      ELSE                                                         ELUIPGN 
00172         IF ECA-CIA-PTR = NULL                                     ELUIPGN 
00173         THEN                                                      ELUIPGN 
00174            EXEC CICS ABEND ABCODE ('EL02') END-EXEC               ELUIPGN 
00175         ELSE                                                      ELUIPGN 
00176            SET ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA           ELUIPGN 
00177             TO ECA-CIA-PTR                                        ELUIPGN 
00178            CALL 'ELUINISM'                                        ELUIPGN 
00179               USING DFHCOMMAREA                                   ELUIPGN 
00180                     ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.     ELUIPGN 
00181                                                                   ELUIPGN 
00182 /*****************************************************************ELUIPGN 
00183 *                                                                *ELUIPGN 
00184 *    SIGNAL UNALLOCATED AREA ERROR                               *ELUIPGN 
00185 *                                                                *ELUIPGN 
00186 ******************************************************************ELUIPGN 
00187                                                                   ELUIPGN 
00188  991-SIGNAL-UNALLOC-AREA.                                         ELUIPGN 
00189      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUIPGN 
00190      PERFORM 999-SIGNAL-ABEND.                                    ELUIPGN 
00191                                                                   ELUIPGN 
00192 ******************************************************************ELUIPGN 
00193 *                                                                *ELUIPGN 
00194 *    SIGNAL ABEND                                                *ELUIPGN 
00195 *                                                                *ELUIPGN 
00196 ******************************************************************ELUIPGN 
00197                                                                   ELUIPGN 
00198  999-SIGNAL-ABEND.                                                ELUIPGN 
00199      EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC.                ELUIPGN 
