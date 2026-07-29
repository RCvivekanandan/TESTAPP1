00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUIPGT 
00003  PROGRAM-ID.           ELUIPGT.                                      LV001
00004                                                                   ELUIPGT 
00005  AUTHOR.               RICK BARILEAU.                             ELUIPGT 
00006                                                                   ELUIPGT 
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELUIPGT 
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELUIPGT 
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELUIPGT 
00010                        233 N. MICHIGAN AVE                        ELUIPGT 
00011                        CHICAGO, ILLINOIS 60601                    ELUIPGT 
00012                                                                   ELUIPGT 
00013  DATE-WRITTEN.         06-SEP-1988.                               ELUIPGT 
00014                                                                   ELUIPGT 
00015  ENVIRONMENT DIVISION.                                            ELUIPGT 
00016                                                                   ELUIPGT 
00017  CONFIGURATION SECTION.                                           ELUIPGT 
00018                                                                   ELUIPGT 
00019  SOURCE-COMPUTER. IBM-3033.                                       ELUIPGT 
00020  OBJECT-COMPUTER. IBM-3033.                                       ELUIPGT 
00021                                                                   ELUIPGT 
00022 ******************************************************************ELUIPGT 
00023 *                                                                *ELUIPGT 
00024 *  ELUIPGT :   THIS MODULE IS BEING CALLLED BY 'ELUOVCFD'.       *ELUIPGT 
00025 *              THE PURPOSE IS DETERMINE THE PARTIAL              *ELUIPGT 
00026 *              CONFIDENCE FACTOR FOR THE OVERALL ATTRIBUTE.      *ELUIPGT 
00027 *                                                                *ELUIPGT 
00028 ******************************************************************ELUIPGT 
00029 *                      MAINTENANCE HISTORY                       *ELUIPGT 
00030 *                                                                *ELUIPGT 
00031 *  MOD     DATE      BY                    ACTION                *ELUIPGT 
00032 * ----- ----------- --- ---------------------------------------  *ELUIPGT 
00033 * 01.00 06-SEP-1988 REB CREATED                                  *ELUIPGT 
00034 *                                                                *ELUIPGT 
00035 *                                                                *ELUIPGT 
00036 ******************************************************************ELUIPGT 
00037                                                                   ELUIPGT 
00038  DATA DIVISION.                                                   ELUIPGT 
00039                                                                   ELUIPGT 
00040  WORKING-STORAGE SECTION.                                         ELUIPGT 
00041                                                                   ELUIPGT 
00042  77  WS-FP-NEG-ONE               COMP-1                           ELUIPGT 
00043                                  VALUE -1.00E+00.                 ELUIPGT 
00044  77  WS-FP-ONE                   COMP-1                           ELUIPGT 
00045                                  VALUE +1.00E+00.                 ELUIPGT 
00046  77  WS-SUB1                     PICTURE S9(04)          COMP.    ELUIPGT 
00047  77  WS-SUB2                     PICTURE S9(04)          COMP.    ELUIPGT 
00048  77  WS-CFT2-SUB                 PICTURE S9(04)          COMP.    ELUIPGT 
00049                                                                   ELUIPGT 
00050  01  WS-PROGRAM-SWITCHES.                                         ELUIPGT 
00051      02 WS-PROVIDER-TYPE                                          ELUIPGT 
00052                                  PICTURE  X(01).                  ELUIPGT 
00053         88 PROVIDER-TYPE-NOT-FOUND                                ELUIPGT 
00054                                  VALUE 'N'.                       ELUIPGT 
00055         88 PROVIDER-TYPE-FOUND   VALUE 'Y'.                       ELUIPGT 
00056                                                                   ELUIPGT 
00057  01  WS-CONF-FACTORS.                                             ELUIPGT 
00058      02 WS-CF-INST               COMP-1.                          ELUIPGT 
00059      02 WS-CF-PROF               COMP-1.                          ELUIPGT 
00060      02 WS-CF-PLAN               COMP-1.                          ELUIPGT 
00061      02 WS-CF-NON-PLAN           COMP-1.                          ELUIPGT 
00062      02 WS-CF-OV                 COMP-1.                          ELUIPGT 
00063                                                                   ELUIPGT 
00064 /    COPYBOOK USED FOR PROVIDER TYPE CONFIDENCE FACTORS TABLE     ELUIPGT 
00065  COPY ELSCFTB2.                                                   ELUIPGT 
00066 /                                                                 ELUIPGT 
00067  LINKAGE SECTION.                                                 ELUIPGT 
00068  01  DFHCOMMAREA.                                                 ELUIPGT 
00069      COPY ELSCOMMC.                                               ELUIPGT 
00070 /                                                                 ELUIPGT 
00071      COPY ELSCIA2C.                                               ELUIPGT 
00072 /    COPYBOOK USED FOR OVERALL ACCUM IPGT TABLE                   ELUIPGT 
00073      COPY ELSIPGTC.                                               ELUIPGT 
00074 /    GCPS COPYBOOK USED FOR IBGN RECORD LAYOUT                    ELUIPGT 
00075  01  IPGT-RECORD.                                                 ELUIPGT 
00076      COPY GCTIPGTC.                                               ELUIPGT 
00077 /*****************************************************************ELUIPGT 
00078 *                                                                *ELUIPGT 
00079 *    DETERMINE IPGT CONFIDENCE FACTORS                           *ELUIPGT 
00080 *    PROCEDURE DIVISION                                          *ELUIPGT 
00081 *                                                                *ELUIPGT 
00082 ******************************************************************ELUIPGT 
00083                                                                   ELUIPGT 
00084  PROCEDURE DIVISION.                                              ELUIPGT 
00085                                                                   ELUIPGT 
00086  000-DET-IPGT-CONF-FACTS.                                         ELUIPGT 
00087      PERFORM 001-INITIALIZE.                                      ELUIPGT 
00088      PERFORM 100-PROCESS.                                         ELUIPGT 
00089      GOBACK.                                                      ELUIPGT 
00090                                                                   ELUIPGT 
00091 ******************************************************************ELUIPGT 
00092 *                                                                *ELUIPGT 
00093 *    INITIALIZE                                                  *ELUIPGT 
00094 *                                                                *ELUIPGT 
00095 ******************************************************************ELUIPGT 
00096                                                                   ELUIPGT 
00097  001-INITIALIZE.                                                  ELUIPGT 
00098      PERFORM 990-ESTAB-ECI-STG-ENVIRON.                           ELUIPGT 
00099      PERFORM 907-ESTAB-ADDR-ELSIPGT.                              ELUIPGT 
00100                                                                   ELUIPGT 
00101 ******************************************************************ELUIPGT 
00102 *                                                                *ELUIPGT 
00103 *    PROCESS                                                     *ELUIPGT 
00104 *                                                                *ELUIPGT 
00105 ******************************************************************ELUIPGT 
00106                                                                   ELUIPGT 
00107  100-PROCESS.                                                     ELUIPGT 
00108      PERFORM 110-SCAN-ALL-IPGT-TABS                               ELUIPGT 
00109         VARYING WS-SUB1 FROM 1 BY 1                               ELUIPGT 
00110           UNTIL WS-SUB1 > IPGT-TBL-CNT.                           ELUIPGT 
00111                                                                   ELUIPGT 
00112 ******************************************************************ELUIPGT 
00113 *                                                                *ELUIPGT 
00114 *    SCAN ALL IPGT RECORDS IN ELSIPGT TABLE                      *ELUIPGT 
00115 *                                                                *ELUIPGT 
00116 ******************************************************************ELUIPGT 
00117                                                                   ELUIPGT 
00118  110-SCAN-ALL-IPGT-TABS.                                          ELUIPGT 
00119      IF IPGT-TABULAR-PTR (WS-SUB1) = NULL                         ELUIPGT 
00120      THEN                                                         ELUIPGT 
00121         CONTINUE                                                  ELUIPGT 
00122      ELSE                                                         ELUIPGT 
00123         PERFORM 120-CALC-IPGT-CONF-FACTORS.                       ELUIPGT 
00124                                                                   ELUIPGT 
00125 ******************************************************************ELUIPGT 
00126 *                                                                *ELUIPGT 
00127 *    CALCULATE THE CONFIDENCE FACTORS FOR THE CURRENT IPGT       *ELUIPGT 
00128 *    TABULAR RECORD                                              *ELUIPGT 
00129 *                                                                *ELUIPGT 
00130 ******************************************************************ELUIPGT 
00131                                                                   ELUIPGT 
00132  120-CALC-IPGT-CONF-FACTORS.                                      ELUIPGT 
00133      INITIALIZE WS-CONF-FACTORS.                                  ELUIPGT 
00134      SET ADDRESS OF IPGT-RECORD TO IPGT-TABULAR-PTR (WS-SUB1).    ELUIPGT 
00135      PERFORM 130-ACCUM-IPGT-CONF-FACTORS                          ELUIPGT 
00136         VARYING WS-SUB2 FROM 1 BY 1                               ELUIPGT 
00137           UNTIL WS-SUB2 > GX3-ENTRY-COUNT                         ELUIPGT 
00138      IF GX3-ID-ARGUMENT-EXCLUDED                                  ELUIPGT 
00139      THEN                                                         ELUIPGT 
00140         PERFORM 140-INVERT-CONF-FACTORS                           ELUIPGT 
00141      ELSE                                                         ELUIPGT 
00142         CONTINUE.                                                 ELUIPGT 
00143      PERFORM 150-SCALE-CONF-FACTORS.                              ELUIPGT 
00144      PERFORM 160-STORE-CONF-FACTORS.                              ELUIPGT 
00145                                                                   ELUIPGT 
00146 ******************************************************************ELUIPGT 
00147 *                                                                *ELUIPGT 
00148 *    ACCUMULATE THE IPGT CONFIDENCE FACTORS                      *ELUIPGT 
00149 *                                                                *ELUIPGT 
00150 ******************************************************************ELUIPGT 
00151                                                                   ELUIPGT 
00152  130-ACCUM-IPGT-CONF-FACTORS.                                     ELUIPGT 
00153      IF GX3-PROVIDER-TYPE-ARGUMENT (WS-SUB2) = HIGH-VALUES        ELUIPGT 
00154      THEN                                                         ELUIPGT 
00155         CONTINUE                                                  ELUIPGT 
00156      ELSE                                                         ELUIPGT 
00157         SET PROVIDER-TYPE-NOT-FOUND TO TRUE                       ELUIPGT 
00158         PERFORM WITH TEST BEFORE                                  ELUIPGT 
00159            VARYING WS-CFT2-SUB FROM 1 BY 1                        ELUIPGT 
00160              UNTIL    PROVIDER-TYPE-FOUND                         ELUIPGT 
00161                    OR WS-CFT2-SUB > CFT2-NBR-TBL-ENTRIES          ELUIPGT 
00162            IF   CFT2-PT (WS-CFT2-SUB)                             ELUIPGT 
00163               = GX3-PROVIDER-TYPE-ARGUMENT (WS-SUB2)              ELUIPGT 
00164            THEN                                                   ELUIPGT 
00165               SET PROVIDER-TYPE-FOUND TO TRUE                     ELUIPGT 
00166               ADD CFT2-CF-PT-INST (WS-CFT2-SUB) TO WS-CF-INST     ELUIPGT 
00167               ADD CFT2-CF-PT-PROF (WS-CFT2-SUB) TO WS-CF-PROF     ELUIPGT 
00168               ADD CFT2-CF-PT-PLAN (WS-CFT2-SUB) TO WS-CF-PLAN     ELUIPGT 
00169               ADD CFT2-CF-PT-NONPLAN (WS-CFT2-SUB)                ELUIPGT 
00170                TO WS-CF-NON-PLAN                                  ELUIPGT 
00171               ADD CFT2-CF-PT-OV (WS-CFT2-SUB) TO WS-CF-OV         ELUIPGT 
00172            ELSE                                                   ELUIPGT 
00173               CONTINUE                                            ELUIPGT 
00174            END-IF                                                 ELUIPGT 
00175            END-PERFORM                                            ELUIPGT 
00176      END-IF.                                                      ELUIPGT 
00177                                                                   ELUIPGT 
00178 ******************************************************************ELUIPGT 
00179 *                                                                *ELUIPGT 
00180 *    INVERT THE CONFIDENCE FACTORS                               *ELUIPGT 
00181 *                                                                *ELUIPGT 
00182 ******************************************************************ELUIPGT 
00183                                                                   ELUIPGT 
00184  140-INVERT-CONF-FACTORS.                                         ELUIPGT 
00185      COMPUTE WS-CF-INST     = WS-FP-ONE - WS-CF-INST.             ELUIPGT 
00186      COMPUTE WS-CF-PROF     = WS-FP-ONE - WS-CF-PROF.             ELUIPGT 
00187      COMPUTE WS-CF-PLAN     = WS-FP-ONE - WS-CF-PLAN.             ELUIPGT 
00188      COMPUTE WS-CF-NON-PLAN = WS-FP-ONE - WS-CF-NON-PLAN.         ELUIPGT 
00189      COMPUTE WS-CF-OV       = WS-FP-ONE - WS-CF-OV.               ELUIPGT 
00190                                                                   ELUIPGT 
00191 ******************************************************************ELUIPGT 
00192 *                                                                *ELUIPGT 
00193 *    SCALE THE CONFIDENCE FACTORS INTO THE RANGE -1..+1          *ELUIPGT 
00194 *                                                                *ELUIPGT 
00195 ******************************************************************ELUIPGT 
00196                                                                   ELUIPGT 
00197  150-SCALE-CONF-FACTORS.                                          ELUIPGT 
00198      COMPUTE WS-CF-INST     = WS-FP-NEG-ONE + (2 * WS-CF-INST).   ELUIPGT 
00199      COMPUTE WS-CF-PROF     = WS-FP-NEG-ONE + (2 * WS-CF-PROF).   ELUIPGT 
00200      COMPUTE WS-CF-PLAN     = WS-FP-NEG-ONE + (2 * WS-CF-PLAN).   ELUIPGT 
00201      COMPUTE WS-CF-NON-PLAN = WS-FP-NEG-ONE + (2 * WS-CF-NON-PLAN)ELUIPGT 
00202      COMPUTE WS-CF-OV       = WS-FP-NEG-ONE + (2 * WS-CF-OV).     ELUIPGT 
00203                                                                   ELUIPGT 
00204 ******************************************************************ELUIPGT 
00205 *                                                                *ELUIPGT 
00206 *    STORE THE CONFIDENCE FACTORS IN THE IPGT CONFIDENCE         *ELUIPGT 
00207 *    FACTORS TABLE                                               *ELUIPGT 
00208 *                                                                *ELUIPGT 
00209 ******************************************************************ELUIPGT 
00210                                                                   ELUIPGT 
00211  160-STORE-CONF-FACTORS.                                          ELUIPGT 
00212      MOVE WS-CF-INST TO IPGT-CF-INST (WS-SUB1).                   ELUIPGT 
00213      MOVE WS-CF-PROF TO IPGT-CF-PROF (WS-SUB1).                   ELUIPGT 
00214      MOVE WS-CF-PLAN TO IPGT-CF-PLAN (WS-SUB1).                   ELUIPGT 
00215      MOVE WS-CF-NON-PLAN TO IPGT-CF-NON-PLAN (WS-SUB1).           ELUIPGT 
00216      MOVE WS-CF-OV TO IPGT-CF-OV (WS-SUB1).                       ELUIPGT 
00217                                                                   ELUIPGT 
00218 /*****************************************************************ELUIPGT 
00219 *                                                                *ELUIPGT 
00220 *    ESTABLISH ADDRESSABILITY OF IPGN TABLE                      *ELUIPGT 
00221 *                                                                *ELUIPGT 
00222 ******************************************************************ELUIPGT 
00223                                                                   ELUIPGT 
00224  907-ESTAB-ADDR-ELSIPGT.                                          ELUIPGT 
00225      SET CIA-ELSIPGT-DDN TO TRUE.                                 ELUIPGT 
00226      CALL 'ELUSETAD'                                              ELUIPGT 
00227         USING DFHCOMMAREA                                         ELUIPGT 
00228               ADDRESS OF IPGT-INTERNAL-TABS-TABLE.                ELUIPGT 
00229      IF CIA-RC-PTR-NULL                                           ELUIPGT 
00230      THEN                                                         ELUIPGT 
00231          PERFORM 991-SIGNAL-UNALLOC-AREA                          ELUIPGT 
00232      ELSE                                                         ELUIPGT 
00233         CONTINUE.                                                 ELUIPGT 
00234                                                                   ELUIPGT 
00235 /*****************************************************************ELUIPGT 
00236 *                                                                *ELUIPGT 
00237 *    ESTABLISH THE ENGLISH CONTRACT INQUIRY STORAGE              *ELUIPGT 
00238 *    MANAGEMENT ENVIRONMENT                                      *ELUIPGT 
00239 *                                                                *ELUIPGT 
00240 *  - CHECK VALIDITY OF COMMAREA, ABEND IF NOT VALID              *ELUIPGT 
00241 *  - ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA,          *ELUIPGT 
00242 *    ABEND IF THE POINTER IS NULL                                *ELUIPGT 
00243 *  - INITIALIZE THE STORAGE MANAGEMENT SYSTEM                    *ELUIPGT 
00244 *                                                                *ELUIPGT 
00245 ******************************************************************ELUIPGT 
00246                                                                   ELUIPGT 
00247  990-ESTAB-ECI-STG-ENVIRON.                                       ELUIPGT 
00248                                                                   ELUIPGT 
00249      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELUIPGT 
00250      THEN                                                         ELUIPGT 
00251         EXEC CICS ABEND ABCODE ('EL01') END-EXEC                  ELUIPGT 
00252      ELSE                                                         ELUIPGT 
00253         IF ECA-CIA-PTR = NULL                                     ELUIPGT 
00254         THEN                                                      ELUIPGT 
00255            EXEC CICS ABEND ABCODE ('EL02') END-EXEC               ELUIPGT 
00256         ELSE                                                      ELUIPGT 
00257            SET ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA           ELUIPGT 
00258             TO ECA-CIA-PTR                                        ELUIPGT 
00259            CALL 'ELUINISM'                                        ELUIPGT 
00260               USING DFHCOMMAREA                                   ELUIPGT 
00261                     ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.     ELUIPGT 
00262                                                                   ELUIPGT 
00263 /*****************************************************************ELUIPGT 
00264 *                                                                *ELUIPGT 
00265 *    SIGNAL UNALLOCATED AREA ERROR                               *ELUIPGT 
00266 *                                                                *ELUIPGT 
00267 ******************************************************************ELUIPGT 
00268                                                                   ELUIPGT 
00269  991-SIGNAL-UNALLOC-AREA.                                         ELUIPGT 
00270      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUIPGT 
00271      PERFORM 999-SIGNAL-ABEND.                                    ELUIPGT 
00272                                                                   ELUIPGT 
00273 ******************************************************************ELUIPGT 
00274 *                                                                *ELUIPGT 
00275 *    SIGNAL ABEND                                                *ELUIPGT 
00276 *                                                                *ELUIPGT 
00277 ******************************************************************ELUIPGT 
00278                                                                   ELUIPGT 
00279  999-SIGNAL-ABEND.                                                ELUIPGT 
00280      EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC.                ELUIPGT 
