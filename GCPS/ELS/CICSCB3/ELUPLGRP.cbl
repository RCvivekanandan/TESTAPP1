00001  ID DIVISION.                                                     06/29/02
00002  PROGRAM-ID.   ELUPLGRP.                                          ELUPLGRP
00003  AUTHOR.       JERRY ARKEMA.                                         LV001
00004  DATE-WRITTEN. 04/01/86.                                          ELUPLGRP
00005  DATE-COMPILED.                                                   ELUPLGRP
00006                                                                   ELUPLGRP
00007 *@>ELUPLGRP                                                       ELUPLGRP
00008 *@¬                                                               ELUPLGRP
00009 *                        PROGRAM ABSTRACT                         ELUPLGRP
00010 *                                                                 ELUPLGRP
00011 *@¬ PROGRAM NAME:   ENGLISH CONTRACT INQUIRY                      ELUPLGRP
00012 *@¬                                                               ELUPLGRP
00013 *@¬ PROGRAM I.D.:   ELUPLGRP                                      ELUPLGRP
00014 *@¬                                                               ELUPLGRP
00015 *@¬ PURPOSE:  PAYMENT LEVEL GROUPING SUBROUTINE                   ELUPLGRP
00016 *@¬                                                               ELUPLGRP
00017 *@¬ OVERVIEW:  THIS   PROGRAM   USES  FROM  ONE  TO  FOUR  BENEFITELUPLGRP
00018 *@¬           PROVISION  LISTS  AND  RELEVANT DATA ELEMENTS PASSEDELUPLGRP
00019 *@¬           TO  IT  FROM  A TOPIC MODULE TO EVALUATE WHAT GROUPSELUPLGRP
00020 *@¬           OF BENEFITS PROVISIONS ARE PAID THE \
00021 *@¬                                                               ELUPLGRP
00022 *@¬            ANY  TWO  BENEFIT  PROVISIONS \
00023 *@¬           THE  SAME  IF EVERY RELEVANT DATA ELEMENT \
00024 *@¬           HAS  THE  SAME  VALUE AS THE CORRESPONDING DATA ELE-ELUPLGRP
00025 *@¬           MENT \
00026 *@¬           SETS  OF  BENEFIT  PROVISION  AND ALL-LEVEL TABULARSELUPLGRP
00027 *@¬           ATTACHED INCLUDING THE SLOT NUMBERS.                ELUPLGRP
00028 *@¬                                                               ELUPLGRP
00029 *@¬                                                               ELUPLGRP
00030 *@¬ RECORDS                                                       ELUPLGRP
00031 *@¬ ACCESSED: ELCDCIA RECORD                                      ELUPLGRP
00032 *@¬           GROUP SPECIFIC RECORD                               ELUPLGRP
00033 *@¬           CONTRACT RECORD(S)                                  ELUPLGRP
00034 *@¬           BENEFIT PROVISION LIST(S)                           ELUPLGRP
00035 *@¬           BENEFIT PROVISION RECORDS                           ELUPLGRP
00036 *@¬           PAYMENT LEVEL SWITCHES AREA                         ELUPLGRP
00037 *@¬           PAYMENT LEVEL TABLE(S)                              ELUPLGRP
00038 *@¬                                                               ELUPLGRP
00039 *@¬ PROCESSING                                                    ELUPLGRP
00040 *@¬ FUNCTIONS: THIS MODULE PERFORMS THE FOLLOWING FUNCTIONS:      ELUPLGRP
00041 *@¬                                                               ELUPLGRP
00042 *@¬            1. PROCESS FOR EACH BENEFIT PROVISION LIST:        ELUPLGRP
00043 *@¬                                                               ELUPLGRP
00044 *@¬               A. ACQUIRE APPROPRIATE SPACE FOR PAYMENT LEVEL  ELUPLGRP
00045 *@¬                   TABLE.                                      ELUPLGRP
00046 *@¬                                                               ELUPLGRP
00047 *@¬               B. LOAD PAYMENT LEVEL TABLE WITH DATA ELEMENTS  ELUPLGRP
00048 *@¬                   FROM  PROVISION  RECORDS  SPECIFIED BY THE  ELUPLGRP
00049 *@¬                   PAYMENT  LEVEL SWITCH SETTINGS PASSED FROM  ELUPLGRP
00050 *@¬                   CALLING PROGRAM.                            ELUPLGRP
00051 *@¬                                                               ELUPLGRP
00052 *@¬               C. IDENTIFY THE PAYMENT LEVEL GROUPINGS IN THE  ELUPLGRP
00053 *@¬                   PAYMENT LEVEL TABLE.                        ELUPLGRP
00054 *@¬                                                               ELUPLGRP
00055 *@¬               E. UPDATE  THE  BENEFIT  PROVISION  LIST  WITH  ELUPLGRP
00056 *@¬                   POINTERS  TO  COMMON PAYMENT LEVEL ENTRIES  ELUPLGRP
00057 *@¬                   IN THE PAYMENT LEVEL TABLE.                 ELUPLGRP
00058 *@¬                                                               ELUPLGRP
00059 *@¬                                                               ELUPLGRP
00060 *@¬                                                               ELUPLGRP
00061                                                                   ELUPLGRP
00062 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          ELUPLGRP
00063 *    *-*         U P D A T E   H I S T O R Y         *-*          ELUPLGRP
00064 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          ELUPLGRP
00065                                                                   ELUPLGRP
00066 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* ELUPLGRP
00067                                                                   ELUPLGRP
00068 *  XXXX      04/01/86  JLA  INITIAL WRITING.                      ELUPLGRP
00069 *                                                                 ELUPLGRP
00070 *  0001      06/10/86  DES  1. CORRECT INVALID SPECIFICATION OF A ELUPLGRP
00071 *                              TABLE ENTRY POINTER                ELUPLGRP
00072 *                           2. CORRECT PROVISION COMBINATION      ELUPLGRP
00073 *                              LOGIC IN SECTION 2500.             ELUPLGRP
00074 *                                                                 ELUPLGRP
00075 *  0002      07/09/86  JLA  1. CORRECT SETTING OF PLT-SAME-AS-    ELUPLGRP
00076 *                              ENTRY IN SECTION 2100.             ELUPLGRP
00077 *  0003      07/29/86  JTC  1. ADDED PLP-PROV-FILE-ELIG-IND TO THEELUPLGRP
00078 *                              BENEFIT PROVISON COMMON PROCESSING ELUPLGRP
00079 *                              IN SECTION 8100.                   ELUPLGRP
00080 *  0004      10/16/86  JTC  1. ADDED PLC-CORRIDOR-OVERRIDE,       ELUPLGRP
00081 *                                    PLD-CORRIDOR-OVERRIDE, AND   ELUPLGRP
00082 *                                    PLE-CORRIDOR-OVERRIDE TO THE ELUPLGRP
00083 *                              BENEFIT PROVISON COMMON PROCESSING ELUPLGRP
00084 *                           2. VS COBOL II CONVERSION             ELUPLGRP
00085 *                                                                 ELUPLGRP
00086 *  0005      11/11/88  NAC  1. CORRECT GROUPING LOGIC - IT WAS    ELUPLGRP
00087 *                              DROPPING THE LAST PROVISION FROM   ELUPLGRP
00088 *                              A PAYMENT GROUP.                   ELUPLGRP
00089 *                           2. INCLUDE STORAGE ENHANCEMENTS       ELUPLGRP
00090 *                                                                 ELUPLGRP
00091 *  0006      05/16/90  GEM  1. FOR POINT OF SERVICE PROCESSING    ELUPLGRP
00092 *                              RENAME PSE-COST-CONT-PYMT-ELIG-IND ELUPLGRP
00093 *                                  TO PSP-COST-CONT-PYMT-ELIG-IND,ELUPLGRP
00094 *                              RENAME PLE-COST-CONT-PYMT-ELIG-IND ELUPLGRP
00095 *                                  TO PLP-COST-CONT-PYMT-ELIG-IND,ELUPLGRP
00096 *                              RENAME GPE-COST-CONT-PYMT-ELIG-IND ELUPLGRP
00097 *                                  TO GCP-COST-CONT-PYMT-ELIG-IND.ELUPLGRP
00098 *                                                                 ELUPLGRP
00099 *  0007      06/27/90  GEM  1. REPLACED LINK TO ELUIOPGM STATEMENTELUPLGRP
00100 *                              WITH A CALL STATEMENT.             ELUPLGRP
00101 *                                                                 ELUPLGRP
00102 *  0008      05/03/91  GEM  1. ADDED LOGIC TO IGNORE DUP BENEFIT  ELUPLGRP
00103 *                              PROVISIONS. SEE 4000-000 SECTION.  ELUPLGRP
00104 ***************************************************************** ELUPLGRP
00105                                                                   ELUPLGRP
00106 /                                                                 ELUPLGRP
00107  ENVIRONMENT DIVISION.                                            ELUPLGRP
00108  DATA DIVISION.                                                   ELUPLGRP
00109                                                                   ELUPLGRP
00110  WORKING-STORAGE SECTION.                                         ELUPLGRP
00111  01  FILLER                              PIC X(26) VALUE          ELUPLGRP
00112                                 '*** ELUPLGRP WS BEGINS ***'.     ELUPLGRP
00113  01  WS-SUBSCRIPTS.                                               ELUPLGRP
00114 *    SUBSCRIPT FOR PROVISION LIST (VARIES 1 TO 25)                ELUPLGRP
00115      05  A                           PIC S9(04) COMP VALUE ZERO.  ELUPLGRP
00116                                                                   ELUPLGRP
00117 *    SUBSCRIPT FOR PAYMENT LEVEL TABLE ENTRIES (VARIES 1 TO 25)   ELUPLGRP
00118      05  X                           PIC S9(04) COMP VALUE ZERO.  ELUPLGRP
00119                                                                   ELUPLGRP
00120 *    SUBSCRIPT FOR PAYMENT LEVEL TBL ENTRIES (VARIES X+1 TO 25)   ELUPLGRP
00121      05  X1                          PIC S9(04) COMP VALUE ZERO.  ELUPLGRP
00122                                                                   ELUPLGRP
00123 *    SUBSCRIPT FOR PAYMENT LEVEL TABLE LINE OF BUSINESS           ELUPLGRP
00124 *    WITHIN TABLE ENTRY (EQUAL 1 OR 2)                            ELUPLGRP
00125      05  Y                           PIC S9(04) COMP VALUE ZERO.  ELUPLGRP
00126                                                                   ELUPLGRP
00127 *    SUBSCRIPT FOR GENERAL PURPOSE USAGE                          ELUPLGRP
00128      05  Z                           PIC S9(04) COMP VALUE ZERO.  ELUPLGRP
00129 /                                                                 ELUPLGRP
00130  01  WS-END                              PIC X(18) VALUE          ELUPLGRP
00131                                          '*** END OF W/S ***'.    ELUPLGRP
00132                                                                   ELUPLGRP
00133  LINKAGE SECTION.                                                 ELUPLGRP
00134  01  DFHCOMMAREA.                                                 ELUPLGRP
00135      COPY ELSCOMMC.                                               ELUPLGRP
00136 /  *** CIA  AREA ***                                              ELUPLGRP
00137      COPY ELSCIA2C.                                               ELUPLGRP
00138 /  *** IO PARM AREA ***                                           ELUPLGRP
00139      COPY ELSIOPMC.                                               ELUPLGRP
00140 /  *** KEY AREA ***                                               ELUPLGRP
00141      COPY ELSKEYSC.                                               ELUPLGRP
00142 /  *** BENEFIT PROVISION TABLE ***                                ELUPLGRP
00143      COPY ELSPRVNC.                                               ELUPLGRP
00144 /  *** PAYMENT LEVEL REQUEST INDICATORS ***                       ELUPLGRP
00145      COPY ELSPLGSW.                                               ELUPLGRP
00146 /  *** BENEFIT PROVISION GROUPING TABLE ***                       ELUPLGRP
00147      COPY ELSPLGTB.                                               ELUPLGRP
00148 /  *** BENEFIT PROVISION RECORD ***                               ELUPLGRP
00149  01  BENEFIT-PROVISION-RECORD.                                    ELUPLGRP
00150      COPY GCBENPVC.                                               ELUPLGRP
00151 /                                                                 ELUPLGRP
00152  PROCEDURE DIVISION.                                              ELUPLGRP
00153                                                                   ELUPLGRP
00154 ******************************************************************ELUPLGRP
00155 *                                                                *ELUPLGRP
00156 *  0000          M A I N L I N E                                 *ELUPLGRP
00157 *                                                                *ELUPLGRP
00158 ******************************************************************ELUPLGRP
00159                                                                   ELUPLGRP
00160  0000-000-MAINLINE SECTION.                                       ELUPLGRP
00161  0000-010.                                                        ELUPLGRP
00162      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELUPLGRP
00163          EXEC CICS ABEND                                          ELUPLGRP
00164                    ABCODE ('EL01')                                ELUPLGRP
00165          END-EXEC                                                 ELUPLGRP
00166      END-IF.                                                      ELUPLGRP
00167                                                                   ELUPLGRP
00168      IF ECA-CIA-PTR = NULL                                        ELUPLGRP
00169          EXEC CICS ABEND                                          ELUPLGRP
00170                    ABCODE ('EL02')                                ELUPLGRP
00171          END-EXEC                                                 ELUPLGRP
00172      END-IF.                                                      ELUPLGRP
00173                                                                   ELUPLGRP
00174      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUPLGRP
00175                      ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.    ELUPLGRP
00176                                                                   ELUPLGRP
00177 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELUPLGRP
00178                                                                   ELUPLGRP
00179      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELUPLGRP
00180      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUPLGRP
00181                      ADDRESS OF KWA-FILE-KEY-WORK-AREA.           ELUPLGRP
00182      IF NOT CIA-RC-OK                                             ELUPLGRP
00183          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELUPLGRP
00184                                                                   ELUPLGRP
00185      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELUPLGRP
00186      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUPLGRP
00187                      ADDRESS OF PVN-BENEFIT-PROVISION-LIST.       ELUPLGRP
00188      IF NOT CIA-RC-OK                                             ELUPLGRP
00189          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELUPLGRP
00190                                                                   ELUPLGRP
00191      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELUPLGRP
00192      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUPLGRP
00193                      ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.       ELUPLGRP
00194      IF NOT CIA-RC-OK                                             ELUPLGRP
00195          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELUPLGRP
00196                                                                   ELUPLGRP
00197      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELUPLGRP
00198      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUPLGRP
00199                      ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.          ELUPLGRP
00200      IF CIA-RC-OK                                                 ELUPLGRP
00201          SET CIA-ELSPLGTB-DDN TO TRUE                             ELUPLGRP
00202          SET CIA-STG-FREEMAIN TO TRUE                             ELUPLGRP
00203                                                                   ELUPLGRP
00204          EXEC CICS LINK                                           ELUPLGRP
00205                    PROGRAM('ELUSTGMG')                            ELUPLGRP
00206                    COMMAREA(DFHCOMMAREA)                          ELUPLGRP
00207          END-EXEC.                                                ELUPLGRP
00208                                                                   ELUPLGRP
00209      COMPUTE CIA-AREA-LEN = (PVN-NBR-BEN-PROVN                    ELUPLGRP
00210                                 *  LENGTH OF PLT-ENTRY)           ELUPLGRP
00211                       + LENGTH OF PLT-ENTRY-COUNT.                ELUPLGRP
00212                                                                   ELUPLGRP
00213      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELUPLGRP
00214                                                                   ELUPLGRP
00215      SET CIA-STG-GETMAIN  TO TRUE.                                ELUPLGRP
00216      EXEC CICS LINK                                               ELUPLGRP
00217                PROGRAM('ELUSTGMG')                                ELUPLGRP
00218                COMMAREA(DFHCOMMAREA)                              ELUPLGRP
00219      END-EXEC.                                                    ELUPLGRP
00220                                                                   ELUPLGRP
00221      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELUPLGRP
00222      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUPLGRP
00223                      ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.          ELUPLGRP
00224      IF NOT CIA-RC-OK                                             ELUPLGRP
00225          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELUPLGRP
00226                                                                   ELUPLGRP
00227      PERFORM 1000-000-PROCESS-PAY-LVL-TBLE.                       ELUPLGRP
00228                                                                   ELUPLGRP
00229      EXEC CICS RETURN END-EXEC.                                   ELUPLGRP
00230      GOBACK.                                                      ELUPLGRP
00231                                                                   ELUPLGRP
00232  0000-900-EXIT.                                                   ELUPLGRP
00233      EXIT.                                                        ELUPLGRP
00234  0098-SIGNAL-UNALL-AREA-ERROR    SECTION.                         ELUPLGRP
00235      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELUPLGRP
00236      EXEC CICS ABEND                                              ELUPLGRP
00237                ABCODE(CIA-ABCODE)                                 ELUPLGRP
00238      END-EXEC.                                                    ELUPLGRP
00239  0098-000-EXIT.                                                   ELUPLGRP
00240      EXIT.                                                        ELUPLGRP
00241 /*****************************************************************ELUPLGRP
00242 *                                                                *ELUPLGRP
00243 * 1000  INSTITUTIONAL INPATIENT  TABLE PROCESSING     OR         *ELUPLGRP
00244 *       INSTITUTIONAL OUTPATIENT TABLE PROCESSING     OR         *ELUPLGRP
00245 *       PROFESSIONAL  INPATIENT  TABLE PROCESSING     OR         *ELUPLGRP
00246 *       PROFESSIONAL  OUTPATIENT TABLE PROCESSING                *ELUPLGRP
00247 *                                                                *ELUPLGRP
00248 *        ++++++++++++++++++++++++++++++++++++++++++++++++++++++  *ELUPLGRP
00249 *  NOTE: ALL FOUR PROVISION LIST COPYBOOKS ARE MIRROR IMAGES     *ELUPLGRP
00250 *        OF EACH OTHER (ELCDELII, ELCDELIO, ELCDELPI, ELCDEDPO), *ELUPLGRP
00251 *        FOR CODING SIMPLICITY,                                  *ELUPLGRP
00252 *        THIS PROGRAM USES JUST THE ELCDELII COPYBOOK TO         *ELUPLGRP
00253 *        REFERENCE ALL LISTS (PREFIX OF ELII- FOR FIELD NAMES)   *ELUPLGRP
00254 *        AS THEY ARE REFERENCED ONE AT A TIME.                   *ELUPLGRP
00255 *        ++++++++++++++++++++++++++++++++++++++++++++++++++++++  *ELUPLGRP
00256 *                                                                *ELUPLGRP
00257 * VS COBOL II CONVERSION HAS ONLY ONE TABLE BEING USED.(PVN-)    *ELUPLGRP
00258 *                                                                *ELUPLGRP
00259 ******************************************************************ELUPLGRP
00260  1000-000-PROCESS-PAY-LVL-TBLE   SECTION.                         ELUPLGRP
00261  1000-010.                                                        ELUPLGRP
00262                                                                   ELUPLGRP
00263      MOVE +0 TO PLT-ENTRY-COUNT                                   ELUPLGRP
00264                 X.                                                ELUPLGRP
00265                                                                   ELUPLGRP
00266      PERFORM 1100-000-LOAD-PLT                                    ELUPLGRP
00267         VARYING A FROM 1 BY 1                                     ELUPLGRP
00268           UNTIL A > PVN-NBR-BEN-PROVN.                            ELUPLGRP
00269                                                                   ELUPLGRP
00270      IF  PLT-ENTRY-COUNT = +0                                     ELUPLGRP
00271          GO TO 1000-900-EXIT.                                     ELUPLGRP
00272                                                                   ELUPLGRP
00273      PERFORM 2000-000-ID-PAY-LVL-GROUPINGS                        ELUPLGRP
00274         VARYING X FROM 1 BY 1                                     ELUPLGRP
00275           UNTIL X  =  PLT-ENTRY-COUNT.                            ELUPLGRP
00276                                                                   ELUPLGRP
00277      PERFORM 2500-000-COMBINE-PROVN-INFO                          ELUPLGRP
00278         VARYING X FROM 1 BY 1                                     ELUPLGRP
00279           UNTIL X > PLT-ENTRY-COUNT.                              ELUPLGRP
00280                                                                   ELUPLGRP
00281      PERFORM 3000-000-UPDATE-PROVISION-LIST                       ELUPLGRP
00282         VARYING X FROM 1 BY 1                                     ELUPLGRP
00283           UNTIL X > PLT-ENTRY-COUNT.                              ELUPLGRP
00284                                                                   ELUPLGRP
00285      PERFORM 4000-000-PROCESS-DUP-BEN-IDS                         ELUPLGRP
00286         VARYING PVN-BEN-PROVN-IDX FROM +1 BY +1                   ELUPLGRP
00287           UNTIL PVN-BEN-PROVN-IDX > PVN-NBR-BEN-PROVN.            ELUPLGRP
00288                                                                   ELUPLGRP
00289  1000-900-EXIT.                                                   ELUPLGRP
00290      EXIT.                                                        ELUPLGRP
00291 /*****************************************************************ELUPLGRP
00292 *                                                                *ELUPLGRP
00293 * 1100   LOAD PAYMENT LEVEL TABLE                                *ELUPLGRP
00294 *                                                                *ELUPLGRP
00295 ******************************************************************ELUPLGRP
00296  1100-000-LOAD-PLT               SECTION.                         ELUPLGRP
00297  1100-010.                                                        ELUPLGRP
00298                                                                   ELUPLGRP
00299      IF  PVN-SLOT-NBR-BAS (A) > +0             OR                 ELUPLGRP
00300          PVN-SLOT-NBR-SUP            (A) > +0                     ELUPLGRP
00301      THEN                                                         ELUPLGRP
00302          ADD +1   TO X                                            ELUPLGRP
00303                      PLT-ENTRY-COUNT                              ELUPLGRP
00304          MOVE A   TO PLT-ENTRY-NUMBER  (X)                        ELUPLGRP
00305          MOVE PVN-BEN-PROVN-ID         (A)                        ELUPLGRP
00306                   TO PLT-PROVISION-ID  (X)                        ELUPLGRP
00307          MOVE 'Y' TO PLT-ENTRY-STATUS  (X)                        ELUPLGRP
00308          MOVE +0  TO PLT-SAME-AS-ENTRY (X)                        ELUPLGRP
00309      ELSE                                                         ELUPLGRP
00310          GO TO 1100-900-EXIT.                                     ELUPLGRP
00311                                                                   ELUPLGRP
00312      IF  PVN-SLOT-NBR-BAS (A) > +0                                ELUPLGRP
00313          GO TO 1100-100-PROCESS-BASIC.                            ELUPLGRP
00314                                                                   ELUPLGRP
00315      IF  PVN-SLOT-NBR-SUP (A)            > +0                     ELUPLGRP
00316          GO TO 1100-400-PROCESS-SUPP.                             ELUPLGRP
00317                                                                   ELUPLGRP
00318                                                                   ELUPLGRP
00319  1100-100-PROCESS-BASIC.                                          ELUPLGRP
00320                                                                   ELUPLGRP
00321      IF  PVN-SLOT-NBR-BAS (A) > +0                                ELUPLGRP
00322      THEN                                                         ELUPLGRP
00323          MOVE PVN-BEN-PROVN-ID (A) TO KWA-GCP-PROVN-ID            ELUPLGRP
00324          MOVE PVN-SLOT-NBR-BAS (A) TO KWA-GCP-PROVN-SLOT-NO       ELUPLGRP
00325          PERFORM 9300-000-READ-PROVISION                          ELUPLGRP
00326          MOVE +1 TO Y                                             ELUPLGRP
00327          PERFORM 8000-000-MOVE-PROVISION                          ELUPLGRP
00328      ELSE                                                         ELUPLGRP
00329          NEXT SENTENCE.                                           ELUPLGRP
00330                                                                   ELUPLGRP
00331  1100-400-PROCESS-SUPP.                                           ELUPLGRP
00332                                                                   ELUPLGRP
00333      IF  PVN-SLOT-NBR-SUP (A) > +0                                ELUPLGRP
00334      THEN                                                         ELUPLGRP
00335          MOVE PVN-BEN-PROVN-ID (A) TO KWA-GCP-PROVN-ID            ELUPLGRP
00336          MOVE PVN-SLOT-NBR-SUP (A) TO KWA-GCP-PROVN-SLOT-NO       ELUPLGRP
00337          PERFORM 9300-000-READ-PROVISION                          ELUPLGRP
00338          MOVE +2 TO Y                                             ELUPLGRP
00339          PERFORM 8000-000-MOVE-PROVISION                          ELUPLGRP
00340      ELSE                                                         ELUPLGRP
00341          GO TO 1100-900-EXIT.                                     ELUPLGRP
00342                                                                   ELUPLGRP
00343  1100-900-EXIT.                                                   ELUPLGRP
00344      EXIT.                                                        ELUPLGRP
00345 /*****************************************************************ELUPLGRP
00346 *                                                                *ELUPLGRP
00347 * 2000    IDENTIFY PAYMENT LEVEL GROUPINGS                       *ELUPLGRP
00348 *                                                                *ELUPLGRP
00349 ******************************************************************ELUPLGRP
00350  2000-000-ID-PAY-LVL-GROUPINGS  SECTION.                          ELUPLGRP
00351  2000-010.                                                        ELUPLGRP
00352                                                                   ELUPLGRP
00353      COMPUTE Z = X + 1.                                           ELUPLGRP
00354      PERFORM 2100-000-ID-PAY-LVL-GROUPINGS                        ELUPLGRP
00355         VARYING X1 FROM Z BY 1                                    ELUPLGRP
00356           UNTIL X1  >  PLT-ENTRY-COUNT.                           ELUPLGRP
00357                                                                   ELUPLGRP
00358  2000-900-EXIT.                                                   ELUPLGRP
00359      EXIT.                                                        ELUPLGRP
00360 /*****************************************************************ELUPLGRP
00361 *                                                                *ELUPLGRP
00362 * 2100    IDENTIFY PAYMENT LEVEL GROUPINGS                       *ELUPLGRP
00363 *                                                                *ELUPLGRP
00364 ******************************************************************ELUPLGRP
00365  2100-000-ID-PAY-LVL-GROUPINGS  SECTION.                          ELUPLGRP
00366  2100-010.                                                        ELUPLGRP
00367                                                                   ELUPLGRP
00368      IF  PLT-BEN-PROV-FORMAT(X) = PLT-BEN-PROV-FORMAT(X1)         ELUPLGRP
00369      THEN                                                         ELUPLGRP
00370          IF  PLP-LOB-AREA(X 1)  = PLP-LOB-AREA(X1 1) AND          ELUPLGRP
00371              PLP-LOB-AREA(X 2)  = PLP-LOB-AREA(X1 2)              ELUPLGRP
00372          THEN                                                     ELUPLGRP
00373              MOVE 'N'                 TO PLT-ENTRY-STATUS(X1)     ELUPLGRP
00374              IF  PLT-ENTRY-STATUS(X) = 'Y'                        ELUPLGRP
00375              THEN                                                 ELUPLGRP
00376                  MOVE X                                           ELUPLGRP
00377                    TO PLT-SAME-AS-ENTRY(X1)                       ELUPLGRP
00378              ELSE                                                 ELUPLGRP
00379                  MOVE PLT-SAME-AS-ENTRY(X)                        ELUPLGRP
00380                    TO PLT-SAME-AS-ENTRY(X1)                       ELUPLGRP
00381          ELSE                                                     ELUPLGRP
00382              NEXT SENTENCE                                        ELUPLGRP
00383      ELSE                                                         ELUPLGRP
00384          IF  PLP-BP-COMMON-AREA(X 1) = PLP-BP-COMMON-AREA(X1 1)   ELUPLGRP
00385           AND PLP-BP-COMMON-AREA(X 2) = PLP-BP-COMMON-AREA(X1 2)  ELUPLGRP
00386          THEN                                                     ELUPLGRP
00387              MOVE 'N'                 TO PLT-ENTRY-STATUS(X1)     ELUPLGRP
00388              IF  PLT-ENTRY-STATUS(X) = 'Y'                        ELUPLGRP
00389              THEN                                                 ELUPLGRP
00390                  MOVE X                                           ELUPLGRP
00391                    TO PLT-SAME-AS-ENTRY(X1)                       ELUPLGRP
00392              ELSE                                                 ELUPLGRP
00393                  MOVE PLT-SAME-AS-ENTRY(X)                        ELUPLGRP
00394                    TO PLT-SAME-AS-ENTRY(X1)                       ELUPLGRP
00395          ELSE                                                     ELUPLGRP
00396              NEXT SENTENCE.                                       ELUPLGRP
00397                                                                   ELUPLGRP
00398  2100-900-EXIT.                                                   ELUPLGRP
00399      EXIT.                                                        ELUPLGRP
00400 /*****************************************************************ELUPLGRP
00401 *                                                                *ELUPLGRP
00402 * 2500    COMBINE PROVISION RECORD INFORMATION FOR TABLE ENTRIES *ELUPLGRP
00403 *          THAT ARE MARKED AS THE SAME.                          *ELUPLGRP
00404 *                                                                *ELUPLGRP
00405 ******************************************************************ELUPLGRP
00406  2500-000-COMBINE-PROVN-INFO    SECTION.                          ELUPLGRP
00407  2500-010.                                                        ELUPLGRP
00408                                                                   ELUPLGRP
00409      IF  PLT-SAME-AS-ENTRY(X) = +0                                ELUPLGRP
00410          GO TO 2500-900-EXIT.                                     ELUPLGRP
00411                                                                   ELUPLGRP
00412      MOVE PLT-SAME-AS-ENTRY(X) TO Z.                              ELUPLGRP
00413                                                                   ELUPLGRP
00414 *---- COMBINE PROVISION \
00415                                                                   ELUPLGRP
00416      IF     PLA-BP-FORMAT-A (Z 1) = LOW-VALUES                    ELUPLGRP
00417         AND PLA-BP-FORMAT-A (X 1) NOT = LOW-VALUES                ELUPLGRP
00418      THEN                                                         ELUPLGRP
00419          MOVE PLA-BP-FORMAT-A (X 1) TO PLA-BP-FORMAT-A (Z 1).     ELUPLGRP
00420                                                                   ELUPLGRP
00421      IF     PLA-BP-FORMAT-A (Z 2) = LOW-VALUES                    ELUPLGRP
00422         AND PLA-BP-FORMAT-A (X 2) NOT = LOW-VALUES                ELUPLGRP
00423      THEN                                                         ELUPLGRP
00424          MOVE PLA-BP-FORMAT-A (X 2) TO PLA-BP-FORMAT-A (Z 2).     ELUPLGRP
00425                                                                   ELUPLGRP
00426 *---- COMBINE PROVISION \
00427                                                                   ELUPLGRP
00428      IF     PLB-BP-FORMAT-B (Z 1) = LOW-VALUES                    ELUPLGRP
00429         AND PLB-BP-FORMAT-B (X 1) NOT = LOW-VALUES                ELUPLGRP
00430      THEN                                                         ELUPLGRP
00431          MOVE PLB-BP-FORMAT-B (X 1) TO PLB-BP-FORMAT-B (Z 1).     ELUPLGRP
00432                                                                   ELUPLGRP
00433      IF     PLB-BP-FORMAT-B (Z 2) = LOW-VALUES                    ELUPLGRP
00434         AND PLB-BP-FORMAT-B (X 2) NOT = LOW-VALUES                ELUPLGRP
00435      THEN                                                         ELUPLGRP
00436          MOVE PLB-BP-FORMAT-B (X 2) TO PLB-BP-FORMAT-B (Z 2).     ELUPLGRP
00437                                                                   ELUPLGRP
00438 *---- COMBINE PROVISION \
00439                                                                   ELUPLGRP
00440      IF     PLC-BP-FORMAT-C (Z 1) = LOW-VALUES                    ELUPLGRP
00441         AND PLC-BP-FORMAT-C (X 1) NOT = LOW-VALUES                ELUPLGRP
00442      THEN                                                         ELUPLGRP
00443          MOVE PLC-BP-FORMAT-C (X 1) TO PLC-BP-FORMAT-C (Z 1).     ELUPLGRP
00444                                                                   ELUPLGRP
00445      IF     PLC-BP-FORMAT-C (Z 2) = LOW-VALUES                    ELUPLGRP
00446         AND PLC-BP-FORMAT-C (X 2) NOT = LOW-VALUES                ELUPLGRP
00447      THEN                                                         ELUPLGRP
00448          MOVE PLC-BP-FORMAT-C (X 2) TO PLC-BP-FORMAT-C (Z 2).     ELUPLGRP
00449                                                                   ELUPLGRP
00450 *---- COMBINE PROVISION \
00451                                                                   ELUPLGRP
00452      IF     PLD-BP-FORMAT-D (Z 1) = LOW-VALUES                    ELUPLGRP
00453         AND PLD-BP-FORMAT-D (X 1) NOT = LOW-VALUES                ELUPLGRP
00454      THEN                                                         ELUPLGRP
00455          MOVE PLD-BP-FORMAT-D (X 1) TO PLD-BP-FORMAT-D (Z 1).     ELUPLGRP
00456                                                                   ELUPLGRP
00457      IF     PLD-BP-FORMAT-D (Z 2) = LOW-VALUES                    ELUPLGRP
00458         AND PLD-BP-FORMAT-D (X 2) NOT = LOW-VALUES                ELUPLGRP
00459      THEN                                                         ELUPLGRP
00460          MOVE PLD-BP-FORMAT-D (X 2) TO PLD-BP-FORMAT-D (Z 2).     ELUPLGRP
00461                                                                   ELUPLGRP
00462 *---- COMBINE PROVISION \
00463                                                                   ELUPLGRP
00464      IF     PLE-BP-FORMAT-E (Z 1) = LOW-VALUES                    ELUPLGRP
00465         AND PLE-BP-FORMAT-E (X 1) NOT = LOW-VALUES                ELUPLGRP
00466      THEN                                                         ELUPLGRP
00467          MOVE PLE-BP-FORMAT-E (X 1) TO PLE-BP-FORMAT-E (Z 1).     ELUPLGRP
00468                                                                   ELUPLGRP
00469      IF     PLE-BP-FORMAT-E (Z 2) = LOW-VALUES                    ELUPLGRP
00470         AND PLE-BP-FORMAT-E (X 2) NOT = LOW-VALUES                ELUPLGRP
00471      THEN                                                         ELUPLGRP
00472          MOVE PLE-BP-FORMAT-E (X 2) TO PLE-BP-FORMAT-E (Z 2).     ELUPLGRP
00473                                                                   ELUPLGRP
00474 *---- COMBINE PROVISION \
00475                                                                   ELUPLGRP
00476      IF     PLW-BP-FORMAT-W (Z 1) = LOW-VALUES                    ELUPLGRP
00477         AND PLW-BP-FORMAT-W (X 1) NOT = LOW-VALUES                ELUPLGRP
00478      THEN                                                         ELUPLGRP
00479          MOVE PLW-BP-FORMAT-W (X 1) TO PLW-BP-FORMAT-W (Z 1).     ELUPLGRP
00480                                                                   ELUPLGRP
00481      IF     PLW-BP-FORMAT-W (Z 2) = LOW-VALUES                    ELUPLGRP
00482         AND PLW-BP-FORMAT-W (X 2) NOT = LOW-VALUES                ELUPLGRP
00483      THEN                                                         ELUPLGRP
00484          MOVE PLW-BP-FORMAT-W (X 2) TO PLW-BP-FORMAT-W (Z 2).     ELUPLGRP
00485                                                                   ELUPLGRP
00486  2500-900-EXIT.                                                   ELUPLGRP
00487      EXIT.                                                        ELUPLGRP
00488 /*****************************************************************ELUPLGRP
00489 *                                                                *ELUPLGRP
00490 * 3000    UPDATE BENEFIT PROVISION LIST                          *ELUPLGRP
00491 *                                                                *ELUPLGRP
00492 ******************************************************************ELUPLGRP
00493  3000-000-UPDATE-PROVISION-LIST SECTION.                          ELUPLGRP
00494  3000-010.                                                        ELUPLGRP
00495                                                                   ELUPLGRP
00496      IF  PLT-SAME-AS-ENTRY(X) > +0                                ELUPLGRP
00497      THEN                                                         ELUPLGRP
00498          MOVE PLT-ENTRY-NUMBER(X) TO A                            ELUPLGRP
00499          MOVE PLT-SAME-AS-ENTRY(X)                                ELUPLGRP
00500            TO PVN-COVG-SAME-AS(A)                                 ELUPLGRP
00501      ELSE                                                         ELUPLGRP
00502          MOVE PLT-ENTRY-NUMBER(X) TO A                            ELUPLGRP
00503          MOVE X                                                   ELUPLGRP
00504            TO PVN-COVG-SAME-AS(A).                                ELUPLGRP
00505                                                                   ELUPLGRP
00506  3000-900-EXIT.                                                   ELUPLGRP
00507      EXIT.                                                        ELUPLGRP
00508 /*****************************************************************ELUPLGRP
00509 *                                                                *ELUPLGRP
00510 * 4000 PROCESS DUPLICATE BENEFITS                                *ELUPLGRP
00511 *  (IF PVN-BEN-IDS EQUAL AND FORMAT = 'W'; ZERO PVN-COVG-SAME-AS)*ELUPLGRP
00512 *                                                                *ELUPLGRP
00513 ******************************************************************ELUPLGRP
00514  4000-000-PROCESS-DUP-BEN-IDS SECTION.                            ELUPLGRP
00515  4000-010.                                                        ELUPLGRP
00516                                                                   ELUPLGRP
00517      IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'W'                     ELUPLGRP
00518                                                                   ELUPLGRP
00519         PERFORM WITH TEST BEFORE                                  ELUPLGRP
00520           VARYING  A  FROM  +1  BY  +1                            ELUPLGRP
00521             UNTIL  A  >  PVN-NBR-BEN-PROVN                        ELUPLGRP
00522                                                                   ELUPLGRP
00523         IF PVN-BEN-ID (A)  =                                      ELUPLGRP
00524            PVN-BEN-ID (PVN-BEN-PROVN-IDX)                         ELUPLGRP
00525            IF PVN-BEN-FMT (A) = 'A' OR 'B'                        ELUPLGRP
00526               MOVE  0  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)    ELUPLGRP
00527            ELSE                                                   ELUPLGRP
00528               CONTINUE                                            ELUPLGRP
00529            END-IF                                                 ELUPLGRP
00530         END-IF                                                    ELUPLGRP
00531                                                                   ELUPLGRP
00532         END-PERFORM                                               ELUPLGRP
00533                                                                   ELUPLGRP
00534      END-IF.                                                      ELUPLGRP
00535                                                                   ELUPLGRP
00536  4000-900-EXIT.                                                   ELUPLGRP
00537      EXIT.                                                        ELUPLGRP
00538 /*****************************************************************ELUPLGRP
00539 *                                                                *ELUPLGRP
00540 * 8000    MOVE PROVISION INFORMATION TO PAYMENT LEVEL TABLE      *ELUPLGRP
00541 *                                                                *ELUPLGRP
00542 ******************************************************************ELUPLGRP
00543  8000-000-MOVE-PROVISION        SECTION.                          ELUPLGRP
00544  8000-010.                                                        ELUPLGRP
00545                                                                   ELUPLGRP
00546                                                                   ELUPLGRP
00547      PERFORM 8100-000-MOVE-PROVISION-COMMON.                      ELUPLGRP
00548                                                                   ELUPLGRP
00549      IF  PLT-BEN-PROV-FORMAT (X) = 'A'                            ELUPLGRP
00550          PERFORM 8200-000-MOVE-PROVISION-A                        ELUPLGRP
00551      ELSE                                                         ELUPLGRP
00552      IF  PLT-BEN-PROV-FORMAT (X) = 'B'                            ELUPLGRP
00553          PERFORM 8300-000-MOVE-PROVISION-B                        ELUPLGRP
00554      ELSE                                                         ELUPLGRP
00555      IF  PLT-BEN-PROV-FORMAT (X) = 'C'                            ELUPLGRP
00556          PERFORM 8400-000-MOVE-PROVISION-C                        ELUPLGRP
00557      ELSE                                                         ELUPLGRP
00558      IF  PLT-BEN-PROV-FORMAT (X) = 'D'                            ELUPLGRP
00559          PERFORM 8500-000-MOVE-PROVISION-D                        ELUPLGRP
00560      ELSE                                                         ELUPLGRP
00561      IF  PLT-BEN-PROV-FORMAT (X) = 'E'                            ELUPLGRP
00562          PERFORM 8600-000-MOVE-PROVISION-E                        ELUPLGRP
00563      ELSE                                                         ELUPLGRP
00564      IF  PLT-BEN-PROV-FORMAT (X) = 'W'                            ELUPLGRP
00565          PERFORM 8700-000-MOVE-PROVISION-W.                       ELUPLGRP
00566                                                                   ELUPLGRP
00567  8000-900-EXIT.                                                   ELUPLGRP
00568      EXIT.                                                        ELUPLGRP
00569 /*****************************************************************ELUPLGRP
00570 *                                                                *ELUPLGRP
00571 * 8100  MOVE PROVISION COMMON INFORMATION TO PAYMENT LEVEL TABLE *ELUPLGRP
00572 *                                                                *ELUPLGRP
00573 ******************************************************************ELUPLGRP
00574  8100-000-MOVE-PROVISION-COMMON SECTION.                          ELUPLGRP
00575  8100-010.                                                        ELUPLGRP
00576                                                                   ELUPLGRP
00577      IF     PSP-USE-COUNT                    = '1'                ELUPLGRP
00578        MOVE GCP-USE-COUNT                                         ELUPLGRP
00579          TO PLP-USE-COUNT                    (X Y).               ELUPLGRP
00580                                                                   ELUPLGRP
00581      IF     PSP-PROVN-PRICING-METHD          = '1'                ELUPLGRP
00582        MOVE GCP-PROVN-PRICING-METHD                               ELUPLGRP
00583          TO PLP-PROVN-PRICING-METHD          (X Y).               ELUPLGRP
00584                                                                   ELUPLGRP
00585      IF     PSP-COV-QUALIF                   = '1'                ELUPLGRP
00586        MOVE GCP-COV-QUALIF                                        ELUPLGRP
00587          TO PLP-COV-QUALIF                   (X Y).               ELUPLGRP
00588                                                                   ELUPLGRP
00589      IF     PSP-AGE-LVL-1                    = '1'                ELUPLGRP
00590        MOVE GCP-AGE-LVL-1                                         ELUPLGRP
00591          TO PLP-AGE-LVL-1                    (X Y).               ELUPLGRP
00592                                                                   ELUPLGRP
00593      IF     PSP-DAY-LVL-1                    = '1'                ELUPLGRP
00594        MOVE GCP-DAY-LVL-1                                         ELUPLGRP
00595          TO PLP-DAY-LVL-1                    (X Y).               ELUPLGRP
00596                                                                   ELUPLGRP
00597      IF     PSP-AGE-LVL-2                    = '1'                ELUPLGRP
00598        MOVE GCP-AGE-LVL-2                                         ELUPLGRP
00599          TO PLP-AGE-LVL-2                    (X Y).               ELUPLGRP
00600                                                                   ELUPLGRP
00601      IF     PSP-DAY-LVL-2                    = '1'                ELUPLGRP
00602        MOVE GCP-DAY-LVL-2                                         ELUPLGRP
00603          TO PLP-DAY-LVL-2                    (X Y).               ELUPLGRP
00604                                                                   ELUPLGRP
00605      IF     PSP-TREAT-RESTRN-IND             = '1'                ELUPLGRP
00606        MOVE GCP-TREAT-RESTRN-IND                                  ELUPLGRP
00607          TO PLP-TREAT-RESTRN-IND             (X Y).               ELUPLGRP
00608                                                                   ELUPLGRP
00609      IF     PSP-AGE-COV-TERMN-IND            = '1'                ELUPLGRP
00610        MOVE GCP-AGE-COV-TERMN-IND                                 ELUPLGRP
00611          TO PLP-AGE-COV-TERMN-IND            (X Y).               ELUPLGRP
00612                                                                   ELUPLGRP
00613      IF     PSP-AGE-COV-TERMN                = '1'                ELUPLGRP
00614        MOVE GCP-AGE-COV-TERMN                                     ELUPLGRP
00615          TO PLP-AGE-COV-TERMN                (X Y).               ELUPLGRP
00616                                                                   ELUPLGRP
00617      IF     PSP-AGE-EFF-DT-COMPRSN-IND       = '1'                ELUPLGRP
00618        MOVE GCP-AGE-EFF-DT-COMPRSN-IND                            ELUPLGRP
00619          TO PLP-AGE-EFF-DT-COMPRSN-IND       (X Y).               ELUPLGRP
00620                                                                   ELUPLGRP
00621      IF     PSP-TRANSF-OTHER-RESP-IND        = '1'                ELUPLGRP
00622        MOVE GCP-TRANSF-OTHER-RESP-IND                             ELUPLGRP
00623          TO PLP-TRANSF-OTHER-RESP-IND        (X Y).               ELUPLGRP
00624                                                                   ELUPLGRP
00625      IF     PSP-TRAUM-INJ-EFF-DT-COMP-IND    = '1'                ELUPLGRP
00626        MOVE GCP-TRAUM-INJ-EFF-DT-COMP-IND                         ELUPLGRP
00627          TO PLP-TRAUM-INJ-EFF-DT-COMP-IND    (X Y).               ELUPLGRP
00628                                                                   ELUPLGRP
00629      IF     PSP-COORD-MCARE-IND              = '1'                ELUPLGRP
00630        MOVE GCP-COORD-MCARE-IND                                   ELUPLGRP
00631          TO PLP-COORD-MCARE-IND              (X Y).               ELUPLGRP
00632                                                                   ELUPLGRP
00633      IF     PSP-SPILL-OVER-COINS-APL-IND     = '1'                ELUPLGRP
00634        MOVE GCP-SPILL-OVER-COINS-APL-IND                          ELUPLGRP
00635          TO PLP-SPILL-OVER-COINS-APL-IND     (X Y).               ELUPLGRP
00636                                                                   ELUPLGRP
00637      IF     PSP-SPILL-OVER-DED-APL-IND       = '1'                ELUPLGRP
00638        MOVE GCP-SPILL-OVER-DED-APL-IND                            ELUPLGRP
00639          TO PLP-SPILL-OVER-DED-APL-IND       (X Y).               ELUPLGRP
00640                                                                   ELUPLGRP
00641      IF     PSP-PROV-FILE-ELIG-IND           = '1'                ELUPLGRP
00642        MOVE GCP-PROV-FILE-ELIG-IND                                ELUPLGRP
00643          TO PLP-PROV-FILE-ELIG-IND           (X Y).               ELUPLGRP
00644                                                                   ELUPLGRP
00645      IF     PSP-CRDT-CARD-PROCS-IND          = '1'                ELUPLGRP
00646        MOVE GCP-CRDT-CARD-PROCS-IND                               ELUPLGRP
00647          TO PLP-CRDT-CARD-PROCS-IND          (X Y).               ELUPLGRP
00648                                                                   ELUPLGRP
00649      IF     PSP-DENT-SURG-PMT-ELG-IP-IND     = '1'                ELUPLGRP
00650        MOVE GCP-DENT-SURG-PMT-ELG-IP-IND                          ELUPLGRP
00651          TO PLP-DENT-SURG-PMT-ELG-IP-IND     (X Y).               ELUPLGRP
00652                                                                   ELUPLGRP
00653      IF     PSP-DENT-SURG-PMT-ELG-OP-IND     = '1'                ELUPLGRP
00654        MOVE GCP-DENT-SURG-PMT-ELG-OP-IND                          ELUPLGRP
00655          TO PLP-DENT-SURG-PMT-ELG-OP-IND     (X Y).               ELUPLGRP
00656                                                                   ELUPLGRP
00657      IF     PSP-ADDITIONAL-PRICING-PRCNT     = '1'                ELUPLGRP
00658        MOVE GCP-ADDITIONAL-PRICING-PRCNT                          ELUPLGRP
00659          TO PLP-ADDITIONAL-PRICING-PRCNT     (X Y).               ELUPLGRP
00660                                                                   ELUPLGRP
00661      IF     PSP-VARIABLE-INDEMNITY-PRCNT     = '1'                ELUPLGRP
00662        MOVE GCP-VARIABLE-INDEMNITY-PRCNT                          ELUPLGRP
00663          TO PLP-VARIABLE-INDEMNITY-PRCNT     (X Y).               ELUPLGRP
00664                                                                   ELUPLGRP
00665      IF     PSP-SPILL-OVR-RM-F-RT-APL-IND    = '1'                ELUPLGRP
00666        MOVE GCP-SPILL-OVR-RM-F-RT-APL-IND                         ELUPLGRP
00667          TO PLP-SPILL-OVR-RM-F-RT-APL-IND    (X Y).               ELUPLGRP
00668                                                                   ELUPLGRP
00669      IF     PSP-PLACE-TREAT-ELIG-IND         = '1'                ELUPLGRP
00670        MOVE GCP-PLACE-TREAT-ELIG-IND                              ELUPLGRP
00671          TO PLP-PLACE-TREAT-ELIG-IND         (X Y).               ELUPLGRP
00672                                                                   ELUPLGRP
00673      IF     PSP-COSM-SURG-PAYMT-IND          = '1'                ELUPLGRP
00674        MOVE GCP-COSM-SURG-PAYMT-IND                               ELUPLGRP
00675          TO PLP-COSM-SURG-PAYMT-IND          (X Y).               ELUPLGRP
00676                                                                   ELUPLGRP
00677      IF     PSP-CONG-DFCT-SURG-PMT-ELG       = '1'                ELUPLGRP
00678        MOVE GCP-CONG-DFCT-SURG-PMT-ELG                            ELUPLGRP
00679          TO PLP-CONG-DFCT-SURG-PMT-ELG       (X Y).               ELUPLGRP
00680                                                                   ELUPLGRP
00681      IF     PSP-SERV-NECESRY-CORP-BIT-IND    = '1'                ELUPLGRP
00682        MOVE GCP-SERV-NECESRY-CORP-BIT-IND                         ELUPLGRP
00683          TO PLP-SERV-NECESRY-CORP-BIT-IND    (X Y).               ELUPLGRP
00684                                                                   ELUPLGRP
00685      IF     PSP-CERTFN-REQRM-IND             = '1'                ELUPLGRP
00686        MOVE GCP-CERTFN-REQRM-IND                                  ELUPLGRP
00687          TO PLP-CERTFN-REQRM-IND             (X Y).               ELUPLGRP
00688                                                                   ELUPLGRP
00689      IF     PSP-COST-CONT-PYMT-ELIG-IND      = '1'                ELUPLGRP
00690        MOVE GCP-COST-CONT-PYMT-ELIG-IND                           ELUPLGRP
00691          TO PLP-COST-CONT-PYMT-ELIG-IND      (X Y).               ELUPLGRP
00692                                                                   ELUPLGRP
00693      IF    PSP-BEN-TAB-PROVN-ID-AAR         = '1'                 ELUPLGRP
00694            SET GCP-INDEX TO +1                                    ELUPLGRP
00695            SEARCH    GCP-BEN-TAB-PROVN-ID                         ELUPLGRP
00696              AT END  MOVE SPACES                                  ELUPLGRP
00697                        TO PLP-BEN-TAB-PROVN-ID-AAR(X Y)           ELUPLGRP
00698                WHEN  GCP-BP-ID (GCP-INDEX) = '#AAR  '             ELUPLGRP
00699                      MOVE GCP-BEN-TAB-PROVN-ID (GCP-INDEX)        ELUPLGRP
00700                        TO PLP-BEN-TAB-PROVN-ID-AAR(X Y).          ELUPLGRP
00701                                                                   ELUPLGRP
00702      IF    PSP-BEN-TAB-PROVN-ID-ABM         = '1'                 ELUPLGRP
00703            SET GCP-INDEX TO +1                                    ELUPLGRP
00704            SEARCH    GCP-BEN-TAB-PROVN-ID                         ELUPLGRP
00705              AT END  MOVE SPACES                                  ELUPLGRP
00706                        TO PLP-BEN-TAB-PROVN-ID-ABM(X Y)           ELUPLGRP
00707                WHEN  GCP-BP-ID (GCP-INDEX) = '#ABM  '             ELUPLGRP
00708                      MOVE GCP-BEN-TAB-PROVN-ID (GCP-INDEX)        ELUPLGRP
00709                        TO PLP-BEN-TAB-PROVN-ID-ABM(X Y).          ELUPLGRP
00710                                                                   ELUPLGRP
00711      IF    PSP-BEN-TAB-PROVN-ID-ACL         = '1'                 ELUPLGRP
00712            SET GCP-INDEX TO +1                                    ELUPLGRP
00713            SEARCH    GCP-BEN-TAB-PROVN-ID                         ELUPLGRP
00714              AT END  MOVE SPACES                                  ELUPLGRP
00715                        TO PLP-BEN-TAB-PROVN-ID-ACL(X Y)           ELUPLGRP
00716                WHEN  GCP-BP-ID (GCP-INDEX) = '#ACL  '             ELUPLGRP
00717                      MOVE GCP-BEN-TAB-PROVN-ID (GCP-INDEX)        ELUPLGRP
00718                        TO PLP-BEN-TAB-PROVN-ID-ACL(X Y).          ELUPLGRP
00719                                                                   ELUPLGRP
00720      IF    PSP-BEN-TAB-PROVN-ID-ADL         = '1'                 ELUPLGRP
00721            SET GCP-INDEX TO +1                                    ELUPLGRP
00722            SEARCH    GCP-BEN-TAB-PROVN-ID                         ELUPLGRP
00723              AT END  MOVE SPACES                                  ELUPLGRP
00724                        TO PLP-BEN-TAB-PROVN-ID-ADL(X Y)           ELUPLGRP
00725                WHEN  GCP-BP-ID (GCP-INDEX) = '#ADL  '             ELUPLGRP
00726                      MOVE GCP-BEN-TAB-PROVN-ID (GCP-INDEX)        ELUPLGRP
00727                        TO PLP-BEN-TAB-PROVN-ID-ADL(X Y).          ELUPLGRP
00728                                                                   ELUPLGRP
00729      IF    PSP-BEN-TAB-PROVN-ID-AOL         = '1'                 ELUPLGRP
00730            SET GCP-INDEX TO +1                                    ELUPLGRP
00731            SEARCH    GCP-BEN-TAB-PROVN-ID                         ELUPLGRP
00732              AT END  MOVE SPACES                                  ELUPLGRP
00733                        TO PLP-BEN-TAB-PROVN-ID-AOL(X Y)           ELUPLGRP
00734                WHEN  GCP-BP-ID (GCP-INDEX) = '#AOL  '             ELUPLGRP
00735                      MOVE GCP-BEN-TAB-PROVN-ID (GCP-INDEX)        ELUPLGRP
00736                        TO PLP-BEN-TAB-PROVN-ID-AOL(X Y).          ELUPLGRP
00737                                                                   ELUPLGRP
00738      IF    PSP-BEN-TAB-PROVN-ID-PPF         = '1'                 ELUPLGRP
00739            SET GCP-INDEX TO +1                                    ELUPLGRP
00740            SEARCH    GCP-BEN-TAB-PROVN-ID                         ELUPLGRP
00741              AT END  MOVE SPACES                                  ELUPLGRP
00742                        TO PLP-BEN-TAB-PROVN-ID-PPF(X Y)           ELUPLGRP
00743                WHEN  GCP-BP-ID (GCP-INDEX) = '#PPF  '             ELUPLGRP
00744                      MOVE GCP-BEN-TAB-PROVN-ID (GCP-INDEX)        ELUPLGRP
00745                        TO PLP-BEN-TAB-PROVN-ID-PPF(X Y).          ELUPLGRP
00746                                                                   ELUPLGRP
00747      IF    PSP-BEN-TAB-PROVN-ID-PVE         = '1'                 ELUPLGRP
00748            SET GCP-INDEX TO +1                                    ELUPLGRP
00749            SEARCH    GCP-BEN-TAB-PROVN-ID                         ELUPLGRP
00750              AT END  MOVE SPACES                                  ELUPLGRP
00751                        TO PLP-BEN-TAB-PROVN-ID-PVE(X Y)           ELUPLGRP
00752                WHEN  GCP-BP-ID (GCP-INDEX) = '#PVE  '             ELUPLGRP
00753                      MOVE GCP-BEN-TAB-PROVN-ID (GCP-INDEX)        ELUPLGRP
00754                        TO PLP-BEN-TAB-PROVN-ID-PVE(X Y).          ELUPLGRP
00755                                                                   ELUPLGRP
00756  8100-900-EXIT.                                                   ELUPLGRP
00757      EXIT.                                                        ELUPLGRP
00758 /*****************************************************************ELUPLGRP
00759 *                                                                *ELUPLGRP
00760 * 8200  MOVE PROVISION \
00761 *                                                                *ELUPLGRP
00762 ******************************************************************ELUPLGRP
00763  8200-000-MOVE-PROVISION-A      SECTION.                          ELUPLGRP
00764  8200-010.                                                        ELUPLGRP
00765                                                                   ELUPLGRP
00766      IF     PSA-HOSP-ADM-RESTRN-IND          = '1'                ELUPLGRP
00767        MOVE GPA-HOSP-ADM-RESTRN-IND                               ELUPLGRP
00768          TO PLA-HOSP-ADM-RESTRN-IND          (X Y).               ELUPLGRP
00769                                                                   ELUPLGRP
00770      IF     PSA-STAY-CD                      = '1'                ELUPLGRP
00771        MOVE GPA-STAY-CD                                           ELUPLGRP
00772          TO PLA-STAY-CD                      (X Y).               ELUPLGRP
00773                                                                   ELUPLGRP
00774      IF     PSA-HOSP-COND-RELATSP-IND        = '1'                ELUPLGRP
00775        MOVE GPA-HOSP-COND-RELATSP-IND                             ELUPLGRP
00776          TO PLA-HOSP-COND-RELATSP-IND        (X Y).               ELUPLGRP
00777                                                                   ELUPLGRP
00778      IF     PSA-REHAB-ADM-RESTRN-IND         = '1'                ELUPLGRP
00779        MOVE GPA-REHAB-ADM-RESTRN-IND                              ELUPLGRP
00780          TO PLA-REHAB-ADM-RESTRN-IND         (X Y).               ELUPLGRP
00781                                                                   ELUPLGRP
00782      IF     PSA-DAYS-RDCN-RAT-IND            = '1'                ELUPLGRP
00783        MOVE GPA-DAYS-RDCN-RAT-IND                                 ELUPLGRP
00784          TO PLA-DAYS-RDCN-RAT-IND            (X Y).               ELUPLGRP
00785                                                                   ELUPLGRP
00786      IF     PSA-DAYS-RDCN-RAT-BASIC-APL      = '1'                ELUPLGRP
00787        MOVE GPA-DAYS-RDCN-RAT-BASIC-APL                           ELUPLGRP
00788          TO PLA-DAYS-RDCN-RAT-BASIC-APL      (X Y).               ELUPLGRP
00789                                                                   ELUPLGRP
00790      IF     PSA-DAYS-RDCN-RAT-BASIC-BASE     = '1'                ELUPLGRP
00791        MOVE GPA-DAYS-RDCN-RAT-BASIC-BASE                          ELUPLGRP
00792          TO PLA-DAYS-RDCN-RAT-BASIC-BASE     (X Y).               ELUPLGRP
00793                                                                   ELUPLGRP
00794      IF     PSA-DAYS-RDCN-RAT-SEC-APL        = '1'                ELUPLGRP
00795        MOVE GPA-DAYS-RDCN-RAT-SEC-APL                             ELUPLGRP
00796          TO PLA-DAYS-RDCN-RAT-SEC-APL        (X Y).               ELUPLGRP
00797                                                                   ELUPLGRP
00798      IF     PSA-DAYS-RDCN-RAT-SEC-BASE       = '1'                ELUPLGRP
00799        MOVE GPA-DAYS-RDCN-RAT-SEC-BASE                            ELUPLGRP
00800          TO PLA-DAYS-RDCN-RAT-SEC-BASE       (X Y).               ELUPLGRP
00801                                                                   ELUPLGRP
00802      IF     PSA-FLAT-RATE-PDM-AMT            = '1'                ELUPLGRP
00803        MOVE GPA-FLAT-RATE-PDM-AMT                                 ELUPLGRP
00804          TO PLA-FLAT-RATE-PDM-AMT            (X Y).               ELUPLGRP
00805                                                                   ELUPLGRP
00806      IF     PSA-CERTFN-REPETN-REQRM-IND      = '1'                ELUPLGRP
00807        MOVE GPA-CERTFN-REPETN-REQRM-IND                           ELUPLGRP
00808          TO PLA-CERTFN-REPETN-REQRM-IND      (X Y).               ELUPLGRP
00809                                                                   ELUPLGRP
00810      IF     PSA-ADDN-ALLOW-AMT-PER-DAY       = '1'                ELUPLGRP
00811        MOVE GPA-ADDN-ALLOW-AMT-PER-DAY                            ELUPLGRP
00812          TO PLA-ADDN-ALLOW-AMT-PER-DAY       (X Y).               ELUPLGRP
00813                                                                   ELUPLGRP
00814      IF     PSA-HSP-ADM-RESTRN-DAYS          = '1'                ELUPLGRP
00815        MOVE GPA-HSP-ADM-RESTRN-DAYS                               ELUPLGRP
00816          TO PLA-HSP-ADM-RESTRN-DAYS          (X Y).               ELUPLGRP
00817                                                                   ELUPLGRP
00818      IF     PSA-NORM-NWBORN-OVRD-IND         = '1'                ELUPLGRP
00819        MOVE GPA-NORM-NWBORN-OVRD-IND                              ELUPLGRP
00820          TO PLA-NORM-NWBORN-OVRD-IND         (X Y).               ELUPLGRP
00821                                                                   ELUPLGRP
00822      IF     PSA-DRUG-ELIG-MEMB-CLS-OVRD      = '1'                ELUPLGRP
00823        MOVE GPA-DRUG-ELIG-MEMB-CLS-OVRD                           ELUPLGRP
00824          TO PLA-DRUG-ELIG-MEMB-CLS-OVRD      (X Y).               ELUPLGRP
00825                                                                   ELUPLGRP
00826      IF     PSA-ALCO-ELIG-MEMB-CLS-OVRD      = '1'                ELUPLGRP
00827        MOVE GPA-ALCO-ELIG-MEMB-CLS-OVRD                           ELUPLGRP
00828          TO PLA-ALCO-ELIG-MEMB-CLS-OVRD      (X Y).               ELUPLGRP
00829                                                                   ELUPLGRP
00830      IF     PSA-ECF-SNF-OVRD-IND             = '1'                ELUPLGRP
00831        MOVE GPA-ECF-SNF-OVRD-IND                                  ELUPLGRP
00832          TO PLA-ECF-SNF-OVRD-IND             (X Y).               ELUPLGRP
00833                                                                   ELUPLGRP
00834      IF     PSA-TRANSSXL-PMT-RESTR-OVRD      = '1'                ELUPLGRP
00835        MOVE GPA-TRANSSXL-PMT-RESTR-OVRD                           ELUPLGRP
00836          TO PLA-TRANSSXL-PMT-RESTR-OVRD      (X Y).               ELUPLGRP
00837                                                                   ELUPLGRP
00838      IF     PSA-ECF-F-RAT-PER-DIEM-AMT       = '1'                ELUPLGRP
00839        MOVE GPA-ECF-F-RAT-PER-DIEM-AMT                            ELUPLGRP
00840          TO PLA-ECF-F-RAT-PER-DIEM-AMT       (X Y).               ELUPLGRP
00841                                                                   ELUPLGRP
00842      IF     PSA-STAY-CODE-IND                = '1'                ELUPLGRP
00843        MOVE GPA-STAY-CODE-IND                                     ELUPLGRP
00844          TO PLA-STAY-CODE-IND                (X Y).               ELUPLGRP
00845                                                                   ELUPLGRP
00846      IF     PSA-PHYS-EXAM-IND                = '1'                ELUPLGRP
00847        MOVE GPA-PHYS-EXAM-IND                                     ELUPLGRP
00848          TO PLA-PHYS-EXAM-IND                (X Y).               ELUPLGRP
00849                                                                   ELUPLGRP
00850  8200-900-EXIT.                                                   ELUPLGRP
00851      EXIT.                                                        ELUPLGRP
00852 /*****************************************************************ELUPLGRP
00853 *                                                                *ELUPLGRP
00854 * 8300  MOVE PROVISION \
00855 *                                                                *ELUPLGRP
00856 ******************************************************************ELUPLGRP
00857  8300-000-MOVE-PROVISION-B      SECTION.                          ELUPLGRP
00858  8300-010.                                                        ELUPLGRP
00859                                                                   ELUPLGRP
00860      IF     PSB-HOSP-ADM-RESTRN-IND          = '1'                ELUPLGRP
00861        MOVE GPB-HOSP-ADM-RESTRN-IND                               ELUPLGRP
00862          TO PLB-HOSP-ADM-RESTRN-IND          (X Y).               ELUPLGRP
00863                                                                   ELUPLGRP
00864      IF     PSB-STAY-CD                      = '1'                ELUPLGRP
00865        MOVE GPB-STAY-CD                                           ELUPLGRP
00866          TO PLB-STAY-CD                      (X Y).               ELUPLGRP
00867                                                                   ELUPLGRP
00868      IF     PSB-HOSP-COND-RELATSP-IND        = '1'                ELUPLGRP
00869        MOVE GPB-HOSP-COND-RELATSP-IND                             ELUPLGRP
00870          TO PLB-HOSP-COND-RELATSP-IND        (X Y).               ELUPLGRP
00871                                                                   ELUPLGRP
00872      IF     PSB-REHAB-ADM-RESTRN-IND         = '1'                ELUPLGRP
00873        MOVE GPB-REHAB-ADM-RESTRN-IND                              ELUPLGRP
00874          TO PLB-REHAB-ADM-RESTRN-IND         (X Y).               ELUPLGRP
00875                                                                   ELUPLGRP
00876      IF     PSB-TREAT-TIME-FACTOR-IND        = '1'                ELUPLGRP
00877        MOVE GPB-TREAT-TIME-FACTOR-IND                             ELUPLGRP
00878          TO PLB-TREAT-TIME-FACTOR-IND        (X Y).               ELUPLGRP
00879                                                                   ELUPLGRP
00880      IF     PSB-TREAT-TIME-FACTOR            = '1'                ELUPLGRP
00881        MOVE GPB-TREAT-TIME-FACTOR                                 ELUPLGRP
00882          TO PLB-TREAT-TIME-FACTOR            (X Y).               ELUPLGRP
00883                                                                   ELUPLGRP
00884      IF     PSB-ELIG-METHD-OF-TREAT-IND      = '1'                ELUPLGRP
00885        MOVE GPB-ELIG-METHD-OF-TREAT-IND                           ELUPLGRP
00886          TO PLB-ELIG-METHD-OF-TREAT-IND      (X Y).               ELUPLGRP
00887                                                                   ELUPLGRP
00888      IF     PSB-REPR-REPLAC-RESTRN-IND       = '1'                ELUPLGRP
00889        MOVE GPB-REPR-REPLAC-RESTRN-IND                            ELUPLGRP
00890          TO PLB-REPR-REPLAC-RESTRN-IND       (X Y).               ELUPLGRP
00891                                                                   ELUPLGRP
00892      IF     PSB-CERTN-REPETN-REQRD-IND       = '1'                ELUPLGRP
00893        MOVE GPB-CERTN-REPETN-REQRD-IND                            ELUPLGRP
00894          TO PLB-CERTN-REPETN-REQRD-IND       (X Y).               ELUPLGRP
00895                                                                   ELUPLGRP
00896      IF     PSB-PHYS-EXAM-IND                = '1'                ELUPLGRP
00897        MOVE GPB-PHYS-EXAM-IND                                     ELUPLGRP
00898          TO PLB-PHYS-EXAM-IND                (X Y).               ELUPLGRP
00899                                                                   ELUPLGRP
00900      IF     PSB-HSP-ADM-RESTRN-DAYS          = '1'                ELUPLGRP
00901        MOVE GPB-HSP-ADM-RESTRN-DAYS                               ELUPLGRP
00902          TO PLB-HSP-ADM-RESTRN-DAYS          (X Y).               ELUPLGRP
00903                                                                   ELUPLGRP
00904      IF     PSB-PROF-CHRG-HSP-CLM            = '1'                ELUPLGRP
00905        MOVE GPB-PROF-CHRG-HSP-CLM                                 ELUPLGRP
00906          TO PLB-PROF-CHRG-HSP-CLM            (X Y).               ELUPLGRP
00907                                                                   ELUPLGRP
00908      IF     PSB-AMBULANCE-ELIG-IND           = '1'                ELUPLGRP
00909        MOVE GPB-AMBULANCE-ELIG-IND                                ELUPLGRP
00910          TO PLB-AMBULANCE-ELIG-IND           (X Y).               ELUPLGRP
00911                                                                   ELUPLGRP
00912      IF     PSB-NORM-NWBORN-OVRD-IND         = '1'                ELUPLGRP
00913        MOVE GPB-NORM-NWBORN-OVRD-IND                              ELUPLGRP
00914          TO PLB-NORM-NWBORN-OVRD-IND         (X Y).               ELUPLGRP
00915                                                                   ELUPLGRP
00916      IF     PSB-DRUG-ELIG-MEMB-CLS-OVRD      = '1'                ELUPLGRP
00917        MOVE GPB-DRUG-ELIG-MEMB-CLS-OVRD                           ELUPLGRP
00918          TO PLB-DRUG-ELIG-MEMB-CLS-OVRD      (X Y).               ELUPLGRP
00919                                                                   ELUPLGRP
00920      IF     PSB-ALCO-ELIG-MEMB-CLS-OVRD      = '1'                ELUPLGRP
00921        MOVE GPB-ALCO-ELIG-MEMB-CLS-OVRD                           ELUPLGRP
00922          TO PLB-ALCO-ELIG-MEMB-CLS-OVRD      (X Y).               ELUPLGRP
00923                                                                   ELUPLGRP
00924      IF     PSB-ECF-SNF-OVRD-IND             = '1'                ELUPLGRP
00925        MOVE GPB-ECF-SNF-OVRD-IND                                  ELUPLGRP
00926          TO PLB-ECF-SNF-OVRD-IND             (X Y).               ELUPLGRP
00927                                                                   ELUPLGRP
00928      IF     PSB-TRANSSXL-PMT-RESTR-OVRD      = '1'                ELUPLGRP
00929        MOVE GPB-TRANSSXL-PMT-RESTR-OVRD                           ELUPLGRP
00930          TO PLB-TRANSSXL-PMT-RESTR-OVRD      (X Y).               ELUPLGRP
00931                                                                   ELUPLGRP
00932      IF     PSB-STAY-CODE-IND                = '1'                ELUPLGRP
00933        MOVE GPB-STAY-CODE-IND                                     ELUPLGRP
00934          TO PLB-STAY-CODE-IND                (X Y).               ELUPLGRP
00935                                                                   ELUPLGRP
00936      IF     PSB-MAX-AMT-PER-VISIT            = '1'                ELUPLGRP
00937        MOVE GPB-MAX-AMT-PER-VISIT                                 ELUPLGRP
00938          TO PLB-MAX-AMT-PER-VISIT            (X Y).               ELUPLGRP
00939                                                                   ELUPLGRP
00940  8300-900-EXIT.                                                   ELUPLGRP
00941      EXIT.                                                        ELUPLGRP
00942 /*****************************************************************ELUPLGRP
00943 *                                                                *ELUPLGRP
00944 * 8400  MOVE PROVISION \
00945 *                                                                *ELUPLGRP
00946 ******************************************************************ELUPLGRP
00947  8400-000-MOVE-PROVISION-C      SECTION.                          ELUPLGRP
00948  8400-010.                                                        ELUPLGRP
00949                                                                   ELUPLGRP
00950      IF     PSC-BEN-SCOPE-ID                 = '1'                ELUPLGRP
00951        MOVE GPC-BEN-SCOPE-ID                                      ELUPLGRP
00952          TO PLC-BEN-SCOPE-ID                 (X Y).               ELUPLGRP
00953                                                                   ELUPLGRP
00954      IF     PSC-EXCP-SCHED-ID                = '1'                ELUPLGRP
00955        MOVE GPC-EXCP-SCHED-ID                                     ELUPLGRP
00956          TO PLC-EXCP-SCHED-ID                (X Y).               ELUPLGRP
00957                                                                   ELUPLGRP
00958      IF     PSC-ELIG-METHD-OF-TREAT-IND      = '1'                ELUPLGRP
00959        MOVE GPC-ELIG-METHD-OF-TREAT-IND                           ELUPLGRP
00960          TO PLC-ELIG-METHD-OF-TREAT-IND      (X Y).               ELUPLGRP
00961                                                                   ELUPLGRP
00962      IF     PSC-MULT-REL-PROC-IND            = '1'                ELUPLGRP
00963        MOVE GPC-MULT-REL-PROC-IND                                 ELUPLGRP
00964          TO PLC-MULT-REL-PROC-IND            (X Y).               ELUPLGRP
00965                                                                   ELUPLGRP
00966      IF     PSC-PRIM-SURG-DEPEND-IND         = '1'                ELUPLGRP
00967        MOVE GPC-PRIM-SURG-DEPEND-IND                              ELUPLGRP
00968          TO PLC-PRIM-SURG-DEPEND-IND         (X Y).               ELUPLGRP
00969                                                                   ELUPLGRP
00970      IF     PSC-HOSP-STAFF-PROV-IND          = '1'                ELUPLGRP
00971        MOVE GPC-HOSP-STAFF-PROV-IND                               ELUPLGRP
00972          TO PLC-HOSP-STAFF-PROV-IND          (X Y).               ELUPLGRP
00973                                                                   ELUPLGRP
00974      IF     PSC-REPEAT-PROC-IND              = '1'                ELUPLGRP
00975        MOVE GPC-REPEAT-PROC-IND                                   ELUPLGRP
00976          TO PLC-REPEAT-PROC-IND              (X Y).               ELUPLGRP
00977                                                                   ELUPLGRP
00978      IF     PSC-MULT-INJ-PRICING-MOD-IND     = '1'                ELUPLGRP
00979        MOVE GPC-MULT-INJ-PRICING-MOD-IND                          ELUPLGRP
00980          TO PLC-MULT-INJ-PRICING-MOD-IND     (X Y).               ELUPLGRP
00981                                                                   ELUPLGRP
00982      IF     PSC-MIN-ELIG-AMT                 = '1'                ELUPLGRP
00983        MOVE GPC-MIN-ELIG-AMT                                      ELUPLGRP
00984          TO PLC-MIN-ELIG-AMT                 (X Y).               ELUPLGRP
00985                                                                   ELUPLGRP
00986      IF     PSC-SURG-MULT-PROC-PRICE-IND     = '1'                ELUPLGRP
00987        MOVE GPC-SURG-MULT-PROC-PRICE-IND                          ELUPLGRP
00988          TO PLC-SURG-MULT-PROC-PRICE-IND     (X Y).               ELUPLGRP
00989                                                                   ELUPLGRP
00990      IF     PSC-PRIM-SURG-DPD-PAY-PCT        = '1'                ELUPLGRP
00991        MOVE GPC-PRIM-SURG-DPD-PAY-PCT                             ELUPLGRP
00992          TO PLC-PRIM-SURG-DPD-PAY-PCT        (X Y).               ELUPLGRP
00993                                                                   ELUPLGRP
00994      IF     PSC-MULT-UNRL-PROC-1-PCT         = '1'                ELUPLGRP
00995        MOVE GPC-MULT-UNRL-PROC-1-PCT                              ELUPLGRP
00996          TO PLC-MULT-UNRL-PROC-1-PCT         (X Y).               ELUPLGRP
00997                                                                   ELUPLGRP
00998      IF     PSC-MULT-UNRL-1-NO-OCCUR         = '1'                ELUPLGRP
00999        MOVE GPC-MULT-UNRL-1-NO-OCCUR                              ELUPLGRP
01000          TO PLC-MULT-UNRL-1-NO-OCCUR         (X Y).               ELUPLGRP
01001                                                                   ELUPLGRP
01002      IF     PSC-MULT-UNRL-PROC-2-PCT         = '1'                ELUPLGRP
01003        MOVE GPC-MULT-UNRL-PROC-2-PCT                              ELUPLGRP
01004          TO PLC-MULT-UNRL-PROC-2-PCT         (X Y).               ELUPLGRP
01005                                                                   ELUPLGRP
01006      IF     PSC-MULT-UNRL-2-NO-OCCUR         = '1'                ELUPLGRP
01007        MOVE GPC-MULT-UNRL-2-NO-OCCUR                              ELUPLGRP
01008          TO PLC-MULT-UNRL-2-NO-OCCUR         (X Y).               ELUPLGRP
01009                                                                   ELUPLGRP
01010      IF     PSC-MULT-RL-PROC-1-PCT           = '1'                ELUPLGRP
01011        MOVE GPC-MULT-RL-PROC-1-PCT                                ELUPLGRP
01012          TO PLC-MULT-RL-PROC-1-PCT           (X Y).               ELUPLGRP
01013                                                                   ELUPLGRP
01014      IF     PSC-MULT-RL-1-NO-OCCUR           = '1'                ELUPLGRP
01015        MOVE GPC-MULT-RL-1-NO-OCCUR                                ELUPLGRP
01016          TO PLC-MULT-RL-1-NO-OCCUR           (X Y).               ELUPLGRP
01017                                                                   ELUPLGRP
01018      IF     PSC-MULT-RL-PROC-2-PCT           = '1'                ELUPLGRP
01019        MOVE GPC-MULT-RL-PROC-2-PCT                                ELUPLGRP
01020          TO PLC-MULT-RL-PROC-2-PCT           (X Y).               ELUPLGRP
01021                                                                   ELUPLGRP
01022      IF     PSC-MULT-RL-2-NO-OCCUR           = '1'                ELUPLGRP
01023        MOVE GPC-MULT-RL-2-NO-OCCUR                                ELUPLGRP
01024          TO PLC-MULT-RL-2-NO-OCCUR           (X Y).               ELUPLGRP
01025                                                                   ELUPLGRP
01026      IF     PSC-MULT-INJ-LVL-1-PCT           = '1'                ELUPLGRP
01027        MOVE GPC-MULT-INJ-LVL-1-PCT                                ELUPLGRP
01028          TO PLC-MULT-INJ-LVL-1-PCT           (X Y).               ELUPLGRP
01029                                                                   ELUPLGRP
01030      IF     PSC-MULT-INJ-1-NO-OCCUR          = '1'                ELUPLGRP
01031        MOVE GPC-MULT-INJ-1-NO-OCCUR                               ELUPLGRP
01032          TO PLC-MULT-INJ-1-NO-OCCUR          (X Y).               ELUPLGRP
01033                                                                   ELUPLGRP
01034      IF     PSC-MULT-INJ-LVL-2-PCT           = '1'                ELUPLGRP
01035        MOVE GPC-MULT-INJ-LVL-2-PCT                                ELUPLGRP
01036          TO PLC-MULT-INJ-LVL-2-PCT           (X Y).               ELUPLGRP
01037                                                                   ELUPLGRP
01038      IF     PSC-MULT-INJ-2-NO-OCCUR          = '1'                ELUPLGRP
01039        MOVE GPC-MULT-INJ-2-NO-OCCUR                               ELUPLGRP
01040          TO PLC-MULT-INJ-2-NO-OCCUR          (X Y).               ELUPLGRP
01041                                                                   ELUPLGRP
01042      IF     PSC-MULT-POD-PROC-PRICE-IND      = '1'                ELUPLGRP
01043        MOVE GPC-MULT-POD-PROC-PRICE-IND                           ELUPLGRP
01044          TO PLC-MULT-POD-PROC-PRICE-IND      (X Y).               ELUPLGRP
01045                                                                   ELUPLGRP
01046      IF     PSC-MULT-POD-PROC-1-PCT          = '1'                ELUPLGRP
01047        MOVE GPC-MULT-POD-PROC-1-PCT                               ELUPLGRP
01048          TO PLC-MULT-POD-PROC-1-PCT          (X Y).               ELUPLGRP
01049                                                                   ELUPLGRP
01050      IF     PSC-MULT-POD-1-NO-OCCUR          = '1'                ELUPLGRP
01051        MOVE GPC-MULT-POD-1-NO-OCCUR                               ELUPLGRP
01052          TO PLC-MULT-POD-1-NO-OCCUR          (X Y).               ELUPLGRP
01053                                                                   ELUPLGRP
01054      IF     PSC-MULT-POD-PROC-2-PCT          = '1'                ELUPLGRP
01055        MOVE GPC-MULT-POD-PROC-2-PCT                               ELUPLGRP
01056          TO PLC-MULT-POD-PROC-2-PCT          (X Y).               ELUPLGRP
01057                                                                   ELUPLGRP
01058      IF     PSC-MULT-POD-2-NO-OCCUR          = '1'                ELUPLGRP
01059        MOVE GPC-MULT-POD-2-NO-OCCUR                               ELUPLGRP
01060          TO PLC-MULT-POD-2-NO-OCCUR          (X Y).               ELUPLGRP
01061                                                                   ELUPLGRP
01062      IF     PSC-MULT-POD-PROC-3-PCT          = '1'                ELUPLGRP
01063        MOVE GPC-MULT-POD-PROC-3-PCT                               ELUPLGRP
01064          TO PLC-MULT-POD-PROC-3-PCT          (X Y).               ELUPLGRP
01065                                                                   ELUPLGRP
01066      IF     PSC-MULT-POD-3-NO-OCCUR          = '1'                ELUPLGRP
01067        MOVE GPC-MULT-POD-3-NO-OCCUR                               ELUPLGRP
01068          TO PLC-MULT-POD-3-NO-OCCUR          (X Y).               ELUPLGRP
01069                                                                   ELUPLGRP
01070      IF     PSC-MULT-POD-PROC-4-PCT          = '1'                ELUPLGRP
01071        MOVE GPC-MULT-POD-PROC-4-PCT                               ELUPLGRP
01072          TO PLC-MULT-POD-PROC-4-PCT          (X Y).               ELUPLGRP
01073                                                                   ELUPLGRP
01074      IF     PSC-MULT-POD-4-NO-OCCUR          = '1'                ELUPLGRP
01075        MOVE GPC-MULT-POD-4-NO-OCCUR                               ELUPLGRP
01076          TO PLC-MULT-POD-4-NO-OCCUR          (X Y).               ELUPLGRP
01077                                                                   ELUPLGRP
01078      IF     PSC-CORRIDOR-OVERRIDE            = '1'                ELUPLGRP
01079        MOVE GPC-CORRIDOR-OVERRIDE                                 ELUPLGRP
01080          TO PLC-CORRIDOR-OVERRIDE            (X Y).               ELUPLGRP
01081                                                                   ELUPLGRP
01082  8400-900-EXIT.                                                   ELUPLGRP
01083      EXIT.                                                        ELUPLGRP
01084 /*****************************************************************ELUPLGRP
01085 *                                                                *ELUPLGRP
01086 * 8500  MOVE PROVISION \
01087 *                                                                *ELUPLGRP
01088 ******************************************************************ELUPLGRP
01089  8500-000-MOVE-PROVISION-D      SECTION.                          ELUPLGRP
01090  8500-010.                                                        ELUPLGRP
01091                                                                   ELUPLGRP
01092      IF     PSD-BEN-SCOPE-ID                 = '1'                ELUPLGRP
01093        MOVE GPD-BEN-SCOPE-ID                                      ELUPLGRP
01094          TO PLD-BEN-SCOPE-ID                 (X Y).               ELUPLGRP
01095                                                                   ELUPLGRP
01096      IF     PSD-EXCP-SCHED-ID                = '1'                ELUPLGRP
01097        MOVE GPD-EXCP-SCHED-ID                                     ELUPLGRP
01098          TO PLD-EXCP-SCHED-ID                (X Y).               ELUPLGRP
01099                                                                   ELUPLGRP
01100      IF     PSD-HOSP-ADM-RESTRN-IND          = '1'                ELUPLGRP
01101        MOVE GPD-HOSP-ADM-RESTRN-IND                               ELUPLGRP
01102          TO PLD-HOSP-ADM-RESTRN-IND          (X Y).               ELUPLGRP
01103                                                                   ELUPLGRP
01104      IF     PSD-STAY-CD                      = '1'                ELUPLGRP
01105        MOVE GPD-STAY-CD                                           ELUPLGRP
01106          TO PLD-STAY-CD                      (X Y).               ELUPLGRP
01107                                                                   ELUPLGRP
01108      IF     PSD-HOSP-COND-RELATSP-IND        = '1'                ELUPLGRP
01109        MOVE GPD-HOSP-COND-RELATSP-IND                             ELUPLGRP
01110          TO PLD-HOSP-COND-RELATSP-IND        (X Y).               ELUPLGRP
01111                                                                   ELUPLGRP
01112      IF     PSD-DAYS-RDCN-RAT-IND            = '1'                ELUPLGRP
01113        MOVE GPD-DAYS-RDCN-RAT-IND                                 ELUPLGRP
01114          TO PLD-DAYS-RDCN-RAT-IND            (X Y).               ELUPLGRP
01115                                                                   ELUPLGRP
01116      IF     PSD-DAYS-RDCN-RAT-BASIC-APL      = '1'                ELUPLGRP
01117        MOVE GPD-DAYS-RDCN-RAT-BASIC-APL                           ELUPLGRP
01118          TO PLD-DAYS-RDCN-RAT-BASIC-APL      (X Y).               ELUPLGRP
01119                                                                   ELUPLGRP
01120      IF     PSD-DAYS-RDCN-RAT-BASIC-BASE     = '1'                ELUPLGRP
01121        MOVE GPD-DAYS-RDCN-RAT-BASIC-BASE                          ELUPLGRP
01122          TO PLD-DAYS-RDCN-RAT-BASIC-BASE     (X Y).               ELUPLGRP
01123                                                                   ELUPLGRP
01124      IF     PSD-DAYS-RDCN-RAT-SEC-APL        = '1'                ELUPLGRP
01125        MOVE GPD-DAYS-RDCN-RAT-SEC-APL                             ELUPLGRP
01126          TO PLD-DAYS-RDCN-RAT-SEC-APL        (X Y).               ELUPLGRP
01127                                                                   ELUPLGRP
01128      IF     PSD-DAYS-RDCN-RAT-SEC-BASE       = '1'                ELUPLGRP
01129        MOVE GPD-DAYS-RDCN-RAT-SEC-BASE                            ELUPLGRP
01130          TO PLD-DAYS-RDCN-RAT-SEC-BASE       (X Y).               ELUPLGRP
01131                                                                   ELUPLGRP
01132      IF     PSD-MAX-AMT-PER-VISIT            = '1'                ELUPLGRP
01133        MOVE GPD-MAX-AMT-PER-VISIT                                 ELUPLGRP
01134          TO PLD-MAX-AMT-PER-VISIT            (X Y).               ELUPLGRP
01135                                                                   ELUPLGRP
01136      IF     PSD-FLAT-RATE-PDM-AMT            = '1'                ELUPLGRP
01137        MOVE GPD-FLAT-RATE-PDM-AMT                                 ELUPLGRP
01138          TO PLD-FLAT-RATE-PDM-AMT            (X Y).               ELUPLGRP
01139                                                                   ELUPLGRP
01140      IF     PSD-CERT-REPT-REQ-IND            = '1'                ELUPLGRP
01141        MOVE GPD-CERT-REPT-REQ-IND                                 ELUPLGRP
01142          TO PLD-CERT-REPT-REQ-IND            (X Y).               ELUPLGRP
01143                                                                   ELUPLGRP
01144      IF     PSD-HSP-ADM-RESTRN-DAYS          = '1'                ELUPLGRP
01145        MOVE GPD-HSP-ADM-RESTRN-DAYS                               ELUPLGRP
01146          TO PLD-HSP-ADM-RESTRN-DAYS          (X Y).               ELUPLGRP
01147                                                                   ELUPLGRP
01148      IF     PSD-STAY-CODE-IND                = '1'                ELUPLGRP
01149        MOVE GPD-STAY-CODE-IND                                     ELUPLGRP
01150          TO PLD-STAY-CODE-IND                (X Y).               ELUPLGRP
01151                                                                   ELUPLGRP
01152      IF     PSD-BEN-MAX-VISIT-IND            = '1'                ELUPLGRP
01153        MOVE GPD-BEN-MAX-VISIT-IND                                 ELUPLGRP
01154          TO PLD-BEN-MAX-VISIT-IND            (X Y).               ELUPLGRP
01155                                                                   ELUPLGRP
01156      IF     PSD-BEN-MAX-VISIT-DAYS           = '1'                ELUPLGRP
01157        MOVE GPD-BEN-MAX-VISIT-DAYS                                ELUPLGRP
01158          TO PLD-BEN-MAX-VISIT-DAYS           (X Y).               ELUPLGRP
01159                                                                   ELUPLGRP
01160      IF     PSD-TREAT-TIME-FACTOR-IND        = '1'                ELUPLGRP
01161        MOVE GPD-TREAT-TIME-FACTOR-IND                             ELUPLGRP
01162          TO PLD-TREAT-TIME-FACTOR-IND        (X Y).               ELUPLGRP
01163                                                                   ELUPLGRP
01164      IF     PSD-TREAT-TIME-FACTOR            = '1'                ELUPLGRP
01165        MOVE GPD-TREAT-TIME-FACTOR                                 ELUPLGRP
01166          TO PLD-TREAT-TIME-FACTOR            (X Y).               ELUPLGRP
01167                                                                   ELUPLGRP
01168      IF     PSD-CORRIDOR-OVERRIDE            = '1'                ELUPLGRP
01169        MOVE GPD-CORRIDOR-OVERRIDE                                 ELUPLGRP
01170          TO PLD-CORRIDOR-OVERRIDE            (X Y).               ELUPLGRP
01171                                                                   ELUPLGRP
01172  8500-900-EXIT.                                                   ELUPLGRP
01173      EXIT.                                                        ELUPLGRP
01174 /*****************************************************************ELUPLGRP
01175 *                                                                *ELUPLGRP
01176 * 8600  MOVE PROVISION \
01177 *                                                                *ELUPLGRP
01178 ******************************************************************ELUPLGRP
01179  8600-000-MOVE-PROVISION-E      SECTION.                          ELUPLGRP
01180  8600-010.                                                        ELUPLGRP
01181                                                                   ELUPLGRP
01182      IF     PSE-BEN-SCOPE-ID                 = '1'                ELUPLGRP
01183        MOVE GPE-BEN-SCOPE-ID                                      ELUPLGRP
01184          TO PLE-BEN-SCOPE-ID                 (X Y).               ELUPLGRP
01185                                                                   ELUPLGRP
01186      IF     PSE-EXCP-SCHED-ID                = '1'                ELUPLGRP
01187        MOVE GPE-EXCP-SCHED-ID                                     ELUPLGRP
01188          TO PLE-EXCP-SCHED-ID                (X Y).               ELUPLGRP
01189                                                                   ELUPLGRP
01190      IF     PSE-HOSP-ADM-RESTRN-IND          = '1'                ELUPLGRP
01191        MOVE GPE-HOSP-ADM-RESTRN-IND                               ELUPLGRP
01192          TO PLE-HOSP-ADM-RESTRN-IND          (X Y).               ELUPLGRP
01193                                                                   ELUPLGRP
01194      IF     PSE-TREAT-TIME-FACTOR-IND        = '1'                ELUPLGRP
01195        MOVE GPE-TREAT-TIME-FACTOR-IND                             ELUPLGRP
01196          TO PLE-TREAT-TIME-FACTOR-IND        (X Y).               ELUPLGRP
01197                                                                   ELUPLGRP
01198      IF     PSE-TREAT-TIME-FACTOR            = '1'                ELUPLGRP
01199        MOVE GPE-TREAT-TIME-FACTOR                                 ELUPLGRP
01200          TO PLE-TREAT-TIME-FACTOR            (X Y).               ELUPLGRP
01201                                                                   ELUPLGRP
01202      IF     PSE-REPR-REPLAC-RESTRN-IND       = '1'                ELUPLGRP
01203        MOVE GPE-REPR-REPLAC-RESTRN-IND                            ELUPLGRP
01204          TO PLE-REPR-REPLAC-RESTRN-IND       (X Y).               ELUPLGRP
01205                                                                   ELUPLGRP
01206      IF     PSE-CERTN-REPETN-REQRD-IND       = '1'                ELUPLGRP
01207        MOVE GPE-CERTN-REPETN-REQRD-IND                            ELUPLGRP
01208          TO PLE-CERTN-REPETN-REQRD-IND       (X Y).               ELUPLGRP
01209                                                                   ELUPLGRP
01210      IF     PSE-PHYS-EXAM-IND                = '1'                ELUPLGRP
01211        MOVE GPE-PHYS-EXAM-IND                                     ELUPLGRP
01212          TO PLE-PHYS-EXAM-IND                (X Y).               ELUPLGRP
01213                                                                   ELUPLGRP
01214      IF     PSE-MAX-AMT-PER-VISIT            = '1'                ELUPLGRP
01215        MOVE GPE-MAX-AMT-PER-VISIT                                 ELUPLGRP
01216          TO PLE-MAX-AMT-PER-VISIT            (X Y).               ELUPLGRP
01217                                                                   ELUPLGRP
01218      IF     PSE-HSP-ADM-RESTRN-DAYS          = '1'                ELUPLGRP
01219        MOVE GPE-HSP-ADM-RESTRN-DAYS                               ELUPLGRP
01220          TO PLE-HSP-ADM-RESTRN-DAYS          (X Y).               ELUPLGRP
01221                                                                   ELUPLGRP
01222      IF     PSE-AMBULANCE-ELIG-IND           = '1'                ELUPLGRP
01223        MOVE GPE-AMBULANCE-ELIG-IND                                ELUPLGRP
01224          TO PLE-AMBULANCE-ELIG-IND           (X Y).               ELUPLGRP
01225                                                                   ELUPLGRP
01226      IF     PSE-BEN-MAX-VISITS-IND           = '1'                ELUPLGRP
01227        MOVE GPE-BEN-MAX-VISITS-IND                                ELUPLGRP
01228          TO PLE-BEN-MAX-VISITS-IND           (X Y).               ELUPLGRP
01229                                                                   ELUPLGRP
01230      IF     PSE-BEN-MAX-VISITS-DAYS          = '1'                ELUPLGRP
01231        MOVE GPE-BEN-MAX-VISITS-DAYS                               ELUPLGRP
01232          TO PLE-BEN-MAX-VISITS-DAYS          (X Y).               ELUPLGRP
01233                                                                   ELUPLGRP
01234      IF     PSE-CORRIDOR-OVERRIDE            = '1'                ELUPLGRP
01235        MOVE GPE-CORRIDOR-OVERRIDE                                 ELUPLGRP
01236          TO PLE-CORRIDOR-OVERRIDE            (X Y).               ELUPLGRP
01237                                                                   ELUPLGRP
01238  8600-900-EXIT.                                                   ELUPLGRP
01239      EXIT.                                                        ELUPLGRP
01240 /*****************************************************************ELUPLGRP
01241 *                                                                *ELUPLGRP
01242 * 8700  MOVE PROVISION \
01243 *                                                                *ELUPLGRP
01244 ******************************************************************ELUPLGRP
01245  8700-000-MOVE-PROVISION-W      SECTION.                          ELUPLGRP
01246  8700-010.                                                        ELUPLGRP
01247                                                                   ELUPLGRP
01248      IF     PSW-HOSP-ADM-RESTRN-IND          = '1'                ELUPLGRP
01249        MOVE GPW-HOSP-ADM-RESTRN-IND                               ELUPLGRP
01250          TO PLW-HOSP-ADM-RESTRN-IND          (X Y).               ELUPLGRP
01251                                                                   ELUPLGRP
01252      IF     PSW-STAY-CD                      = '1'                ELUPLGRP
01253        MOVE GPW-STAY-CD                                           ELUPLGRP
01254          TO PLW-STAY-CD                      (X Y).               ELUPLGRP
01255                                                                   ELUPLGRP
01256      IF     PSW-HOSP-COND-RELATSP-IND        = '1'                ELUPLGRP
01257        MOVE GPW-HOSP-COND-RELATSP-IND                             ELUPLGRP
01258          TO PLW-HOSP-COND-RELATSP-IND        (X Y).               ELUPLGRP
01259                                                                   ELUPLGRP
01260      IF     PSW-DAYS-RDCN-RAT-IND            = '1'                ELUPLGRP
01261        MOVE GPW-DAYS-RDCN-RAT-IND                                 ELUPLGRP
01262          TO PLW-DAYS-RDCN-RAT-IND            (X Y).               ELUPLGRP
01263                                                                   ELUPLGRP
01264      IF     PSW-DAYS-RDCN-RAT-BASIC-APL      = '1'                ELUPLGRP
01265        MOVE GPW-DAYS-RDCN-RAT-BASIC-APL                           ELUPLGRP
01266          TO PLW-DAYS-RDCN-RAT-BASIC-APL      (X Y).               ELUPLGRP
01267                                                                   ELUPLGRP
01268      IF     PSW-DAYS-RDCN-RAT-BASIC-BASE     = '1'                ELUPLGRP
01269        MOVE GPW-DAYS-RDCN-RAT-BASIC-BASE                          ELUPLGRP
01270          TO PLW-DAYS-RDCN-RAT-BASIC-BASE     (X Y).               ELUPLGRP
01271                                                                   ELUPLGRP
01272      IF     PSW-DAYS-RDCN-RAT-SEC-APL        = '1'                ELUPLGRP
01273        MOVE GPW-DAYS-RDCN-RAT-SEC-APL                             ELUPLGRP
01274          TO PLW-DAYS-RDCN-RAT-SEC-APL        (X Y).               ELUPLGRP
01275                                                                   ELUPLGRP
01276      IF     PSW-DAYS-RDCN-RAT-SEC-BASE       = '1'                ELUPLGRP
01277        MOVE GPW-DAYS-RDCN-RAT-SEC-BASE                            ELUPLGRP
01278          TO PLW-DAYS-RDCN-RAT-SEC-BASE       (X Y).               ELUPLGRP
01279                                                                   ELUPLGRP
01280      IF     PSW-FLAT-RATE-PDM-AMT            = '1'                ELUPLGRP
01281        MOVE GPW-FLAT-RATE-PDM-AMT                                 ELUPLGRP
01282          TO PLW-FLAT-RATE-PDM-AMT            (X Y).               ELUPLGRP
01283                                                                   ELUPLGRP
01284      IF     PSW-ADDN-ALLOW-AMT-PER-DAY       = '1'                ELUPLGRP
01285        MOVE GPW-ADDN-ALLOW-AMT-PER-DAY                            ELUPLGRP
01286          TO PLW-ADDN-ALLOW-AMT-PER-DAY       (X Y).               ELUPLGRP
01287                                                                   ELUPLGRP
01288      IF     PSW-CERTFN-REPETN-REQRM-IND      = '1'                ELUPLGRP
01289        MOVE GPW-CERTFN-REPETN-REQRM-IND                           ELUPLGRP
01290          TO PLW-CERTFN-REPETN-REQRM-IND      (X Y).               ELUPLGRP
01291                                                                   ELUPLGRP
01292      IF     PSW-TREAT-TIME-FACTOR-IND        = '1'                ELUPLGRP
01293        MOVE GPW-TREAT-TIME-FACTOR-IND                             ELUPLGRP
01294          TO PLW-TREAT-TIME-FACTOR-IND        (X Y).               ELUPLGRP
01295                                                                   ELUPLGRP
01296      IF     PSW-TREAT-TIME-FACTOR            = '1'                ELUPLGRP
01297        MOVE GPW-TREAT-TIME-FACTOR                                 ELUPLGRP
01298          TO PLW-TREAT-TIME-FACTOR            (X Y).               ELUPLGRP
01299                                                                   ELUPLGRP
01300      IF     PSW-ELIG-METHD-OF-TREAT-IND      = '1'                ELUPLGRP
01301        MOVE GPW-ELIG-METHD-OF-TREAT-IND                           ELUPLGRP
01302          TO PLW-ELIG-METHD-OF-TREAT-IND      (X Y).               ELUPLGRP
01303                                                                   ELUPLGRP
01304      IF     PSW-PHYS-EXAM-IND                = '1'                ELUPLGRP
01305        MOVE GPW-PHYS-EXAM-IND                                     ELUPLGRP
01306          TO PLW-PHYS-EXAM-IND                (X Y).               ELUPLGRP
01307                                                                   ELUPLGRP
01308      IF     PSW-REHAB-ADM-RESTRN-IND         = '1'                ELUPLGRP
01309        MOVE GPW-REHAB-ADM-RESTRN-IND                              ELUPLGRP
01310          TO PLW-REHAB-ADM-RESTRN-IND         (X Y).               ELUPLGRP
01311                                                                   ELUPLGRP
01312      IF     PSW-HSP-ADM-RESTRN-DAYS          = '1'                ELUPLGRP
01313        MOVE GPW-HSP-ADM-RESTRN-DAYS                               ELUPLGRP
01314          TO PLW-HSP-ADM-RESTRN-DAYS          (X Y).               ELUPLGRP
01315                                                                   ELUPLGRP
01316      IF     PSW-NORM-NWBORN-OVRD-IND         = '1'                ELUPLGRP
01317        MOVE GPW-NORM-NWBORN-OVRD-IND                              ELUPLGRP
01318          TO PLW-NORM-NWBORN-OVRD-IND         (X Y).               ELUPLGRP
01319                                                                   ELUPLGRP
01320      IF     PSW-DRUG-ELIG-MEMB-CLS-OVRD      = '1'                ELUPLGRP
01321        MOVE GPW-DRUG-ELIG-MEMB-CLS-OVRD                           ELUPLGRP
01322          TO PLW-DRUG-ELIG-MEMB-CLS-OVRD      (X Y).               ELUPLGRP
01323                                                                   ELUPLGRP
01324      IF     PSW-ALCO-ELIG-MEMB-CLS-OVRD      = '1'                ELUPLGRP
01325        MOVE GPW-ALCO-ELIG-MEMB-CLS-OVRD                           ELUPLGRP
01326          TO PLW-ALCO-ELIG-MEMB-CLS-OVRD      (X Y).               ELUPLGRP
01327                                                                   ELUPLGRP
01328      IF     PSW-STAY-CODE-IND                = '1'                ELUPLGRP
01329        MOVE GPW-STAY-CODE-IND                                     ELUPLGRP
01330          TO PLW-STAY-CODE-IND                (X Y).               ELUPLGRP
01331                                                                   ELUPLGRP
01332      IF     PSW-TRNS-SEX-REST-OVRD-IND       = '1'                ELUPLGRP
01333        MOVE GPW-TRNS-SEX-REST-OVRD-IND                            ELUPLGRP
01334          TO PLW-TRNS-SEX-REST-OVRD-IND       (X Y).               ELUPLGRP
01335                                                                   ELUPLGRP
01336  8700-900-EXIT.                                                   ELUPLGRP
01337      EXIT.                                                        ELUPLGRP
01338 /*****************************************************************ELUPLGRP
01339 *                                                                *ELUPLGRP
01340 * 9300    READ PROVISION RECORD                                  *ELUPLGRP
01341 *                                                                *ELUPLGRP
01342 ******************************************************************ELUPLGRP
01343  9300-000-READ-PROVISION SECTION.                                 ELUPLGRP
01344  9300-010.                                                        ELUPLGRP
01345                                                                   ELUPLGRP
01346      SET CIA-GCBENPRV-DDN TO TRUE.                                ELUPLGRP
01347      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUPLGRP
01348                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.      ELUPLGRP
01349      IF NOT CIA-RC-OK                                             ELUPLGRP
01350          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELUPLGRP
01351                                                                   ELUPLGRP
01352      SET CIA-GCBENPRV-DDN TO TRUE.                                ELUPLGRP
01353      MOVE KWA-GCBENPRV-KEY          TO IOP-FILE-KEY.              ELUPLGRP
01354                                                                   ELUPLGRP
01355      SET IOP-RD                     TO TRUE.                      ELUPLGRP
01356      SET IOP-FCQ-NONE               TO TRUE.                      ELUPLGRP
01357      SET IOP-KVQ-NONE               TO TRUE.                      ELUPLGRP
01358                                                                   ELUPLGRP
01359      CALL 'ELUIOPGM' USING DFHEIBLK, DFHCOMMAREA.                 ELUPLGRP
01360                                                                   ELUPLGRP
01361      IF IOP-RC-NOTFND                                             ELUPLGRP
01362         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELUPLGRP
01363         EXEC CICS ABEND                                           ELUPLGRP
01364                   ABCODE(CIA-ABCODE)                              ELUPLGRP
01365         END-EXEC.                                                 ELUPLGRP
01366                                                                   ELUPLGRP
01367      IF NOT IOP-RC-OK                                             ELUPLGRP
01368         SET CIA-AB-CRITIO          TO TRUE                        ELUPLGRP
01369         EXEC CICS ABEND                                           ELUPLGRP
01370                   ABCODE(CIA-ABCODE)                              ELUPLGRP
01371         END-EXEC.                                                 ELUPLGRP
01372                                                                   ELUPLGRP
01373      SET ADDRESS OF BENEFIT-PROVISION-RECORD TO IOP-REC-PTR.      ELUPLGRP
01374                                                                   ELUPLGRP
01375  9300-000-EXIT.                                                   ELUPLGRP
01376      EXIT.                                                        ELUPLGRP
