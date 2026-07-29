00001  ID DIVISION.                                                     09/03/03
00002  PROGRAM-ID.     ELXSTRMI.                                        ELXSTRMI
00003  AUTHOR.         DIANE FLOWERS.                                      LV002
00004  DATE-WRITTEN.   02/28/00.                                        ELXSTRMI
00005  DATE-COMPILED.                                                   ELXSTRMI
00006 ******************************************************************ELXSTRMI
00007 *                                                                *ELXSTRMI
00008 *        M A I N T E N A N C E     L O G                         *ELXSTRMI
00009 *                                                                *ELXSTRMI
00010 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELXSTRMI
00011 *                                                                *ELXSTRMI
00012 * 16614      02/28/00  DAF  CREATED PMCI INTERFACE TO BLUESTORM. *ELXSTRMI
00013 * PROD       04/09/03  DAF  GENERATED PROGRAM ABOVE THE LINE.    *ELXSTRMI
00014 *                                                                *ELXSTRMI
00015 ******************************************************************ELXSTRMI
00016 /*****************************************************************ELXSTRMI
00017 *      P R O G R A M   N A R R A T I V E                         *ELXSTRMI
00018 ******************************************************************ELXSTRMI
00019 *                                                                *ELXSTRMI
00020 *   PURPOSE:   HANDLE THE PASSING OF DATA BETWEEN BLUESTORM AND  *ELXSTRMI
00021 *              PMCI.                                             *ELXSTRMI
00022 *                                                                *ELXSTRMI
00023 *   FUNCTIONS: THIS MODULE IS CALLED FROM BLUESTORM.             *ELXSTRMI
00024 *                                                                *ELXSTRMI
00025 ******************************************************************ELXSTRMI
00026 /                                                                 ELXSTRMI
00027  ENVIRONMENT DIVISION.                                            ELXSTRMI
00028  DATA DIVISION.                                                   ELXSTRMI
00029                                                                   ELXSTRMI
00030  WORKING-STORAGE SECTION.                                         ELXSTRMI
00031  01  WS-BEGIN                    PIC X(58) VALUE                  ELXSTRMI
00032      '*** ELXSTRMI WORKING-STORAGE BEGINS HERE ***'.              ELXSTRMI
00033                                                                   ELXSTRMI
00034  01  WS-FIELDS.                                                   ELXSTRMI
00035      05  WS-FOOTNOTE-COUNT       PIC S9(02)  COMP.                ELXSTRMI
00036      05  WS-FOOTNOTE-IDX         PIC S9(02)  COMP-3.              ELXSTRMI
00037      05  WS-SAVE-BEN-IDX         PIC S9(02)  COMP-3.              ELXSTRMI
00038      05  WS-BEN-IDX              PIC S9(02)  COMP-3.              ELXSTRMI
00039      05  WS-BEN-IDX2             PIC S9(02)  COMP-3.              ELXSTRMI
00040      05  WS-DESC-FORMAT          PIC 99      VALUE 0.             ELXSTRMI
00041      05  WS-FORMAT-COMB          PIC 99      VALUE 0.             ELXSTRMI
00042      05  WS-FORMAT-OTHER         PIC 99      VALUE 0.             ELXSTRMI
00043      05  WS-FORMAT-VERIF         PIC 99      VALUE 0.             ELXSTRMI
00044      05  WS-FORMAT-OPX           PIC 99      VALUE 0.             ELXSTRMI
00045      05  WS-FORMAT-PRECERT       PIC 99      VALUE 0.             ELXSTRMI
00046      05  WS-FORMAT-TREAT         PIC 99      VALUE 0.             ELXSTRMI
00047      05  WS-FORMAT-LM            PIC 99      VALUE 0.             ELXSTRMI
00048      05  WS-FORMAT-COPAY         PIC 99      VALUE 0.             ELXSTRMI
00049      05  WS-HOURS                PIC S9(5)   COMP-3.              ELXSTRMI
00050      05  WS-DAYS                 PIC S9(5)   COMP-3.              ELXSTRMI
00051      05  WS-SAVE-FAM-DED         PIC S9(5)   COMP-3  VALUE +0.    ELXSTRMI
00052      05  WS-SAVE-IND-DED         PIC S9(5)   COMP-3  VALUE +0.    ELXSTRMI
00053      05  WS-SAVE-LM              PIC S9(7)   COMP-3  VALUE +0.    ELXSTRMI
00054      05  WS-SAVE-OPX             PIC S9(5)   COMP-3  VALUE +0.    ELXSTRMI
00055      05  WS-SAVE-EAC-COPAY       PIC S9(7)   COMP-3  VALUE +0.    ELXSTRMI
00056      05  WS-SAVE-ELT-COPAY       PIC S9(7)   COMP-3  VALUE +0.    ELXSTRMI
00057      05  WS-SAVE-EMC-COPAY       PIC S9(7)   COMP-3  VALUE +0.    ELXSTRMI
00058      05  WS-SAVE-MSA-DEDUCT      PIC S9(7)   COMP-3  VALUE +0.    ELXSTRMI
00059      05  WS-SAVE-OFC-COPAY       PIC S9(7)   COMP-3  VALUE +0.    ELXSTRMI
00060      05  WS-PAT-AGE              PIC X(6).                        ELXSTRMI
00061      05  R-WS-PAT-AGE REDEFINES WS-PAT-AGE.                       ELXSTRMI
00062          10  WS-PAT-AGE-2        PIC X(2).                        ELXSTRMI
00063          10  WS-PAT-AGE-3        PIC X(3).                        ELXSTRMI
00064          10  WS-PAT-AGE-1        PIC X(1).                        ELXSTRMI
00065      05  WS-DOLLAR-AMT           PIC $$,$$$,$$9.                  ELXSTRMI
00066      05  WS-WHOLE-NUMBER         PIC ZZZZZZZZZ9.                  ELXSTRMI
00067      05  WS-PERCENT.                                              ELXSTRMI
00068          10  WS-PERCENT-NUMBER   PIC ZZZ9.                        ELXSTRMI
00069          10  WS-PERCENT-SYMBOL   PIC X VALUE '%'.                 ELXSTRMI
00070      05  WS-HRDAY-LITERAL.                                        ELXSTRMI
00071          10  WS-HOUR-DAY         PIC ZZ9.                         ELXSTRMI
00072          10  WS-HRDAY-LIT        PIC X(6).                        ELXSTRMI
00073      05  WS-DED-LITERAL.                                          ELXSTRMI
00074          10  WS-DED-IND          PIC ZZ9.                         ELXSTRMI
00075          10  WS-DED-LIT-1        PIC X(10) VALUE ' MEMBERS/ '.    ELXSTRMI
00076          10  WS-DED-AMT          PIC $$$,$$9.                     ELXSTRMI
00077          10  WS-DED-LIT-2        PIC X(5) VALUE ' EACH'.          ELXSTRMI
00078      05  WS-COPAY-LITERAL.                                        ELXSTRMI
00079          10  WS-COPAY-NUM        PIC $$$,$$9.                     ELXSTRMI
00080          10  WS-COPAY-LIT        PIC X(18).                       ELXSTRMI
00081                                                                   ELXSTRMI
00082  01  WS-SWITCHES.                                                 ELXSTRMI
00083      05  WS-FOOTNOTE-ONE-SW      PIC X.                           ELXSTRMI
00084          88  FOOTNOTE-ONE            VALUE '1'.                   ELXSTRMI
00085          88  NO-FOOTNOTE-ONE         VALUE '0'.                   ELXSTRMI
00086      05  WS-FOOTNOTE-TWO-SW      PIC X.                           ELXSTRMI
00087          88  FOOTNOTE-TWO            VALUE '1'.                   ELXSTRMI
00088          88  NO-FOOTNOTE-TWO         VALUE '0'.                   ELXSTRMI
00089      05  WS-FOOTNOTE-THREE-SW    PIC X.                           ELXSTRMI
00090          88  FOOTNOTE-THREE          VALUE '1'.                   ELXSTRMI
00091          88  NO-FOOTNOTE-THREE       VALUE '0'.                   ELXSTRMI
00092      05  WS-FOOTNOTE-FOUR-SW     PIC X.                           ELXSTRMI
00093          88  FOOTNOTE-FOUR           VALUE '1'.                   ELXSTRMI
00094          88  NO-FOOTNOTE-FIVE        VALUE '0'.                   ELXSTRMI
00095      05  WS-FOOTNOTE-FIVE-SW     PIC X.                           ELXSTRMI
00096          88  FOOTNOTE-FIVE           VALUE '1'.                   ELXSTRMI
00097          88  NO-FOOTNOTE-FIVE        VALUE '0'.                   ELXSTRMI
00098      05  WS-FOOTNOTE-SIX-SW      PIC X.                           ELXSTRMI
00099          88  FOOTNOTE-SIX            VALUE '1'.                   ELXSTRMI
00100          88  NO-FOOTNOTE-SIX         VALUE '0'.                   ELXSTRMI
00101      05  WS-FOOTNOTE-SEVEN-SW    PIC X.                           ELXSTRMI
00102          88  FOOTNOTE-SEVEN          VALUE '1'.                   ELXSTRMI
00103          88  NO-FOOTNOTE-SEVEN       VALUE '0'.                   ELXSTRMI
00104      05  WS-FOOTNOTE-EIGHT-SW    PIC X.                           ELXSTRMI
00105          88  FOOTNOTE-EIGHT          VALUE '1'.                   ELXSTRMI
00106          88  NO-FOOTNOTE-EIGHT       VALUE '0'.                   ELXSTRMI
00107                                                                   ELXSTRMI
00108  01  ELX-TABLE.                                                   ELXSTRMI
00109      05  FILLER                  PIC X(13) VALUE                  ELXSTRMI
00110          '* ELX-TABLE *'.                                         ELXSTRMI
00111 ******************************************************************ELXSTRMI
00112 *    ELX     FOOTNOTE TABLE                                      *ELXSTRMI
00113 ******************************************************************ELXSTRMI
00114  01  FILLER.                                                      ELXSTRMI
00115      05  ELX-FOOTNOTE-VALUES.                                     ELXSTRMI
00116                                                                   ELXSTRMI
00117 *----------------------------------------------------------------*ELXSTRMI
00118          10  ELX-MSG-TEXT-001.                                    ELXSTRMI
00119              15  FILLER              PIC X(100) VALUE             ELXSTRMI
00120                  'Deductibles may be combined, please refer to youELXSTRMI
00121 -                'r benefit booklet for details.                '.ELXSTRMI
00122                                                                   ELXSTRMI
00123 *----------------------------------------------------------------*ELXSTRMI
00124          10  ELX-MSG-TEXT-002.                                    ELXSTRMI
00125              15  FILLER              PIC X(100) VALUE             ELXSTRMI
00126                  'Other Non PPO reductions may apply.             ELXSTRMI
00127 -                '                                              '.ELXSTRMI
00128                                                                   ELXSTRMI
00129 *----------------------------------------------------------------*ELXSTRMI
00130          10  ELX-MSG-TEXT-003.                                    ELXSTRMI
00131              15  FILLER              PIC X(100) VALUE             ELXSTRMI
00132                  'Verification of other PPO/Non PPO program requirELXSTRMI
00133 -                'ements may be needed.                         '.ELXSTRMI
00134                                                                   ELXSTRMI
00135 *----------------------------------------------------------------*ELXSTRMI
00136          10  ELX-MSG-TEXT-004.                                    ELXSTRMI
00137              15  FILLER              PIC X(100) VALUE             ELXSTRMI
00138                  'PPO/Non PPO out of pocket limits may be combinedELXSTRMI
00139 -                '.                            '.                 ELXSTRMI
00140                                                                   ELXSTRMI
00141 *----------------------------------------------------------------*ELXSTRMI
00142          10  ELX-MSG-TEXT-005.                                    ELXSTRMI
00143              15  FILLER              PIC X(100) VALUE             ELXSTRMI
00144                  'Additional pre-certification may be required, plELXSTRMI
00145 -                'ease call the number on the back of your card.'.ELXSTRMI
00146                                                                   ELXSTRMI
00147 *----------------------------------------------------------------*ELXSTRMI
00148          10  ELX-MSG-TEXT-006.                                    ELXSTRMI
00149              15  FILLER              PIC X(100) VALUE             ELXSTRMI
00150                  'Initial treatment should be sought within the nuELXSTRMI
00151 -                'mber of hours/days specified.'.                 ELXSTRMI
00152                                                                   ELXSTRMI
00153 *----------------------------------------------------------------*ELXSTRMI
00154          10  ELX-MSG-TEXT-007.                                    ELXSTRMI
00155              15  FILLER              PIC X(100) VALUE             ELXSTRMI
00156                  'Lifetime Maximum may be combined, please refer tELXSTRMI
00157 -                'o your benefit booklet for details.'.           ELXSTRMI
00158                                                                   ELXSTRMI
00159 *----------------------------------------------------------------*ELXSTRMI
00160          10  ELX-MSG-TEXT-008.                                    ELXSTRMI
00161              15  FILLER              PIC X(100) VALUE             ELXSTRMI
00162                  'Reduced benefit levels and additional deductibleELXSTRMI
00163 -                's may apply.'.                                  ELXSTRMI
00164                                                                   ELXSTRMI
00165 *----------------------------------------------------------------*ELXSTRMI
00166                                                                   ELXSTRMI
00167      05  ELX-FOOTNOTE-TABLE       REDEFINES                       ELXSTRMI
00168          ELX-FOOTNOTE-VALUES      OCCURS 08 TIMES                 ELXSTRMI
00169                                   INDEXED BY ELX-IDX.             ELXSTRMI
00170          10  ELX-ENTRY.                                           ELXSTRMI
00171              15  ELX-FOOTNOTE-TEXT   PIC X(100).                  ELXSTRMI
00172                                                                   ELXSTRMI
00173 /-------- MILLENNIUM DATE ROUTINE COMMAREA --------------------*  ELXSTRMI
00174  COPY MLDATE01.                                                   ELXSTRMI
00175                                                                   ELXSTRMI
00176 /-------- COMMAREA PASSED TO ELXPMCIF -------------------------*  ELXSTRMI
00177  01  PMCI-COMM-AREA.                                              ELXSTRMI
00178      COPY PMCCOMM.                                                ELXSTRMI
00179                                                                   ELXSTRMI
00180 /                                                                 ELXSTRMI
00181  01  WS-END                       PIC X(58) VALUE                 ELXSTRMI
00182      '*** ELXSTRMI WORKING-STORAGE ENDS HERE ***'.                ELXSTRMI
00183 /                                                                 ELXSTRMI
00184  LINKAGE SECTION.                                                 ELXSTRMI
00185                                                                   ELXSTRMI
00186 *--- COMMAREA PASSED FROM CALLER --------------------------------*ELXSTRMI
00187                                                                   ELXSTRMI
00188  01  DFHCOMMAREA.                                                 ELXSTRMI
00189      COPY PMCSTRMC.                                               ELXSTRMI
00190                                                                   ELXSTRMI
00191 /                                                                 ELXSTRMI
00192  PROCEDURE DIVISION.                                              ELXSTRMI
00193                                                                   ELXSTRMI
00194 ****************************************************************  ELXSTRMI
00195 *                                                              *  ELXSTRMI
00196 *           P R O C E S S     C O N T R O L                    *  ELXSTRMI
00197 *                                                              *  ELXSTRMI
00198 ****************************************************************  ELXSTRMI
00199  0000-000-PROCESS-CONTROL       SECTION.                          ELXSTRMI
00200  0000-010.                                                        ELXSTRMI
00201                                                                   ELXSTRMI
00202      PERFORM  1000-000-PROCESS-IN-NETWORK.                        ELXSTRMI
00203      PERFORM  2000-000-PROCESS-OUT-NETWORK.                       ELXSTRMI
00204                                                                   ELXSTRMI
00205  0000-800-RETURN.                                                 ELXSTRMI
00206                                                                   ELXSTRMI
00207      EXEC CICS RETURN END-EXEC.                                   ELXSTRMI
00208                                                                   ELXSTRMI
00209      GOBACK.                                                      ELXSTRMI
00210                                                                   ELXSTRMI
00211  0000-900-EXIT.                                                   ELXSTRMI
00212      EXIT.                                                        ELXSTRMI
00213 /***************************************************************  ELXSTRMI
00214 *                                                              *  ELXSTRMI
00215 * 1000    P R O C E S S    I N   N E T W O R K                 *  ELXSTRMI
00216 *                                                              *  ELXSTRMI
00217 ****************************************************************  ELXSTRMI
00218  1000-000-PROCESS-IN-NETWORK    SECTION.                          ELXSTRMI
00219  1000-010.                                                        ELXSTRMI
00220                                                                   ELXSTRMI
00221      INITIALIZE PMCI-COMM-AREA.                                   ELXSTRMI
00222      MOVE BSTRM-PLAN-CODE        TO PMCI-PLAN-CODE.               ELXSTRMI
00223      MOVE BSTRM-GROUP-NBR        TO PMCI-GROUP-NBR.               ELXSTRMI
00224      MOVE BSTRM-SECT-NUM         TO PMCI-SECT-NUM.                ELXSTRMI
00225      MOVE BSTRM-PACKAGE-CODE     TO PMCI-PACKAGE-CODE.            ELXSTRMI
00226      MOVE BSTRM-SUBSCRIBER-NBR   TO PMCI-SUBSCRIBER-NBR.          ELXSTRMI
00227      MOVE BSTRM-ADS-PROG-TYPE    TO PMCI-ADS-PROG-TYPE.           ELXSTRMI
00228      IF PMCI-POS-APPLIES                                          ELXSTRMI
00229          MOVE 'N'                TO PMCI-REFERRAL-INDICATOR.      ELXSTRMI
00230      MOVE BSTRM-PAT-BIRTH-DATE   TO PMCI-PAT-BIRTH-DATE.          ELXSTRMI
00231      MOVE BSTRM-PAT-RELATIONSHIP TO PMCI-PAT-RELATIONSHIP.        ELXSTRMI
00232      MOVE BSTRM-MEDICARE-ELIGIBILITY                              ELXSTRMI
00233                                  TO PMCI-MEDICARE-ELIGIBILITY.    ELXSTRMI
00234      MOVE BSTRM-DATE-OF-SERVICE  TO PMCI-DATE-OF-SERVICE.         ELXSTRMI
00235      MOVE BSTRM-PROVIDER-INDICATOR                                ELXSTRMI
00236                                  TO PMCI-PROVIDER-INDICATOR.      ELXSTRMI
00237      IF BSTRM-PROVIDER-INDICATOR = 'I'                            ELXSTRMI
00238          MOVE '0000000001'       TO PMCI-PROVIDER-NUMBER          ELXSTRMI
00239          MOVE '0B'               TO PMCI-PROVIDER-TYPE            ELXSTRMI
00240          MOVE '1'                TO PMCI-MPP-INDICATOR            ELXSTRMI
00241          MOVE '0'                TO PMCI-RPO-INDICATOR            ELXSTRMI
00242      ELSE                                                         ELXSTRMI
00243          MOVE '0021623192'       TO PMCI-PROVIDER-NUMBER          ELXSTRMI
00244          MOVE 'A1'               TO PMCI-PROVIDER-TYPE            ELXSTRMI
00245          MOVE '0'                TO PMCI-MPP-INDICATOR            ELXSTRMI
00246          MOVE '0'                TO PMCI-RPO-INDICATOR.           ELXSTRMI
00247      MOVE '1'                    TO PMCI-PLAN-INDICATOR.          ELXSTRMI
00248      MOVE '1'                    TO PMCI-PPO-INDICATOR.           ELXSTRMI
00249      MOVE BSTRM-IP-OR-OP-INQUIRY TO PMCI-IP-OR-OP-INQUIRY.        ELXSTRMI
00250      MOVE '0'                    TO PMCI-FAC-ROOM-TYPE.           ELXSTRMI
00251      MOVE 'Y'                    TO PMCI-BLUE-STORM-SWITCH.       ELXSTRMI
00252      MOVE BSTRM-PAT-BIRTH-DATE   TO MLDATE-DATE1.                 ELXSTRMI
00253      PERFORM 9999-000-CALCULATE-AGE.                              ELXSTRMI
00254      IF MLDATE-RETURN = ZEROS                                     ELXSTRMI
00255          MOVE MLDATE-AMOUNT      TO WS-PAT-AGE                    ELXSTRMI
00256          MOVE WS-PAT-AGE-3       TO PMCI-PAT-AGE                  ELXSTRMI
00257      ELSE                                                         ELXSTRMI
00258          MOVE ZEROS              TO PMCI-PAT-AGE.                 ELXSTRMI
00259                                                                   ELXSTRMI
00260      PERFORM 9000-000-ELXPMCIF-IO.                                ELXSTRMI
00261      PERFORM 4000-000-RETURN-ERROR-MSG.                           ELXSTRMI
00262      IF BSTRM-ERROR-CODE = 01 OR 02 OR 03 OR 04 OR 05             ELXSTRMI
00263          GO TO 1000-900-EXIT.                                     ELXSTRMI
00264      PERFORM 3000-000-RETURN-BENEFITS.                            ELXSTRMI
00265                                                                   ELXSTRMI
00266  1000-900-EXIT.                                                   ELXSTRMI
00267      EXIT.                                                        ELXSTRMI
00268 /***************************************************************  ELXSTRMI
00269 *                                                              *  ELXSTRMI
00270 * 2000    P R O C E S S    O U T   O F   N E T W O R K         *  ELXSTRMI
00271 *                                                              *  ELXSTRMI
00272 ****************************************************************  ELXSTRMI
00273  2000-000-PROCESS-OUT-NETWORK     SECTION.                        ELXSTRMI
00274  2000-010.                                                        ELXSTRMI
00275                                                                   ELXSTRMI
00276      INITIALIZE PMCI-COMM-AREA.                                   ELXSTRMI
00277      MOVE BSTRM-PLAN-CODE        TO PMCI-PLAN-CODE.               ELXSTRMI
00278      MOVE BSTRM-GROUP-NBR        TO PMCI-GROUP-NBR.               ELXSTRMI
00279      MOVE BSTRM-SECT-NUM         TO PMCI-SECT-NUM.                ELXSTRMI
00280      MOVE BSTRM-PACKAGE-CODE     TO PMCI-PACKAGE-CODE.            ELXSTRMI
00281      MOVE BSTRM-SUBSCRIBER-NBR   TO PMCI-SUBSCRIBER-NBR.          ELXSTRMI
00282      MOVE BSTRM-ADS-PROG-TYPE    TO PMCI-ADS-PROG-TYPE.           ELXSTRMI
00283      IF PMCI-POS-APPLIES                                          ELXSTRMI
00284          MOVE SPACE              TO PMCI-REFERRAL-INDICATOR.      ELXSTRMI
00285      MOVE BSTRM-PAT-BIRTH-DATE   TO PMCI-PAT-BIRTH-DATE.          ELXSTRMI
00286      MOVE BSTRM-PAT-RELATIONSHIP TO PMCI-PAT-RELATIONSHIP.        ELXSTRMI
00287      MOVE BSTRM-MEDICARE-ELIGIBILITY                              ELXSTRMI
00288                                  TO PMCI-MEDICARE-ELIGIBILITY.    ELXSTRMI
00289      MOVE BSTRM-DATE-OF-SERVICE  TO PMCI-DATE-OF-SERVICE.         ELXSTRMI
00290      MOVE BSTRM-PROVIDER-INDICATOR                                ELXSTRMI
00291                                  TO PMCI-PROVIDER-INDICATOR.      ELXSTRMI
00292      IF BSTRM-PROVIDER-INDICATOR = 'I'                            ELXSTRMI
00293          MOVE '0000000001'       TO PMCI-PROVIDER-NUMBER          ELXSTRMI
00294          MOVE '0B'               TO PMCI-PROVIDER-TYPE            ELXSTRMI
00295          MOVE '1'                TO PMCI-MPP-INDICATOR            ELXSTRMI
00296          MOVE '0'                TO PMCI-RPO-INDICATOR            ELXSTRMI
00297      ELSE                                                         ELXSTRMI
00298          MOVE '0021623192'       TO PMCI-PROVIDER-NUMBER          ELXSTRMI
00299          MOVE 'A1'               TO PMCI-PROVIDER-TYPE            ELXSTRMI
00300          MOVE '0'                TO PMCI-MPP-INDICATOR            ELXSTRMI
00301          MOVE '0'                TO PMCI-RPO-INDICATOR.           ELXSTRMI
00302      MOVE '0'                    TO PMCI-PLAN-INDICATOR.          ELXSTRMI
00303      MOVE '0'                    TO PMCI-PPO-INDICATOR.           ELXSTRMI
00304      MOVE BSTRM-IP-OR-OP-INQUIRY TO PMCI-IP-OR-OP-INQUIRY.        ELXSTRMI
00305      MOVE '0'                    TO PMCI-FAC-ROOM-TYPE.           ELXSTRMI
00306      MOVE 'Y'                    TO PMCI-BLUE-STORM-SWITCH.       ELXSTRMI
00307      MOVE BSTRM-PAT-BIRTH-DATE   TO MLDATE-DATE1                  ELXSTRMI
00308      PERFORM 9999-000-CALCULATE-AGE.                              ELXSTRMI
00309      IF MLDATE-RETURN = ZEROS                                     ELXSTRMI
00310          MOVE MLDATE-AMOUNT      TO WS-PAT-AGE                    ELXSTRMI
00311          MOVE WS-PAT-AGE-3       TO PMCI-PAT-AGE                  ELXSTRMI
00312      ELSE                                                         ELXSTRMI
00313          MOVE ZEROS              TO PMCI-PAT-AGE.                 ELXSTRMI
00314                                                                   ELXSTRMI
00315      PERFORM 9000-000-ELXPMCIF-IO.                                ELXSTRMI
00316      PERFORM 4000-000-RETURN-ERROR-MSG.                           ELXSTRMI
00317      IF BSTRM-ERROR-CODE = 01 OR 02 OR 03 OR 04 OR 05             ELXSTRMI
00318          GO TO 2000-900-EXIT.                                     ELXSTRMI
00319      PERFORM 3000-000-RETURN-BENEFITS.                            ELXSTRMI
00320      PERFORM 7000-000-PROCESS-BENEFITS                            ELXSTRMI
00321          VARYING WS-BEN-IDX FROM 1 BY 1                           ELXSTRMI
00322              UNTIL WS-BEN-IDX > BSTRM-BEN-POINTERS.               ELXSTRMI
00323                                                                   ELXSTRMI
00324  2000-900-EXIT.                                                   ELXSTRMI
00325      EXIT.                                                        ELXSTRMI
00326 /***************************************************************  ELXSTRMI
00327 *                                                              *  ELXSTRMI
00328 * 3000    R E T U R N     B E N E F I T S                      *  ELXSTRMI
00329 *                                                              *  ELXSTRMI
00330 ****************************************************************  ELXSTRMI
00331  3000-000-RETURN-BENEFITS         SECTION.                        ELXSTRMI
00332  3000-010.                                                        ELXSTRMI
00333                                                                   ELXSTRMI
00334      IF PMCI-NON-PPO-PROVIDER                                     ELXSTRMI
00335          SET WS-SAVE-BEN-IDX TO BSTRM-BEN-IDX.                    ELXSTRMI
00336      SET BSTRM-BEN-IDX TO +1.                                     ELXSTRMI
00337      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00338          SET BSTRM-COL-IDX TO +1                                  ELXSTRMI
00339      ELSE                                                         ELXSTRMI
00340          SET BSTRM-COL-IDX TO +2.                                 ELXSTRMI
00341      IF PMCI-NEED-PPO-HEADING                                     ELXSTRMI
00342          IF BSTRM-PROVIDER-INDICATOR = 'P'                        ELXSTRMI
00343              IF PMCI-CMM                                          ELXSTRMI
00344                IF PMCI-PPO-PROVIDER                               ELXSTRMI
00345                  MOVE 'Plan Summary'                              ELXSTRMI
00346                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00347                  MOVE ' H'                                        ELXSTRMI
00348                            TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00349                  MOVE 'PPO'                                       ELXSTRMI
00350                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00351                  GO TO 3000-COINSURANCE                           ELXSTRMI
00352                ELSE                                               ELXSTRMI
00353                  MOVE 'Non PPO'                                   ELXSTRMI
00354                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00355                  GO TO 3000-COINSURANCE.                          ELXSTRMI
00356      IF PMCI-PRG-NO                                               ELXSTRMI
00357          IF PMCI-CMM                                              ELXSTRMI
00358              MOVE 'Plan Summary'                                  ELXSTRMI
00359                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00360              MOVE ' C'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00361              MOVE 'COMP MAJOR MED'                                ELXSTRMI
00362                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00363          ELSE                                                     ELXSTRMI
00364              MOVE SPACES                                          ELXSTRMI
00365                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00366      ELSE                                                         ELXSTRMI
00367      IF PMCI-POS-APPLIES                                          ELXSTRMI
00368          IF PMCI-REFERRAL-NOT-REQUIRED                            ELXSTRMI
00369              MOVE 'Plan Summary'                                  ELXSTRMI
00370                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00371              MOVE ' H'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00372              MOVE 'In Network'                                    ELXSTRMI
00373                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00374          ELSE                                                     ELXSTRMI
00375              MOVE 'Out Of Network'                                ELXSTRMI
00376                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00377      ELSE                                                         ELXSTRMI
00378      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00379          MOVE 'Plan Summary'                                      ELXSTRMI
00380                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00381          MOVE ' H'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00382          MOVE 'PPO'                                               ELXSTRMI
00383                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00384      ELSE                                                         ELXSTRMI
00385          MOVE 'Non PPO'                                           ELXSTRMI
00386                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00387  3000-COINSURANCE.                                                ELXSTRMI
00388      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00389      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00390          MOVE 'Coinsurance'                                       ELXSTRMI
00391                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
00392      IF PMCI-COINS-PERCENT                                        ELXSTRMI
00393          MOVE PMCI-COINS-PERCENT-VALUE                            ELXSTRMI
00394                            TO WS-PERCENT-NUMBER                   ELXSTRMI
00395          MOVE WS-PERCENT                                          ELXSTRMI
00396                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00397      ELSE                                                         ELXSTRMI
00398      IF PMCI-COINS-NONE                                           ELXSTRMI
00399          MOVE 'None'                                              ELXSTRMI
00400                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00401      ELSE                                                         ELXSTRMI
00402      IF PMCI-COINS-NOT-APPL                                       ELXSTRMI
00403          MOVE 'Not Applicable'                                    ELXSTRMI
00404                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00405      ELSE                                                         ELXSTRMI
00406      IF PMCI-COINS-CALL                                           ELXSTRMI
00407          MOVE 'Please call for details'                           ELXSTRMI
00408                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00409      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00410      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00411          MOVE 'Family Deductible'                                 ELXSTRMI
00412                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00413          MOVE '01'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
00414      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00415          IF (PMCI-DEDF-NUMBER-IND) AND                            ELXSTRMI
00416             (PMCI-DEDI-CALL)                                      ELXSTRMI
00417              MOVE 'Please call for details'                       ELXSTRMI
00418                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00419              MOVE SPACES   TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00420              GO TO 3000-IND.                                      ELXSTRMI
00421      IF (BSTRM-BENEFIT(BSTRM-BEN-IDX 1) = 'None') AND             ELXSTRMI
00422         (PMCI-DEDF-DOLLARS)                                       ELXSTRMI
00423          MOVE SPACES       TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
00424      IF PMCI-POS-APPLIES                                          ELXSTRMI
00425          GO TO 3000-CONT-FAM.                                     ELXSTRMI
00426      IF (PMCI-NON-PPO-PROVIDER) AND                               ELXSTRMI
00427         (PMCI-DEDF-NUMBER-IND)                                    ELXSTRMI
00428         IF (PMCI-DEDI-FROM-MEMBERSHIP) OR                         ELXSTRMI
00429            (PMCI-DEDI-NO-IND-DEDUCTIBLE) OR                       ELXSTRMI
00430            (PMCI-DEDI-CALL)                                       ELXSTRMI
00431             MOVE 'Please call for details'                        ELXSTRMI
00432                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00433             MOVE '03'      TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00434             PERFORM 5000-000-INCREASE-INDEX                       ELXSTRMI
00435             MOVE 'Please call for details'                        ELXSTRMI
00436                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00437             MOVE '03'      TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00438             GO TO 3000-LM.                                        ELXSTRMI
00439      IF (PMCI-NON-PPO-PROVIDER) AND                               ELXSTRMI
00440         (PMCI-DEDUCTIBLE-FAM = +0) AND                            ELXSTRMI
00441         (WS-SAVE-FAM-DED = +0)                                    ELXSTRMI
00442          MOVE 'Please call for details'                           ELXSTRMI
00443                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00444          MOVE '02'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00445          GO TO 3000-IND.                                          ELXSTRMI
00446      IF (PMCI-NON-PPO-PROVIDER) AND                               ELXSTRMI
00447         (PMCI-DEDUCTIBLE-FAM = +0) AND                            ELXSTRMI
00448         (WS-SAVE-FAM-DED > +0)                                    ELXSTRMI
00449          MOVE 'Please call for details'                           ELXSTRMI
00450                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00451          MOVE '03'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00452          GO TO 3000-IND.                                          ELXSTRMI
00453      IF (PMCI-DEDF-NUMBER-IND) AND                                ELXSTRMI
00454         (PMCI-DEDUCTIBLE-FAM > +0) AND                            ELXSTRMI
00455         (PMCI-DEDI-DOLLARS) AND                                   ELXSTRMI
00456         (PMCI-DEDUCTIBLE-IND > +0)                                ELXSTRMI
00457          MOVE PMCI-DEDUCTIBLE-FAM                                 ELXSTRMI
00458                            TO WS-DED-IND                          ELXSTRMI
00459          MOVE PMCI-DEDUCTIBLE-IND                                 ELXSTRMI
00460                            TO WS-DED-AMT                          ELXSTRMI
00461          MOVE WS-DED-LITERAL                                      ELXSTRMI
00462                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00463          GO TO 3000-IND.                                          ELXSTRMI
00464  3000-CONT-FAM.                                                   ELXSTRMI
00465      IF PMCI-DEDF-DOLLARS                                         ELXSTRMI
00466          MOVE PMCI-DEDUCTIBLE-FAM                                 ELXSTRMI
00467                            TO WS-DOLLAR-AMT                       ELXSTRMI
00468          MOVE WS-DOLLAR-AMT                                       ELXSTRMI
00469                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00470          MOVE PMCI-DEDUCTIBLE-FAM                                 ELXSTRMI
00471                            TO WS-SAVE-FAM-DED                     ELXSTRMI
00472      ELSE                                                         ELXSTRMI
00473      IF PMCI-DEDF-NUMBER-IND                                      ELXSTRMI
00474          MOVE PMCI-DEDUCTIBLE-FAM                                 ELXSTRMI
00475                            TO WS-WHOLE-NUMBER                     ELXSTRMI
00476          MOVE WS-WHOLE-NUMBER                                     ELXSTRMI
00477                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00478      ELSE                                                         ELXSTRMI
00479      IF PMCI-DEDF-FROM-MEMBERSHIP                                 ELXSTRMI
00480          MOVE 'From Membership'                                   ELXSTRMI
00481                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00482      ELSE                                                         ELXSTRMI
00483      IF PMCI-DEDF-NO-FAM-DEDUCTIBLE                               ELXSTRMI
00484          MOVE 'None'                                              ELXSTRMI
00485                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00486      ELSE                                                         ELXSTRMI
00487      IF PMCI-DEDF-CALL                                            ELXSTRMI
00488          MOVE 'Please call for details'                           ELXSTRMI
00489                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00490  3000-IND.                                                        ELXSTRMI
00491      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00492      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00493          MOVE 'Individual Deductible'                             ELXSTRMI
00494                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00495          MOVE '01'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00496          GO TO 3000-CONT-IND.                                     ELXSTRMI
00497      IF (BSTRM-BENEFIT(BSTRM-BEN-IDX 1) = 'None') AND             ELXSTRMI
00498         (PMCI-DEDI-DOLLARS)                                       ELXSTRMI
00499          MOVE SPACES       TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
00500      IF PMCI-POS-APPLIES                                          ELXSTRMI
00501          GO TO 3000-CONT-IND.                                     ELXSTRMI
00502      IF (PMCI-NON-PPO-PROVIDER) AND                               ELXSTRMI
00503         (PMCI-DEDUCTIBLE-IND = +0) AND                            ELXSTRMI
00504         (WS-SAVE-IND-DED = +0)                                    ELXSTRMI
00505          MOVE 'Please call for details'                           ELXSTRMI
00506                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00507          MOVE '02'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00508          GO TO 3000-LM.                                           ELXSTRMI
00509      IF (PMCI-NON-PPO-PROVIDER) AND                               ELXSTRMI
00510         (PMCI-DEDUCTIBLE-IND = +0) AND                            ELXSTRMI
00511         (WS-SAVE-IND-DED > +0)                                    ELXSTRMI
00512          MOVE 'Please call for details'                           ELXSTRMI
00513                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00514          MOVE '03'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00515          GO TO 3000-LM.                                           ELXSTRMI
00516  3000-CONT-IND.                                                   ELXSTRMI
00517      IF PMCI-DEDI-DOLLARS                                         ELXSTRMI
00518          MOVE PMCI-DEDUCTIBLE-IND                                 ELXSTRMI
00519                            TO WS-DOLLAR-AMT                       ELXSTRMI
00520          MOVE WS-DOLLAR-AMT                                       ELXSTRMI
00521                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00522          MOVE PMCI-DEDUCTIBLE-IND                                 ELXSTRMI
00523                            TO WS-SAVE-IND-DED                     ELXSTRMI
00524      ELSE                                                         ELXSTRMI
00525      IF PMCI-DEDI-FROM-MEMBERSHIP                                 ELXSTRMI
00526          MOVE 'From Membership'                                   ELXSTRMI
00527                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00528      ELSE                                                         ELXSTRMI
00529      IF PMCI-DEDI-NO-IND-DEDUCTIBLE                               ELXSTRMI
00530          MOVE 'None'                                              ELXSTRMI
00531                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00532      ELSE                                                         ELXSTRMI
00533      IF PMCI-DEDI-CALL                                            ELXSTRMI
00534          MOVE 'Please call for details'                           ELXSTRMI
00535                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00536  3000-LM.                                                         ELXSTRMI
00537      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00538      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00539          MOVE 'Lifetime Maximum'                                  ELXSTRMI
00540                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00541          MOVE '07'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
00542      IF (BSTRM-BENEFIT(BSTRM-BEN-IDX 1) = 'No Limit') AND         ELXSTRMI
00543         (PMCI-LM-DOLLARS)                                         ELXSTRMI
00544          MOVE SPACES       TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
00545      IF (PMCI-POS-APPLIES) AND                                    ELXSTRMI
00546         (PMCI-REFERRAL-INDICATOR = SPACE) AND                     ELXSTRMI
00547         (PMCI-LM-NO-LIMIT) AND                                    ELXSTRMI
00548         (WS-SAVE-LM > +0)                                         ELXSTRMI
00549          MOVE BSTRM-BENEFIT(BSTRM-BEN-IDX 1)                      ELXSTRMI
00550                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00551      ELSE                                                         ELXSTRMI
00552      IF (PMCI-NON-PPO-PROVIDER) AND                               ELXSTRMI
00553         (PMCI-LM-NO-LIMIT) AND                                    ELXSTRMI
00554         (WS-SAVE-LM > +0)                                         ELXSTRMI
00555          MOVE BSTRM-BENEFIT(BSTRM-BEN-IDX 1)                      ELXSTRMI
00556                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00557      ELSE                                                         ELXSTRMI
00558      IF PMCI-LM-DOLLARS                                           ELXSTRMI
00559          MOVE PMCI-LIFETIME-MAX                                   ELXSTRMI
00560                            TO WS-DOLLAR-AMT                       ELXSTRMI
00561          MOVE WS-DOLLAR-AMT                                       ELXSTRMI
00562                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00563          MOVE PMCI-LIFETIME-MAX                                   ELXSTRMI
00564                            TO WS-SAVE-LM                          ELXSTRMI
00565      ELSE                                                         ELXSTRMI
00566      IF PMCI-LM-NO-LIMIT                                          ELXSTRMI
00567          MOVE 'No Limit'                                          ELXSTRMI
00568                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00569      ELSE                                                         ELXSTRMI
00570      IF PMCI-LM-CALL                                              ELXSTRMI
00571          MOVE 'Please call for details'                           ELXSTRMI
00572                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00573      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00574      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00575          MOVE 'Out of Pocket Expense'                             ELXSTRMI
00576                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
00577      IF (PMCI-NON-PPO-PROVIDER) AND                               ELXSTRMI
00578         (WS-SAVE-OPX > +0) AND                                    ELXSTRMI
00579         (PMCI-OUT-OF-POCKET-EXPENSE = WS-SAVE-OPX)                ELXSTRMI
00580          MOVE '04'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
00581      IF OPX-DOLLARS                                               ELXSTRMI
00582          MOVE PMCI-OUT-OF-POCKET-EXPENSE                          ELXSTRMI
00583                            TO WS-DOLLAR-AMT                       ELXSTRMI
00584          MOVE WS-DOLLAR-AMT                                       ELXSTRMI
00585                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00586          MOVE PMCI-OUT-OF-POCKET-EXPENSE                          ELXSTRMI
00587                            TO WS-SAVE-OPX                         ELXSTRMI
00588      ELSE                                                         ELXSTRMI
00589      IF OPX-FROM-MEMBERSHIP                                       ELXSTRMI
00590          MOVE 'From Membership'                                   ELXSTRMI
00591                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00592      ELSE                                                         ELXSTRMI
00593      IF OPX-NO-LIMIT                                              ELXSTRMI
00594          MOVE 'No Limit'                                          ELXSTRMI
00595                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00596      ELSE                                                         ELXSTRMI
00597      IF OPX-CALL                                                  ELXSTRMI
00598          MOVE 'Please call for details'                           ELXSTRMI
00599                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00600      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00601      IF PMCI-PRG-NO                                               ELXSTRMI
00602          IF PMCI-CMM                                              ELXSTRMI
00603              MOVE 'General Benefits'                              ELXSTRMI
00604                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00605              MOVE ' C'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00606              MOVE 'COMP MAJOR MED'                                ELXSTRMI
00607                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00608          ELSE                                                     ELXSTRMI
00609              MOVE SPACES                                          ELXSTRMI
00610                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00611      ELSE                                                         ELXSTRMI
00612      IF PMCI-POS-APPLIES                                          ELXSTRMI
00613          IF PMCI-REFERRAL-NOT-REQUIRED                            ELXSTRMI
00614              MOVE 'General Benefits'                              ELXSTRMI
00615                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00616              MOVE ' H'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00617              MOVE 'In Network'                                    ELXSTRMI
00618                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00619          ELSE                                                     ELXSTRMI
00620              MOVE 'Out Of Network'                                ELXSTRMI
00621                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00622      ELSE                                                         ELXSTRMI
00623      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00624          MOVE 'General Benefits'                                  ELXSTRMI
00625                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00626          MOVE ' H'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00627          MOVE 'PPO'                                               ELXSTRMI
00628                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00629      ELSE                                                         ELXSTRMI
00630          MOVE 'Non PPO'                                           ELXSTRMI
00631                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00632      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00633      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00634          MOVE 'Additional Second Opinion Program'                 ELXSTRMI
00635                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
00636      IF PMCI-ASOP-NO                                              ELXSTRMI
00637          MOVE 'Not Covered'                                       ELXSTRMI
00638                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00639      ELSE                                                         ELXSTRMI
00640      IF PMCI-ASOP-YES                                             ELXSTRMI
00641          MOVE 'Covered'                                           ELXSTRMI
00642                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00643      ELSE                                                         ELXSTRMI
00644      IF PMCI-ASOP-CALL                                            ELXSTRMI
00645          MOVE 'Please call for details'                           ELXSTRMI
00646                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00647      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00648      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00649          MOVE 'Alcohol Dependency'                                ELXSTRMI
00650                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00651          MOVE '05'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
00652      IF SUB-ABUSE-ALC-NO                                          ELXSTRMI
00653          MOVE 'Not Covered'                                       ELXSTRMI
00654                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00655      ELSE                                                         ELXSTRMI
00656      IF SUB-ABUSE-ALC-YES                                         ELXSTRMI
00657          MOVE 'Covered'                                           ELXSTRMI
00658                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00659      ELSE                                                         ELXSTRMI
00660      IF SUB-ABUSE-ALC-CALL                                        ELXSTRMI
00661          MOVE 'Please call for details'                           ELXSTRMI
00662                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00663      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00664      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00665          MOVE 'Ambulance Services'                                ELXSTRMI
00666                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
00667      IF AMBULANCE-NO                                              ELXSTRMI
00668          MOVE 'Not Covered'                                       ELXSTRMI
00669                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00670      ELSE                                                         ELXSTRMI
00671      IF AMBULANCE-YES                                             ELXSTRMI
00672          MOVE 'Covered'                                           ELXSTRMI
00673                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00674      ELSE                                                         ELXSTRMI
00675      IF AMBULANCE-CALL                                            ELXSTRMI
00676          MOVE 'Please call for details'                           ELXSTRMI
00677                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00678      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00679      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00680          MOVE 'Chemical Dependency'                               ELXSTRMI
00681                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00682          MOVE '05'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
00683      IF SUB-ABUSE-DRG-NO                                          ELXSTRMI
00684          MOVE 'Not Covered'                                       ELXSTRMI
00685                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00686      ELSE                                                         ELXSTRMI
00687      IF SUB-ABUSE-DRG-YES                                         ELXSTRMI
00688          MOVE 'Covered'                                           ELXSTRMI
00689                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00690      ELSE                                                         ELXSTRMI
00691      IF SUB-ABUSE-DRG-CALL                                        ELXSTRMI
00692          MOVE 'Please call for details'                           ELXSTRMI
00693                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00694      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00695      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00696          MOVE 'Coordinated Home Care'                             ELXSTRMI
00697                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00698          MOVE '05'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
00699      IF CHC-NO-COVERAGE                                           ELXSTRMI
00700          MOVE 'Not Covered'                                       ELXSTRMI
00701                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00702      ELSE                                                         ELXSTRMI
00703      IF CHC-NO-REQUIREMENTS OR                                    ELXSTRMI
00704         CHC-HOURS-REQUIREMENT OR                                  ELXSTRMI
00705         CHC-DAYS-REQUIREMENT OR                                   ELXSTRMI
00706         CHC-UNLIMITED                                             ELXSTRMI
00707          MOVE 'Covered'                                           ELXSTRMI
00708                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00709      ELSE                                                         ELXSTRMI
00710      IF CHC-CALL                                                  ELXSTRMI
00711          MOVE 'Please call for details'                           ELXSTRMI
00712                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00713      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00714      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00715          MOVE 'Coordination of Benefits'                          ELXSTRMI
00716                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
00717      IF COB-NO-CLAUSE                                             ELXSTRMI
00718          MOVE 'Does not apply'                                    ELXSTRMI
00719                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00720      ELSE                                                         ELXSTRMI
00721      IF COB-BIRTHDAY-RULE OR                                      ELXSTRMI
00722         COB-GENDER-RULE OR                                        ELXSTRMI
00723         COB-APPLIES                                               ELXSTRMI
00724          MOVE 'Applies'                                           ELXSTRMI
00725                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00726      ELSE                                                         ELXSTRMI
00727      IF COB-CALL                                                  ELXSTRMI
00728          MOVE 'Please call for details'                           ELXSTRMI
00729                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00730      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00731      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00732          MOVE 'Diagnostic Laboratory Services'                    ELXSTRMI
00733                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
00734      IF PMCI-LAB-NO                                               ELXSTRMI
00735          MOVE 'Not Covered'                                       ELXSTRMI
00736                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00737      ELSE                                                         ELXSTRMI
00738      IF PMCI-LAB-YES                                              ELXSTRMI
00739          MOVE 'Covered'                                           ELXSTRMI
00740                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00741      ELSE                                                         ELXSTRMI
00742      IF PMCI-LAB-CALL                                             ELXSTRMI
00743          MOVE 'Please call for details'                           ELXSTRMI
00744                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00745      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00746      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00747          MOVE 'Diagnostic Xray'                                   ELXSTRMI
00748                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
00749      IF XRAY-NO                                                   ELXSTRMI
00750          MOVE 'Not Covered'                                       ELXSTRMI
00751                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00752      ELSE                                                         ELXSTRMI
00753      IF XRAY-YES                                                  ELXSTRMI
00754          MOVE 'Covered'                                           ELXSTRMI
00755                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00756      ELSE                                                         ELXSTRMI
00757      IF XRAY-CALL                                                 ELXSTRMI
00758          MOVE 'Please call for details'                           ELXSTRMI
00759                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00760      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00761      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00762          MOVE 'Durable Medical Equipment'                         ELXSTRMI
00763                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
00764      IF DME-NO                                                    ELXSTRMI
00765          MOVE 'Not Covered'                                       ELXSTRMI
00766                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00767      ELSE                                                         ELXSTRMI
00768      IF DME-YES                                                   ELXSTRMI
00769          MOVE 'Covered'                                           ELXSTRMI
00770                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00771      ELSE                                                         ELXSTRMI
00772      IF DME-CALL                                                  ELXSTRMI
00773          MOVE 'Please call for details'                           ELXSTRMI
00774                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00775      ELSE                                                         ELXSTRMI
00776      IF DME-RENTAL-ONLY                                           ELXSTRMI
00777          MOVE 'Rental'                                            ELXSTRMI
00778                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00779      ELSE                                                         ELXSTRMI
00780      IF DME-PURCH-ONLY                                            ELXSTRMI
00781          MOVE 'Purchase'                                          ELXSTRMI
00782                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00783      ELSE                                                         ELXSTRMI
00784      IF DME-RENT-AND-PURCH                                        ELXSTRMI
00785          MOVE 'Rental and Purchase'                               ELXSTRMI
00786                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00787      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00788      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00789          MOVE 'Emergency Accident Care'                           ELXSTRMI
00790                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
00791      IF EAC-NO-COVERAGE                                           ELXSTRMI
00792          MOVE 'Not Covered'                                       ELXSTRMI
00793                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00794      ELSE                                                         ELXSTRMI
00795      IF EAC-NO-HOURS                                              ELXSTRMI
00796          MOVE 'Covered'                                           ELXSTRMI
00797                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00798      ELSE                                                         ELXSTRMI
00799      IF EAC-HOURS                                                 ELXSTRMI
00800          IF PMCI-EMERGENCY-ACCIDENT-CARE = +0                     ELXSTRMI
00801              MOVE 'Covered'                                       ELXSTRMI
00802                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00803          ELSE                                                     ELXSTRMI
00804              MOVE PMCI-EMERGENCY-ACCIDENT-CARE TO WS-HOURS        ELXSTRMI
00805              PERFORM 8000-000-CALCULATE-DAYS                      ELXSTRMI
00806              MOVE WS-HRDAY-LITERAL                                ELXSTRMI
00807                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00808              MOVE '06' TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)        ELXSTRMI
00809      ELSE                                                         ELXSTRMI
00810      IF EAC-CALL                                                  ELXSTRMI
00811          MOVE 'Please call for details'                           ELXSTRMI
00812                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00813      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00814      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00815          MOVE 'Emergency Accident Care Copay'                     ELXSTRMI
00816                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00817          MOVE PMCI-EMERG-ACCDNT-CO-PAY-VL                         ELXSTRMI
00818                            TO WS-SAVE-EAC-COPAY.                  ELXSTRMI
00819      IF PMCI-NON-PPO-PROVIDER                                     ELXSTRMI
00820          IF (WS-SAVE-EAC-COPAY > +0) AND                          ELXSTRMI
00821             (PMCI-EMERG-ACCDNT-CO-PAY-VL = +0)                    ELXSTRMI
00822              MOVE '08'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
00823      IF PMCI-NON-PPO-PROVIDER                                     ELXSTRMI
00824          IF (WS-SAVE-EAC-COPAY = +0) AND                          ELXSTRMI
00825             (PMCI-EMERG-ACCDNT-CO-PAY-VL = +0)                    ELXSTRMI
00826              SET WS-BEN-IDX TO BSTRM-BEN-IDX                      ELXSTRMI
00827              PERFORM 6000-000-LOOP-THRU-BENS                      ELXSTRMI
00828              SET BSTRM-BEN-IDX DOWN BY +1                         ELXSTRMI
00829              GO TO 3000-ELT.                                      ELXSTRMI
00830      IF PMCI-EMERG-ACCDNT-CO-PAY-VL > +0                          ELXSTRMI
00831          MOVE PMCI-EMERG-ACCDNT-CO-PAY-VL                         ELXSTRMI
00832                            TO WS-COPAY-NUM                        ELXSTRMI
00833          MOVE SPACES       TO WS-COPAY-LIT                        ELXSTRMI
00834          IF EMAC-PER-VISIT                                        ELXSTRMI
00835              MOVE ' Per Visit'                                    ELXSTRMI
00836                            TO WS-COPAY-LIT                        ELXSTRMI
00837          ELSE                                                     ELXSTRMI
00838          IF EMAC-PER-OCCURRENCE                                   ELXSTRMI
00839              MOVE ' Per Occurrence'                               ELXSTRMI
00840                            TO WS-COPAY-LIT                        ELXSTRMI
00841          ELSE                                                     ELXSTRMI
00842          IF EMAC-PER-CONFINEMENT                                  ELXSTRMI
00843              MOVE ' Per Confinement'                              ELXSTRMI
00844                            TO WS-COPAY-LIT                        ELXSTRMI
00845          ELSE                                                     ELXSTRMI
00846          IF EMAC-PER-TREATMENT                                    ELXSTRMI
00847              MOVE ' Per Treatment'                                ELXSTRMI
00848                            TO WS-COPAY-LIT                        ELXSTRMI
00849          ELSE                                                     ELXSTRMI
00850          IF EMAC-PER-DAY                                          ELXSTRMI
00851              MOVE ' Per Day'                                      ELXSTRMI
00852                            TO WS-COPAY-LIT                        ELXSTRMI
00853          ELSE                                                     ELXSTRMI
00854          IF EMAC-PER-WEEK                                         ELXSTRMI
00855              MOVE ' Per Week'                                     ELXSTRMI
00856                            TO WS-COPAY-LIT                        ELXSTRMI
00857          ELSE                                                     ELXSTRMI
00858          IF EMAC-PER-MONTH                                        ELXSTRMI
00859              MOVE ' Per Month'                                    ELXSTRMI
00860                            TO WS-COPAY-LIT                        ELXSTRMI
00861          ELSE                                                     ELXSTRMI
00862          IF EMAC-PER-YEAR                                         ELXSTRMI
00863              MOVE ' Per Year'                                     ELXSTRMI
00864                            TO WS-COPAY-LIT                        ELXSTRMI
00865          ELSE                                                     ELXSTRMI
00866          IF EMAC-PER-LIFETIME                                     ELXSTRMI
00867              MOVE ' Per Lifetime'                                 ELXSTRMI
00868                            TO WS-COPAY-LIT.                       ELXSTRMI
00869      IF (EMAC-CALL) OR                                            ELXSTRMI
00870         (EMAC-FROM-MEMBERSHIP)                                    ELXSTRMI
00871          MOVE 'Please call for details'                           ELXSTRMI
00872                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00873      ELSE                                                         ELXSTRMI
00874      IF (EMAC-NO-COVERAGE) OR                                     ELXSTRMI
00875         (EMAC-NOT-APPLICABLE) OR                                  ELXSTRMI
00876         (EMAC-NONE)                                               ELXSTRMI
00877          MOVE 'Does Not Apply'                                    ELXSTRMI
00878                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00879      ELSE                                                         ELXSTRMI
00880      IF EMAC-UNLIMITED                                            ELXSTRMI
00881          MOVE 'No Limit'                                          ELXSTRMI
00882                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00883      ELSE                                                         ELXSTRMI
00884          MOVE WS-COPAY-LITERAL                                    ELXSTRMI
00885                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00886  3000-ELT.                                                        ELXSTRMI
00887      IF ELT-NO-COVERAGE OR                                        ELXSTRMI
00888         ELT-NOT-APPLICABLE                                        ELXSTRMI
00889          GO TO 3000-EMC.                                          ELXSTRMI
00890      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00891      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00892          MOVE 'Emergency Life Threatening'                        ELXSTRMI
00893                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
00894      IF ELT-NO-HOURS                                              ELXSTRMI
00895          MOVE 'Covered'                                           ELXSTRMI
00896                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00897      ELSE                                                         ELXSTRMI
00898      IF ELT-HOURS                                                 ELXSTRMI
00899          IF PMCI-EMERGENCY-LIFE-THREAT = +0                       ELXSTRMI
00900              MOVE 'Covered'                                       ELXSTRMI
00901                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00902          ELSE                                                     ELXSTRMI
00903              MOVE PMCI-EMERGENCY-LIFE-THREAT TO WS-HOURS          ELXSTRMI
00904              PERFORM 8000-000-CALCULATE-DAYS                      ELXSTRMI
00905              MOVE WS-HRDAY-LITERAL                                ELXSTRMI
00906                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00907              MOVE '06'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
00908      ELSE                                                         ELXSTRMI
00909      IF ELT-CALL                                                  ELXSTRMI
00910          MOVE 'Please call for details'                           ELXSTRMI
00911                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00912      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00913      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00914          MOVE 'Emergency Life Threatening Copay'                  ELXSTRMI
00915                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
00916          MOVE PMCI-EMERG-LFTHRN-CO-PAY-VL                         ELXSTRMI
00917                            TO WS-SAVE-ELT-COPAY.                  ELXSTRMI
00918      IF PMCI-NON-PPO-PROVIDER                                     ELXSTRMI
00919          IF (WS-SAVE-ELT-COPAY > +0) AND                          ELXSTRMI
00920             (PMCI-EMERG-LFTHRN-CO-PAY-VL = +0)                    ELXSTRMI
00921              MOVE '08'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
00922      IF PMCI-NON-PPO-PROVIDER                                     ELXSTRMI
00923          IF (WS-SAVE-ELT-COPAY = +0) AND                          ELXSTRMI
00924             (PMCI-EMERG-LFTHRN-CO-PAY-VL = +0)                    ELXSTRMI
00925              SET WS-BEN-IDX TO BSTRM-BEN-IDX                      ELXSTRMI
00926              PERFORM 6000-000-LOOP-THRU-BENS                      ELXSTRMI
00927              SET BSTRM-BEN-IDX DOWN BY +1                         ELXSTRMI
00928              GO TO 3000-EMC.                                      ELXSTRMI
00929      IF PMCI-EMERG-LFTHRN-CO-PAY-VL > +0                          ELXSTRMI
00930          MOVE PMCI-EMERG-LFTHRN-CO-PAY-VL                         ELXSTRMI
00931                            TO WS-COPAY-NUM                        ELXSTRMI
00932          MOVE SPACES       TO WS-COPAY-LIT                        ELXSTRMI
00933          IF EMLF-PER-VISIT                                        ELXSTRMI
00934              MOVE ' Per Visit'                                    ELXSTRMI
00935                            TO WS-COPAY-LIT                        ELXSTRMI
00936          ELSE                                                     ELXSTRMI
00937          IF EMLF-PER-OCCURRENCE                                   ELXSTRMI
00938              MOVE ' Per Occurrence'                               ELXSTRMI
00939                            TO WS-COPAY-LIT                        ELXSTRMI
00940          ELSE                                                     ELXSTRMI
00941          IF EMLF-PER-CONFINEMENT                                  ELXSTRMI
00942              MOVE ' Per Confinement'                              ELXSTRMI
00943                            TO WS-COPAY-LIT                        ELXSTRMI
00944          ELSE                                                     ELXSTRMI
00945          IF EMLF-PER-TREATMENT                                    ELXSTRMI
00946              MOVE ' Per Treatment'                                ELXSTRMI
00947                            TO WS-COPAY-LIT                        ELXSTRMI
00948          ELSE                                                     ELXSTRMI
00949          IF EMLF-PER-DAY                                          ELXSTRMI
00950              MOVE ' Per Day'                                      ELXSTRMI
00951                            TO WS-COPAY-LIT                        ELXSTRMI
00952          ELSE                                                     ELXSTRMI
00953          IF EMLF-PER-WEEK                                         ELXSTRMI
00954              MOVE ' Per Week'                                     ELXSTRMI
00955                            TO WS-COPAY-LIT                        ELXSTRMI
00956          ELSE                                                     ELXSTRMI
00957          IF EMLF-PER-MONTH                                        ELXSTRMI
00958              MOVE ' Per Month'                                    ELXSTRMI
00959                            TO WS-COPAY-LIT                        ELXSTRMI
00960          ELSE                                                     ELXSTRMI
00961          IF EMLF-PER-YEAR                                         ELXSTRMI
00962              MOVE ' Per Year'                                     ELXSTRMI
00963                            TO WS-COPAY-LIT                        ELXSTRMI
00964          ELSE                                                     ELXSTRMI
00965          IF EMLF-PER-LIFETIME                                     ELXSTRMI
00966              MOVE ' Per Lifetime'                                 ELXSTRMI
00967                            TO WS-COPAY-LIT.                       ELXSTRMI
00968      IF (EMLF-CALL) OR                                            ELXSTRMI
00969         (EMLF-FROM-MEMBERSHIP)                                    ELXSTRMI
00970          MOVE 'Please call for details'                           ELXSTRMI
00971                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00972      ELSE                                                         ELXSTRMI
00973      IF (EMLF-NO-COVERAGE) OR                                     ELXSTRMI
00974         (EMLF-NOT-APPLICABLE) OR                                  ELXSTRMI
00975         (EMLF-NONE)                                               ELXSTRMI
00976          MOVE 'Does Not Apply'                                    ELXSTRMI
00977                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00978      ELSE                                                         ELXSTRMI
00979      IF EMLF-UNLIMITED                                            ELXSTRMI
00980          MOVE 'No Limit'                                          ELXSTRMI
00981                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00982      ELSE                                                         ELXSTRMI
00983          MOVE WS-COPAY-LITERAL                                    ELXSTRMI
00984                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
00985  3000-EMC.                                                        ELXSTRMI
00986      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
00987      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
00988          MOVE 'Emergency Medical Care'                            ELXSTRMI
00989                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
00990      IF EMC-NO-COVERAGE                                           ELXSTRMI
00991          MOVE 'Not Covered'                                       ELXSTRMI
00992                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00993      ELSE                                                         ELXSTRMI
00994      IF EMC-NO-HOURS                                              ELXSTRMI
00995          MOVE 'Covered'                                           ELXSTRMI
00996                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
00997      ELSE                                                         ELXSTRMI
00998      IF EMC-HOURS                                                 ELXSTRMI
00999          IF PMCI-EMERGENCY-MEDICAL-CARE = +0                      ELXSTRMI
01000              MOVE 'Covered'                                       ELXSTRMI
01001                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01002          ELSE                                                     ELXSTRMI
01003              MOVE PMCI-EMERGENCY-MEDICAL-CARE TO WS-HOURS         ELXSTRMI
01004              PERFORM 8000-000-CALCULATE-DAYS                      ELXSTRMI
01005              MOVE WS-HRDAY-LITERAL                                ELXSTRMI
01006                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01007              MOVE '06'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01008      ELSE                                                         ELXSTRMI
01009      IF EMC-CALL                                                  ELXSTRMI
01010          MOVE 'Please call for details'                           ELXSTRMI
01011                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01012      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
01013      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
01014          MOVE 'Emergency Medical Care Copay'                      ELXSTRMI
01015                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01016          MOVE PMCI-EMERG-MEDCL-CO-PAY-VL                          ELXSTRMI
01017                            TO WS-SAVE-EMC-COPAY.                  ELXSTRMI
01018      IF PMCI-NON-PPO-PROVIDER                                     ELXSTRMI
01019          IF (WS-SAVE-EMC-COPAY > +0) AND                          ELXSTRMI
01020             (PMCI-EMERG-MEDCL-CO-PAY-VL = +0)                     ELXSTRMI
01021              MOVE '08'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
01022      IF PMCI-NON-PPO-PROVIDER                                     ELXSTRMI
01023          IF (WS-SAVE-EMC-COPAY = +0) AND                          ELXSTRMI
01024             (PMCI-EMERG-MEDCL-CO-PAY-VL = +0)                     ELXSTRMI
01025              SET WS-BEN-IDX TO BSTRM-BEN-IDX                      ELXSTRMI
01026              PERFORM 6000-000-LOOP-THRU-BENS                      ELXSTRMI
01027              SET BSTRM-BEN-IDX DOWN BY +1                         ELXSTRMI
01028              GO TO 3000-HOSPICE.                                  ELXSTRMI
01029      IF PMCI-EMERG-MEDCL-CO-PAY-VL > +0                           ELXSTRMI
01030          MOVE PMCI-EMERG-MEDCL-CO-PAY-VL                          ELXSTRMI
01031                            TO WS-COPAY-NUM                        ELXSTRMI
01032          MOVE SPACES       TO WS-COPAY-LIT                        ELXSTRMI
01033          IF EMMD-PER-VISIT                                        ELXSTRMI
01034              MOVE ' Per Visit'                                    ELXSTRMI
01035                            TO WS-COPAY-LIT                        ELXSTRMI
01036          ELSE                                                     ELXSTRMI
01037          IF EMMD-PER-OCCURRENCE                                   ELXSTRMI
01038              MOVE ' Per Occurrence'                               ELXSTRMI
01039                            TO WS-COPAY-LIT                        ELXSTRMI
01040          ELSE                                                     ELXSTRMI
01041          IF EMMD-PER-CONFINEMENT                                  ELXSTRMI
01042              MOVE ' Per Confinement'                              ELXSTRMI
01043                            TO WS-COPAY-LIT                        ELXSTRMI
01044          ELSE                                                     ELXSTRMI
01045          IF EMMD-PER-TREATMENT                                    ELXSTRMI
01046              MOVE ' Per Treatment'                                ELXSTRMI
01047                            TO WS-COPAY-LIT                        ELXSTRMI
01048          ELSE                                                     ELXSTRMI
01049          IF EMMD-PER-DAY                                          ELXSTRMI
01050              MOVE ' Per Day'                                      ELXSTRMI
01051                            TO WS-COPAY-LIT                        ELXSTRMI
01052          ELSE                                                     ELXSTRMI
01053          IF EMMD-PER-WEEK                                         ELXSTRMI
01054              MOVE ' Per Week'                                     ELXSTRMI
01055                            TO WS-COPAY-LIT                        ELXSTRMI
01056          ELSE                                                     ELXSTRMI
01057          IF EMMD-PER-MONTH                                        ELXSTRMI
01058              MOVE ' Per Month'                                    ELXSTRMI
01059                            TO WS-COPAY-LIT                        ELXSTRMI
01060          ELSE                                                     ELXSTRMI
01061          IF EMMD-PER-YEAR                                         ELXSTRMI
01062              MOVE ' Per Year'                                     ELXSTRMI
01063                            TO WS-COPAY-LIT                        ELXSTRMI
01064          ELSE                                                     ELXSTRMI
01065          IF EMMD-PER-LIFETIME                                     ELXSTRMI
01066              MOVE ' Per Lifetime'                                 ELXSTRMI
01067                            TO WS-COPAY-LIT.                       ELXSTRMI
01068      IF (EMMD-CALL) OR                                            ELXSTRMI
01069         (EMMD-FROM-MEMBERSHIP)                                    ELXSTRMI
01070          MOVE 'Please call for details'                           ELXSTRMI
01071                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01072      ELSE                                                         ELXSTRMI
01073      IF (EMMD-NO-COVERAGE) OR                                     ELXSTRMI
01074         (EMMD-NOT-APPLICABLE) OR                                  ELXSTRMI
01075         (EMMD-NONE)                                               ELXSTRMI
01076          MOVE 'Does Not Apply'                                    ELXSTRMI
01077                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01078      ELSE                                                         ELXSTRMI
01079      IF EMMD-UNLIMITED                                            ELXSTRMI
01080          MOVE 'No Limit'                                          ELXSTRMI
01081                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01082      ELSE                                                         ELXSTRMI
01083          MOVE WS-COPAY-LITERAL                                    ELXSTRMI
01084                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01085  3000-HOSPICE.                                                    ELXSTRMI
01086      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
01087      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
01088          MOVE 'Hospice Care'                                      ELXSTRMI
01089                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
01090      IF (BSTRM-BENEFIT(BSTRM-BEN-IDX 1) = 'Not Covered') AND      ELXSTRMI
01091         (HOSPICE-NO)                                              ELXSTRMI
01092          SET WS-BEN-IDX TO BSTRM-BEN-IDX                          ELXSTRMI
01093          PERFORM 6000-000-LOOP-THRU-BENS                          ELXSTRMI
01094          SET BSTRM-BEN-IDX DOWN BY +1                             ELXSTRMI
01095          GO TO 3000-DRB.                                          ELXSTRMI
01096      IF HOSPICE-NO                                                ELXSTRMI
01097          MOVE 'Not Covered'                                       ELXSTRMI
01098                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01099      ELSE                                                         ELXSTRMI
01100      IF HOSPICE-YES                                               ELXSTRMI
01101          MOVE 'Covered'                                           ELXSTRMI
01102                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01103          IF PMCI-HOSPICE-LIFETIME-MAX = +0                        ELXSTRMI
01104              NEXT SENTENCE                                        ELXSTRMI
01105          ELSE                                                     ELXSTRMI
01106              PERFORM 5000-000-INCREASE-INDEX                      ELXSTRMI
01107              IF PMCI-PPO-PROVIDER                                 ELXSTRMI
01108                  MOVE 'Hospice Care Maximum'                      ELXSTRMI
01109                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01110                  MOVE ' I' TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01111                  MOVE PMCI-HOSPICE-LIFETIME-MAX                   ELXSTRMI
01112                    TO WS-DOLLAR-AMT                               ELXSTRMI
01113                  MOVE WS-DOLLAR-AMT                               ELXSTRMI
01114                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01115              ELSE                                                 ELXSTRMI
01116                  MOVE ' I' TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01117                  MOVE PMCI-HOSPICE-LIFETIME-MAX                   ELXSTRMI
01118                    TO WS-DOLLAR-AMT                               ELXSTRMI
01119                  MOVE WS-DOLLAR-AMT                               ELXSTRMI
01120                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01121      IF HOSPICE-CALL                                              ELXSTRMI
01122          MOVE 'Please call for details'                           ELXSTRMI
01123                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01124  3000-DRB.                                                        ELXSTRMI
01125      IF PMCI-INPATIENT AND                                        ELXSTRMI
01126         PMCI-INSTITUTIONAL                                        ELXSTRMI
01127          PERFORM 5000-000-INCREASE-INDEX.                         ELXSTRMI
01128      IF PMCI-INPATIENT AND                                        ELXSTRMI
01129         PMCI-INSTITUTIONAL                                        ELXSTRMI
01130          IF PMCI-PPO-PROVIDER                                     ELXSTRMI
01131              MOVE 'Inpatient Daily Room and Board'                ELXSTRMI
01132                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
01133      IF PMCI-INPATIENT AND                                        ELXSTRMI
01134         PMCI-INSTITUTIONAL                                        ELXSTRMI
01135          IF DRB-NO                                                ELXSTRMI
01136              MOVE 'Not Covered'                                   ELXSTRMI
01137                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01138          ELSE                                                     ELXSTRMI
01139          IF DRB-YES                                               ELXSTRMI
01140              MOVE 'Covered'                                       ELXSTRMI
01141                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01142          ELSE                                                     ELXSTRMI
01143          IF DRB-IND-CALL                                          ELXSTRMI
01144              MOVE 'Please call for details'                       ELXSTRMI
01145                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01146      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
01147      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
01148          MOVE 'Maternity Services'                                ELXSTRMI
01149                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
01150      IF OB-NORM-NO                                                ELXSTRMI
01151          MOVE 'Not Covered'                                       ELXSTRMI
01152                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01153      ELSE                                                         ELXSTRMI
01154      IF OB-NORM-YES                                               ELXSTRMI
01155          MOVE 'Covered'                                           ELXSTRMI
01156                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01157      ELSE                                                         ELXSTRMI
01158      IF OB-NORM-CALL                                              ELXSTRMI
01159          MOVE 'Please call for details'                           ELXSTRMI
01160                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01161      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
01162      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
01163          MOVE 'Medical Services Department'                       ELXSTRMI
01164                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
01165      IF PMCI-MSA-NO                                               ELXSTRMI
01166          MOVE 'Does Not Apply'                                    ELXSTRMI
01167                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01168      ELSE                                                         ELXSTRMI
01169      IF PMCI-MSA-YES                                              ELXSTRMI
01170          MOVE 'Applies'                                           ELXSTRMI
01171                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01172      ELSE                                                         ELXSTRMI
01173      IF PMCI-MSA-CALL                                             ELXSTRMI
01174          MOVE 'Please call for details'                           ELXSTRMI
01175                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01176      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
01177      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
01178          MOVE 'Office Visits'                                     ELXSTRMI
01179                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
01180      IF PMCI-OFF-VISITS-NOT-COVERED                               ELXSTRMI
01181          MOVE 'Not Covered'                                       ELXSTRMI
01182                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01183      ELSE                                                         ELXSTRMI
01184      IF PMCI-OFF-VISITS-COVERED                                   ELXSTRMI
01185          MOVE 'Covered'                                           ELXSTRMI
01186                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01187      ELSE                                                         ELXSTRMI
01188      IF PMCI-OFF-VISITS-CALL                                      ELXSTRMI
01189          MOVE 'Please call for details'                           ELXSTRMI
01190                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01191      IF (BSTRM-IP-OR-OP-INQUIRY = 'O') AND                        ELXSTRMI
01192         (BSTRM-PROVIDER-INDICATOR = 'P')                          ELXSTRMI
01193          PERFORM 5000-000-INCREASE-INDEX                          ELXSTRMI
01194          IF PMCI-PPO-PROVIDER                                     ELXSTRMI
01195              MOVE 'Office Visit Copay'                            ELXSTRMI
01196                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01197              MOVE PMCI-OFFICE-VISITS-CO-PAY-VL                    ELXSTRMI
01198                            TO WS-SAVE-OFC-COPAY.                  ELXSTRMI
01199      IF (BSTRM-IP-OR-OP-INQUIRY = 'O') AND                        ELXSTRMI
01200         (BSTRM-PROVIDER-INDICATOR = 'P') AND                      ELXSTRMI
01201         (PMCI-NON-PPO-PROVIDER)                                   ELXSTRMI
01202          IF (WS-SAVE-OFC-COPAY > +0) AND                          ELXSTRMI
01203             (PMCI-OFFICE-VISITS-CO-PAY-VL = +0)                   ELXSTRMI
01204              MOVE '08'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
01205      IF (BSTRM-IP-OR-OP-INQUIRY = 'O') AND                        ELXSTRMI
01206         (BSTRM-PROVIDER-INDICATOR = 'P') AND                      ELXSTRMI
01207         (PMCI-NON-PPO-PROVIDER)                                   ELXSTRMI
01208          IF (WS-SAVE-OFC-COPAY = +0) AND                          ELXSTRMI
01209             (PMCI-OFFICE-VISITS-CO-PAY-VL = +0)                   ELXSTRMI
01210              SET WS-BEN-IDX TO BSTRM-BEN-IDX                      ELXSTRMI
01211              PERFORM 6000-000-LOOP-THRU-BENS                      ELXSTRMI
01212              SET BSTRM-BEN-IDX DOWN BY +1                         ELXSTRMI
01213              GO TO 3000-OT.                                       ELXSTRMI
01214      IF (BSTRM-IP-OR-OP-INQUIRY = 'O') AND                        ELXSTRMI
01215         (BSTRM-PROVIDER-INDICATOR = 'P')                          ELXSTRMI
01216          IF PMCI-OFFICE-VISITS-CO-PAY-VL > +0                     ELXSTRMI
01217              MOVE PMCI-OFFICE-VISITS-CO-PAY-VL                    ELXSTRMI
01218                            TO WS-COPAY-NUM                        ELXSTRMI
01219              MOVE SPACES   TO WS-COPAY-LIT                        ELXSTRMI
01220              IF OFVS-PER-VISIT                                    ELXSTRMI
01221                  MOVE ' Per Visit'                                ELXSTRMI
01222                            TO WS-COPAY-LIT                        ELXSTRMI
01223              ELSE                                                 ELXSTRMI
01224              IF OFVS-PER-OCCURRENCE                               ELXSTRMI
01225                  MOVE ' Per Occurrence'                           ELXSTRMI
01226                            TO WS-COPAY-LIT                        ELXSTRMI
01227              ELSE                                                 ELXSTRMI
01228              IF OFVS-PER-CONFINEMENT                              ELXSTRMI
01229                  MOVE ' Per Confinement'                          ELXSTRMI
01230                            TO WS-COPAY-LIT                        ELXSTRMI
01231              ELSE                                                 ELXSTRMI
01232              IF OFVS-PER-TREATMENT                                ELXSTRMI
01233                  MOVE ' Per Treatment'                            ELXSTRMI
01234                            TO WS-COPAY-LIT                        ELXSTRMI
01235              ELSE                                                 ELXSTRMI
01236              IF OFVS-PER-DAY                                      ELXSTRMI
01237                  MOVE ' Per Day'                                  ELXSTRMI
01238                            TO WS-COPAY-LIT                        ELXSTRMI
01239              ELSE                                                 ELXSTRMI
01240              IF OFVS-PER-WEEK                                     ELXSTRMI
01241                  MOVE ' Per Week'                                 ELXSTRMI
01242                            TO WS-COPAY-LIT                        ELXSTRMI
01243              ELSE                                                 ELXSTRMI
01244              IF OFVS-PER-MONTH                                    ELXSTRMI
01245                  MOVE ' Per Month'                                ELXSTRMI
01246                            TO WS-COPAY-LIT                        ELXSTRMI
01247              ELSE                                                 ELXSTRMI
01248              IF OFVS-PER-YEAR                                     ELXSTRMI
01249                  MOVE ' Per Year'                                 ELXSTRMI
01250                            TO WS-COPAY-LIT                        ELXSTRMI
01251              ELSE                                                 ELXSTRMI
01252              IF OFVS-PER-LIFETIME                                 ELXSTRMI
01253                  MOVE ' Per Lifetime'                             ELXSTRMI
01254                            TO WS-COPAY-LIT.                       ELXSTRMI
01255      IF (BSTRM-IP-OR-OP-INQUIRY = 'O') AND                        ELXSTRMI
01256         (BSTRM-PROVIDER-INDICATOR = 'P')                          ELXSTRMI
01257          IF (OFVS-CALL) OR                                        ELXSTRMI
01258             (OFVS-FROM-MEMBERSHIP)                                ELXSTRMI
01259              MOVE 'Please call for details'                       ELXSTRMI
01260                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01261          ELSE                                                     ELXSTRMI
01262          IF (OFVS-NO-COVERAGE) OR                                 ELXSTRMI
01263             (OFVS-NOT-APPLICABLE) OR                              ELXSTRMI
01264             (PMCI-OFFICE-VISITS-CO-PAY-BP = ' ')                  ELXSTRMI
01265              MOVE 'Does Not Apply'                                ELXSTRMI
01266                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01267          ELSE                                                     ELXSTRMI
01268          IF OFVS-UNLIMITED                                        ELXSTRMI
01269              MOVE 'No Limit'                                      ELXSTRMI
01270                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01271          ELSE                                                     ELXSTRMI
01272              MOVE WS-COPAY-LITERAL                                ELXSTRMI
01273                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01274  3000-OT.                                                         ELXSTRMI
01275      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
01276      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
01277          MOVE 'Organ Transplant'                                  ELXSTRMI
01278                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
01279      IF (BSTRM-BENEFIT(BSTRM-BEN-IDX 1) = 'Not Covered') AND      ELXSTRMI
01280         (HOTS-NO)                                                 ELXSTRMI
01281          SET WS-BEN-IDX TO BSTRM-BEN-IDX                          ELXSTRMI
01282          PERFORM 6000-000-LOOP-THRU-BENS                          ELXSTRMI
01283          SET BSTRM-BEN-IDX DOWN BY +1                             ELXSTRMI
01284          GO TO 3000-PDN.                                          ELXSTRMI
01285      IF HOTS-NO                                                   ELXSTRMI
01286          MOVE 'Not Covered'                                       ELXSTRMI
01287                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01288      ELSE                                                         ELXSTRMI
01289      IF HOTS-YES                                                  ELXSTRMI
01290          MOVE 'Covered'                                           ELXSTRMI
01291                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01292          IF PMCI-HOTS-LIFETIME-MAX = +0                           ELXSTRMI
01293              NEXT SENTENCE                                        ELXSTRMI
01294          ELSE                                                     ELXSTRMI
01295              PERFORM 5000-000-INCREASE-INDEX                      ELXSTRMI
01296              IF PMCI-PPO-PROVIDER                                 ELXSTRMI
01297                  MOVE 'Organ Transplant Maximum'                  ELXSTRMI
01298                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01299                  MOVE ' I' TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01300                  MOVE PMCI-HOTS-LIFETIME-MAX                      ELXSTRMI
01301                    TO WS-DOLLAR-AMT                               ELXSTRMI
01302                  MOVE WS-DOLLAR-AMT                               ELXSTRMI
01303                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01304              ELSE                                                 ELXSTRMI
01305                  MOVE ' I' TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01306                  MOVE PMCI-HOTS-LIFETIME-MAX                      ELXSTRMI
01307                    TO WS-DOLLAR-AMT                               ELXSTRMI
01308                  MOVE WS-DOLLAR-AMT                               ELXSTRMI
01309                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01310      IF HOTS-CALL                                                 ELXSTRMI
01311          MOVE 'Please call for details'                           ELXSTRMI
01312                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01313  3000-PDN.                                                        ELXSTRMI
01314      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
01315      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
01316          MOVE 'Private Duty Nursing'                              ELXSTRMI
01317                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01318          MOVE '05'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
01319      IF PMCI-PDN-NOT-COVERED                                      ELXSTRMI
01320          MOVE 'Not Covered'                                       ELXSTRMI
01321                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01322      ELSE                                                         ELXSTRMI
01323      IF PMCI-PDN-COVERED                                          ELXSTRMI
01324          MOVE 'Covered'                                           ELXSTRMI
01325                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01326      ELSE                                                         ELXSTRMI
01327      IF PMCI-PDN-CALL                                             ELXSTRMI
01328          MOVE 'Please call for details'                           ELXSTRMI
01329                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01330      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
01331      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
01332          MOVE 'Psychiatric Services'                              ELXSTRMI
01333                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01334          MOVE '05'         TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX).   ELXSTRMI
01335      IF PSY-NO                                                    ELXSTRMI
01336          MOVE 'Not Covered'                                       ELXSTRMI
01337                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01338      ELSE                                                         ELXSTRMI
01339      IF PSY-YES                                                   ELXSTRMI
01340          MOVE 'Covered'                                           ELXSTRMI
01341                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01342      ELSE                                                         ELXSTRMI
01343      IF PSY-CALL                                                  ELXSTRMI
01344          MOVE 'Please call for details'                           ELXSTRMI
01345                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01346 *    PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
01347 *    IF PMCI-PPO-PROVIDER                                         ELXSTRMI
01348 *        MOVE 'Skilled Nursing/Extended Care Facility'            ELXSTRMI
01349 *                          TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
01350 *    IF PMCI-SNF-NO                                               ELXSTRMI
01351 *        MOVE 'Not Covered'                                       ELXSTRMI
01352 *                  TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01353 *    ELSE                                                         ELXSTRMI
01354 *    IF PMCI-SNF-YES                                              ELXSTRMI
01355 *        MOVE 'Covered'                                           ELXSTRMI
01356 *                  TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01357      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
01358      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
01359          MOVE 'Therapies:' TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
01360      IF PMCI-OCC-THRPY-NOT-COVERED                                ELXSTRMI
01361          MOVE SPACES                                              ELXSTRMI
01362                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01363          PERFORM 5000-000-INCREASE-INDEX                          ELXSTRMI
01364          IF PMCI-PPO-PROVIDER                                     ELXSTRMI
01365              MOVE 'Functional Occupational'                       ELXSTRMI
01366                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01367              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01368              MOVE 'Not Covered'                                   ELXSTRMI
01369                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01370          ELSE                                                     ELXSTRMI
01371              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01372              MOVE 'Not Covered'                                   ELXSTRMI
01373                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01374      IF PMCI-OCC-THRPY-COVERED                                    ELXSTRMI
01375          MOVE SPACES                                              ELXSTRMI
01376                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01377          PERFORM 5000-000-INCREASE-INDEX                          ELXSTRMI
01378          IF PMCI-PPO-PROVIDER                                     ELXSTRMI
01379              MOVE 'Functional Occupational'                       ELXSTRMI
01380                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01381              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01382              MOVE 'Covered'                                       ELXSTRMI
01383                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01384          ELSE                                                     ELXSTRMI
01385              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01386              MOVE 'Covered'                                       ELXSTRMI
01387                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01388      IF PMCI-OCC-THRPY-CALL                                       ELXSTRMI
01389          MOVE SPACES                                              ELXSTRMI
01390                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01391          PERFORM 5000-000-INCREASE-INDEX                          ELXSTRMI
01392          IF PMCI-PPO-PROVIDER                                     ELXSTRMI
01393              MOVE 'Functional Occupational'                       ELXSTRMI
01394                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01395              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01396              MOVE 'Please call for details'                       ELXSTRMI
01397                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01398          ELSE                                                     ELXSTRMI
01399              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01400              MOVE 'Please call for details'                       ELXSTRMI
01401                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01402      IF PMCI-PHY-THRPY-NOT-COVERED                                ELXSTRMI
01403          PERFORM 5000-000-INCREASE-INDEX                          ELXSTRMI
01404          IF PMCI-PPO-PROVIDER                                     ELXSTRMI
01405              MOVE 'Physical'                                      ELXSTRMI
01406                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01407              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01408              MOVE 'Not Covered'                                   ELXSTRMI
01409                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01410          ELSE                                                     ELXSTRMI
01411              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01412              MOVE 'Not Covered'                                   ELXSTRMI
01413                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01414      IF PMCI-PHY-THRPY-COVERED                                    ELXSTRMI
01415          PERFORM 5000-000-INCREASE-INDEX                          ELXSTRMI
01416          IF PMCI-PPO-PROVIDER                                     ELXSTRMI
01417              MOVE 'Physical'                                      ELXSTRMI
01418                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01419              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01420              MOVE 'Covered'                                       ELXSTRMI
01421                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01422          ELSE                                                     ELXSTRMI
01423              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01424              MOVE 'Covered'                                       ELXSTRMI
01425                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01426      IF PMCI-PHY-THRPY-CALL                                       ELXSTRMI
01427          PERFORM 5000-000-INCREASE-INDEX                          ELXSTRMI
01428          IF PMCI-PPO-PROVIDER                                     ELXSTRMI
01429              MOVE 'Physical'                                      ELXSTRMI
01430                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01431              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01432              MOVE 'Please call for details'                       ELXSTRMI
01433                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01434          ELSE                                                     ELXSTRMI
01435              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01436              MOVE 'Please call for details'                       ELXSTRMI
01437                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01438      IF SPT-NO                                                    ELXSTRMI
01439          PERFORM 5000-000-INCREASE-INDEX                          ELXSTRMI
01440          IF PMCI-PPO-PROVIDER                                     ELXSTRMI
01441              MOVE 'Speech'                                        ELXSTRMI
01442                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01443              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01444              MOVE 'Not Covered'                                   ELXSTRMI
01445                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01446          ELSE                                                     ELXSTRMI
01447              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01448              MOVE 'Not Covered'                                   ELXSTRMI
01449                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01450      IF SPT-YES                                                   ELXSTRMI
01451          PERFORM 5000-000-INCREASE-INDEX                          ELXSTRMI
01452          IF PMCI-PPO-PROVIDER                                     ELXSTRMI
01453              MOVE 'Speech'                                        ELXSTRMI
01454                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01455              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01456              MOVE 'Covered'                                       ELXSTRMI
01457                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01458          ELSE                                                     ELXSTRMI
01459              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01460              MOVE 'Covered'                                       ELXSTRMI
01461                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01462      IF SPT-CALL                                                  ELXSTRMI
01463          PERFORM 5000-000-INCREASE-INDEX                          ELXSTRMI
01464          IF PMCI-PPO-PROVIDER                                     ELXSTRMI
01465              MOVE 'Speech'                                        ELXSTRMI
01466                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX)   ELXSTRMI
01467              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01468              MOVE 'Please call for details'                       ELXSTRMI
01469                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX)  ELXSTRMI
01470          ELSE                                                     ELXSTRMI
01471              MOVE ' I'     TO BSTRM-DESC-FORMAT(BSTRM-BEN-IDX)    ELXSTRMI
01472              MOVE 'Please call for details'                       ELXSTRMI
01473                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01474      PERFORM 5000-000-INCREASE-INDEX.                             ELXSTRMI
01475      IF PMCI-PPO-PROVIDER                                         ELXSTRMI
01476          MOVE 'Timely Filing Period'                              ELXSTRMI
01477                            TO BSTRM-BENEFIT-DESC(BSTRM-BEN-IDX).  ELXSTRMI
01478      IF TIMELY-FILING-NO-LIMIT OR                                 ELXSTRMI
01479         TIMELY-FILING-MONTHS OR                                   ELXSTRMI
01480         TIMELY-FILING-CALL                                        ELXSTRMI
01481          MOVE 'Please call for details'                           ELXSTRMI
01482                    TO BSTRM-BENEFIT(BSTRM-BEN-IDX BSTRM-COL-IDX). ELXSTRMI
01483                                                                   ELXSTRMI
01484  3000-900-EXIT.                                                   ELXSTRMI
01485      EXIT.                                                        ELXSTRMI
01486 /***************************************************************  ELXSTRMI
01487 *                                                              *  ELXSTRMI
01488 * 4000    R E T U R N   E R R O R   M E S S A G E              *  ELXSTRMI
01489 *                                                              *  ELXSTRMI
01490 ****************************************************************  ELXSTRMI
01491  4000-000-RETURN-ERROR-MSG        SECTION.                        ELXSTRMI
01492  4000-010.                                                        ELXSTRMI
01493                                                                   ELXSTRMI
01494      IF PMCI-BLUE-CHIP-RETURN-CODE = 0                            ELXSTRMI
01495          MOVE 00                 TO BSTRM-ERROR-CODE              ELXSTRMI
01496          MOVE 'Successful'       TO BSTRM-ERROR-DESCRIPTION       ELXSTRMI
01497      ELSE                                                         ELXSTRMI
01498      IF PMCI-BLUE-CHIP-RETURN-CODE = +16                          ELXSTRMI
01499          MOVE 01                 TO BSTRM-ERROR-CODE              ELXSTRMI
01500          MOVE 'No Contract'      TO BSTRM-ERROR-DESCRIPTION       ELXSTRMI
01501      ELSE                                                         ELXSTRMI
01502      IF PMCI-BLUE-CHIP-RETURN-CODE = +32                          ELXSTRMI
01503          MOVE 02                 TO BSTRM-ERROR-CODE              ELXSTRMI
01504          MOVE 'Multiple Contracts'                                ELXSTRMI
01505                                  TO BSTRM-ERROR-DESCRIPTION       ELXSTRMI
01506      ELSE                                                         ELXSTRMI
01507      IF PMCI-BLUE-CHIP-RETURN-CODE = +64                          ELXSTRMI
01508          MOVE 03                 TO BSTRM-ERROR-CODE              ELXSTRMI
01509          MOVE 'Invalid Data'     TO BSTRM-ERROR-DESCRIPTION       ELXSTRMI
01510      ELSE                                                         ELXSTRMI
01511      IF PMCI-BLUE-CHIP-RETURN-CODE = +128                         ELXSTRMI
01512          MOVE 04                 TO BSTRM-ERROR-CODE              ELXSTRMI
01513          MOVE 'Insufficient Data'                                 ELXSTRMI
01514                                  TO BSTRM-ERROR-DESCRIPTION       ELXSTRMI
01515      ELSE                                                         ELXSTRMI
01516      IF PMCI-BLUE-CHIP-RETURN-CODE = +255                         ELXSTRMI
01517          MOVE 05                 TO BSTRM-ERROR-CODE              ELXSTRMI
01518          MOVE 'Internal Error'   TO BSTRM-ERROR-DESCRIPTION.      ELXSTRMI
01519                                                                   ELXSTRMI
01520  4000-900-EXIT.                                                   ELXSTRMI
01521      EXIT.                                                        ELXSTRMI
01522 /***************************************************************  ELXSTRMI
01523 *                                                              *  ELXSTRMI
01524 * 5000   I N C R E A S E   I N D E X                           *  ELXSTRMI
01525 *                                                              *  ELXSTRMI
01526 ****************************************************************  ELXSTRMI
01527  5000-000-INCREASE-INDEX        SECTION.                          ELXSTRMI
01528  5000-010.                                                        ELXSTRMI
01529                                                                   ELXSTRMI
01530      SET BSTRM-BEN-IDX UP BY +1.                                  ELXSTRMI
01531      SET BSTRM-BEN-POINTERS TO BSTRM-BEN-IDX.                     ELXSTRMI
01532                                                                   ELXSTRMI
01533  5000-900-EXIT.                                                   ELXSTRMI
01534      EXIT.                                                        ELXSTRMI
01535 /***************************************************************  ELXSTRMI
01536 *                                                              *  ELXSTRMI
01537 * 6000   L O O P   T H R U   B E N E F I T S                   *  ELXSTRMI
01538 *                                                              *  ELXSTRMI
01539 ****************************************************************  ELXSTRMI
01540  6000-000-LOOP-THRU-BENS        SECTION.                          ELXSTRMI
01541  6000-010.                                                        ELXSTRMI
01542                                                                   ELXSTRMI
01543      PERFORM 6010-000-BENEFIT-EXCHANGE                            ELXSTRMI
01544          VARYING WS-BEN-IDX FROM WS-BEN-IDX BY 1                  ELXSTRMI
01545              UNTIL WS-BEN-IDX > WS-SAVE-BEN-IDX.                  ELXSTRMI
01546                                                                   ELXSTRMI
01547  6000-900-EXIT.                                                   ELXSTRMI
01548      EXIT.                                                        ELXSTRMI
01549 /***************************************************************  ELXSTRMI
01550 *                                                              *  ELXSTRMI
01551 * 6010   B E N E F I T   E X C H A N G E                       *  ELXSTRMI
01552 *                                                              *  ELXSTRMI
01553 ****************************************************************  ELXSTRMI
01554  6010-000-BENEFIT-EXCHANGE      SECTION.                          ELXSTRMI
01555  6010-010.                                                        ELXSTRMI
01556                                                                   ELXSTRMI
01557      COMPUTE WS-BEN-IDX2 = WS-BEN-IDX + 1.                        ELXSTRMI
01558                                                                   ELXSTRMI
01559      IF WS-BEN-IDX > WS-SAVE-BEN-IDX                              ELXSTRMI
01560          GO TO 6010-900-EXIT.                                     ELXSTRMI
01561                                                                   ELXSTRMI
01562      MOVE BSTRM-BENEFITS(WS-BEN-IDX2)                             ELXSTRMI
01563                               TO BSTRM-BENEFITS(WS-BEN-IDX).      ELXSTRMI
01564                                                                   ELXSTRMI
01565  6010-900-EXIT.                                                   ELXSTRMI
01566      EXIT.                                                        ELXSTRMI
01567 /***************************************************************  ELXSTRMI
01568 *                                                              *  ELXSTRMI
01569 * 7000    P R O C E S S   B E N E F I T S                      *  ELXSTRMI
01570 *                                                              *  ELXSTRMI
01571 ****************************************************************  ELXSTRMI
01572  7000-000-PROCESS-BENEFITS        SECTION.                        ELXSTRMI
01573  7000-010.                                                        ELXSTRMI
01574                                                                   ELXSTRMI
01575      MOVE BSTRM-DESC-FORMAT(WS-BEN-IDX)                           ELXSTRMI
01576                               TO WS-DESC-FORMAT.                  ELXSTRMI
01577                                                                   ELXSTRMI
01578      IF WS-DESC-FORMAT NUMERIC                                    ELXSTRMI
01579          NEXT SENTENCE                                            ELXSTRMI
01580      ELSE                                                         ELXSTRMI
01581          GO TO 7000-900-EXIT.                                     ELXSTRMI
01582                                                                   ELXSTRMI
01583      IF WS-DESC-FORMAT = 01                                       ELXSTRMI
01584          IF FOOTNOTE-ONE                                          ELXSTRMI
01585              MOVE WS-FORMAT-COMB                                  ELXSTRMI
01586                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX)    ELXSTRMI
01587          ELSE                                                     ELXSTRMI
01588              ADD +1           TO WS-FOOTNOTE-COUNT                ELXSTRMI
01589              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01590                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01591              SET BSTRM-FOOT-IDX                                   ELXSTRMI
01592                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01593              MOVE BSTRM-DESC-FORMAT(WS-BEN-IDX)                   ELXSTRMI
01594                               TO WS-DESC-FORMAT                   ELXSTRMI
01595              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01596                               TO WS-FOOTNOTE-IDX                  ELXSTRMI
01597              MOVE ELX-FOOTNOTE-TEXT(WS-FOOTNOTE-IDX)              ELXSTRMI
01598                               TO BSTRM-FOOTNOTE(BSTRM-FOOT-IDX)   ELXSTRMI
01599              MOVE '1'         TO WS-FOOTNOTE-ONE-SW               ELXSTRMI
01600              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01601                               TO WS-FORMAT-COMB                   ELXSTRMI
01602              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01603                               TO WS-DESC-FORMAT                   ELXSTRMI
01604              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01605                               TO BSTRM-NOTE-FORMAT(BSTRM-FOOT-IDX)ELXSTRMI
01606              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01607                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX).   ELXSTRMI
01608                                                                   ELXSTRMI
01609      IF WS-DESC-FORMAT = 02                                       ELXSTRMI
01610          IF FOOTNOTE-TWO                                          ELXSTRMI
01611              MOVE WS-FORMAT-OTHER                                 ELXSTRMI
01612                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX)    ELXSTRMI
01613          ELSE                                                     ELXSTRMI
01614              ADD +1           TO WS-FOOTNOTE-COUNT                ELXSTRMI
01615              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01616                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01617              SET BSTRM-FOOT-IDX                                   ELXSTRMI
01618                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01619              MOVE BSTRM-DESC-FORMAT(WS-BEN-IDX)                   ELXSTRMI
01620                               TO WS-DESC-FORMAT                   ELXSTRMI
01621              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01622                               TO WS-FOOTNOTE-IDX                  ELXSTRMI
01623              MOVE ELX-FOOTNOTE-TEXT(WS-FOOTNOTE-IDX)              ELXSTRMI
01624                               TO BSTRM-FOOTNOTE(BSTRM-FOOT-IDX)   ELXSTRMI
01625              MOVE '1'         TO WS-FOOTNOTE-TWO-SW               ELXSTRMI
01626              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01627                               TO WS-FORMAT-OTHER                  ELXSTRMI
01628              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01629                               TO WS-DESC-FORMAT                   ELXSTRMI
01630              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01631                               TO BSTRM-NOTE-FORMAT(BSTRM-FOOT-IDX)ELXSTRMI
01632              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01633                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX).   ELXSTRMI
01634                                                                   ELXSTRMI
01635      IF WS-DESC-FORMAT = 03                                       ELXSTRMI
01636          IF FOOTNOTE-THREE                                        ELXSTRMI
01637              MOVE WS-FORMAT-VERIF                                 ELXSTRMI
01638                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX)    ELXSTRMI
01639          ELSE                                                     ELXSTRMI
01640              ADD +1           TO WS-FOOTNOTE-COUNT                ELXSTRMI
01641              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01642                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01643              SET BSTRM-FOOT-IDX                                   ELXSTRMI
01644                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01645              MOVE BSTRM-DESC-FORMAT(WS-BEN-IDX)                   ELXSTRMI
01646                               TO WS-DESC-FORMAT                   ELXSTRMI
01647              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01648                               TO WS-FOOTNOTE-IDX                  ELXSTRMI
01649              MOVE ELX-FOOTNOTE-TEXT(WS-FOOTNOTE-IDX)              ELXSTRMI
01650                               TO BSTRM-FOOTNOTE(BSTRM-FOOT-IDX)   ELXSTRMI
01651              MOVE '1'         TO WS-FOOTNOTE-THREE-SW             ELXSTRMI
01652              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01653                               TO WS-FORMAT-VERIF                  ELXSTRMI
01654              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01655                               TO WS-DESC-FORMAT                   ELXSTRMI
01656              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01657                               TO BSTRM-NOTE-FORMAT(BSTRM-FOOT-IDX)ELXSTRMI
01658              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01659                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX).   ELXSTRMI
01660                                                                   ELXSTRMI
01661      IF WS-DESC-FORMAT = 04                                       ELXSTRMI
01662          IF FOOTNOTE-FOUR                                         ELXSTRMI
01663              MOVE WS-FORMAT-OPX                                   ELXSTRMI
01664                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX)    ELXSTRMI
01665          ELSE                                                     ELXSTRMI
01666              ADD +1           TO WS-FOOTNOTE-COUNT                ELXSTRMI
01667              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01668                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01669              SET BSTRM-FOOT-IDX                                   ELXSTRMI
01670                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01671              MOVE BSTRM-DESC-FORMAT(WS-BEN-IDX)                   ELXSTRMI
01672                               TO WS-DESC-FORMAT                   ELXSTRMI
01673              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01674                               TO WS-FOOTNOTE-IDX                  ELXSTRMI
01675              MOVE ELX-FOOTNOTE-TEXT(WS-FOOTNOTE-IDX)              ELXSTRMI
01676                               TO BSTRM-FOOTNOTE(BSTRM-FOOT-IDX)   ELXSTRMI
01677              MOVE '1'         TO WS-FOOTNOTE-FOUR-SW              ELXSTRMI
01678              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01679                               TO WS-FORMAT-OPX                    ELXSTRMI
01680              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01681                               TO WS-DESC-FORMAT                   ELXSTRMI
01682              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01683                               TO BSTRM-NOTE-FORMAT(BSTRM-FOOT-IDX)ELXSTRMI
01684              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01685                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX).   ELXSTRMI
01686                                                                   ELXSTRMI
01687      IF WS-DESC-FORMAT = 05                                       ELXSTRMI
01688          IF FOOTNOTE-FIVE                                         ELXSTRMI
01689              MOVE WS-FORMAT-PRECERT                               ELXSTRMI
01690                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX)    ELXSTRMI
01691          ELSE                                                     ELXSTRMI
01692              ADD +1           TO WS-FOOTNOTE-COUNT                ELXSTRMI
01693              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01694                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01695              SET BSTRM-FOOT-IDX                                   ELXSTRMI
01696                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01697              MOVE BSTRM-DESC-FORMAT(WS-BEN-IDX)                   ELXSTRMI
01698                               TO WS-DESC-FORMAT                   ELXSTRMI
01699              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01700                               TO WS-FOOTNOTE-IDX                  ELXSTRMI
01701              MOVE ELX-FOOTNOTE-TEXT(WS-FOOTNOTE-IDX)              ELXSTRMI
01702                               TO BSTRM-FOOTNOTE(BSTRM-FOOT-IDX)   ELXSTRMI
01703              MOVE '1'         TO WS-FOOTNOTE-FIVE-SW              ELXSTRMI
01704              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01705                               TO WS-FORMAT-PRECERT                ELXSTRMI
01706              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01707                               TO WS-DESC-FORMAT                   ELXSTRMI
01708              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01709                               TO BSTRM-NOTE-FORMAT(BSTRM-FOOT-IDX)ELXSTRMI
01710              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01711                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX).   ELXSTRMI
01712                                                                   ELXSTRMI
01713      IF WS-DESC-FORMAT = 06                                       ELXSTRMI
01714          IF FOOTNOTE-SIX                                          ELXSTRMI
01715              MOVE WS-FORMAT-TREAT                                 ELXSTRMI
01716                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX)    ELXSTRMI
01717          ELSE                                                     ELXSTRMI
01718              ADD +1           TO WS-FOOTNOTE-COUNT                ELXSTRMI
01719              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01720                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01721              SET BSTRM-FOOT-IDX                                   ELXSTRMI
01722                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01723              MOVE BSTRM-DESC-FORMAT(WS-BEN-IDX)                   ELXSTRMI
01724                               TO WS-DESC-FORMAT                   ELXSTRMI
01725              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01726                               TO WS-FOOTNOTE-IDX                  ELXSTRMI
01727              MOVE ELX-FOOTNOTE-TEXT(WS-FOOTNOTE-IDX)              ELXSTRMI
01728                               TO BSTRM-FOOTNOTE(BSTRM-FOOT-IDX)   ELXSTRMI
01729              MOVE '1'         TO WS-FOOTNOTE-SIX-SW               ELXSTRMI
01730              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01731                               TO WS-FORMAT-TREAT                  ELXSTRMI
01732              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01733                               TO WS-DESC-FORMAT                   ELXSTRMI
01734              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01735                               TO BSTRM-NOTE-FORMAT(BSTRM-FOOT-IDX)ELXSTRMI
01736              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01737                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX).   ELXSTRMI
01738                                                                   ELXSTRMI
01739      IF WS-DESC-FORMAT = 07                                       ELXSTRMI
01740          IF FOOTNOTE-SEVEN                                        ELXSTRMI
01741              MOVE WS-FORMAT-LM                                    ELXSTRMI
01742                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX)    ELXSTRMI
01743          ELSE                                                     ELXSTRMI
01744              ADD +1           TO WS-FOOTNOTE-COUNT                ELXSTRMI
01745              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01746                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01747              SET BSTRM-FOOT-IDX                                   ELXSTRMI
01748                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01749              MOVE BSTRM-DESC-FORMAT(WS-BEN-IDX)                   ELXSTRMI
01750                               TO WS-DESC-FORMAT                   ELXSTRMI
01751              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01752                               TO WS-FOOTNOTE-IDX                  ELXSTRMI
01753              MOVE ELX-FOOTNOTE-TEXT(WS-FOOTNOTE-IDX)              ELXSTRMI
01754                               TO BSTRM-FOOTNOTE(BSTRM-FOOT-IDX)   ELXSTRMI
01755              MOVE '1'         TO WS-FOOTNOTE-SEVEN-SW             ELXSTRMI
01756              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01757                               TO WS-FORMAT-LM                     ELXSTRMI
01758              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01759                               TO WS-DESC-FORMAT                   ELXSTRMI
01760              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01761                               TO BSTRM-NOTE-FORMAT(BSTRM-FOOT-IDX)ELXSTRMI
01762              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01763                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX).   ELXSTRMI
01764                                                                   ELXSTRMI
01765      IF WS-DESC-FORMAT = 08                                       ELXSTRMI
01766          IF FOOTNOTE-EIGHT                                        ELXSTRMI
01767              MOVE WS-FORMAT-COPAY                                 ELXSTRMI
01768                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX)    ELXSTRMI
01769          ELSE                                                     ELXSTRMI
01770              ADD +1           TO WS-FOOTNOTE-COUNT                ELXSTRMI
01771              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01772                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01773              SET BSTRM-FOOT-IDX                                   ELXSTRMI
01774                               TO BSTRM-FOOTNOTE-COUNT             ELXSTRMI
01775              MOVE BSTRM-DESC-FORMAT(WS-BEN-IDX)                   ELXSTRMI
01776                               TO WS-DESC-FORMAT                   ELXSTRMI
01777              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01778                               TO WS-FOOTNOTE-IDX                  ELXSTRMI
01779              MOVE ELX-FOOTNOTE-TEXT(WS-FOOTNOTE-IDX)              ELXSTRMI
01780                               TO BSTRM-FOOTNOTE(BSTRM-FOOT-IDX)   ELXSTRMI
01781              MOVE '1'         TO WS-FOOTNOTE-EIGHT-SW             ELXSTRMI
01782              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01783                               TO WS-FORMAT-COPAY                  ELXSTRMI
01784              MOVE WS-FOOTNOTE-COUNT                               ELXSTRMI
01785                               TO WS-DESC-FORMAT                   ELXSTRMI
01786              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01787                               TO BSTRM-NOTE-FORMAT(BSTRM-FOOT-IDX)ELXSTRMI
01788              MOVE WS-DESC-FORMAT                                  ELXSTRMI
01789                               TO BSTRM-DESC-FORMAT(WS-BEN-IDX).   ELXSTRMI
01790                                                                   ELXSTRMI
01791  7000-900-EXIT.                                                   ELXSTRMI
01792      EXIT.                                                        ELXSTRMI
01793 /***************************************************************  ELXSTRMI
01794 *                                                              *  ELXSTRMI
01795 * 8000    C A L C U L A T E   H O U R S   T O   D A Y S        *  ELXSTRMI
01796 *                                                              *  ELXSTRMI
01797 ****************************************************************  ELXSTRMI
01798  8000-000-CALCULATE-DAYS          SECTION.                        ELXSTRMI
01799  8000-010.                                                        ELXSTRMI
01800                                                                   ELXSTRMI
01801      IF WS-HOURS > 72                                             ELXSTRMI
01802          COMPUTE WS-DAYS = WS-HOURS / 24                          ELXSTRMI
01803          MOVE WS-DAYS TO WS-HOUR-DAY                              ELXSTRMI
01804          MOVE ' Days ' TO WS-HRDAY-LIT                            ELXSTRMI
01805      ELSE                                                         ELXSTRMI
01806          MOVE WS-HOURS TO WS-DAYS                                 ELXSTRMI
01807          MOVE WS-DAYS TO WS-HOUR-DAY                              ELXSTRMI
01808          MOVE ' Hours' TO WS-HRDAY-LIT.                           ELXSTRMI
01809                                                                   ELXSTRMI
01810  8000-900-EXIT.                                                   ELXSTRMI
01811      EXIT.                                                        ELXSTRMI
01812 /***************************************************************  ELXSTRMI
01813 *                                                              *  ELXSTRMI
01814 * 9000   CALL ELXPMCIF  TO PERFORM I/O ON THE PRODUCTION       *  ELXSTRMI
01815 *         GRP SPEC RECORD.                                     *  ELXSTRMI
01816 *                                                              *  ELXSTRMI
01817 ****************************************************************  ELXSTRMI
01818  9000-000-ELXPMCIF-IO           SECTION.                          ELXSTRMI
01819  9000-010.                                                        ELXSTRMI
01820                                                                   ELXSTRMI
01821      EXEC CICS  LINK  PROGRAM ('ELXPMCIF')                        ELXSTRMI
01822                       COMMAREA(PMCI-COMM-AREA)                    ELXSTRMI
01823                       LENGTH  (LENGTH OF PMCI-COMM-AREA)          ELXSTRMI
01824                       END-EXEC.                                   ELXSTRMI
01825                                                                   ELXSTRMI
01826  9000-900-EXIT.                                                   ELXSTRMI
01827      EXIT.                                                        ELXSTRMI
01828 /***************************************************************  ELXSTRMI
01829 *                                                              *  ELXSTRMI
01830 * 9999    C A L C U L A T E   P A T I E N T S   A G E          *  ELXSTRMI
01831 *                                                              *  ELXSTRMI
01832 ****************************************************************  ELXSTRMI
01833  9999-000-CALCULATE-AGE           SECTION.                        ELXSTRMI
01834  9999-010.                                                        ELXSTRMI
01835                                                                   ELXSTRMI
01836      MOVE 'AGE'   TO MLDATE-FUNC.                                 ELXSTRMI
01837      MOVE 'Y'     TO MLDATE-FORM1.                                ELXSTRMI
01838      MOVE SPACE   TO MLDATE-FORM2.                                ELXSTRMI
01839      MOVE ZEROS   TO MLDATE-RETURN                                ELXSTRMI
01840                      MLDATE-AMOUNT.                               ELXSTRMI
01841      EXEC CICS  LINK  PROGRAM ('MLDATEC')                         ELXSTRMI
01842                       COMMAREA(MLDATE01)                          ELXSTRMI
01843                       LENGTH  (LENGTH OF MLDATE01)                ELXSTRMI
01844                       END-EXEC.                                   ELXSTRMI
01845                                                                   ELXSTRMI
01846  9999-900-EXIT.                                                   ELXSTRMI
01847      EXIT.                                                        ELXSTRMI
