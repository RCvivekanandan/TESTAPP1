00001  ID DIVISION.                                                     09/03/03
00002  PROGRAM-ID.     ELXRMEDI.                                        ELXRMEDI
00003  AUTHOR.         DIANE FLOWERS.                                      LV002
00004  DATE-WRITTEN.   09/20/00.                                        ELXRMEDI
00005  DATE-COMPILED.                                                   ELXRMEDI
00006 ******************************************************************ELXRMEDI
00007 *                                                                *ELXRMEDI
00008 *        M A I N T E N A N C E     L O G                         *ELXRMEDI
00009 *                                                                *ELXRMEDI
00010 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELXRMEDI
00011 *                                                                *ELXRMEDI
00012 * P000345    09/20/00  DAF  CREATED REALMED INTERFACE TO PMCI.   *ELXRMEDI
00013 * P03424     03/14/03  DAF  ADDED PACKAGE CODE FOR TEXAS AND     *ELXRMEDI
00014 *                           GENERATED PROGRAM ABOVE THE LINE.    *ELXRMEDI
00015 ******************************************************************ELXRMEDI
00016 /*****************************************************************ELXRMEDI
00017 *      P R O G R A M   N A R R A T I V E                         *ELXRMEDI
00018 ******************************************************************ELXRMEDI
00019 *                                                                *ELXRMEDI
00020 *   PURPOSE:   HANDLE THE PASSING OF DATA BETWEEN REALMED AND    *ELXRMEDI
00021 *              PMCI.                                             *ELXRMEDI
00022 *                                                                *ELXRMEDI
00023 *   FUNCTIONS: THIS MODULE IS CALLED FROM REALMED TRANSACTION    *ELXRMEDI
00024 *              ELRM.                                             *ELXRMEDI
00025 *                                                                *ELXRMEDI
00026 ******************************************************************ELXRMEDI
00027 /                                                                 ELXRMEDI
00028  ENVIRONMENT DIVISION.                                            ELXRMEDI
00029  DATA DIVISION.                                                   ELXRMEDI
00030                                                                   ELXRMEDI
00031  WORKING-STORAGE SECTION.                                         ELXRMEDI
00032  01  WS-BEGIN                    PIC X(58) VALUE                  ELXRMEDI
00033      '*** ELXRMEDI WORKING-STORAGE BEGINS HERE ***'.              ELXRMEDI
00034                                                                   ELXRMEDI
00035  01  WS-FIELDS.                                                   ELXRMEDI
00036      05  WS-BEN-IDX              PIC S9(02) COMP-3.               ELXRMEDI
00037                                                                   ELXRMEDI
00038  01  WS-AMT-AREA.                                                 ELXRMEDI
00039      05  AMT-IN.                                                  ELXRMEDI
00040          10  AMT-IN-ITEM OCCURS 25 TIMES                          ELXRMEDI
00041              INDEXED BY AMT-IN-INDEX                              ELXRMEDI
00042                                  PIC X.                           ELXRMEDI
00043      05  AMT-OUT.                                                 ELXRMEDI
00044          10  AMT-OUT-ITEM OCCURS 25 TIMES                         ELXRMEDI
00045              INDEXED BY AMT-OUT-INDEX                             ELXRMEDI
00046                                  PIC X.                           ELXRMEDI
00047                                                                   ELXRMEDI
00048  01  WS-SWITCHES.                                                 ELXRMEDI
00049      05  WS-COINSURANCE-SW       PIC X VALUE 'N'.                 ELXRMEDI
00050          88  COINSURANCE-FOUND         VALUE 'Y'.                 ELXRMEDI
00051          88  COINSURANCE-NOT-FOUND     VALUE 'N'.                 ELXRMEDI
00052      05  WS-FAM-DED-SW           PIC X VALUE 'N'.                 ELXRMEDI
00053          88  FAM-DED-FOUND             VALUE 'Y'.                 ELXRMEDI
00054          88  FAM-DED-NOT-FOUND         VALUE 'N'.                 ELXRMEDI
00055      05  WS-IND-DED-SW           PIC X VALUE 'N'.                 ELXRMEDI
00056          88  IND-DED-FOUND             VALUE 'Y'.                 ELXRMEDI
00057          88  IND-DED-NOT-FOUND         VALUE 'N'.                 ELXRMEDI
00058      05  WS-COPAY-SW             PIC X VALUE 'N'.                 ELXRMEDI
00059          88  COPAY-FOUND               VALUE 'Y'.                 ELXRMEDI
00060          88  COPAY-NOT-FOUND           VALUE 'N'.                 ELXRMEDI
00061      05  WS-CHAR-SW              PIC X VALUE 'N'.                 ELXRMEDI
00062          88  CHAR-FOUND                VALUE 'Y'.                 ELXRMEDI
00063          88  CHAR-NOT-FOUND            VALUE 'N'.                 ELXRMEDI
00064                                                                   ELXRMEDI
00065 /-------- MILLENNIUM DATE ROUTINE COMMAREA --------------------*  ELXRMEDI
00066  COPY MLDATE01.                                                   ELXRMEDI
00067                                                                   ELXRMEDI
00068 /                                                                 ELXRMEDI
00069  01  WS-END                       PIC X(58) VALUE                 ELXRMEDI
00070      '*** ELXRMEDI WORKING-STORAGE ENDS HERE ***'.                ELXRMEDI
00071 /                                                                 ELXRMEDI
00072  01  WS-ELXSTRMI-COMMAREA.                                        ELXRMEDI
00073      COPY PMCSTRMC.                                               ELXRMEDI
00074 /                                                                 ELXRMEDI
00075  LINKAGE SECTION.                                                 ELXRMEDI
00076                                                                   ELXRMEDI
00077 *--- COMMAREA PASSED FROM CALLER --------------------------------*ELXRMEDI
00078                                                                   ELXRMEDI
00079  01  DFHCOMMAREA.                                                 ELXRMEDI
00080      05  RMED-GROUP-NBR               PIC X(9).                   ELXRMEDI
00081      05  RMED-SECT-NUM                PIC X(5).                   ELXRMEDI
00082      05  RMED-SUBSCRIBER-NBR          PIC X(9).                   ELXRMEDI
00083      05  RMED-PRODUCT                 PIC X(2).                   ELXRMEDI
00084      05  RMED-DATE-OF-SERVICE         PIC X(8).                   ELXRMEDI
00085      05  RMED-LAST-NAME               PIC X(5).                   ELXRMEDI
00086      05  RMED-FIRST-NAME              PIC X(9).                   ELXRMEDI
00087      05  RMED-SEX                     PIC X(2).                   ELXRMEDI
00088      05  RMED-BIRTH-DATE              PIC X(8).                   ELXRMEDI
00089      05  RMED-RELATIONSHIP            PIC X.                      ELXRMEDI
00090      05  RMED-PKG-CODE                PIC X(3).                   ELXRMEDI
00091      05  RMED-ERROR-CODE              PIC 9(2).                   ELXRMEDI
00092      05  RMED-ERROR-DESCRIPTION       PIC X(60).                  ELXRMEDI
00093      05  RMED-IND-DED-IN              PIC X(25).                  ELXRMEDI
00094      05  RMED-IND-DED-IN-MET          PIC X(25).                  ELXRMEDI
00095      05  RMED-IND-DED-OUT             PIC X(25).                  ELXRMEDI
00096      05  RMED-IND-DED-OUT-MET         PIC X(25).                  ELXRMEDI
00097      05  RMED-FAM-DED-IN              PIC X(25).                  ELXRMEDI
00098      05  RMED-FAM-DED-IN-MET          PIC X(25).                  ELXRMEDI
00099      05  RMED-FAM-DED-OUT             PIC X(25).                  ELXRMEDI
00100      05  RMED-FAM-DED-OUT-MET         PIC X(25).                  ELXRMEDI
00101      05  RMED-COPAY-IN                PIC X(25).                  ELXRMEDI
00102      05  RMED-COPAY-OUT               PIC X(25).                  ELXRMEDI
00103      05  RMED-COINSURANCE-IN          PIC X(25).                  ELXRMEDI
00104      05  RMED-COINSURANCE-OUT         PIC X(25).                  ELXRMEDI
00105                                                                   ELXRMEDI
00106 /                                                                 ELXRMEDI
00107  PROCEDURE DIVISION.                                              ELXRMEDI
00108                                                                   ELXRMEDI
00109 ****************************************************************  ELXRMEDI
00110 *                                                              *  ELXRMEDI
00111 *           P R O C E S S     C O N T R O L                    *  ELXRMEDI
00112 *                                                              *  ELXRMEDI
00113 ****************************************************************  ELXRMEDI
00114  0000-000-PROCESS-CONTROL       SECTION.                          ELXRMEDI
00115  0000-010.                                                        ELXRMEDI
00116                                                                   ELXRMEDI
00117      MOVE '000'                  TO BSTRM-PLAN-CODE.              ELXRMEDI
00118      MOVE RMED-GROUP-NBR         TO BSTRM-GROUP-NBR.              ELXRMEDI
00119      MOVE RMED-SECT-NUM          TO BSTRM-SECT-NUM.               ELXRMEDI
00120      MOVE RMED-PKG-CODE          TO BSTRM-PACKAGE-CODE.           ELXRMEDI
00121      MOVE RMED-SUBSCRIBER-NBR    TO BSTRM-SUBSCRIBER-NBR.         ELXRMEDI
00122      MOVE RMED-PRODUCT           TO BSTRM-ADS-PROG-TYPE.          ELXRMEDI
00123 *    MOVE RMED-LAST-NAME                                          ELXRMEDI
00124 *    MOVE RMED-FIRST-NAME                                         ELXRMEDI
00125 *    MOVE RMED-SEX                                                ELXRMEDI
00126      MOVE RMED-BIRTH-DATE        TO BSTRM-PAT-BIRTH-DATE.         ELXRMEDI
00127      MOVE RMED-RELATIONSHIP      TO BSTRM-PAT-RELATIONSHIP.       ELXRMEDI
00128      MOVE '0'                    TO BSTRM-MEDICARE-ELIGIBILITY.   ELXRMEDI
00129      MOVE RMED-DATE-OF-SERVICE   TO BSTRM-DATE-OF-SERVICE.        ELXRMEDI
00130      MOVE 'P'                    TO BSTRM-PROVIDER-INDICATOR.     ELXRMEDI
00131      MOVE 'O'                    TO BSTRM-IP-OR-OP-INQUIRY.       ELXRMEDI
00132      MOVE +1                     TO BSTRM-BEN-POINTERS.           ELXRMEDI
00133                                                                   ELXRMEDI
00134      EXEC CICS  LINK  PROGRAM ('ELXSTRMI')                        ELXRMEDI
00135                       COMMAREA(WS-ELXSTRMI-COMMAREA)              ELXRMEDI
00136                       LENGTH  (LENGTH OF WS-ELXSTRMI-COMMAREA)    ELXRMEDI
00137                       END-EXEC.                                   ELXRMEDI
00138                                                                   ELXRMEDI
00139      SET BSTRM-BEN-IDX           TO BSTRM-BEN-POINTERS.           ELXRMEDI
00140      PERFORM 1000-000-PROCESS-BENEFITS                            ELXRMEDI
00141          VARYING WS-BEN-IDX FROM 1 BY 1                           ELXRMEDI
00142              UNTIL WS-BEN-IDX > BSTRM-BEN-IDX.                    ELXRMEDI
00143                                                                   ELXRMEDI
00144      IF COINSURANCE-FOUND                                         ELXRMEDI
00145          NEXT SENTENCE                                            ELXRMEDI
00146      ELSE                                                         ELXRMEDI
00147          MOVE 'Not Applicable'   TO RMED-COINSURANCE-IN           ELXRMEDI
00148          MOVE 'Not Applicable'   TO RMED-COINSURANCE-OUT.         ELXRMEDI
00149                                                                   ELXRMEDI
00150      IF FAM-DED-FOUND                                             ELXRMEDI
00151          NEXT SENTENCE                                            ELXRMEDI
00152      ELSE                                                         ELXRMEDI
00153          MOVE 'Not Applicable'   TO RMED-FAM-DED-IN               ELXRMEDI
00154          MOVE 'Not Applicable'   TO RMED-FAM-DED-OUT              ELXRMEDI
00155          MOVE 'Not Applicable'   TO RMED-FAM-DED-IN-MET           ELXRMEDI
00156          MOVE 'Not Applicable'   TO RMED-FAM-DED-OUT-MET.         ELXRMEDI
00157                                                                   ELXRMEDI
00158      IF IND-DED-FOUND                                             ELXRMEDI
00159          NEXT SENTENCE                                            ELXRMEDI
00160      ELSE                                                         ELXRMEDI
00161          MOVE 'Not Applicable'   TO RMED-IND-DED-IN               ELXRMEDI
00162          MOVE 'Not Applicable'   TO RMED-IND-DED-OUT              ELXRMEDI
00163          MOVE 'Not Applicable'   TO RMED-IND-DED-IN-MET           ELXRMEDI
00164          MOVE 'Not Applicable'   TO RMED-IND-DED-OUT-MET.         ELXRMEDI
00165                                                                   ELXRMEDI
00166      IF COPAY-FOUND                                               ELXRMEDI
00167          NEXT SENTENCE                                            ELXRMEDI
00168      ELSE                                                         ELXRMEDI
00169          MOVE 'None'             TO RMED-COPAY-IN                 ELXRMEDI
00170          MOVE 'None'             TO RMED-COPAY-OUT.               ELXRMEDI
00171                                                                   ELXRMEDI
00172      MOVE SPACES TO AMT-OUT.                                      ELXRMEDI
00173      MOVE RMED-IND-DED-IN TO AMT-IN.                              ELXRMEDI
00174      PERFORM 2000-LEFT-JUSTIFY-AMT.                               ELXRMEDI
00175      MOVE AMT-OUT TO RMED-IND-DED-IN.                             ELXRMEDI
00176      MOVE SPACES TO AMT-OUT.                                      ELXRMEDI
00177      MOVE RMED-IND-DED-IN-MET TO AMT-IN.                          ELXRMEDI
00178      PERFORM 2000-LEFT-JUSTIFY-AMT.                               ELXRMEDI
00179      MOVE AMT-OUT TO RMED-IND-DED-IN-MET.                         ELXRMEDI
00180      MOVE SPACES TO AMT-OUT.                                      ELXRMEDI
00181      MOVE RMED-IND-DED-OUT TO AMT-IN.                             ELXRMEDI
00182      PERFORM 2000-LEFT-JUSTIFY-AMT.                               ELXRMEDI
00183      MOVE AMT-OUT TO RMED-IND-DED-OUT.                            ELXRMEDI
00184      MOVE SPACES TO AMT-OUT.                                      ELXRMEDI
00185      MOVE RMED-IND-DED-OUT-MET TO AMT-IN.                         ELXRMEDI
00186      PERFORM 2000-LEFT-JUSTIFY-AMT.                               ELXRMEDI
00187      MOVE AMT-OUT TO RMED-IND-DED-OUT-MET.                        ELXRMEDI
00188      MOVE SPACES TO AMT-OUT.                                      ELXRMEDI
00189      MOVE RMED-FAM-DED-IN TO AMT-IN.                              ELXRMEDI
00190      PERFORM 2000-LEFT-JUSTIFY-AMT.                               ELXRMEDI
00191      MOVE AMT-OUT TO RMED-FAM-DED-IN.                             ELXRMEDI
00192      MOVE SPACES TO AMT-OUT.                                      ELXRMEDI
00193      MOVE RMED-FAM-DED-IN-MET TO AMT-IN.                          ELXRMEDI
00194      PERFORM 2000-LEFT-JUSTIFY-AMT.                               ELXRMEDI
00195      MOVE AMT-OUT TO RMED-FAM-DED-IN-MET.                         ELXRMEDI
00196      MOVE SPACES TO AMT-OUT.                                      ELXRMEDI
00197      MOVE RMED-FAM-DED-OUT TO AMT-IN.                             ELXRMEDI
00198      PERFORM 2000-LEFT-JUSTIFY-AMT.                               ELXRMEDI
00199      MOVE AMT-OUT TO RMED-FAM-DED-OUT.                            ELXRMEDI
00200      MOVE SPACES TO AMT-OUT.                                      ELXRMEDI
00201      MOVE RMED-FAM-DED-OUT-MET TO AMT-IN.                         ELXRMEDI
00202      PERFORM 2000-LEFT-JUSTIFY-AMT.                               ELXRMEDI
00203      MOVE AMT-OUT TO RMED-FAM-DED-OUT-MET.                        ELXRMEDI
00204      MOVE SPACES TO AMT-OUT.                                      ELXRMEDI
00205      MOVE RMED-COPAY-IN TO AMT-IN.                                ELXRMEDI
00206      PERFORM 2000-LEFT-JUSTIFY-AMT.                               ELXRMEDI
00207      MOVE AMT-OUT TO RMED-COPAY-IN.                               ELXRMEDI
00208      MOVE SPACES TO AMT-OUT.                                      ELXRMEDI
00209      MOVE RMED-COPAY-OUT TO AMT-IN.                               ELXRMEDI
00210      PERFORM 2000-LEFT-JUSTIFY-AMT.                               ELXRMEDI
00211      MOVE AMT-OUT TO RMED-COPAY-OUT.                              ELXRMEDI
00212      MOVE SPACES TO AMT-OUT.                                      ELXRMEDI
00213      MOVE RMED-COINSURANCE-IN TO AMT-IN.                          ELXRMEDI
00214      PERFORM 2000-LEFT-JUSTIFY-AMT.                               ELXRMEDI
00215      MOVE AMT-OUT TO RMED-COINSURANCE-IN.                         ELXRMEDI
00216      MOVE SPACES TO AMT-OUT.                                      ELXRMEDI
00217      MOVE RMED-COINSURANCE-OUT TO AMT-IN.                         ELXRMEDI
00218      PERFORM 2000-LEFT-JUSTIFY-AMT.                               ELXRMEDI
00219      MOVE AMT-OUT TO RMED-COINSURANCE-OUT.                        ELXRMEDI
00220                                                                   ELXRMEDI
00221      MOVE BSTRM-ERROR-CODE       TO RMED-ERROR-CODE.              ELXRMEDI
00222      MOVE BSTRM-ERROR-DESCRIPTION                                 ELXRMEDI
00223                                  TO RMED-ERROR-DESCRIPTION.       ELXRMEDI
00224                                                                   ELXRMEDI
00225  0000-800-RETURN.                                                 ELXRMEDI
00226                                                                   ELXRMEDI
00227      EXEC CICS RETURN END-EXEC.                                   ELXRMEDI
00228                                                                   ELXRMEDI
00229      GOBACK.                                                      ELXRMEDI
00230                                                                   ELXRMEDI
00231  0000-900-EXIT.                                                   ELXRMEDI
00232      EXIT.                                                        ELXRMEDI
00233                                                                   ELXRMEDI
00234  1000-000-PROCESS-BENEFITS      SECTION.                          ELXRMEDI
00235  1000-010.                                                        ELXRMEDI
00236                                                                   ELXRMEDI
00237      IF BSTRM-BENEFIT-DESC(WS-BEN-IDX) = 'Coinsurance'            ELXRMEDI
00238          MOVE BSTRM-BENEFIT(WS-BEN-IDX 1)                         ELXRMEDI
00239                                  TO RMED-COINSURANCE-IN           ELXRMEDI
00240          MOVE BSTRM-BENEFIT(WS-BEN-IDX 2)                         ELXRMEDI
00241                                  TO RMED-COINSURANCE-OUT          ELXRMEDI
00242          MOVE 'Y'                TO WS-COINSURANCE-SW.            ELXRMEDI
00243                                                                   ELXRMEDI
00244      IF BSTRM-BENEFIT-DESC(WS-BEN-IDX) = 'Family Deductible'      ELXRMEDI
00245          MOVE BSTRM-BENEFIT(WS-BEN-IDX 1)                         ELXRMEDI
00246                                  TO RMED-FAM-DED-IN               ELXRMEDI
00247          MOVE BSTRM-BENEFIT(WS-BEN-IDX 2)                         ELXRMEDI
00248                                  TO RMED-FAM-DED-OUT              ELXRMEDI
00249          MOVE 'Not Applicable'   TO RMED-FAM-DED-IN-MET           ELXRMEDI
00250          MOVE 'Not Applicable'   TO RMED-FAM-DED-OUT-MET          ELXRMEDI
00251          MOVE 'Y'                TO WS-FAM-DED-SW.                ELXRMEDI
00252                                                                   ELXRMEDI
00253      IF BSTRM-BENEFIT-DESC(WS-BEN-IDX) = 'Individual Deductible'  ELXRMEDI
00254          MOVE BSTRM-BENEFIT(WS-BEN-IDX 1)                         ELXRMEDI
00255                                  TO RMED-IND-DED-IN               ELXRMEDI
00256          MOVE BSTRM-BENEFIT(WS-BEN-IDX 2)                         ELXRMEDI
00257                                  TO RMED-IND-DED-OUT              ELXRMEDI
00258          MOVE 'Not Applicable'   TO RMED-IND-DED-IN-MET           ELXRMEDI
00259          MOVE 'Not Applicable'   TO RMED-IND-DED-OUT-MET          ELXRMEDI
00260          MOVE 'Y'                TO WS-IND-DED-SW.                ELXRMEDI
00261                                                                   ELXRMEDI
00262      IF BSTRM-BENEFIT-DESC(WS-BEN-IDX) = 'Office Visit Copay'     ELXRMEDI
00263          MOVE BSTRM-BENEFIT(WS-BEN-IDX 1)                         ELXRMEDI
00264                                  TO RMED-COPAY-IN                 ELXRMEDI
00265          MOVE BSTRM-BENEFIT(WS-BEN-IDX 2)                         ELXRMEDI
00266                                  TO RMED-COPAY-OUT                ELXRMEDI
00267          MOVE 'Y'                TO WS-COPAY-SW.                  ELXRMEDI
00268                                                                   ELXRMEDI
00269  1000-900-EXIT.                                                   ELXRMEDI
00270      EXIT.                                                        ELXRMEDI
00271 ************************************************************      ELXRMEDI
00272 *                                                          *      ELXRMEDI
00273 *        STRIP AMT                                         *      ELXRMEDI
00274 *                                                          *      ELXRMEDI
00275 ************************************************************      ELXRMEDI
00276  2000-LEFT-JUSTIFY-AMT  SECTION.                                  ELXRMEDI
00277  2000-010.                                                        ELXRMEDI
00278      SET AMT-OUT-INDEX TO 1.                                      ELXRMEDI
00279      MOVE 'N' TO WS-CHAR-SW.                                      ELXRMEDI
00280      PERFORM 3000-STRIP-AMT-INPUT                                 ELXRMEDI
00281          VARYING AMT-IN-INDEX FROM 1 BY 1                         ELXRMEDI
00282          UNTIL AMT-IN-INDEX > 25.                                 ELXRMEDI
00283                                                                   ELXRMEDI
00284  2000-900-EXIT.                                                   ELXRMEDI
00285      EXIT.                                                        ELXRMEDI
00286                                                                   ELXRMEDI
00287 ************************************************************      ELXRMEDI
00288 *                                                          *      ELXRMEDI
00289 *        STRIP AMT INPUT                                   *      ELXRMEDI
00290 *                                                          *      ELXRMEDI
00291 ************************************************************      ELXRMEDI
00292  3000-STRIP-AMT-INPUT  SECTION.                                   ELXRMEDI
00293  3000-010.                                                        ELXRMEDI
00294      IF AMT-IN-ITEM (AMT-IN-INDEX) = SPACE                        ELXRMEDI
00295          IF CHAR-NOT-FOUND                                        ELXRMEDI
00296              GO TO 3000-900-EXIT.                                 ELXRMEDI
00297      MOVE 'Y' TO WS-CHAR-SW.                                      ELXRMEDI
00298      PERFORM 4000-MOVE-AMT-CHAR.                                  ELXRMEDI
00299                                                                   ELXRMEDI
00300  3000-900-EXIT.                                                   ELXRMEDI
00301      EXIT.                                                        ELXRMEDI
00302                                                                   ELXRMEDI
00303 ************************************************************      ELXRMEDI
00304 *                                                          *      ELXRMEDI
00305 *        MOVE AMT CHAR                                     *      ELXRMEDI
00306 *                                                          *      ELXRMEDI
00307 ************************************************************      ELXRMEDI
00308  4000-MOVE-AMT-CHAR  SECTION.                                     ELXRMEDI
00309  4000-010.                                                        ELXRMEDI
00310                                                                   ELXRMEDI
00311      MOVE AMT-IN-ITEM (AMT-IN-INDEX)                              ELXRMEDI
00312                             TO AMT-OUT-ITEM (AMT-OUT-INDEX).      ELXRMEDI
00313      SET AMT-OUT-INDEX UP BY 1.                                   ELXRMEDI
00314                                                                   ELXRMEDI
00315  4000-900-EXIT.                                                   ELXRMEDI
00316      EXIT.                                                        ELXRMEDI
