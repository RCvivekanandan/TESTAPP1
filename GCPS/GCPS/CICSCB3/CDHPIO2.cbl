00001  ID DIVISION.                                                     03/09/05
00002  PROGRAM-ID.     CDHPIO2.                                         CDHPIO2 
00003  AUTHOR.         GARY D MULLINGS.                                    LV002
00004  DATE-WRITTEN.   NOVEMBER 2004.                                   CDHPIO2 
00005  DATE-COMPILED.                                                   CDHPIO2 
00006                                                                   CDHPIO2 
00007 ***************************************************************** CDHPIO2 
00008 ***************************************************************** CDHPIO2 
00009 *               PROGRAM HISTORY/CHANGES                         * CDHPIO2 
00010 *---------------------------------------------------------------* CDHPIO2 
00011 * LOG#         DATE     AUTHOR        DESCRIPTION               * CDHPIO2 
00012 *---------------------------------------------------------------* CDHPIO2 
00013 *            11/11/04    GDM          CREATE NEW MODULE.        * CDHPIO2 
00014 *            12/10/04    GDM          MODIFY CONAGRA EFF DATE.  * CDHPIO2 
00015 *            12/16/04    GTF          MODIFY CONAGRA VENDOR ID. * CDHPIO2 
00016 *            12/16/04    JP           MODIFY BEN-PRD DATES &    * CDHPIO2 
00017 *                                     ACCUMID FOR UNITED.       * CDHPIO2 
00018 *            12/30/04    KIKI         ADDED A GROUP TO ACCO,    * CDHPIO2 
00019 *                                     INITIALIZED IN 2000-      * CDHPIO2 
00020 *                                     FIELD GCDHP-DATA-TO-BCHP, * CDHPIO2 
00021 *                                     FIXED INCREMENTING THE    * CDHPIO2 
00022 *                                     GCDHP-INDEX, REENTERED    * CDHPIO2 
00023 *                                     THE IF STATEMENTS FOR     * CDHPIO2 
00024 *                                     GCDHP-TYPE-OF-ACCUM,      * CDHPIO2 
00025 *                                     CHANDED TO 2005-01-01     * CDHPIO2 
00026 *                                     GCDHP-BEN-PRD-BEGIN-DATE  * CDHPIO2 
00027 *                                     FOR CONAGRA.              * CDHPIO2 
00028 *            01/12/05    GDM          MODIFY UNITED EFF DATE.   * CDHPIO2 
00029 *            02/22/05    GDM          ADD STS LTD               * CDHPIO2 
00030 *            03/03/05    GDM          ADD 3 OCCURENCE           * CDHPIO2 
00031 *                                     - 1 ACCO                  * CDHPIO2 
00032 *                                     - 2 STS                   * CDHPIO2 
00033 *                                                               * CDHPIO2 
00034 ***************************************************************** CDHPIO2 
00035  ENVIRONMENT DIVISION.                                            CDHPIO2 
00036  DATA DIVISION.                                                   CDHPIO2 
00037  WORKING-STORAGE SECTION.                                         CDHPIO2 
00038  01  WS-BEGIN                    PIC X(58) VALUE                  CDHPIO2 
00039      '*** CDHPIO2  WORKING-STORAGE BEGINS HERE ***'.              CDHPIO2 
00040 /                                                                 CDHPIO2 
00041  01  WS-01-ABEND-AREA.                                            CDHPIO2 
00042      05  FILLER                   PIC X(16)  VALUE                CDHPIO2 
00043          '** ABEND AREA **'.                                      CDHPIO2 
00044 *                                                                 CDHPIO2 
00045      05  WS-01-ABEND-CODES-AND-MSG.                               CDHPIO2 
00046          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. CDHPIO2 
00047          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. CDHPIO2 
00048                                                                   CDHPIO2 
00049          10  WS-01-ABCODE-2BF1          PIC X(04)  VALUE  '2BF1'. CDHPIO2 
00050          10  WS-01-ABCODE-2BF1-MSG      PIC X(44)  VALUE          CDHPIO2 
00051             'CONTRACT RECORD CANNOT BE FOUND          '.          CDHPIO2 
00052                                                                   CDHPIO2 
00053          10  WS-01-ABCODE-2BL1          PIC X(04)  VALUE  '2BL1'. CDHPIO2 
00054          10  WS-01-ABCODE-2BL1-MSG      PIC X(44)  VALUE          CDHPIO2 
00055             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          CDHPIO2 
00056                                                                   CDHPIO2 
00057          10  WS-01-ABCODE-2BP1          PIC X(04)  VALUE  '2BP1'. CDHPIO2 
00058          10  WS-01-ABCODE-2BP1-MSG      PIC X(44)  VALUE          CDHPIO2 
00059             'ENTRY GAINED FROM UNKNOWN PROGRAM        '.          CDHPIO2 
00060                                                                   CDHPIO2 
00061          10  WS-01-ABCODE-2BP2          PIC X(04)  VALUE  '2BP2'. CDHPIO2 
00062          10  WS-01-ABCODE-2BP2-MSG      PIC X(44)  VALUE          CDHPIO2 
00063             'INVALID COMMAREA RECEIVED FROM CALLER    '.          CDHPIO2 
00064                                                                   CDHPIO2 
00065  01  WS-02-AREA.                                                  CDHPIO2 
00066      05  FILLER                  PIC X(16)  VALUE                 CDHPIO2 
00067          '** WS-02-AREA **'.                                      CDHPIO2 
00068                                                                   CDHPIO2 
00069      05  WS-GROUP-NUMBER         PIC X(09).                       CDHPIO2 
00070          88 UNITED               VALUE '000016345' '000016346'    CDHPIO2 
00071                                        '000016347' '000016348'    CDHPIO2 
00072                                        '000016349' '000016350'    CDHPIO2 
00073                                        '000016351' '000016352'    CDHPIO2 
00074                                        '000016353' '000016354'    CDHPIO2 
00075                                        '000016355' '000016358'    CDHPIO2 
00076                                        '000016359' '000016360'    CDHPIO2 
00077                                        '000016361' '000016362'    CDHPIO2 
00078                                        '000016363' '000016365'    CDHPIO2 
00079                                        '000016366' '000016367'    CDHPIO2 
00080                                        '000016368' '000016369'    CDHPIO2 
00081                                        '000016370' '000016373'.   CDHPIO2 
00082                                                                   CDHPIO2 
00083          88 CONAGRA              VALUE '000015980' '000015981'    CDHPIO2 
00084                                        '000015982' '000015983'    CDHPIO2 
00085                                        '000015984' '000015998'.   CDHPIO2 
00086                                                                   CDHPIO2 
00087          88 ACCO                 VALUE '000264033' '000264034'.   CDHPIO2 
00088                                                                   CDHPIO2 
00089          88 EBY-BROWN            VALUE '000P78599'.               CDHPIO2 
00090                                                                   CDHPIO2 
00091          88 NAPERVILLE           VALUE '000P06677'.               CDHPIO2 
00092                                                                   CDHPIO2 
00093          88 TTI                  VALUE '000079455'.               CDHPIO2 
00094                                                                   CDHPIO2 
00095          88 STS                  VALUE '000P35308'.               CDHPIO2 
00096                                                                   CDHPIO2 
00097  01  WS-SUB                       PIC 99  COMP.                   CDHPIO2 
00098                                                                   CDHPIO2 
00099                                                                   CDHPIO2 
00100  01  WS-END                       PIC X(58) VALUE                 CDHPIO2 
00101      '*** CDHPIO2  WORKING-STORAGE ENDS HERE ***'.                CDHPIO2 
00102 /                                                                 CDHPIO2 
00103  LINKAGE SECTION.                                                 CDHPIO2 
00104 /                                                                 CDHPIO2 
00105  01  DFHCOMMAREA.                                                 CDHPIO2 
00106      COPY  CDHPACCM.                                              CDHPIO2 
00107 /                                                                 CDHPIO2 
00108  PROCEDURE DIVISION.                                              CDHPIO2 
00109                                                                   CDHPIO2 
00110 ****************************************************************  CDHPIO2 
00111 *                                                              *  CDHPIO2 
00112 *           P R O C E S S     C O N T R O L                    *  CDHPIO2 
00113 *                                                              *  CDHPIO2 
00114 ****************************************************************  CDHPIO2 
00115  0000-000-PROCESS-CONTROL.                                        CDHPIO2 
00116                                                                   CDHPIO2 
00117      PERFORM  2000-PROCESS  THRU 2000-EXIT.                       CDHPIO2 
00118                                                                   CDHPIO2 
00119      PERFORM  9999-RETURN   THRU 9999-EXIT.                       CDHPIO2 
00120                                                                   CDHPIO2 
00121  0000-900-EXIT.                                                   CDHPIO2 
00122      EXIT.                                                        CDHPIO2 
00123 /***************************************************************  CDHPIO2 
00124 *                                                              *  CDHPIO2 
00125 * 2000  PROCESS                                                *  CDHPIO2 
00126 *                                                              *  CDHPIO2 
00127 ****************************************************************  CDHPIO2 
00128  2000-PROCESS.                                                    CDHPIO2 
00129                                                                   CDHPIO2 
00130      INITIALIZE  GCDHP-STATUS-CODE                                CDHPIO2 
00131                  GCDHP-ENTRY-COUNT.                               CDHPIO2 
00132                                                                   CDHPIO2 
00133      PERFORM  9800-INITLZ-ACCUMS-CHOSEN  THRU  9800-EXIT          CDHPIO2 
00134        VARYING  WS-SUB  FROM  +1  BY  +1                          CDHPIO2 
00135          UNTIL  WS-SUB     >  15.                                 CDHPIO2 
00136                                                                   CDHPIO2 
00137      MOVE GCDHP-GROUP  TO  WS-GROUP-NUMBER.                       CDHPIO2 
00138                                                                   CDHPIO2 
00139      SET  GCDHP-INDEX   TO +1.                                    CDHPIO2 
00140      SET  GCDHP-INDEX   DOWN BY +1.                               CDHPIO2 
00141                                                                   CDHPIO2 
00142      IF UNITED                                                    CDHPIO2 
00143         PERFORM 3000-000-FORMAT-UNITED      THRU  3000-EXIT       CDHPIO2 
00144      ELSE                                                         CDHPIO2 
00145      IF CONAGRA                                                   CDHPIO2 
00146         PERFORM 4000-000-FORMAT-CONAGRA     THRU  4000-EXIT       CDHPIO2 
00147      ELSE                                                         CDHPIO2 
00148      IF ACCO                                                      CDHPIO2 
00149         PERFORM 5000-000-FORMAT-ACCO        THRU  5000-EXIT       CDHPIO2 
00150      ELSE                                                         CDHPIO2 
00151      IF EBY-BROWN                                                 CDHPIO2 
00152         PERFORM 6000-000-FORMAT-EBY-BROWN   THRU  6000-EXIT       CDHPIO2 
00153      ELSE                                                         CDHPIO2 
00154      IF NAPERVILLE                                                CDHPIO2 
00155         PERFORM 7000-000-FORMAT-NAPERVILLE  THRU  7000-EXIT       CDHPIO2 
00156      ELSE                                                         CDHPIO2 
00157      IF TTI                                                       CDHPIO2 
00158         PERFORM 8000-000-FORMAT-TTI         THRU  8000-EXIT       CDHPIO2 
00159      ELSE                                                         CDHPIO2 
00160      IF STS                                                       CDHPIO2 
00161         PERFORM 9000-000-FORMAT-STS         THRU  9000-EXIT       CDHPIO2 
00162      ELSE                                                         CDHPIO2 
00163      MOVE '01'          TO GCDHP-STATUS-CODE.                     CDHPIO2 
00164                                                                   CDHPIO2 
00165  2000-EXIT.                                                       CDHPIO2 
00166      EXIT.                                                        CDHPIO2 
00167 /***************************************************************  CDHPIO2 
00168 *                                                              *  CDHPIO2 
00169 * 3000  UNITED                                                 *  CDHPIO2 
00170 *                                                              *  CDHPIO2 
00171 ****************************************************************  CDHPIO2 
00172  3000-000-FORMAT-UNITED.                                          CDHPIO2 
00173                                                                   CDHPIO2 
00174 ***  MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)     CDHPIO2 
00175      MOVE '00'          TO GCDHP-STATUS-CODE.                     CDHPIO2 
00176                                                                   CDHPIO2 
00177 ***  SET  GCDHP-INDEX   TO +1.                                    CDHPIO2 
00178 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
00179 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
00180                                                                   CDHPIO2 
00181      IF GCDHP-ACCUM-DED                                           CDHPIO2 
00182         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
00183         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
00184                                                                   CDHPIO2 
00185         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
00186         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
00187         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
00188         MOVE  20050101                                            CDHPIO2 
00189                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
00190         MOVE  20051231                                            CDHPIO2 
00191                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
00192         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
00193         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
00194         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
00195         MOVE '00'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
00196         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
00197         MOVE 'DEDUCTIBL'                                          CDHPIO2 
00198                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
00199         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
00200         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
00201         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
00202         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
00203         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
00204         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
00205         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
00206         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
00207         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
00208         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
00209         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
00210         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
00211         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
00212         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
00213         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
00214         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
00215         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
00216         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
00217         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
00218         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
00219         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
00220         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
00221         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
00222         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
00223         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
00224         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
00225         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
00226         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
00227         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
00228         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
00229         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
00230         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
00231         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
00232         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
00233         MOVE 'IADD'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
00234         MOVE +000     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
00235         MOVE +000     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
00236         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
00237         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
00238         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
00239         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
00240         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
00241         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
00242         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
00243         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
00244         MOVE +30000   TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
00245         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
00246         MOVE '0'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
00247         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
00248                                                                   CDHPIO2 
00249      IF GCDHP-ACCUM-DED                                           CDHPIO2 
00250         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
00251         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
00252                                                                   CDHPIO2 
00253         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
00254         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
00255         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
00256         MOVE  20050101                                            CDHPIO2 
00257                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
00258         MOVE  20051231                                            CDHPIO2 
00259                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
00260         MOVE 'F'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
00261         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
00262         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
00263         MOVE '00'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
00264         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
00265         MOVE 'DEDUCTIBL'                                          CDHPIO2 
00266                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
00267         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
00268         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
00269         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
00270         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
00271         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
00272         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
00273         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
00274         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
00275         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
00276         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
00277         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
00278         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
00279         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
00280         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
00281         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
00282         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
00283         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
00284         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
00285         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
00286         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
00287         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
00288         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
00289         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
00290         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
00291         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
00292         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
00293         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
00294         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
00295         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
00296         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
00297         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
00298         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
00299         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
00300         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
00301         MOVE 'FADD'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
00302         MOVE +000     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
00303         MOVE +000     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
00304         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
00305         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
00306         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
00307         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
00308         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
00309         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
00310         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
00311         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
00312         MOVE +300000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
00313         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
00314         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
00315         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
00316                                                                   CDHPIO2 
00317      SET  GCDHP-ENTRY-COUNT                                       CDHPIO2 
00318                         TO GCDHP-INDEX.                           CDHPIO2 
00319                                                                   CDHPIO2 
00320  3000-EXIT.                                                       CDHPIO2 
00321      EXIT.                                                        CDHPIO2 
00322 /*                                                                CDHPIO2 
00323 /***************************************************************  CDHPIO2 
00324 *                                                              *  CDHPIO2 
00325 * 4000  CONAGRA                                                *  CDHPIO2 
00326 *                                                              *  CDHPIO2 
00327 ****************************************************************  CDHPIO2 
00328  4000-000-FORMAT-CONAGRA.                                         CDHPIO2 
00329                                                                   CDHPIO2 
00330 ***  MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN(GCDHP-INDEX).      CDHPIO2 
00331      MOVE '00'          TO GCDHP-STATUS-CODE.                     CDHPIO2 
00332                                                                   CDHPIO2 
00333 ***  SET  GCDHP-INDEX   TO +1.                                    CDHPIO2 
00334 *-   #ABM - BENEFIT AGGREGATE MAXIMUMS                            CDHPIO2 
00335 *-   IF GCDHP-ACCUM-DED      HERE S/B   GCDHP-ACCUM-MAX           CDHPIO2 
00336                                                                   CDHPIO2 
00337      IF GCDHP-ACCUM-MAX                                           CDHPIO2 
00338         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
00339         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN(GCDHP-INDEX)    CDHPIO2 
00340                                                                   CDHPIO2 
00341         MOVE '02'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
00342         MOVE LOW-VALUE                                            CDHPIO2 
00343                       TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
00344         MOVE '0B'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
00345 ***     MOVE  20040101                                            CDHPIO2 
00346 ***                   TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
00347 ***     MOVE  20051231                                            CDHPIO2 
00348 ***                   TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
00349         MOVE  ZEROES  TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
00350                          GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
00351                                                                   CDHPIO2 
00352         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
00353         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
00354         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
00355         MOVE '0P'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
00356         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
00357 ***     MOVE '000000000'                                          CDHPIO2 
00358         MOVE SPACES   TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
00359         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
00360         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
00361         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
00362         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
00363         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
00364         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
00365         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
00366         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
00367         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
00368         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
00369         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
00370         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
00371         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
00372         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
00373         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
00374         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
00375         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
00376         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
00377         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
00378         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
00379         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
00380         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
00381         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
00382         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
00383         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
00384         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
00385         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
00386         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
00387         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
00388         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
00389         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
00390         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
00391         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
00392         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
00393         MOVE 'OALT'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
00394         MOVE +0       TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
00395         MOVE +0       TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
00396         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
00397         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
00398         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
00399         MOVE LOW-VALUE                                            CDHPIO2 
00400                       TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
00401         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
00402         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
00403         MOVE '3'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
00404         MOVE +200000000                                           CDHPIO2 
00405                       TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
00406         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
00407         MOVE '0'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
00408         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
00409                                                                   CDHPIO2 
00410 ***  SET  GCDHP-INDEX   TO +2.                                    CDHPIO2 
00411 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
00412 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
00413                                                                   CDHPIO2 
00414      IF GCDHP-ACCUM-DED                                           CDHPIO2 
00415         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
00416         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
00417                                                                   CDHPIO2 
00418         MOVE '02'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
00419         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
00420         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
00421         MOVE  20050101                                            CDHPIO2 
00422                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
00423         MOVE  20051231                                            CDHPIO2 
00424                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
00425         MOVE 'F'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
00426         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
00427         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
00428         MOVE '13'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
00429         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
00430         MOVE 'EEPLUSSP '                                          CDHPIO2 
00431                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
00432         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
00433         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
00434         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
00435         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
00436         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
00437         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
00438         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
00439         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
00440         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
00441         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
00442         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
00443         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
00444         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
00445         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
00446         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
00447         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
00448         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
00449         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
00450         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
00451         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
00452         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
00453         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
00454         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
00455         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
00456         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
00457         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
00458         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
00459         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
00460         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
00461         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
00462         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
00463         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
00464         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
00465         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
00466         MOVE 'FADD'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
00467         MOVE +000     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
00468         MOVE +000     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
00469         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
00470         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
00471         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
00472         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
00473         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
00474         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
00475         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
00476         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
00477         MOVE +210000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
00478         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
00479         MOVE '0'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
00480         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
00481                                                                   CDHPIO2 
00482 ***  SET  GCDHP-INDEX   TO +3.                                    CDHPIO2 
00483 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
00484 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
00485                                                                   CDHPIO2 
00486      IF GCDHP-ACCUM-DED                                           CDHPIO2 
00487         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
00488         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
00489                                                                   CDHPIO2 
00490         MOVE '02'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
00491         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
00492         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
00493         MOVE  20050101                                            CDHPIO2 
00494                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
00495         MOVE  20051231                                            CDHPIO2 
00496                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
00497         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
00498         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
00499         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
00500         MOVE '13'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
00501         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
00502         MOVE 'SINGLE   '                                          CDHPIO2 
00503                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
00504         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
00505         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
00506         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
00507         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
00508         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
00509         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
00510         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
00511         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
00512         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
00513         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
00514         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
00515         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
00516         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
00517         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
00518         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
00519         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
00520         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
00521         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
00522         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
00523         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
00524         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
00525         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
00526         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
00527         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
00528         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
00529         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
00530         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
00531         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
00532         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
00533         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
00534         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
00535         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
00536         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
00537         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
00538         MOVE 'IADD'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
00539         MOVE +001     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
00540         MOVE +018     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
00541         MOVE 'D'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
00542         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
00543         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
00544         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
00545         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
00546         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
00547         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
00548         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
00549         MOVE +140000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
00550         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
00551         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
00552         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
00553                                                                   CDHPIO2 
00554 ***  SET  GCDHP-INDEX   TO +4.                                    CDHPIO2 
00555 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
00556 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
00557                                                                   CDHPIO2 
00558      IF GCDHP-ACCUM-DED                                           CDHPIO2 
00559         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
00560         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
00561                                                                   CDHPIO2 
00562         MOVE '02'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
00563         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
00564         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
00565         MOVE  20050101                                            CDHPIO2 
00566                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
00567         MOVE  20051231                                            CDHPIO2 
00568                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
00569         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
00570         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
00571         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
00572         MOVE '13'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
00573         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
00574         MOVE 'SINGLE   '                                          CDHPIO2 
00575                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
00576         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
00577         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
00578         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
00579         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
00580         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
00581         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
00582         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
00583         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
00584         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
00585         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
00586         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
00587         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
00588         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
00589         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
00590         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
00591         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
00592         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
00593         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
00594         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
00595         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
00596         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
00597         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
00598         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
00599         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
00600         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
00601         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
00602         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
00603         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
00604         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
00605         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
00606         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
00607         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
00608         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
00609         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
00610         MOVE 'IADD'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
00611         MOVE +019     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
00612         MOVE +999     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
00613         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
00614         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
00615         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
00616         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
00617         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
00618         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
00619         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
00620         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
00621         MOVE +000000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
00622         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
00623         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
00624         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
00625                                                                   CDHPIO2 
00626 ***  SET  GCDHP-INDEX   TO +5.                                    CDHPIO2 
00627 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
00628 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
00629                                                                   CDHPIO2 
00630      IF GCDHP-ACCUM-DED                                           CDHPIO2 
00631         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
00632         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
00633                                                                   CDHPIO2 
00634         MOVE '02'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
00635         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
00636         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
00637         MOVE  20050101                                            CDHPIO2 
00638                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
00639         MOVE  20051231                                            CDHPIO2 
00640                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
00641         MOVE 'F'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
00642         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
00643         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
00644         MOVE '13'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
00645         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
00646         MOVE 'EEPLUSDEP'                                          CDHPIO2 
00647                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
00648         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
00649         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
00650         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
00651         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
00652         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
00653         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
00654         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
00655         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
00656         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
00657         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
00658         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
00659         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
00660         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
00661         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
00662         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
00663         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
00664         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
00665         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
00666         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
00667         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
00668         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
00669         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
00670         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
00671         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
00672         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
00673         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
00674         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
00675         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
00676         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
00677         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
00678         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
00679         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
00680         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
00681         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
00682         MOVE 'FADD'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
00683         MOVE +019     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
00684         MOVE +999     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
00685         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
00686         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
00687         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
00688         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
00689         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
00690         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
00691         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
00692         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
00693         MOVE +210000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
00694         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
00695         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
00696         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
00697                                                                   CDHPIO2 
00698 ***  SET  GCDHP-INDEX   TO +6.                                    CDHPIO2 
00699 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
00700 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
00701                                                                   CDHPIO2 
00702      IF GCDHP-ACCUM-DED                                           CDHPIO2 
00703         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
00704         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
00705                                                                   CDHPIO2 
00706         MOVE '02'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
00707         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
00708         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
00709         MOVE  20050101                                            CDHPIO2 
00710                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
00711         MOVE  20051231                                            CDHPIO2 
00712                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
00713         MOVE 'F'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
00714         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
00715         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
00716         MOVE '13'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
00717         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
00718         MOVE 'EEPLUSDEP'                                          CDHPIO2 
00719                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
00720         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
00721         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
00722         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
00723         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
00724         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
00725         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
00726         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
00727         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
00728         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
00729         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
00730         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
00731         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
00732         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
00733         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
00734         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
00735         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
00736         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
00737         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
00738         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
00739         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
00740         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
00741         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
00742         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
00743         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
00744         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
00745         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
00746         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
00747         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
00748         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
00749         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
00750         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
00751         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
00752         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
00753         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
00754         MOVE 'FADD'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
00755         MOVE +001     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
00756         MOVE +018     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
00757         MOVE 'D'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
00758         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
00759         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
00760         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
00761         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
00762         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
00763         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
00764         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
00765         MOVE +210000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
00766         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
00767         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
00768         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
00769                                                                   CDHPIO2 
00770 ***  SET  GCDHP-INDEX   TO +7.                                    CDHPIO2 
00771 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
00772 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
00773                                                                   CDHPIO2 
00774      IF GCDHP-ACCUM-DED                                           CDHPIO2 
00775         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
00776         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
00777                                                                   CDHPIO2 
00778         MOVE '02'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
00779         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
00780         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
00781         MOVE  20050101                                            CDHPIO2 
00782                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
00783         MOVE  20051231                                            CDHPIO2 
00784                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
00785         MOVE 'F'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
00786         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
00787         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
00788         MOVE '13'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
00789         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
00790         MOVE 'FAMILY   '                                          CDHPIO2 
00791                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
00792         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
00793         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
00794         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
00795         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
00796         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
00797         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
00798         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
00799         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
00800         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
00801         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
00802         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
00803         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
00804         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
00805         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
00806         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
00807         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
00808         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
00809         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
00810         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
00811         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
00812         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
00813         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
00814         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
00815         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
00816         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
00817         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
00818         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
00819         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
00820         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
00821         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
00822         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
00823         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
00824         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
00825         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
00826         MOVE 'FADD'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
00827         MOVE +001     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
00828         MOVE +018     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
00829         MOVE 'D'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
00830         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
00831         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
00832         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
00833         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
00834         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
00835         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
00836         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
00837         MOVE +280000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
00838         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
00839         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
00840         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
00841                                                                   CDHPIO2 
00842 ***  SET  GCDHP-INDEX   TO +8.                                    CDHPIO2 
00843 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
00844 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
00845                                                                   CDHPIO2 
00846      IF GCDHP-ACCUM-DED                                           CDHPIO2 
00847         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
00848         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
00849                                                                   CDHPIO2 
00850         MOVE '02'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
00851         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
00852         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
00853         MOVE  20050101                                            CDHPIO2 
00854                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
00855         MOVE  20051231                                            CDHPIO2 
00856                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
00857         MOVE 'F'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
00858         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
00859         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
00860         MOVE '13'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
00861         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
00862         MOVE 'FAMILY   '                                          CDHPIO2 
00863                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
00864         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
00865         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
00866         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
00867         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
00868         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
00869         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
00870         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
00871         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
00872         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
00873         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
00874         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
00875         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
00876         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
00877         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
00878         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
00879         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
00880         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
00881         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
00882         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
00883         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
00884         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
00885         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
00886         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
00887         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
00888         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
00889         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
00890         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
00891         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
00892         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
00893         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
00894         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
00895         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
00896         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
00897         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
00898         MOVE 'FADD'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
00899         MOVE +019     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
00900         MOVE +999     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
00901         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
00902         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
00903         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
00904         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
00905         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
00906         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
00907         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
00908         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
00909         MOVE +280000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
00910         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
00911         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
00912         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
00913                                                                   CDHPIO2 
00914 ***  SET  GCDHP-INDEX   TO +9.                                    CDHPIO2 
00915 *-   #AOL - OUT-OF-POCKET LIMITS                                  CDHPIO2 
00916 *-   IF GCDHP-ACCUM-OPX                                           CDHPIO2 
00917                                                                   CDHPIO2 
00918      IF GCDHP-ACCUM-OPX                                           CDHPIO2 
00919         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
00920         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
00921                                                                   CDHPIO2 
00922         MOVE '02'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
00923         MOVE LOW-VALUE                                            CDHPIO2 
00924                       TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
00925         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
00926         MOVE  20050101                                            CDHPIO2 
00927                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
00928         MOVE  20051231                                            CDHPIO2 
00929                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
00930         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
00931         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
00932         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
00933         MOVE '04'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
00934         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
00935         MOVE 'SINGLE   '                                          CDHPIO2 
00936                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
00937         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
00938         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
00939         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
00940         MOVE '1'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
00941         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
00942         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
00943         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
00944         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
00945         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
00946         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
00947         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
00948         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
00949         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
00950         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
00951         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
00952         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
00953         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
00954         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
00955         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
00956         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
00957         MOVE '1'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
00958         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
00959         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
00960         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
00961         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
00962         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
00963         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
00964         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
00965         MOVE 'G'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
00966         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
00967         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
00968         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
00969         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
00970         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
00971         MOVE 'IIOP'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
00972         MOVE +0       TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
00973         MOVE +0       TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
00974         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
00975         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
00976         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
00977         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
00978         MOVE LOW-VALUE                                            CDHPIO2 
00979                       TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
00980         MOVE +050     TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
00981         MOVE '1'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
00982         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
00983         MOVE +60000   TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
00984         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
00985         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
00986         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
00987                                                                   CDHPIO2 
00988 ***  SET  GCDHP-INDEX   TO +10.                                   CDHPIO2 
00989 *-   #AOL - OUT-OF-POCKET LIMITS                                  CDHPIO2 
00990 *-   IF GCDHP-ACCUM-OPX                                           CDHPIO2 
00991                                                                   CDHPIO2 
00992      IF GCDHP-ACCUM-OPX                                           CDHPIO2 
00993         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
00994         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
00995                                                                   CDHPIO2 
00996         MOVE '02'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
00997         MOVE LOW-VALUE                                            CDHPIO2 
00998                       TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
00999         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
01000         MOVE  20050101                                            CDHPIO2 
01001                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
01002         MOVE  20051231                                            CDHPIO2 
01003                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
01004         MOVE 'F'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
01005         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
01006         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
01007         MOVE '04'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
01008         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
01009         MOVE 'FAMILY   '                                          CDHPIO2 
01010                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
01011         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
01012         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
01013         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
01014         MOVE '1'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
01015         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
01016         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
01017         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
01018         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
01019         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
01020         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
01021         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
01022         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
01023         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
01024         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
01025         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
01026         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
01027         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
01028         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
01029         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
01030         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
01031         MOVE '1'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
01032         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
01033         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
01034         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
01035         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
01036         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
01037         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
01038         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
01039         MOVE 'G'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
01040         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
01041         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
01042         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
01043         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
01044         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
01045         MOVE 'FIOP'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
01046         MOVE +0       TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
01047         MOVE +0       TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
01048         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
01049         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
01050         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
01051         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
01052         MOVE LOW-VALUE                                            CDHPIO2 
01053                       TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
01054         MOVE +050     TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
01055         MOVE '1'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
01056         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
01057         MOVE +120000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
01058         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
01059         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
01060         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
01061                                                                   CDHPIO2 
01062 ***  SET  GCDHP-INDEX   TO +11.                                   CDHPIO2 
01063 *-   #AOL - OUT-OF-POCKET LIMITS                                  CDHPIO2 
01064 *-   IF GCDHP-ACCUM-OPX                                           CDHPIO2 
01065                                                                   CDHPIO2 
01066      IF GCDHP-ACCUM-OPX                                           CDHPIO2 
01067         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
01068         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
01069                                                                   CDHPIO2 
01070         MOVE '02'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
01071         MOVE LOW-VALUE                                            CDHPIO2 
01072                       TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
01073         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
01074         MOVE  20050101                                            CDHPIO2 
01075                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
01076         MOVE  20051231                                            CDHPIO2 
01077                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
01078         MOVE 'F'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
01079         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
01080         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
01081         MOVE '04'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
01082         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
01083         MOVE 'EEPLUSSP '                                          CDHPIO2 
01084                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
01085         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
01086         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
01087         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
01088         MOVE '1'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
01089         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
01090         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
01091         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
01092         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
01093         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
01094         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
01095         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
01096         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
01097         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
01098         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
01099         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
01100         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
01101         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
01102         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
01103         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
01104         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
01105         MOVE '1'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
01106         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
01107         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
01108         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
01109         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
01110         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
01111         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
01112         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
01113         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
01114         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
01115         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
01116         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
01117         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
01118         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
01119         MOVE 'FIOP'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
01120         MOVE +0       TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
01121         MOVE +0       TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
01122         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
01123         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
01124         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
01125         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
01126         MOVE LOW-VALUE                                            CDHPIO2 
01127                       TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
01128         MOVE +050     TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
01129         MOVE '1'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
01130         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
01131         MOVE +90000   TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
01132         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
01133         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
01134         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
01135                                                                   CDHPIO2 
01136 ***  SET  GCDHP-INDEX   TO +12.                                   CDHPIO2 
01137 *-   #AOL - OUT-OF-POCKET LIMITS                                  CDHPIO2 
01138 *-   IF GCDHP-ACCUM-OPX                                           CDHPIO2 
01139                                                                   CDHPIO2 
01140      IF GCDHP-ACCUM-OPX                                           CDHPIO2 
01141         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
01142         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
01143                                                                   CDHPIO2 
01144         MOVE '02'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
01145         MOVE LOW-VALUE                                            CDHPIO2 
01146                       TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
01147         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
01148         MOVE  20050101                                            CDHPIO2 
01149                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
01150         MOVE  20051231                                            CDHPIO2 
01151                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
01152         MOVE 'F'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
01153         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
01154         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
01155         MOVE '04'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
01156         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
01157         MOVE 'EEPLUSDEP'                                          CDHPIO2 
01158                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
01159         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
01160         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
01161         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
01162         MOVE '1'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
01163         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
01164         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
01165         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
01166         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
01167         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
01168         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
01169         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
01170         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
01171         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
01172         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
01173         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
01174         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
01175         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
01176         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
01177         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
01178         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
01179         MOVE '1'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
01180         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
01181         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
01182         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
01183         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
01184         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
01185         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
01186         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
01187         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
01188         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
01189         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
01190         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
01191         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
01192         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
01193         MOVE 'FIOP'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
01194         MOVE +0       TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
01195         MOVE +0       TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
01196         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
01197         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
01198         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
01199         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
01200         MOVE LOW-VALUE                                            CDHPIO2 
01201                       TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
01202         MOVE +050     TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
01203         MOVE '1'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
01204         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
01205         MOVE +90000   TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
01206         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
01207         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
01208         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
01209                                                                   CDHPIO2 
01210      SET  GCDHP-ENTRY-COUNT                                       CDHPIO2 
01211                         TO GCDHP-INDEX.                           CDHPIO2 
01212  4000-EXIT.                                                       CDHPIO2 
01213      EXIT.                                                        CDHPIO2 
01214 /*                                                                CDHPIO2 
01215 /***************************************************************  CDHPIO2 
01216 *                                                              *  CDHPIO2 
01217 * 5000  ACCO                                                   *  CDHPIO2 
01218 *                                                              *  CDHPIO2 
01219 ****************************************************************  CDHPIO2 
01220  5000-000-FORMAT-ACCO.                                            CDHPIO2 
01221                                                                   CDHPIO2 
01222 ***  MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN(GCDHP-INDEX).      CDHPIO2 
01223      MOVE '00'          TO GCDHP-STATUS-CODE.                     CDHPIO2 
01224                                                                   CDHPIO2 
01225 ***  SET  GCDHP-INDEX   TO +1.                                    CDHPIO2 
01226 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
01227 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
01228                                                                   CDHPIO2 
01229      IF GCDHP-ACCUM-DED                                           CDHPIO2 
01230         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
01231         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
01232                                                                   CDHPIO2 
01233         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
01234         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
01235         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
01236         MOVE  20050101                                            CDHPIO2 
01237                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
01238         MOVE  20051231                                            CDHPIO2 
01239                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
01240         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
01241         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
01242         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
01243         MOVE '00'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
01244         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
01245         MOVE 'DEDUCTIBL'                                          CDHPIO2 
01246                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
01247         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
01248         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
01249         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
01250         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
01251         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
01252         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
01253         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
01254         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
01255         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
01256         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
01257         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
01258         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
01259         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
01260         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
01261         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
01262         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
01263         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
01264         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
01265         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
01266         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
01267         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
01268         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
01269         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
01270         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
01271         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
01272         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
01273         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
01274         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
01275         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
01276         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
01277         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
01278         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
01279         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
01280         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
01281         MOVE 'IADD'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
01282         MOVE +000     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
01283         MOVE +000     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
01284         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
01285         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
01286         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
01287         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
01288         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
01289         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
01290         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
01291         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
01292         MOVE +150000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
01293         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
01294         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
01295         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
01296                                                                   CDHPIO2 
01297 ***  SET  GCDHP-INDEX   TO +2.                                    CDHPIO2 
01298 *-   #AOL - OUT-OF-POCKET LIMITS                                  CDHPIO2 
01299 *-   IF GCDHP-ACCUM-OPX                                           CDHPIO2 
01300                                                                   CDHPIO2 
01301      IF GCDHP-ACCUM-OPX                                           CDHPIO2 
01302         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
01303         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
01304                                                                   CDHPIO2 
01305         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
01306         MOVE LOW-VALUE                                            CDHPIO2 
01307                       TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
01308         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
01309         MOVE  20050101                                            CDHPIO2 
01310                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
01311         MOVE  20051231                                            CDHPIO2 
01312                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
01313         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
01314         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
01315         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
01316         MOVE '04'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
01317         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
01318         MOVE 'OUTPOCKET'                                          CDHPIO2 
01319                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
01320         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
01321         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
01322         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
01323         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
01324         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
01325         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
01326         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
01327         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
01328         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
01329         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
01330         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
01331         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
01332         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
01333         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
01334         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
01335         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
01336         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
01337         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
01338         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
01339         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
01340         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
01341         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
01342         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
01343         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
01344         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
01345         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
01346         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
01347         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
01348         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
01349         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
01350         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
01351         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
01352         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
01353         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
01354         MOVE 'IAOP'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
01355         MOVE +0       TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
01356         MOVE +0       TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
01357         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
01358         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
01359         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
01360         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
01361         MOVE LOW-VALUE                                            CDHPIO2 
01362                       TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
01363         MOVE +090     TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
01364         MOVE '1'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
01365         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
01366         MOVE +300000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
01367         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
01368         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
01369         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
01370                                                                   CDHPIO2 
01371      IF GCDHP-ACCUM-DED                                           CDHPIO2 
01372         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
01373         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
01374                                                                   CDHPIO2 
01375         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
01376         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
01377         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
01378         MOVE  20050101                                            CDHPIO2 
01379                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
01380         MOVE  20051231                                            CDHPIO2 
01381                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
01382         MOVE 'F'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
01383         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
01384         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
01385         MOVE '00'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
01386         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
01387         MOVE 'DEDUCTIBL'                                          CDHPIO2 
01388                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
01389         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
01390         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
01391         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
01392         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
01393         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
01394         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
01395         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
01396         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
01397         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
01398         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
01399         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
01400         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
01401         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
01402         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
01403         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
01404         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
01405         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
01406         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
01407         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
01408         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
01409         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
01410         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
01411         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
01412         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
01413         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
01414         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
01415         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
01416         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
01417         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
01418         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
01419         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
01420         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
01421         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
01422         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
01423         MOVE 'FIDD'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
01424         MOVE +000     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
01425         MOVE +000     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
01426         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
01427         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
01428         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
01429         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
01430         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
01431         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
01432         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
01433         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
01434         MOVE +300000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
01435         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
01436         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
01437         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
01438                                                                   CDHPIO2 
01439 *-   #AOL - OUT-OF-POCKET LIMITS                                  CDHPIO2 
01440 *-   IF GCDHP-ACCUM-OPX                                           CDHPIO2 
01441                                                                   CDHPIO2 
01442      IF GCDHP-ACCUM-OPX                                           CDHPIO2 
01443         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
01444         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
01445                                                                   CDHPIO2 
01446         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
01447         MOVE LOW-VALUE                                            CDHPIO2 
01448                       TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
01449         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
01450         MOVE  20050101                                            CDHPIO2 
01451                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
01452         MOVE  20051231                                            CDHPIO2 
01453                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
01454         MOVE 'F'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
01455         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
01456         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
01457         MOVE '04'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
01458         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
01459         MOVE 'OUTPOCKET'                                          CDHPIO2 
01460                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
01461         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
01462         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
01463         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
01464         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
01465         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
01466         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
01467         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
01468         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
01469         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
01470         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
01471         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
01472         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
01473         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
01474         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
01475         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
01476         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
01477         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
01478         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
01479         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
01480         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
01481         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
01482         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
01483         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
01484         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
01485         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
01486         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
01487         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
01488         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
01489         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
01490         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
01491         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
01492         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
01493         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
01494         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
01495         MOVE 'FIOP'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
01496         MOVE +0       TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
01497         MOVE +0       TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
01498         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
01499         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
01500         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
01501         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
01502         MOVE LOW-VALUE                                            CDHPIO2 
01503                       TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
01504         MOVE +090     TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
01505         MOVE '1'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
01506         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
01507         MOVE +600000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
01508         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
01509         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
01510         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
01511                                                                   CDHPIO2 
01512      SET  GCDHP-ENTRY-COUNT                                       CDHPIO2 
01513                         TO GCDHP-INDEX.                           CDHPIO2 
01514  5000-EXIT.                                                       CDHPIO2 
01515      EXIT.                                                        CDHPIO2 
01516 /*                                                                CDHPIO2 
01517 /***************************************************************  CDHPIO2 
01518 *                                                              *  CDHPIO2 
01519 * 6000  EBY BROWN                                              *  CDHPIO2 
01520 *                                                              *  CDHPIO2 
01521 ****************************************************************  CDHPIO2 
01522  6000-000-FORMAT-EBY-BROWN.                                       CDHPIO2 
01523                                                                   CDHPIO2 
01524 ***  MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN(GCDHP-INDEX).      CDHPIO2 
01525      MOVE '00'          TO GCDHP-STATUS-CODE.                     CDHPIO2 
01526                                                                   CDHPIO2 
01527 ***  SET  GCDHP-INDEX   TO +1.                                    CDHPIO2 
01528 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
01529                                                                   CDHPIO2 
01530      IF GCDHP-ACCUM-DED                                           CDHPIO2 
01531         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
01532         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
01533                                                                   CDHPIO2 
01534         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
01535         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
01536         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
01537         MOVE  20050101                                            CDHPIO2 
01538                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
01539         MOVE  20051231                                            CDHPIO2 
01540                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
01541         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
01542         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
01543         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
01544         MOVE '13'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
01545         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
01546         MOVE 'SPECANCIL'                                          CDHPIO2 
01547                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
01548         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
01549         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
01550         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
01551         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
01552         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
01553         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
01554         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
01555         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
01556         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
01557         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
01558         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
01559         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
01560         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
01561         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
01562         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
01563         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
01564         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
01565         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
01566         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
01567         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
01568         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
01569         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
01570         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
01571         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
01572         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
01573         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
01574         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
01575         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
01576         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
01577         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
01578         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
01579         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
01580         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
01581         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
01582         MOVE '0000'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
01583         MOVE +001     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
01584         MOVE +015     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
01585         MOVE 'D'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
01586         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
01587         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
01588         MOVE '1'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
01589         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
01590         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
01591         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
01592         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
01593         MOVE +30000   TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
01594         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
01595         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
01596         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
01597                                                                   CDHPIO2 
01598 ***  SET  GCDHP-INDEX   TO +2.                                    CDHPIO2 
01599 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
01600 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
01601                                                                   CDHPIO2 
01602      IF GCDHP-ACCUM-DED                                           CDHPIO2 
01603         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
01604         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
01605                                                                   CDHPIO2 
01606         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
01607         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
01608         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
01609         MOVE  20050101                                            CDHPIO2 
01610                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
01611         MOVE  20051231                                            CDHPIO2 
01612                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
01613         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
01614         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
01615         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
01616         MOVE '13'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
01617         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
01618         MOVE 'SPECANCIL'                                          CDHPIO2 
01619                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
01620         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
01621         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
01622         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
01623         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
01624         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
01625         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
01626         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
01627         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
01628         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
01629         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
01630         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
01631         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
01632         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
01633         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
01634         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
01635         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
01636         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
01637         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
01638         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
01639         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
01640         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
01641         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
01642         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
01643         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
01644         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
01645         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
01646         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
01647         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
01648         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
01649         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
01650         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
01651         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
01652         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
01653         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
01654         MOVE '0000'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
01655         MOVE +016     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
01656         MOVE +999     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
01657         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
01658         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
01659         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
01660         MOVE '1'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
01661         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
01662         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
01663         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
01664         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
01665         MOVE +30000   TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
01666         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
01667         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
01668         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
01669                                                                   CDHPIO2 
01670 ***  SET  GCDHP-INDEX   TO +3.                                    CDHPIO2 
01671 *-   #AOL - OUT-OF-POCKET LIMITS                                  CDHPIO2 
01672 *-   IF GCDHP-ACCUM-OPX                                           CDHPIO2 
01673                                                                   CDHPIO2 
01674      IF GCDHP-ACCUM-OPX                                           CDHPIO2 
01675         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
01676         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
01677                                                                   CDHPIO2 
01678         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
01679         MOVE LOW-VALUE                                            CDHPIO2 
01680                       TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
01681         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
01682         MOVE  20050101                                            CDHPIO2 
01683                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
01684         MOVE  20051231                                            CDHPIO2 
01685                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
01686         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
01687         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
01688         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
01689         MOVE '18'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
01690         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
01691         MOVE 'SPECANCIL'                                          CDHPIO2 
01692                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
01693         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
01694         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
01695         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
01696         MOVE '1'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
01697         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
01698         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
01699         MOVE '1'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
01700         MOVE '1'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
01701         MOVE '1'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
01702         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
01703         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
01704         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
01705         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
01706         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
01707         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
01708         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
01709         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
01710         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
01711         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
01712         MOVE '1'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
01713         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
01714         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
01715         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
01716         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
01717         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
01718         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
01719         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
01720         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
01721         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
01722         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
01723         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
01724         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
01725         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
01726         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
01727         MOVE 'IIOP'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
01728         MOVE +0       TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
01729         MOVE +0       TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
01730         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
01731         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
01732         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
01733         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
01734         MOVE LOW-VALUE                                            CDHPIO2 
01735                       TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
01736         MOVE +080     TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
01737         MOVE '1'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
01738         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
01739         MOVE +95000   TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
01740         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
01741         MOVE '0'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
01742         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
01743                                                                   CDHPIO2 
01744      SET  GCDHP-ENTRY-COUNT                                       CDHPIO2 
01745                         TO GCDHP-INDEX.                           CDHPIO2 
01746  6000-EXIT.                                                       CDHPIO2 
01747      EXIT.                                                        CDHPIO2 
01748 /*                                                                CDHPIO2 
01749 /***************************************************************  CDHPIO2 
01750 *                                                              *  CDHPIO2 
01751 * 7000  NAPERVILLE                                             *  CDHPIO2 
01752 *                                                              *  CDHPIO2 
01753 ****************************************************************  CDHPIO2 
01754  7000-000-FORMAT-NAPERVILLE.                                      CDHPIO2 
01755                                                                   CDHPIO2 
01756 ***  MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN(GCDHP-INDEX).      CDHPIO2 
01757      MOVE '00'          TO GCDHP-STATUS-CODE.                     CDHPIO2 
01758                                                                   CDHPIO2 
01759 ***  SET  GCDHP-INDEX   TO +1.                                    CDHPIO2 
01760 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
01761 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
01762                                                                   CDHPIO2 
01763      IF GCDHP-ACCUM-DED                                           CDHPIO2 
01764         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
01765         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
01766                                                                   CDHPIO2 
01767         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
01768         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
01769         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
01770         MOVE  20050101                                            CDHPIO2 
01771                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
01772         MOVE  20051231                                            CDHPIO2 
01773                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
01774         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
01775         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
01776         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
01777         MOVE '00'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
01778         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
01779         MOVE 'MISC AGE '                                          CDHPIO2 
01780                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
01781         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
01782         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
01783         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
01784         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
01785         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
01786         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
01787         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
01788         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
01789         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
01790         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
01791         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
01792         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
01793         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
01794         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
01795         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
01796         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
01797         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
01798         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
01799         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
01800         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
01801         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
01802         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
01803         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
01804         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
01805         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
01806         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
01807         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
01808         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
01809         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
01810         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
01811         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
01812         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
01813         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
01814         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
01815         MOVE '0000'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
01816         MOVE +001     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
01817         MOVE +015     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
01818         MOVE 'D'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
01819         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
01820         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
01821         MOVE '1'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
01822         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
01823         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
01824         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
01825         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
01826         MOVE +25000   TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
01827         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
01828         MOVE '0'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
01829         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
01830                                                                   CDHPIO2 
01831 ***  SET  GCDHP-INDEX   TO +2.                                    CDHPIO2 
01832 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
01833 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
01834                                                                   CDHPIO2 
01835      IF GCDHP-ACCUM-DED                                           CDHPIO2 
01836         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
01837         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
01838                                                                   CDHPIO2 
01839         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
01840         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
01841         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
01842         MOVE  20050101                                            CDHPIO2 
01843                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
01844         MOVE  20051231                                            CDHPIO2 
01845                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
01846         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
01847         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
01848         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
01849         MOVE '00'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
01850         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
01851         MOVE 'MISC AGE '                                          CDHPIO2 
01852                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
01853         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
01854         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
01855         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
01856         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
01857         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
01858         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
01859         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
01860         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
01861         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
01862         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
01863         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
01864         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
01865         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
01866         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
01867         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
01868         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
01869         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
01870         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
01871         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
01872         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
01873         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
01874         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
01875         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
01876         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
01877         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
01878         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
01879         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
01880         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
01881         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
01882         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
01883         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
01884         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
01885         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
01886         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
01887         MOVE '0000'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
01888         MOVE +016     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
01889         MOVE +999     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
01890         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
01891         MOVE 'Y'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
01892         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
01893         MOVE '1'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
01894         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
01895         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
01896         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
01897         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
01898         MOVE +25000   TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
01899         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
01900         MOVE '0'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
01901         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
01902                                                                   CDHPIO2 
01903 ***  SET  GCDHP-INDEX   TO +3.                                    CDHPIO2 
01904 *-   #AOL - OUT-OF-POCKET LIMITS                                  CDHPIO2 
01905 *-   IF GCDHP-ACCUM-OPX                                           CDHPIO2 
01906                                                                   CDHPIO2 
01907      IF GCDHP-ACCUM-OPX                                           CDHPIO2 
01908         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
01909         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
01910                                                                   CDHPIO2 
01911         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
01912         MOVE LOW-VALUE                                            CDHPIO2 
01913                       TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
01914         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
01915         MOVE  20050101                                            CDHPIO2 
01916                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
01917         MOVE  20051231                                            CDHPIO2 
01918                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
01919         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
01920         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
01921         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
01922         MOVE '04'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
01923         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
01924         MOVE 'SPECANCIL'                                          CDHPIO2 
01925                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
01926         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
01927         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
01928         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
01929         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
01930         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
01931         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
01932         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
01933         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
01934         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
01935         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
01936         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
01937         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
01938         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
01939         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
01940         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
01941         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
01942         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
01943         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
01944         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
01945         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
01946         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
01947         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
01948         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
01949         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
01950         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
01951         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
01952         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
01953         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
01954         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
01955         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
01956         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
01957         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
01958         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
01959         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
01960         MOVE 'IIOP'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
01961         MOVE +0       TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
01962         MOVE +0       TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
01963         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
01964         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
01965         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
01966         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
01967         MOVE LOW-VALUE                                            CDHPIO2 
01968                       TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
01969         MOVE +080     TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
01970         MOVE '1'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
01971         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
01972         MOVE +85000   TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
01973         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
01974         MOVE 'G'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
01975         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
01976                                                                   CDHPIO2 
01977      SET  GCDHP-ENTRY-COUNT                                       CDHPIO2 
01978                         TO GCDHP-INDEX.                           CDHPIO2 
01979  7000-EXIT.                                                       CDHPIO2 
01980      EXIT.                                                        CDHPIO2 
01981 /*                                                                CDHPIO2 
01982 /***************************************************************  CDHPIO2 
01983 *                                                              *  CDHPIO2 
01984 * 8000  TTI                                                    *  CDHPIO2 
01985 *                                                              *  CDHPIO2 
01986 ****************************************************************  CDHPIO2 
01987  8000-000-FORMAT-TTI.                                             CDHPIO2 
01988                                                                   CDHPIO2 
01989 ***  MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN(GCDHP-INDEX).      CDHPIO2 
01990      MOVE '00'          TO GCDHP-STATUS-CODE.                     CDHPIO2 
01991                                                                   CDHPIO2 
01992 ***  SET  GCDHP-INDEX   TO +1.                                    CDHPIO2 
01993 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
01994 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
01995                                                                   CDHPIO2 
01996      IF GCDHP-ACCUM-DED                                           CDHPIO2 
01997         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
01998         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
01999                                                                   CDHPIO2 
02000         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
02001         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
02002         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
02003         MOVE  20050101                                            CDHPIO2 
02004                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
02005         MOVE  20051231                                            CDHPIO2 
02006                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
02007         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
02008         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
02009         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
02010         MOVE '00'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
02011         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
02012         MOVE 'NONDEDUCT'                                          CDHPIO2 
02013                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
02014         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
02015         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
02016         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
02017         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
02018         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
02019         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
02020         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
02021         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
02022         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
02023         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
02024         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
02025         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
02026         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
02027         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
02028         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
02029         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
02030         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
02031         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
02032         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
02033         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
02034         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
02035         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
02036         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
02037         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
02038         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
02039         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
02040         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
02041         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
02042         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
02043         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
02044         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
02045         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
02046         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
02047         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
02048         MOVE '0000'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
02049         MOVE +000     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
02050         MOVE +000     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
02051         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
02052         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
02053         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
02054         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
02055         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
02056         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
02057         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
02058         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
02059         MOVE +50000   TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
02060         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
02061         MOVE '0'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
02062         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
02063                                                                   CDHPIO2 
02064 ***  SET  GCDHP-INDEX   TO +2.                                    CDHPIO2 
02065 *-   #AOL - OUT-OF-POCKET LIMITS                                  CDHPIO2 
02066 *-   IF GCDHP-ACCUM-OPX                                           CDHPIO2 
02067                                                                   CDHPIO2 
02068      IF GCDHP-ACCUM-OPX                                           CDHPIO2 
02069         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
02070         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
02071                                                                   CDHPIO2 
02072         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
02073         MOVE LOW-VALUE                                            CDHPIO2 
02074                       TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
02075         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
02076         MOVE  20050101                                            CDHPIO2 
02077                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
02078         MOVE  20051231                                            CDHPIO2 
02079                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
02080         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
02081         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
02082         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
02083         MOVE '04'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
02084         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
02085         MOVE 'SPECANCIL'                                          CDHPIO2 
02086                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
02087         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
02088         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
02089         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
02090         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
02091         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
02092         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
02093         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
02094         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
02095         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
02096         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
02097         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
02098         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
02099         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
02100         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
02101         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
02102         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
02103         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
02104         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
02105         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
02106         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
02107         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
02108         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
02109         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
02110         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
02111         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
02112         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
02113         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
02114         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
02115         MOVE 'G'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
02116         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
02117         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
02118         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
02119         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
02120         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
02121         MOVE 'IIOP'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
02122         MOVE +0       TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
02123         MOVE +0       TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
02124         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
02125         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
02126         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
02127         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
02128         MOVE LOW-VALUE                                            CDHPIO2 
02129                       TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
02130         MOVE +050     TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
02131         MOVE '1'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
02132         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
02133         MOVE +100000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
02134         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
02135         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
02136         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
02137                                                                   CDHPIO2 
02138      SET  GCDHP-ENTRY-COUNT                                       CDHPIO2 
02139                         TO GCDHP-INDEX.                           CDHPIO2 
02140  8000-EXIT.                                                       CDHPIO2 
02141      EXIT.                                                        CDHPIO2 
02142 /*                                                                CDHPIO2 
02143 /***************************************************************  CDHPIO2 
02144 *                                                              *  CDHPIO2 
02145 * 9000  STS                                                    *  CDHPIO2 
02146 *                                                              *  CDHPIO2 
02147 ****************************************************************  CDHPIO2 
02148  9000-000-FORMAT-STS.                                             CDHPIO2 
02149                                                                   CDHPIO2 
02150 ***  MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN(GCDHP-INDEX).      CDHPIO2 
02151      MOVE '00'          TO GCDHP-STATUS-CODE.                     CDHPIO2 
02152                                                                   CDHPIO2 
02153 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
02154 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
02155                                                                   CDHPIO2 
02156      IF GCDHP-ACCUM-DED                                           CDHPIO2 
02157         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
02158         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
02159                                                                   CDHPIO2 
02160         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
02161         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
02162         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
02163         MOVE  20050101                                            CDHPIO2 
02164                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
02165         MOVE  20051231                                            CDHPIO2 
02166                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
02167         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
02168         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
02169         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
02170         MOVE '00'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
02171         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
02172         MOVE 'DEDUCTIBL'                                          CDHPIO2 
02173                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
02174         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
02175         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
02176         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
02177         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
02178         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
02179         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
02180         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
02181         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
02182         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
02183         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
02184         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
02185         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
02186         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
02187         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
02188         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
02189         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
02190         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
02191         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
02192         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
02193         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
02194         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
02195         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
02196         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
02197         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
02198         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
02199         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
02200         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
02201         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
02202         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
02203         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
02204         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
02205         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
02206         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
02207         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
02208         MOVE 'IIDD'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
02209         MOVE +000     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
02210         MOVE +000     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
02211         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
02212         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
02213         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
02214         MOVE '1'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
02215         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
02216         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
02217         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
02218         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
02219         MOVE +200000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
02220         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
02221         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
02222         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
02223                                                                   CDHPIO2 
02224 *-   #AOL - OUT-OF-POCKET LIMITS                                  CDHPIO2 
02225 *-   IF GCDHP-ACCUM-OPX                                           CDHPIO2 
02226                                                                   CDHPIO2 
02227      IF GCDHP-ACCUM-OPX                                           CDHPIO2 
02228         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
02229         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
02230                                                                   CDHPIO2 
02231         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
02232         MOVE LOW-VALUE                                            CDHPIO2 
02233                       TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
02234         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
02235         MOVE  20050101                                            CDHPIO2 
02236                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
02237         MOVE  20051231                                            CDHPIO2 
02238                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
02239         MOVE 'I'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
02240         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
02241         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
02242         MOVE '04'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
02243         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
02244         MOVE 'OUTPOCKET'                                          CDHPIO2 
02245                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
02246         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
02247         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
02248         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
02249         MOVE '1'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
02250         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
02251         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
02252         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
02253         MOVE '1'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
02254         MOVE '1'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
02255         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
02256         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
02257         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
02258         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
02259         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
02260         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
02261         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
02262         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
02263         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
02264         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
02265         MOVE '1'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
02266         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
02267         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
02268         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
02269         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
02270         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
02271         MOVE '1'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
02272         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
02273         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
02274         MOVE 'G'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
02275         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
02276         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
02277         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
02278         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
02279         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
02280         MOVE 'IIOP'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
02281         MOVE +0       TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
02282         MOVE +0       TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
02283         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
02284         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
02285         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
02286         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
02287         MOVE LOW-VALUE                                            CDHPIO2 
02288                       TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
02289         MOVE +080     TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
02290         MOVE '1'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
02291         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
02292         MOVE +200000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
02293         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
02294         MOVE '0'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
02295         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
02296                                                                   CDHPIO2 
02297 *-   #ADL - DEDUCTIBLE LIMITS                                     CDHPIO2 
02298 *-   IF GCDHP-ACCUM-DED                                           CDHPIO2 
02299                                                                   CDHPIO2 
02300      IF GCDHP-ACCUM-DED                                           CDHPIO2 
02301         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
02302         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
02303                                                                   CDHPIO2 
02304         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
02305         MOVE '0'      TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
02306         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
02307         MOVE  20050101                                            CDHPIO2 
02308                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
02309         MOVE  20051231                                            CDHPIO2 
02310                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
02311         MOVE 'F'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
02312         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
02313         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
02314         MOVE '00'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
02315         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
02316         MOVE 'DEDUCTIBL'                                          CDHPIO2 
02317                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
02318         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
02319         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
02320         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
02321         MOVE '0'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
02322         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
02323         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
02324         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
02325         MOVE '0'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
02326         MOVE '0'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
02327         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
02328         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
02329         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
02330         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
02331         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
02332         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
02333         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
02334         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
02335         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
02336         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
02337         MOVE '0'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
02338         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
02339         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
02340         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
02341         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
02342         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
02343         MOVE '0'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
02344         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
02345         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
02346         MOVE '0'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
02347         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
02348         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
02349         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
02350         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
02351         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
02352         MOVE 'FIDD'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
02353         MOVE +000     TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
02354         MOVE +000     TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
02355         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
02356         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
02357         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
02358         MOVE '1'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
02359         MOVE '0'      TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
02360         MOVE +0       TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
02361         MOVE '0'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
02362         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
02363         MOVE +400000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
02364         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
02365         MOVE 'F'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
02366         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
02367                                                                   CDHPIO2 
02368 *-   #AOL - OUT-OF-POCKET LIMITS                                  CDHPIO2 
02369 *-   IF GCDHP-ACCUM-OPX                                           CDHPIO2 
02370                                                                   CDHPIO2 
02371      IF GCDHP-ACCUM-OPX                                           CDHPIO2 
02372         SET  GCDHP-INDEX   UP BY +1                               CDHPIO2 
02373         MOVE ZEROES        TO GCDHP-ACCUMS-CHOSEN  (GCDHP-INDEX)  CDHPIO2 
02374                                                                   CDHPIO2 
02375         MOVE '01'     TO GCDHP-ACCUM-VENDOR (GCDHP-INDEX)         CDHPIO2 
02376         MOVE LOW-VALUE                                            CDHPIO2 
02377                       TO GCDHP-MANDATORY-IND (GCDHP-INDEX)        CDHPIO2 
02378         MOVE '0D'     TO GCDHP-BENEFIT-PERIOD (GCDHP-INDEX)       CDHPIO2 
02379         MOVE  20050101                                            CDHPIO2 
02380                       TO GCDHP-BEN-PRD-BEGIN-DATE (GCDHP-INDEX)   CDHPIO2 
02381         MOVE  20051231                                            CDHPIO2 
02382                       TO GCDHP-BEN-PRD-END-DATEN (GCDHP-INDEX)    CDHPIO2 
02383         MOVE 'F'      TO GCDHP-FAM-OR-INDIV (GCDHP-INDEX)         CDHPIO2 
02384         MOVE '4'      TO GCDHP-L-O-B (GCDHP-INDEX)                CDHPIO2 
02385         MOVE '00'     TO GCDHP-TIME-DOLLAR-IND (GCDHP-INDEX)      CDHPIO2 
02386         MOVE '04'     TO GCDHP-DEFINITION (GCDHP-INDEX)           CDHPIO2 
02387         MOVE '0'      TO GCDHP-DAY-FACTOR-IND (GCDHP-INDEX)       CDHPIO2 
02388         MOVE 'OUTPOCKET'                                          CDHPIO2 
02389                       TO GCDHP-INTERNAL-DESCRIPTOR (GCDHP-INDEX)  CDHPIO2 
02390         MOVE '00'     TO GCDHP-SERVICE-GROUP (GCDHP-INDEX)        CDHPIO2 
02391         MOVE '0R'     TO GCDHP-PLACE-OF-TREATMENT (GCDHP-INDEX)   CDHPIO2 
02392         MOVE '1'      TO GCDHP-ALL-BIT (GCDHP-INDEX)              CDHPIO2 
02393         MOVE '1'      TO GCDHP-EXCLUSION-BIT (GCDHP-INDEX)        CDHPIO2 
02394         MOVE '0'      TO GCDHP-ICD-BIT (GCDHP-INDEX)              CDHPIO2 
02395         MOVE '0'      TO GCDHP-TB-BIT (GCDHP-INDEX)               CDHPIO2 
02396         MOVE '0'      TO GCDHP-MENTAL-BIT (GCDHP-INDEX)           CDHPIO2 
02397         MOVE '1'      TO GCDHP-DRUG-BIT (GCDHP-INDEX)             CDHPIO2 
02398         MOVE '1'      TO GCDHP-ALCOHOL-BIT (GCDHP-INDEX)          CDHPIO2 
02399         MOVE '0'      TO GCDHP-OB-COMP-BIT (GCDHP-INDEX)          CDHPIO2 
02400         MOVE '0'      TO GCDHP-OB-NORM-BIT (GCDHP-INDEX)          CDHPIO2 
02401         MOVE '0'      TO GCDHP-MALIGNANCY-BIT (GCDHP-INDEX)       CDHPIO2 
02402         MOVE '0'      TO GCDHP-CARDIAC-DISEASE-BIT (GCDHP-INDEX)  CDHPIO2 
02403         MOVE '0'      TO GCDHP-OBESITY-BIT (GCDHP-INDEX)          CDHPIO2 
02404         MOVE '0'      TO GCDHP-KIDNEY-DISEASE-BIT (GCDHP-INDEX)   CDHPIO2 
02405         MOVE '0'      TO GCDHP-ACCIDENT-BIT (GCDHP-INDEX)         CDHPIO2 
02406         MOVE '0'      TO GCDHP-PRE-EXIST-BIT (GCDHP-INDEX)        CDHPIO2 
02407         MOVE '0'      TO GCDHP-NON-EMER-BIT (GCDHP-INDEX)         CDHPIO2 
02408         MOVE '0'      TO GCDHP-SUICIDE-BIT (GCDHP-INDEX)          CDHPIO2 
02409         MOVE '1'      TO GCDHP-TMJ-BIT (GCDHP-INDEX)              CDHPIO2 
02410         MOVE '0'      TO GCDHP-INF-BIT (GCDHP-INDEX)              CDHPIO2 
02411         MOVE '0'      TO GCDHP-LIFE-THREAT-BIT (GCDHP-INDEX)      CDHPIO2 
02412         MOVE '0'      TO GCDHP-EMER-MED-BIT (GCDHP-INDEX)         CDHPIO2 
02413         MOVE '0'      TO GCDHP-EMER-ACC-BIT (GCDHP-INDEX)         CDHPIO2 
02414         MOVE '0'      TO GCDHP-SER-MEN-ILL-BIT (GCDHP-INDEX)      CDHPIO2 
02415         MOVE '1'      TO GCDHP-NON-SER-MEN-ILL-BIT (GCDHP-INDEX)  CDHPIO2 
02416         MOVE '000000' TO GCDHP-FILLER-BIT (GCDHP-INDEX)           CDHPIO2 
02417         MOVE '0'      TO GCDHP-BISCENDING-IND (GCDHP-INDEX)       CDHPIO2 
02418         MOVE 'G'      TO GCDHP-CLAIM-LVL-ACCUM-IND (GCDHP-INDEX)  CDHPIO2 
02419         MOVE '0'      TO GCDHP-CO-PAY-IND (GCDHP-INDEX)           CDHPIO2 
02420         MOVE '00'     TO GCDHP-COST-CONTAIN-IND (GCDHP-INDEX)     CDHPIO2 
02421         MOVE '0'      TO GCDHP-COINS-1ST-DOLR-COVRGE (GCDHP-INDEX)CDHPIO2 
02422         MOVE '0'      TO GCDHP-SEL-ADDL-BEN-DET (GCDHP-INDEX)     CDHPIO2 
02423         MOVE '00'     TO GCDHP-COMB-APPLIED-IND (GCDHP-INDEX)     CDHPIO2 
02424         MOVE 'FIOP'   TO GCDHP-ACCUMID (GCDHP-INDEX)              CDHPIO2 
02425         MOVE +0       TO GCDHP-AGE-LIMIT-FROM (GCDHP-INDEX)       CDHPIO2 
02426         MOVE +0       TO GCDHP-AGE-LIMIT-TO (GCDHP-INDEX)         CDHPIO2 
02427         MOVE '0'      TO GCDHP-AGE-QUAL-IND-FROM (GCDHP-INDEX)    CDHPIO2 
02428         MOVE '0'      TO GCDHP-AGE-QUAL-IND-TO (GCDHP-INDEX)      CDHPIO2 
02429         MOVE '00'     TO GCDHP-RELATIONSHIP-IND (GCDHP-INDEX)     CDHPIO2 
02430         MOVE '0'      TO GCDHP-CARRY-OVER-CREDIT-IND (GCDHP-INDEX)CDHPIO2 
02431         MOVE LOW-VALUE                                            CDHPIO2 
02432                       TO GCDHP-LMT-MANDATORY-IND (GCDHP-INDEX)    CDHPIO2 
02433         MOVE +080     TO GCDHP-PERCENT-LEVEL (GCDHP-INDEX)        CDHPIO2 
02434         MOVE '1'      TO GCDHP-ASCEND-DESCEND-IND (GCDHP-INDEX)   CDHPIO2 
02435         MOVE '0'      TO GCDHP-REINSTATEMENT-IND (GCDHP-INDEX)    CDHPIO2 
02436         MOVE +400000  TO GCDHP-VALUE-LIMIT (GCDHP-INDEX)          CDHPIO2 
02437         MOVE '5'      TO GCDHP-VALUE-QUALIFIER (GCDHP-INDEX)      CDHPIO2 
02438         MOVE '0'      TO GCDHP-FEAK-IND (GCDHP-INDEX)             CDHPIO2 
02439         MOVE '000'    TO GCDHP-FYI-VALUE (GCDHP-INDEX).           CDHPIO2 
02440                                                                   CDHPIO2 
02441      SET  GCDHP-ENTRY-COUNT                                       CDHPIO2 
02442                         TO GCDHP-INDEX.                           CDHPIO2 
02443  9000-EXIT.                                                       CDHPIO2 
02444      EXIT.                                                        CDHPIO2 
02445 /*                                                                CDHPIO2 
02446  9800-INITLZ-ACCUMS-CHOSEN.                                       CDHPIO2 
02447                                                                   CDHPIO2 
02448      INITIALIZE  GCDHP-ACCUM-VENDOR           (WS-SUB)            CDHPIO2 
02449                  GCDHP-MANDATORY-IND          (WS-SUB)            CDHPIO2 
02450                  GCDHP-BENEFIT-PERIOD         (WS-SUB)            CDHPIO2 
02451                  GCDHP-BEN-PRD-BEGIN-DATE     (WS-SUB)            CDHPIO2 
02452                  GCDHP-BEN-PRD-END-DATEN      (WS-SUB)            CDHPIO2 
02453                  GCDHP-FAM-OR-INDIV           (WS-SUB)            CDHPIO2 
02454                  GCDHP-L-O-B                  (WS-SUB)            CDHPIO2 
02455                  GCDHP-TIME-DOLLAR-IND        (WS-SUB)            CDHPIO2 
02456                  GCDHP-DEFINITION             (WS-SUB)            CDHPIO2 
02457                  GCDHP-DAY-FACTOR-IND         (WS-SUB)            CDHPIO2 
02458                  GCDHP-INTERNAL-DESCRIPTOR    (WS-SUB)            CDHPIO2 
02459                  GCDHP-SERVICE-GROUP          (WS-SUB)            CDHPIO2 
02460                  GCDHP-PLACE-OF-TREATMENT     (WS-SUB)            CDHPIO2 
02461                  GCDHP-ALL-BIT                (WS-SUB)            CDHPIO2 
02462                  GCDHP-EXCLUSION-BIT          (WS-SUB)            CDHPIO2 
02463                  GCDHP-ICD-BIT                (WS-SUB)            CDHPIO2 
02464                  GCDHP-TB-BIT                 (WS-SUB)            CDHPIO2 
02465                  GCDHP-MENTAL-BIT             (WS-SUB)            CDHPIO2 
02466                  GCDHP-DRUG-BIT               (WS-SUB)            CDHPIO2 
02467                  GCDHP-ALCOHOL-BIT            (WS-SUB)            CDHPIO2 
02468                  GCDHP-OB-COMP-BIT            (WS-SUB)            CDHPIO2 
02469                  GCDHP-OB-NORM-BIT            (WS-SUB)            CDHPIO2 
02470                  GCDHP-MALIGNANCY-BIT         (WS-SUB)            CDHPIO2 
02471                  GCDHP-CARDIAC-DISEASE-BIT    (WS-SUB)            CDHPIO2 
02472                  GCDHP-OBESITY-BIT            (WS-SUB)            CDHPIO2 
02473                  GCDHP-KIDNEY-DISEASE-BIT     (WS-SUB)            CDHPIO2 
02474                  GCDHP-ACCIDENT-BIT           (WS-SUB)            CDHPIO2 
02475                  GCDHP-PRE-EXIST-BIT          (WS-SUB)            CDHPIO2 
02476                  GCDHP-NON-EMER-BIT           (WS-SUB)            CDHPIO2 
02477                  GCDHP-SUICIDE-BIT            (WS-SUB)            CDHPIO2 
02478                  GCDHP-TMJ-BIT                (WS-SUB)            CDHPIO2 
02479                  GCDHP-INF-BIT                (WS-SUB)            CDHPIO2 
02480                  GCDHP-LIFE-THREAT-BIT        (WS-SUB)            CDHPIO2 
02481                  GCDHP-EMER-MED-BIT           (WS-SUB)            CDHPIO2 
02482                  GCDHP-EMER-ACC-BIT           (WS-SUB)            CDHPIO2 
02483                  GCDHP-SER-MEN-ILL-BIT        (WS-SUB)            CDHPIO2 
02484                  GCDHP-NON-SER-MEN-ILL-BIT    (WS-SUB)            CDHPIO2 
02485                  GCDHP-FILLER-BIT             (WS-SUB)            CDHPIO2 
02486                  GCDHP-BISCENDING-IND         (WS-SUB)            CDHPIO2 
02487                  GCDHP-CLAIM-LVL-ACCUM-IND    (WS-SUB)            CDHPIO2 
02488                  GCDHP-CO-PAY-IND             (WS-SUB)            CDHPIO2 
02489                  GCDHP-COST-CONTAIN-IND       (WS-SUB)            CDHPIO2 
02490                  GCDHP-COINS-1ST-DOLR-COVRGE  (WS-SUB)            CDHPIO2 
02491                  GCDHP-SEL-ADDL-BEN-DET       (WS-SUB)            CDHPIO2 
02492                  GCDHP-COMB-APPLIED-IND       (WS-SUB)            CDHPIO2 
02493                  GCDHP-ACCUMID                (WS-SUB)            CDHPIO2 
02494                  GCDHP-AGE-LIMIT-FROM         (WS-SUB)            CDHPIO2 
02495                  GCDHP-AGE-LIMIT-TO           (WS-SUB)            CDHPIO2 
02496                  GCDHP-AGE-QUAL-IND-FROM      (WS-SUB)            CDHPIO2 
02497                  GCDHP-AGE-QUAL-IND-TO        (WS-SUB)            CDHPIO2 
02498                  GCDHP-RELATIONSHIP-IND       (WS-SUB)            CDHPIO2 
02499                  GCDHP-CARRY-OVER-CREDIT-IND  (WS-SUB)            CDHPIO2 
02500                  GCDHP-LMT-MANDATORY-IND      (WS-SUB)            CDHPIO2 
02501                  GCDHP-PERCENT-LEVEL          (WS-SUB)            CDHPIO2 
02502                  GCDHP-ASCEND-DESCEND-IND     (WS-SUB)            CDHPIO2 
02503                  GCDHP-REINSTATEMENT-IND      (WS-SUB)            CDHPIO2 
02504                  GCDHP-VALUE-LIMIT            (WS-SUB)            CDHPIO2 
02505                  GCDHP-VALUE-QUALIFIER        (WS-SUB)            CDHPIO2 
02506                  GCDHP-FEAK-IND               (WS-SUB)            CDHPIO2 
02507                  GCDHP-FYI-VALUE              (WS-SUB).           CDHPIO2 
02508                                                                   CDHPIO2 
02509  9800-EXIT.                                                       CDHPIO2 
02510      EXIT.                                                        CDHPIO2 
02511 /*                                                                CDHPIO2 
02512 /***************************************************************  CDHPIO2 
02513 *                                                              *  CDHPIO2 
02514 * 9999  RETURN TO CALLED PROGRAM                               *  CDHPIO2 
02515 *                                                              *  CDHPIO2 
02516 ****************************************************************  CDHPIO2 
02517  9999-RETURN.                                                     CDHPIO2 
02518                                                                   CDHPIO2 
02519                                                                   CDHPIO2 
02520      EXEC CICS RETURN                                             CDHPIO2 
02521                END-EXEC.                                          CDHPIO2 
02522                                                                   CDHPIO2 
02523  9999-EXIT.                                                       CDHPIO2 
02524      EXIT.                                                        CDHPIO2 
