00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELP032  
00003  PROGRAM-ID.         ELP032.                                         LV001
00004                                                                   ELP032  
00005  AUTHOR.             ANNE KEFFER KING.                            ELP032  
00006                                                                   ELP032  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELP032  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELP032  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELP032  
00010                      233 N. MICHIGAN AVE                          ELP032  
00011                      CHICAGO, ILLINOIS 60601                      ELP032  
00012                                                                   ELP032  
00013  DATE-WRITTEN.       23-MAY-1989.                                 ELP032  
00014                                                                   ELP032  
00015  DATE-COMPILED.                                                   ELP032  
00016                                                                   ELP032  
00017  SECURITY.           COPYRIGHT 1988,                              ELP032  
00018                      HEALTH CARE SERVICE CORPORATION              ELP032  
00019      SKIP3                                                        ELP032  
00020  ENVIRONMENT DIVISION.                                            ELP032  
00021                                                                   ELP032  
00022  CONFIGURATION SECTION.                                           ELP032  
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELP032  
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELP032  
00025      EJECT                                                        ELP032  
00026 ******************************************************************ELP032  
00027 *                                                                *ELP032  
00028 *                    ELS ABEND PROCESSING                        *ELP032  
00029 *                                                                *ELP032  
00030 *   ELP032 - THIS PROGRAM WILL SORT AND OUTPUT THE 'SNAP         *ELP032  
00031 *             SHOT' FILE CREATED BY THE ON-LINE PROGRAM          *ELP032  
00032 *             ELUABEND.  THE INITIAL READ IS DONE TO BYPASS      *ELP032  
00033 *             THE HEADER RECORD REQUIRED BY VSAM.                *ELP032  
00034 *                                                                *ELP032  
00035 * NOTE:  THE PROGRAM LOGIC DESIGN INCLUDES LOGIC FOR             *ELP032  
00036 *        HANDLING CODES MANUAL ABEND SITUATIONS.  HOWEVER,       *ELP032  
00037 *        NO LOGIC IS INCLUDED HERE FOR CODES MANUAL AS IT        *ELP032  
00038 *        IS TO BE ADDED AS AN ENHANCEMENT AT A LATER DATE.       *ELP032  
00039 ******************************************************************ELP032  
00040 *                                                                *ELP032  
00041 *                      MAINTENANCE HISTORY                       *ELP032  
00042 *                                                                *ELP032  
00043 *  MOD     DATE     BY  DRPT                ACTION               *ELP032  
00044 * ----- ----------- --- ----- ---------------------------------- *ELP032  
00045 * 01.00 23-MAY-1989 AKK       CREATED                            *ELP032  
00046 * 01.01 25-SEP-1989 EGL       ADDED SUMMARY FILE                 *ELP032  
00047 * 01.02 16-MAR-1990 AKK       ADDED CODES VALUE AND DATA ELEMENT *ELP032  
00048 *                             FILE. NOT FOUNDS FROM CODES MANUAL *ELP032  
00049 * 01.03 20-APR-1990 AKK       ADDED CODES VALUE FOR THOSE ELE-   *ELP032  
00050 *                             MENTS NOT HANDLED BY ELIQ.         *ELP032  
00051 *                                                                *ELP032  
00052 * 01.04 10-JAN-2000 KIKI      OMITTED NEGATIVE LENGTH RECORDS    *ELP032  
00053 *                             FROM BEING SORTED/PROCESSED.       *ELP032  
00054 *                                                                *ELP032  
00055 ******************************************************************ELP032  
00056                                                                   ELP032  
00057  INPUT-OUTPUT SECTION.                                            ELP032  
00058  FILE-CONTROL.                                                    ELP032  
00059      SELECT SNAP-SHOT-FILE  ASSIGN TO UT-AS-SNAPSHI               ELP032  
00060          ACCESS MODE IS SEQUENTIAL                                ELP032  
00061          ORGANIZATION IS SEQUENTIAL.                              ELP032  
00062      SELECT SORT-FILE   ASSIGN TO SORTFLE.                        ELP032  
00063      SELECT ABEND-EXTRACT-FILE   ASSIGN TO ABEXTRO.               ELP032  
00064      SELECT SUMMARY-EXTRACT-FILE ASSIGN TO SUMMARYO.              ELP032  
00065      SELECT CODE-VALUE-EXTRACT-FILE ASSIGN TO CODVALXO.           ELP032  
00066      SELECT DATA-ELEMENT-EXTRACT-FILE ASSIGN TO DATAELXO.         ELP032  
00067      SELECT CV-PROGRAM-EXTRACT-FILE ASSIGN TO CVPROGXO.           ELP032  
00068                                                                   ELP032  
00069  DATA DIVISION.                                                   ELP032  
00070  FILE SECTION.                                                    ELP032  
00071  FD  SNAP-SHOT-FILE.                                              ELP032  
00072  01  SNAP-SHOT-RECORD-IN.                                         ELP032  
00073      COPY ELSNAP2C.                                               ELP032  
00074 *                                                                 ELP032  
00075  SD  SORT-FILE.                                                   ELP032  
00076      COPY ELSNAPSC.                                               ELP032  
00077 *                                                                 ELP032  
00078  FD  ABEND-EXTRACT-FILE                                           ELP032  
00079      LABEL RECORDS ARE STANDARD                                   ELP032  
00080      BLOCK CONTAINS 0 RECORDS                                     ELP032  
00081      RECORD CONTAINS 51 TO 4089 CHARACTERS                        ELP032  
00082      RECORDING MODE V.                                            ELP032  
00083  01  ABEND-EXTRACT-RECORD.                                        ELP032  
00084      COPY ELSNAP2C                                                ELP032  
00085              REPLACING SSR2-SUB-SIZE                              ELP032  
00086            BY SSR2-SUB-SIZEX                                      ELP032  
00087               SSR2-IDX                                            ELP032  
00088           BY SSR2X-IDX                                            ELP032  
00089              SSR2-VALID-RECORD-TYPE                               ELP032  
00090           BY SSR2-VALID-RECORD-TYPEX                              ELP032  
00091              SSR2-ABEND-CODE                                      ELP032  
00092           BY SSR2-ABEND-CODEX.                                    ELP032  
00093 *                                                                 ELP032  
00094  FD  SUMMARY-EXTRACT-FILE                                         ELP032  
00095      LABEL RECORDS ARE STANDARD                                   ELP032  
00096      BLOCK CONTAINS 0 RECORDS                                     ELP032  
00097      RECORD CONTAINS 51 TO 4089 CHARACTERS                        ELP032  
00098      RECORDING MODE V.                                            ELP032  
00099  01  SUMMARY-EXTRACT-RECORD.                                      ELP032  
00100      COPY ELSNAP2C                                                ELP032  
00101              REPLACING SSR2-SUB-SIZE                              ELP032  
00102            BY SSRS-SUB-SIZEX                                      ELP032  
00103               SSR2-IDX                                            ELP032  
00104           BY SSRSX-IDX                                            ELP032  
00105              SSR2-VALID-RECORD-TYPE                               ELP032  
00106           BY SSRS-VALID-RECORD-TYPEX                              ELP032  
00107              SSR2-ABEND-CODE                                      ELP032  
00108           BY SSRS-ABEND-CODEX.                                    ELP032  
00109 * THE TWO FILES IMMEDIATELY BELOW ARE CREATED FROM 'NOT FOUND'    ELP032  
00110 *CODES MANUAL ELEMENTS AND VALUES                                 ELP032  
00111  FD  CODE-VALUE-EXTRACT-FILE                                      ELP032  
00112      LABEL RECORDS ARE STANDARD                                   ELP032  
00113      BLOCK CONTAINS 0 RECORDS                                     ELP032  
00114      RECORD CONTAINS 51 TO 4089 CHARACTERS                        ELP032  
00115      RECORDING MODE V.                                            ELP032  
00116  01  CODE-VALUE-EXTRACT-RECORD.                                   ELP032  
00117      COPY ELSNAP2C                                                ELP032  
00118              REPLACING SSR2-SUB-SIZE                              ELP032  
00119            BY SSRV-SUB-SIZEX                                      ELP032  
00120               SSR2-IDX                                            ELP032  
00121           BY SSRVX-IDX                                            ELP032  
00122              SSR2-VALID-RECORD-TYPE                               ELP032  
00123           BY SSRV-VALID-RECORD-TYPEX                              ELP032  
00124              SSR2-ABEND-CODE                                      ELP032  
00125           BY SSRV-ABEND-CODEX.                                    ELP032  
00126 *                                                                 ELP032  
00127  FD  DATA-ELEMENT-EXTRACT-FILE                                    ELP032  
00128      LABEL RECORDS ARE STANDARD                                   ELP032  
00129      BLOCK CONTAINS 0 RECORDS                                     ELP032  
00130      RECORD CONTAINS 51 TO 4089 CHARACTERS                        ELP032  
00131      RECORDING MODE V.                                            ELP032  
00132  01  DATA-ELEMENT-EXTRACT-RECORD.                                 ELP032  
00133      COPY ELSNAP2C                                                ELP032  
00134              REPLACING SSR2-SUB-SIZE                              ELP032  
00135            BY SSRD-SUB-SIZEX                                      ELP032  
00136               SSR2-IDX                                            ELP032  
00137           BY SSRDX-IDX                                            ELP032  
00138              SSR2-VALID-RECORD-TYPE                               ELP032  
00139           BY SSRD-VALID-RECORD-TYPEX                              ELP032  
00140              SSR2-ABEND-CODE                                      ELP032  
00141           BY SSRD-ABEND-CODEX.                                    ELP032  
00142 *THE FILE BELOW IS CREATED FORM CODE VALUES THAT ARE HARD CODED   ELP032  
00143 *INTO ELS PROGRAMS, NEW VALUES ARE ADDED TO CODES MANUAL BUT NOT  ELP032  
00144 *TO THE APPROPRIATE ELS PROGRAMS.                                 ELP032  
00145  FD  CV-PROGRAM-EXTRACT-FILE                                      ELP032  
00146      LABEL RECORDS ARE STANDARD                                   ELP032  
00147      BLOCK CONTAINS 0 RECORDS                                     ELP032  
00148      RECORD CONTAINS 51 TO 4089 CHARACTERS                        ELP032  
00149      RECORDING MODE V.                                            ELP032  
00150  01  CV-PROGRAM-EXTRACT-RECORD.                                   ELP032  
00151      COPY ELSNAP2C                                                ELP032  
00152              REPLACING SSR2-SUB-SIZE                              ELP032  
00153            BY SSRP-SUB-SIZEX                                      ELP032  
00154               SSR2-IDX                                            ELP032  
00155           BY SSRPX-IDX                                            ELP032  
00156              SSR2-VALID-RECORD-TYPE                               ELP032  
00157           BY SSRP-VALID-RECORD-TYPEX                              ELP032  
00158              SSR2-ABEND-CODE                                      ELP032  
00159           BY SSRP-ABEND-CODEX.                                    ELP032  
00160 *                                                                 ELP032  
00161 /                                                                 ELP032  
00162  WORKING-STORAGE SECTION.                                         ELP032  
00163  01  FILLER                 PIC X(17)   VALUE '*START OF ELP032*'.ELP032  
00164                                                                   ELP032  
00165  01  WS-SWITCHES.                                                 ELP032  
00166      05 WS-EOF-INDICATOR    PIC X(03)   VALUE 'YES'.              ELP032  
00167         88  MORE-RECORDS-TO-READ        VALUE 'YES'.              ELP032  
00168         88  EOF-FOUND                   VALUE 'EOF'.              ELP032  
00169      05 WS-EOF-RETURN-IND   PIC X(03)   VALUE 'YES'.              ELP032  
00170         88  MORE-RECORDS-TO-RETURN      VALUE 'YES'.              ELP032  
00171         88  END-OF-RETURN               VALUE 'EOR'.              ELP032  
00172  01  WS-VALID-RECORD-TYPES  PIC X       VALUE SPACES.             ELP032  
00173      88  VALID-RECORD-TYPE              VALUE '0' '1'             ELP032  
00174                                               '2' '3'             ELP032  
00175                                               '4' '5'.            ELP032  
00176 *                                                                 ELP032  
00177  01  WS-COUNTERS.                                                 ELP032  
00178      05  WS-NEG-RECS        PIC 9(8)  VALUE ZEROES.               ELP032  
00179                                                                   ELP032  
00180  01  PROGRAM-CONSTANTS.                                           ELP032  
00181      05  PC-ABEND-CODE      PIC X(04)   VALUE 'ELXX'.             ELP032  
00182 *                                                                 ELP032  
00183  01  FILLER                 PIC X(15)   VALUE '*END OF ELP032*'.  ELP032  
00184 /                                                                 ELP032  
00185  PROCEDURE DIVISION.                                              ELP032  
00186 *                                                                 ELP032  
00187  ABEND-REPORT-SORT-EXTRACT.                                       ELP032  
00188      PERFORM INTIALIZATION.                                       ELP032  
00189      PERFORM PROCESS-REPORT-EXTRACT.                              ELP032  
00190      PERFORM TERMINATION.                                         ELP032  
00191      GOBACK.                                                      ELP032  
00192 *                                                                 ELP032  
00193  INTIALIZATION.                                                   ELP032  
00194      OPEN INPUT SNAP-SHOT-FILE.                                   ELP032  
00195      OPEN OUTPUT ABEND-EXTRACT-FILE                               ELP032  
00196                  SUMMARY-EXTRACT-FILE                             ELP032  
00197                  CODE-VALUE-EXTRACT-FILE                          ELP032  
00198                  DATA-ELEMENT-EXTRACT-FILE                        ELP032  
00199                  CV-PROGRAM-EXTRACT-FILE.                         ELP032  
00200      MOVE  ZEROES  TO  WS-NEG-RECS.                               ELP032  
00201                                                                   ELP032  
00202      READ SNAP-SHOT-FILE                                          ELP032  
00203         AT END                                                    ELP032  
00204            SET EOF-FOUND TO TRUE.                                 ELP032  
00205 *                                                                 ELP032  
00206  PROCESS-REPORT-EXTRACT.                                          ELP032  
00207      SORT SORT-FILE                                               ELP032  
00208         ON ASCENDING KEY SSR-ABEND-CODE                           ELP032  
00209                          SSR-TERMINAL-ID                          ELP032  
00210                          SSR-ABEND-DATE                           ELP032  
00211                          SSR-ABEND-TIME                           ELP032  
00212                          SSR-RECORD-TYPE                          ELP032  
00213                          SSR-DDNAME                               ELP032  
00214                          SSR-SORT-ID                              ELP032  
00215                          SSR-SEQUENCE-NUM                         ELP032  
00216      INPUT PROCEDURE IS                                           ELP032  
00217         READ-SORT-PROCEDURE                                       ELP032  
00218      OUTPUT PROCEDURE IS                                          ELP032  
00219         WRITE-RECORD-PROCEDURE.                                   ELP032  
00220 *                                                                 ELP032  
00221  READ-SORT-PROCEDURE SECTION.                                     ELP032  
00222  DO-READ-SORT.                                                    ELP032  
00223      PERFORM READ-SNAP-SHOT-FILE                                  ELP032  
00224           UNTIL EOF-FOUND.                                        ELP032  
00225  READ-SORT-PROCEDURE-EXIT.                                        ELP032  
00226        EXIT.                                                      ELP032  
00227 *                                                                 ELP032  
00228  WRITE-RECORD-PROCEDURE SECTION.                                  ELP032  
00229  DO-WRITE-RECORD.                                                 ELP032  
00230      PERFORM RETURN-AND-WRITE                                     ELP032  
00231         UNTIL END-OF-RETURN.                                      ELP032  
00232  WRITE-RECORD-PROCEDURE-EXIT.                                     ELP032  
00233        EXIT.                                                      ELP032  
00234 *                                                                 ELP032  
00235  DUMMY-SECTION SECTION.                                           ELP032  
00236  READ-SNAP-SHOT-FILE.                                             ELP032  
00237      READ SNAP-SHOT-FILE                                          ELP032  
00238           AT END                                                  ELP032  
00239              SET EOF-FOUND TO TRUE.                               ELP032  
00240      IF MORE-RECORDS-TO-READ                                      ELP032  
00241         PERFORM SETUP-RETURN-DATA.                                ELP032  
00242 *                                                                 ELP032  
00243  SETUP-RETURN-DATA.                                               ELP032  
00244                                                                   ELP032  
00245       IF SSR2-VALID-RECORD-TYPE                                   ELP032  
00246          CONTINUE                                                 ELP032  
00247       ELSE                                                        ELP032  
00248          MOVE PC-ABEND-CODE TO SSR2-ABEND-CODE.                   ELP032  
00249                                                                   ELP032  
00250       MOVE  SSR2-SUB-SIZE        TO  SSR-SUB-SIZE.                ELP032  
00251       MOVE  SNAP-SHOT-RECORD-IN  TO  SSR-SNAP-SHOT-RECORD.        ELP032  
00252                                                                   ELP032  
00253       IF  SSR-AREA-LENGTH      IS  NEGATIVE                       ELP032  
00254           COMPUTE  WS-NEG-RECS  =  WS-NEG-RECS  +  1              ELP032  
00255           MOVE  LOW-VALUES     TO  SSR-SNAP-SHOT-RECORD           ELP032  
00256       ELSE                                                        ELP032  
00257           RELEASE SSR-SNAP-SHOT-RECORD.                           ELP032  
00258                                                                   ELP032  
00259 *                                                                 ELP032  
00260  RETURN-AND-WRITE.                                                ELP032  
00261      RETURN SORT-FILE RECORD                                      ELP032  
00262         AT END                                                    ELP032  
00263             SET END-OF-RETURN TO TRUE.                            ELP032  
00264                                                                   ELP032  
00265      IF (SSR-C-M-VALUE OR SSR-C-M-ELEMENT)                        ELP032  
00266         AND MORE-RECORDS-TO-RETURN                                ELP032  
00267         PERFORM WRITE-CODES-MAN-EXTRACT                           ELP032  
00268      ELSE                                                         ELP032  
00269      IF SSR-C-S-PPM                                               ELP032  
00270         AND MORE-RECORDS-TO-RETURN                                ELP032  
00271         MOVE SSR-SUB-SIZE TO SSRP-SUB-SIZEX                       ELP032  
00272         MOVE SSR-SNAP-SHOT-RECORD TO CV-PROGRAM-EXTRACT-RECORD    ELP032  
00273         WRITE CV-PROGRAM-EXTRACT-RECORD                           ELP032  
00274      ELSE                                                         ELP032  
00275      IF MORE-RECORDS-TO-RETURN                                    ELP032  
00276         MOVE SSR-SUB-SIZE TO SSR2-SUB-SIZEX                       ELP032  
00277         MOVE SSR-SNAP-SHOT-RECORD TO ABEND-EXTRACT-RECORD         ELP032  
00278         PERFORM WRITE-ABEND-EXTRACT-FILE.                         ELP032  
00279 *                                                                 ELP032  
00280  WRITE-CODES-MAN-EXTRACT.                                         ELP032  
00281      IF SSR-C-M-VALUE                                             ELP032  
00282         MOVE SSR-SUB-SIZE TO SSRV-SUB-SIZEX                       ELP032  
00283         MOVE SSR-SNAP-SHOT-RECORD TO CODE-VALUE-EXTRACT-RECORD    ELP032  
00284         WRITE CODE-VALUE-EXTRACT-RECORD                           ELP032  
00285      ELSE                                                         ELP032  
00286         IF SSR-C-M-ELEMENT                                        ELP032  
00287            MOVE SSR-SUB-SIZE TO SSRD-SUB-SIZEX                    ELP032  
00288            MOVE SSR-SNAP-SHOT-RECORD TO                           ELP032  
00289                           DATA-ELEMENT-EXTRACT-RECORD             ELP032  
00290            WRITE DATA-ELEMENT-EXTRACT-RECORD.                     ELP032  
00291 *                                                                 ELP032  
00292  WRITE-ABEND-EXTRACT-FILE.                                        ELP032  
00293      WRITE ABEND-EXTRACT-RECORD.                                  ELP032  
00294      IF SSR-DUMP-HEADER                                           ELP032  
00295          PERFORM WRITE-SUMMARY-EXTRACT-FILE.                      ELP032  
00296 *                                                                 ELP032  
00297  WRITE-SUMMARY-EXTRACT-FILE.                                      ELP032  
00298      MOVE SSR-SUB-SIZE TO SSRS-SUB-SIZEX.                         ELP032  
00299      MOVE SSR-SNAP-SHOT-RECORD TO SUMMARY-EXTRACT-RECORD.         ELP032  
00300      WRITE SUMMARY-EXTRACT-RECORD.                                ELP032  
00301 *                                                                 ELP032  
00302  TERMINATION.                                                     ELP032  
00303                                                                   ELP032  
00304      DISPLAY  '   '.                                              ELP032  
00305      DISPLAY  'NEGATIVE LENGTH RECORDS  --> '  WS-NEG-RECS.       ELP032  
00306      DISPLAY  '   '.                                              ELP032  
00307                                                                   ELP032  
00308      CLOSE SNAP-SHOT-FILE                                         ELP032  
00309            ABEND-EXTRACT-FILE                                     ELP032  
00310            SUMMARY-EXTRACT-FILE                                   ELP032  
00311            DATA-ELEMENT-EXTRACT-FILE                              ELP032  
00312            CODE-VALUE-EXTRACT-FILE                                ELP032  
00313            CV-PROGRAM-EXTRACT-FILE.                               ELP032  
