00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID.    ELGPPF.                                           ELGPPF  
00003  AUTHOR.        LUCY TORRES.                                         LV002
00004  DATE-WRITTEN.  04/11/86.                                         ELGPPF  
00005  DATE-COMPILED.                                                   ELGPPF  
00006 ******************************************************************ELGPPF  
00007 *     THIS PROGRAM IS A SUBROUTINE THAT WILL SEND TO TRANSLATE   *ELGPPF  
00008 * AND SEND TO OUTPUT THE PAYMENT FACTOR IND, PAYMENT FACTOR      *ELGPPF  
00009 * VALUE, AND THE PAYMENT FACTOR LIMIT FOR THE #PPF TABULAR.      *ELGPPF  
00010 ******************************************************************ELGPPF  
00011 *                                                                *ELGPPF  
00012 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGPPF  
00013 *       *-*         U P D A T E   H I S T O R Y         *-*      *ELGPPF  
00014 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGPPF  
00015 *                                                                *ELGPPF  
00016 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELGPPF  
00017 *                                                                *ELGPPF  
00018 *            04/11/86  LET  ORIGINAL VERSION                     *ELGPPF  
00019 *            04/26/86  LET  CHANGED FORMAT OF OUTPUT             *ELGPPF  
00020 *            04/29/86  LET  CHANGED FORMAT OF OUTPUT AGAIN       *ELGPPF  
00021 * VSCOBOLII  10/03/86  AMJ  1. CONVERT TO VS COBOL II.           *ELGPPF  
00022 *  ELS 2.0                  2. CONVERT TO USE NEW CONTROL BLOCKS.*ELGPPF  
00023 *  ELS 3.0   01/14/90  JPB  MADE STORAGE MANAGEMENT CHANGES.     *ELGPPF  
00024 *            08/12/03  AKK  REGEN'D TO CHECK ORDER OF THE COMPILE*ELGPPF  
00025 ******************************************************************ELGPPF  
00026 /                                                                 ELGPPF  
00027  ENVIRONMENT DIVISION.                                            ELGPPF  
00028  SKIP3                                                            ELGPPF  
00029  DATA DIVISION.                                                   ELGPPF  
00030  WORKING-STORAGE SECTION.                                         ELGPPF  
00031                                                                   ELGPPF  
00032  01  WS-BEGIN                    PIC  X(24) VALUE                 ELGPPF  
00033          '** ELGPPF  WS BEGINS **'.                               ELGPPF  
00034  01  WS-PARA-ID                  PIC  X(04) VALUE 'XXXX'.         ELGPPF  
00035                                                                   ELGPPF  
00036 ****************************************************************  ELGPPF  
00037 *                      LITERALS                                *  ELGPPF  
00038 ****************************************************************  ELGPPF  
00039  01  WS-BLANK-LINE               PIC  X(79) VALUE SPACES.         ELGPPF  
00040                                                                   ELGPPF  
00041  01  WS-DAYS-LITERAL             PIC  X(07) VALUE ' DAYS  '.      ELGPPF  
00042  01  WS-VISITS-LITERAL           PIC  X(07) VALUE ' VISITS'.      ELGPPF  
00043  01  WS-FIRST-LITERAL            PIC  X(06) VALUE 'FIRST '.       ELGPPF  
00044  01  WS-NEXT-LITERAL             PIC  X(06) VALUE 'NEXT  '.       ELGPPF  
00045 ****************************************************************  ELGPPF  
00046 *                     WORK  AREA                               *  ELGPPF  
00047 ****************************************************************  ELGPPF  
00048  01  WS-TEXT-LINE-1.                                              ELGPPF  
00049      05  FILLER                  PIC  X(28) VALUE                 ELGPPF  
00050              'IN HOSPITAL MEDICAL PAYMENT:'.                      ELGPPF  
00051      05  WS-TXT-1-SP             PIC  X(51) VALUE SPACES.         ELGPPF  
00052                                                                   ELGPPF  
00053  01  WS-TEXT-LINE-OTH.                                            ELGPPF  
00054      05  FILLER                  PIC  X(28) VALUE                 ELGPPF  
00055              '                       THEN '.                      ELGPPF  
00056      05  WS-TXT-OTH-SP           PIC  X(51) VALUE SPACES.         ELGPPF  
00057                                                                   ELGPPF  
00058  01  WS-DOLLAR-AMT               PIC  ZZZ.99.                     ELGPPF  
00059  01  WS-LIMIT-AMT.                                                ELGPPF  
00060      05  WS-LIMIT-AMT-X          PIC  X(09).                      ELGPPF  
00061      05  WS-LIMIT-AMT-N  REDEFINES  WS-LIMIT-AMT-X                ELGPPF  
00062                                  PIC  ZZZZZZZZ9.                  ELGPPF  
00063                                                                   ELGPPF  
00064  01  WS-DAYS-DESC                PIC  X(06) VALUE SPACES.         ELGPPF  
00065  01  WS-DAY-VISITS               PIC  X(07) VALUE SPACES.         ELGPPF  
00066  01  WS-CUR-PAY-IND              PIC  X(02) VALUE SPACES.         ELGPPF  
00067  01  WS-PREV-PAY-IND             PIC  X(02) VALUE SPACES.         ELGPPF  
00068                                                                   ELGPPF  
00069  01  WS-UNPACK-PPF-VALUE         PIC S9(03)V99 VALUE ZEROS.       ELGPPF  
00070  01  WS-UNPACK-PPF-LIMIT         PIC S9(07)    VALUE ZEROS.       ELGPPF  
00071                                                                   ELGPPF  
00072  01  WS-END                      PIC  X(16) VALUE                 ELGPPF  
00073          '*** W/S ENDS ***'.                                      ELGPPF  
00074 /                                                                 ELGPPF  
00075  LINKAGE SECTION.                                                 ELGPPF  
00076  01  DFHCOMMAREA.                                                 ELGPPF  
00077      COPY ELSCOMMC.                                               ELGPPF  
00078 /                                                                 ELGPPF  
00079      COPY ELSCIA2C.                                               ELGPPF  
00080 /                                                                 ELGPPF  
00081      COPY ELSCMDSC.                                               ELGPPF  
00082 /                                                                 ELGPPF  
00083      COPY ELSCMIFC.                                               ELGPPF  
00084 /                                                                 ELGPPF  
00085      COPY ELSIOPMC.                                               ELGPPF  
00086 /                                                                 ELGPPF  
00087      COPY ELSKEYSC.                                               ELGPPF  
00088 /                                                                 ELGPPF  
00089      COPY ELSOUTPC.                                               ELGPPF  
00090 /                                                                 ELGPPF  
00091      COPY ELSSRTPC.                                               ELGPPF  
00092 /                                                                 ELGPPF  
00093      COPY ELSTCWAC.                                               ELGPPF  
00094 /                                                                 ELGPPF  
00095      COPY ELSSSCBC.                                               ELGPPF  
00096 /                                                                 ELGPPF  
00097  01  PPF-TABULAR-RECORD.                                          ELGPPF  
00098      COPY GCTPPFC.                                                ELGPPF  
00099  PROCEDURE DIVISION.                                              ELGPPF  
00100  SKIP3                                                            ELGPPF  
00101 ****************************************************************  ELGPPF  
00102 *            THIS IS THE MAINLINE FOR THE PROGRAM.             *  ELGPPF  
00103 ****************************************************************  ELGPPF  
00104  0000-MAIN-CONTROL SECTION.                                       ELGPPF  
00105  0000-MAINLINE.                                                   ELGPPF  
00106                                                                   ELGPPF  
00107      MOVE '0000'  TO  WS-PARA-ID.                                 ELGPPF  
00108                                                                   ELGPPF  
00109      IF EIBCALEN = LENGTH OF DFHCOMMAREA                          ELGPPF  
00110         SET ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA              ELGPPF  
00111            TO ECA-CIA-PTR                                         ELGPPF  
00112         SET CIA-ELSCMIF-DDN TO TRUE                               ELGPPF  
00113         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGPPF  
00114                               ADDRESS OF                          ELGPPF  
00115               CMF-CODES-MANUAL-INTERFACE                          ELGPPF  
00116         SET CIA-ELSCMDSC-DDN TO TRUE                              ELGPPF  
00117         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGPPF  
00118                               ADDRESS OF CMF-DESCR                ELGPPF  
00119         SET CIA-ELSOUTP-DDN TO TRUE                               ELGPPF  
00120         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGPPF  
00121                               ADDRESS OF COF-OUTPUT-INTERFACE     ELGPPF  
00122         SET CIA-ELSSSCB-DDN TO TRUE                               ELGPPF  
00123         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGPPF  
00124                               ADDRESS OF                          ELGPPF  
00125               SSB-SELECTOR-STATUS-CTL-BLK                         ELGPPF  
00126         SET CIA-ELSKEYS-DDN TO TRUE                               ELGPPF  
00127         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGPPF  
00128                               ADDRESS OF KWA-FILE-KEY-WORK-AREA   ELGPPF  
00129         SET CIA-ELSTCWA-DDN TO TRUE                               ELGPPF  
00130         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGPPF  
00131                               ADDRESS OF                          ELGPPF  
00132              TCAR-COMPRESSION-WORK-AREA                           ELGPPF  
00133         SET CIA-GCTABULR-DDN TO TRUE                              ELGPPF  
00134         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGPPF  
00135                               ADDRESS OF                          ELGPPF  
00136              IOP-INPUT-OUTPUT-PARAMETERS                          ELGPPF  
00137         SET ADDRESS OF PPF-TABULAR-RECORD                         ELGPPF  
00138            TO IOP-REC-PTR                                         ELGPPF  
00139      ELSE                                                         ELGPPF  
00140         IF EIBCALEN > 4 AND ECA-CIA-PTR NOT = NULL                ELGPPF  
00141            SET ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA           ELGPPF  
00142               TO ECA-CIA-PTR                                      ELGPPF  
00143            SET CIA-AB-DFHCOMMAREA TO TRUE                         ELGPPF  
00144            PERFORM 9999-ABEND                                     ELGPPF  
00145         ELSE                                                      ELGPPF  
00146            EXEC CICS ABEND ABCODE ('EL01') END-EXEC.              ELGPPF  
00147                                                                   ELGPPF  
00148      PERFORM 2000-PPF-TABULAR                                     ELGPPF  
00149         THRU 2000-EXIT VARYING GBB-INDEX FROM 1 BY 1              ELGPPF  
00150                        UNTIL GBB-INDEX = GBB-ENTRY-COUNT.         ELGPPF  
00151                                                                   ELGPPF  
00152      EXEC CICS RETURN END-EXEC.                                   ELGPPF  
00153                                                                   ELGPPF  
00154 /                                                                 ELGPPF  
00155  2000-PPF-TABULAR.                                                ELGPPF  
00156                                                                   ELGPPF  
00157      MOVE '2000'  TO  WS-PARA-ID.                                 ELGPPF  
00158 ****************************************************************  ELGPPF  
00159 *        SETUP AND PRINT OCCURANCES OF THE PPF TABULAR         *  ELGPPF  
00160 ****************************************************************  ELGPPF  
00161                                                                   ELGPPF  
00162      MOVE GBB-PAYMT-FACTOR-IND (GBB-INDEX)  TO  WS-CUR-PAY-IND.   ELGPPF  
00163                                                                   ELGPPF  
00164      IF GBB-PAYMT-FACTOR-IND (GBB-INDEX)  =  '01'                 ELGPPF  
00165          MOVE WS-DAYS-LITERAL    TO  WS-DAY-VISITS                ELGPPF  
00166      ELSE                                                         ELGPPF  
00167          MOVE WS-VISITS-LITERAL  TO  WS-DAY-VISITS.               ELGPPF  
00168                                                                   ELGPPF  
00169      IF WS-CUR-PAY-IND   NOT  =  WS-PREV-PAY-IND                  ELGPPF  
00170          MOVE WS-FIRST-LITERAL  TO  WS-DAYS-DESC                  ELGPPF  
00171      ELSE                                                         ELGPPF  
00172          MOVE WS-NEXT-LITERAL  TO  WS-DAYS-DESC.                  ELGPPF  
00173                                                                   ELGPPF  
00174      PERFORM 9100-VALUE-N-LIMIT                                   ELGPPF  
00175         THRU 9100-EXIT.                                           ELGPPF  
00176                                                                   ELGPPF  
00177      MOVE SPACES TO TCAR-FROM-AREA.                               ELGPPF  
00178      STRING WS-DOLLAR-AMT '  DOLLARS FOR  ' WS-DAYS-DESC ' '      ELGPPF  
00179             WS-LIMIT-AMT ' ' WS-DAY-VISITS   DELIMITED BY SIZE    ELGPPF  
00180             INTO  TCAR-FROM-AREA.                                 ELGPPF  
00181                                                                   ELGPPF  
00182      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELGPPF  
00183                                                                   ELGPPF  
00184      MOVE +1   TO  TCAR-OUTPUT-FIELD-COUNT.                       ELGPPF  
00185      MOVE +51  TO  TCAR-OUTPUT-FIELD-1-LEN.                       ELGPPF  
00186      PERFORM TCPR-000-TEXT-UNSTRING.                              ELGPPF  
00187                                                                   ELGPPF  
00188      IF WS-CUR-PAY-IND  NOT  =  WS-PREV-PAY-IND                   ELGPPF  
00189          MOVE WS-CUR-PAY-IND     TO  WS-PREV-PAY-IND              ELGPPF  
00190          MOVE WS-BLANK-LINE      TO  COF-DTL-LINE (1)             ELGPPF  
00191          MOVE TCAR-OPF-DATA (1)  TO  WS-TXT-1-SP                  ELGPPF  
00192          MOVE WS-TEXT-LINE-1     TO  COF-DTL-LINE (2)             ELGPPF  
00193          MOVE +2                 TO  COF-NBR-DTL-LINES            ELGPPF  
00194      ELSE                                                         ELGPPF  
00195          MOVE TCAR-OPF-DATA (1)  TO  WS-TXT-OTH-SP                ELGPPF  
00196          MOVE WS-TEXT-LINE-OTH   TO  COF-DTL-LINE (1)             ELGPPF  
00197          MOVE +1                 TO  COF-NBR-DTL-LINES.           ELGPPF  
00198                                                                   ELGPPF  
00199      PERFORM 9200-OUTPUT-TEXT-RTN                                 ELGPPF  
00200         THRU 9200-EXIT.                                           ELGPPF  
00201                                                                   ELGPPF  
00202      MOVE WS-CUR-PAY-IND  TO  WS-PREV-PAY-IND.                    ELGPPF  
00203                                                                   ELGPPF  
00204  2000-EXIT.  EXIT.                                                ELGPPF  
00205 /                                                                 ELGPPF  
00206  9100-VALUE-N-LIMIT.                                              ELGPPF  
00207 ****************************************************************  ELGPPF  
00208 *          SETUP THE PAYMENT FACTOR VALUE AND LIMITS.          *  ELGPPF  
00209 ****************************************************************  ELGPPF  
00210                                                                   ELGPPF  
00211      MOVE GBB-PAYMT-FACTOR-VALUE (GBB-INDEX)                      ELGPPF  
00212                                TO  WS-UNPACK-PPF-VALUE.           ELGPPF  
00213      MOVE WS-UNPACK-PPF-VALUE  TO  WS-DOLLAR-AMT.                 ELGPPF  
00214      MOVE GBB-PAYMT-FACTOR-LIMIT (GBB-INDEX)                      ELGPPF  
00215                                TO  WS-UNPACK-PPF-LIMIT.           ELGPPF  
00216                                                                   ELGPPF  
00217      IF WS-UNPACK-PPF-LIMIT  =  9999999                           ELGPPF  
00218          MOVE 'REMAINING'          TO  WS-LIMIT-AMT-X             ELGPPF  
00219          MOVE  SPACES              TO  WS-DAYS-DESC               ELGPPF  
00220      ELSE                                                         ELGPPF  
00221          MOVE WS-UNPACK-PPF-LIMIT  TO  WS-LIMIT-AMT-N.            ELGPPF  
00222                                                                   ELGPPF  
00223  9100-EXIT.  EXIT.                                                ELGPPF  
00224                                                                   ELGPPF  
00225  9200-OUTPUT-TEXT-RTN.                                            ELGPPF  
00226 ****************************************************************  ELGPPF  
00227 *         PRINT THE TEXT LINES FOR PPF TABULAR.                *  ELGPPF  
00228 ****************************************************************  ELGPPF  
00229                                                                   ELGPPF  
00230      MOVE +0     TO COF-NBR-HDR-LINES.                            ELGPPF  
00231      MOVE SPACES TO COF-FUNCTION.                                 ELGPPF  
00232                                                                   ELGPPF  
00233      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELGPPF  
00234                     COMMAREA (DFHCOMMAREA)                        ELGPPF  
00235                     LENGTH (LENGTH OF DFHCOMMAREA)                ELGPPF  
00236                     END-EXEC.                                     ELGPPF  
00237                                                                   ELGPPF  
00238  9200-EXIT.  EXIT.                                                ELGPPF  
00239                                                                   ELGPPF  
00240      COPY ELSTCOMP.                                               ELGPPF  
00241                                                                   ELGPPF  
00242  9999-ABEND SECTION.                                              ELGPPF  
00243                                                                   ELGPPF  
00244      EXEC CICS ABEND                                              ELGPPF  
00245           ABCODE (CIA-ABCODE)                                     ELGPPF  
00246           END-EXEC.                                               ELGPPF  
00247                                                                   ELGPPF  
00248  9999-DUMMY-EXIT.                                                 ELGPPF  
00249      GOBACK.                                                      ELGPPF  
