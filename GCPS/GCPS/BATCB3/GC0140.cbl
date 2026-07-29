00001  IDENTIFICATION DIVISION.                                         12/09/02
00002  PROGRAM-ID.         GC0140.                                      GC0140  
00003  AUTHOR.             DELORES FRY.                                    LV002
00004  INSTALLATION.       HCSC.                                        GC0140  
00005  DATE-WRITTEN.       JANUARY 1988.                                GC0140  
00006  DATE-COMPILED.                                                   GC0140  
00007 ******************************************************************GC0140  
00008 *                                                                *GC0140  
00009 *             BENEFIT PROVISION RECORD                           *GC0140  
00010 *             MATCH AND SLOT UPDATE PROGRAM                      *GC0140  
00011 *                                                                *GC0140  
00012 ******************************************************************GC0140  
00013 ******************************************************************GC0140  
00014 ******************************************************************GC0140  
00015 *                                                                *GC0140  
00016 *       ***-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0140  
00017 *       *-*         U P D A T E   H I S T O R Y         *-*      *GC0140  
00018 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0140  
00019 *                                                                *GC0140  
00020 *                                                                *GC0140  
00021 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION--------------- *GC0140  
00022 *                                                                *GC0140  
00023 *    D1009    1/11/88  FRY    CREATED THIS PROGRAM......         *GC0140  
00024 *                                                                *GC0140  
00025 * PXXXX   01/09/91  ENW  PROGRAM ABENDED WITH S0C1 DUE TO        *GC0140  
00026 *                        GARBAGE MOVED INTO WS FROM LAST SLOT    *GC0140  
00027 *                        RECORD. REVISED TO MOVE ONLY KEY FROM   *GC0140  
00028 *                        INPUT AREA TO WS.                       *GC0140  
00029 *                                                                *GC0140  
00030 *  1293   2/02/93  JGR  REPLACE CURRENT PROCESSING WITH MATCHES  *GC0140  
00031 *                       AGAINST EXISTING TABULARS USING THE      *GC0140  
00032 *                       TABULAR HASHING PROGRAM.                 *GC0140  
00033 *                                                                *GC0140  
00034 *         1/17/95  EMS  CONVERTED TO COBOL II.                   *GC0140  
00035 *                                                                *GC0140  
00036 * 14726/     11/11/97  GSP  MODIFIED TO BECOME MILLENNIUM        *GC0140  
00037 * 15057                     COMPLIANT. CHANGED FILLER FROM       *GC0140  
00038 *                           64 TO 100 IN THE BEN-PROV RECORDS.   *GC0140  
00039 *                                                                *GC0140  
00040 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC0140  
00041 *                                                                *GC0140  
00042 ******************************************************************GC0140  
00043 ******************************************************************GC0140  
00044  ENVIRONMENT DIVISION.                                            GC0140  
00045                                                                   GC0140  
00046  CONFIGURATION SECTION.                                           GC0140  
00047  SOURCE-COMPUTER.  IBM-370.                                       GC0140  
00048  OBJECT-COMPUTER.  IBM-370.                                       GC0140  
00049                                                                   GC0140  
00050  INPUT-OUTPUT SECTION.                                            GC0140  
00051                                                                   GC0140  
00052  FILE-CONTROL.                                                    GC0140  
00053      SELECT   INPUT-WRK-FILE     ASSIGN  TO   UT-S-GC0140A.       GC0140  
00054      SELECT   OUTPUT-WRK-FILE    ASSIGN  TO   UT-S-GC0140B.       GC0140  
00055                                                                   GC0140  
00056                                                                   GC0140  
00057  DATA DIVISION.                                                   GC0140  
00058  FILE SECTION.                                                    GC0140  
00059                                                                   GC0140  
00060  FD  INPUT-WRK-FILE                                               GC0140  
00061      LABEL RECORDS ARE STANDARD                                   GC0140  
00062      RECORDING MODE IS V                                          GC0140  
00063      BLOCK CONTAINS  0  RECORDS.                                  GC0140  
00064                                                                   GC0140  
00065  01  INPUT-WRK-RECORD                      PIC X(459).            GC0140  
00066 /                                                                 GC0140  
00067  01  INPUT-BEN-PROV-RECORD.                                       GC0140  
00068 *    05  FILLER                            PIC X(100).            GC0140  
00069          COPY GCWRKDCC.                                           GC0140  
00070 /                                                                 GC0140  
00071      COPY GCBENPV3.                                               GC0140  
00072 /                                                                 GC0140  
00073  FD  OUTPUT-WRK-FILE                                              GC0140  
00074      LABEL RECORDS ARE STANDARD                                   GC0140  
00075      RECORDING MODE IS V                                          GC0140  
00076      BLOCK CONTAINS  0  RECORDS.                                  GC0140  
00077                                                                   GC0140  
00078  01  OUTPUT-WRK-RECORD                PIC X(459).                 GC0140  
00079 /                                                                 GC0140  
00080  01  OUTPUT-BEN-PROV-RECORD.                                      GC0140  
00081      05 FILLER                        PIC X(100).                 GC0140  
00082      COPY  GCBENPVC.                                              GC0140  
00083 /                                                                 GC0140  
00084                                                                   GC0140  
00085  WORKING-STORAGE SECTION.                                         GC0140  
00086                                                                   GC0140  
00087  01  WS-PROGRAM-ID                 PIC  X(27)  VALUE              GC0140  
00088                                   '* GC0140 WORKING STORAGE *'.   GC0140  
00089                                                                   GC0140  
00090  01  WORK-AREAS.                                                  GC0140  
00091      05  HASH-PROGRAM              PIC X(17) VALUE 'GHS2BAT'.     GC0140  
00092                                                                   GC0140  
00093 **** GHS2BAT LINKAGE AREA                                         GC0140  
00094                                                                   GC0140  
00095  01  WS-GHS2BAT-CALL-AREA.                                        GC0140  
00096    03  WS-GHS2BAT-PROCESS-IND      PIC X.                         GC0140  
00097        88  CALL-FOR-OPEN                       VALUE 'O'.         GC0140  
00098        88  CALL-FOR-CLOSE                      VALUE 'C'.         GC0140  
00099        88  CALL-FOR-PROCESS                    VALUE 'P'.         GC0140  
00100                                                                   GC0140  
00101  01  ACCUM-SLOT-AREA.                                             GC0140  
00102      05  HASH-RETURN-CODE          PIC X(02).                     GC0140  
00103      05  ASUR-REC-AREA.                                           GC0140  
00104          10  ASUR-TAB-ID           PIC X(0006).                   GC0140  
00105          10  ASUR-SLOT             PIC S9(7)  COMP-3.             GC0140  
00106          10  FILLER                PIC X(385).                    GC0140  
00107                                                                   GC0140  
00108 ****                                                              GC0140  
00109                                                                   GC0140  
00110  01  WS-BEN-PROV-RECORD.                                          GC0140  
00111      05  FILLER                            PIC X(100).            GC0140  
00112 /                                                                 GC0140  
00113      COPY GCBENPV2.                                               GC0140  
00114 /                                                                 GC0140  
00115                                                                   GC0140  
00116  01  WS-SWITCHES.                                                 GC0140  
00117      05  FILLER                    PIC  X(16)  VALUE              GC0140  
00118                                    '*** SWITCHES ***'.            GC0140  
00119      05  WS-END-OF-FILE-SW         PIC  X(01)  VALUE '0'.         GC0140  
00120          88  WS-END-OF-FILE-ON                 VALUE '1'.         GC0140  
00121                                                                   GC0140  
00122  01  WS-WORK-AREA.                                                GC0140  
00123      05  FILLER                    PIC  X(17)  VALUE              GC0140  
00124                                    '*** WORK AREA ***'.           GC0140  
00125      05  WS-HOLD-LAST-SLOT         PIC S9(07)  VALUE +0    COMP-3.GC0140  
00126 /                                                                 GC0140  
00127                                                                   GC0140  
00128  PROCEDURE DIVISION.                                              GC0140  
00129                                                                   GC0140  
00130 ******************************************************************GC0140  
00131 **                                                                GC0140  
00132 **               P R O C E S S    C O N T R O L                   GC0140  
00133 **                                                                GC0140  
00134 ******************************************************************GC0140  
00135  0000-MAINLINE.                                                   GC0140  
00136                                                                   GC0140  
00137      PERFORM 1000-OPEN-THE-FILES  THRU  1000-EXIT.                GC0140  
00138                                                                   GC0140  
00139      IF WS-END-OF-FILE-ON                                         GC0140  
00140          DISPLAY 'GC0140--NO INPUT RECORDS RECEIVED'              GC0140  
00141      ELSE                                                         GC0140  
00142         PERFORM 3000-PROCESS-BEN-PROVISIONS  THRU 3000-EXIT       GC0140  
00143             UNTIL  WS-END-OF-FILE-ON.                             GC0140  
00144                                                                   GC0140  
00145      PERFORM 9000-CLOSE-THE-FILES  THRU  9000-EXIT.               GC0140  
00146                                                                   GC0140  
00147      STOP RUN.                                                    GC0140  
00148                                                                   GC0140  
00149  0000-EXIT.                                                       GC0140  
00150      EXIT.                                                        GC0140  
00151 /                                                                 GC0140  
00152 ******************************************************************GC0140  
00153 ***                                                               GC0140  
00154 **       OPEN SEQUENTIAL FILES AND READ THE FIRST RECORD          GC0140  
00155 ***                                                               GC0140  
00156 ******************************************************************GC0140  
00157  1000-OPEN-THE-FILES.                                             GC0140  
00158                                                                   GC0140  
00159      OPEN INPUT    INPUT-WRK-FILE,                                GC0140  
00160           OUTPUT   OUTPUT-WRK-FILE.                               GC0140  
00161                                                                   GC0140  
00162      MOVE 'O'    TO WS-GHS2BAT-PROCESS-IND.                       GC0140  
00163      CALL HASH-PROGRAM  USING WS-GHS2BAT-CALL-AREA.               GC0140  
00164                                                                   GC0140  
00165 **--- READ THE FIRST RECORD.                                      GC0140  
00166 **                                                                GC0140  
00167      PERFORM 2000-READ-INPUT-WRK-FILE THRU 2000-EXIT.             GC0140  
00168                                                                   GC0140  
00169  1000-EXIT.                                                       GC0140  
00170      EXIT.                                                        GC0140  
00171 /                                                                 GC0140  
00172 ******************************************************************GC0140  
00173 **            SEQUENTIAL ALL LEVEL TABULAR FILE                   GC0140  
00174 ******************************************************************GC0140  
00175  2000-READ-INPUT-WRK-FILE.                                        GC0140  
00176                                                                   GC0140  
00177      READ INPUT-WRK-FILE                                          GC0140  
00178          AT END                                                   GC0140  
00179              MOVE  '1'   TO   WS-END-OF-FILE-SW.                  GC0140  
00180                                                                   GC0140  
00181  2000-EXIT.                                                       GC0140  
00182      EXIT.                                                        GC0140  
00183 /                                                                 GC0140  
00184 ******************************************************************GC0140  
00185 ****                BENEFIT PROVISION RECORD                      GC0140  
00186 ******************************************************************GC0140  
00187  3000-PROCESS-BEN-PROVISIONS.                                     GC0140  
00188                                                                   GC0140  
00189      PERFORM 4000-PROCESS-THE-RECORD  THRU 4000-EXIT.             GC0140  
00190                                                                   GC0140  
00191      PERFORM 2000-READ-INPUT-WRK-FILE THRU 2000-EXIT.             GC0140  
00192                                                                   GC0140  
00193  3000-EXIT.                                                       GC0140  
00194      EXIT.                                                        GC0140  
00195 /                                                                 GC0140  
00196 ******************************************************************GC0140  
00197 ****                BENEFIT PROVISION RECORD                      GC0140  
00198 ******************************************************************GC0140  
00199  4000-PROCESS-THE-RECORD.                                         GC0140  
00200                                                                   GC0140  
00201 *********************************************************         GC0140  
00202 ***** IF SLOT IS LESS THAN 9,000,000                *****         GC0140  
00203 ***** BYPASS THIS RECORD.                           *****         GC0140  
00204 *********************************************************         GC0140  
00205                                                                   GC0140  
00206      IF GCP3-PROVN-SLOT-NO   NOT >    +8999999                    GC0140  
00207          GO TO 4000-EXIT.                                         GC0140  
00208                                                                   GC0140  
00209 *********************************************************         GC0140  
00210 ***** CALL TABULAR HASHING PROGRAM                  *****         GC0140  
00211 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         GC0140  
00212 *********************************************************         GC0140  
00213                                                                   GC0140  
00214                                                                   GC0140  
00215      MOVE LOW-VALUES TO ASUR-REC-AREA.                            GC0140  
00216      MOVE GCP3-BEN-PROVN-RECORD TO ASUR-REC-AREA.                 GC0140  
00217      MOVE 'P' TO WS-GHS2BAT-PROCESS-IND.                          GC0140  
00218      CALL HASH-PROGRAM USING WS-GHS2BAT-CALL-AREA                 GC0140  
00219                              ACCUM-SLOT-AREA.                     GC0140  
00220                                                                   GC0140  
00221 *********************************************************         GC0140  
00222 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         GC0140  
00223 *********************************************************         GC0140  
00224                                                                   GC0140  
00225      IF HASH-RETURN-CODE = '00'                                   GC0140  
00226         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  GC0140  
00227      ELSE                                                         GC0140  
00228         IF HASH-RETURN-CODE = '01'                                GC0140  
00229            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  GC0140  
00230         ELSE                                                      GC0140  
00231            GO TO 4000-EXIT.                                       GC0140  
00232                                                                   GC0140  
00233                                                                   GC0140  
00234 *********************************************************         GC0140  
00235 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         GC0140  
00236 ***** THE SEQUENTIAL FILE.                          *****         GC0140  
00237 *********************************************************         GC0140  
00238                                                                   GC0140  
00239      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         GC0140  
00240      MOVE GCP3-COUNT-TAB-PROVN-POINTERS TO                        GC0140  
00241                                      GCP-COUNT-TAB-PROVN-POINTERS.GC0140  
00242      MOVE INPUT-BEN-PROV-RECORD     TO OUTPUT-WRK-RECORD.         GC0140  
00243      MOVE ASUR-SLOT                 TO GCP-PROVN-SLOT-NO.         GC0140  
00244      WRITE  OUTPUT-BEN-PROV-RECORD.                               GC0140  
00245                                                                   GC0140  
00246  4000-EXIT.                                                       GC0140  
00247      EXIT.                                                        GC0140  
00248 /                                                                 GC0140  
00249 ******************************************************************GC0140  
00250 **                                                                GC0140  
00251 **                  CLOSE SEQUENTIAL FILES                        GC0140  
00252 **                                                                GC0140  
00253 ******************************************************************GC0140  
00254  9000-CLOSE-THE-FILES.                                            GC0140  
00255                                                                   GC0140  
00256      CLOSE INPUT-WRK-FILE,                                        GC0140  
00257            OUTPUT-WRK-FILE.                                       GC0140  
00258                                                                   GC0140  
00259      MOVE 'C'    TO WS-GHS2BAT-PROCESS-IND.                       GC0140  
00260      CALL HASH-PROGRAM  USING WS-GHS2BAT-CALL-AREA.               GC0140  
00261                                                                   GC0140  
00262  9000-EXIT.                                                       GC0140  
00263      EXIT.                                                        GC0140  
00264 /                                                                 GC0140  
