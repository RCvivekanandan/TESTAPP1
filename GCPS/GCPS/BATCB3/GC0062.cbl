00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID.       GC0062.                                        GC0062  
00003  AUTHOR.           DELORES FRY.                                      LV002
00004  INSTALLATION.     HCSC.                                          GC0062  
00005  DATE-WRITTEN.     DECEMBER 1987.                                 GC0062  
00006                                                                   GC0062  
00007 ******************************************************************GC0062  
00008 ******************************************************************GC0062  
00009 *                                                                *GC0062  
00010 *          GENERIC CONTRACT PROCESSING SYSTEM (GCPS)             *GC0062  
00011 *                                                                *GC0062  
00012 *                                                                *GC0062  
00013 * INPUT FILES:   1. TABULAR FILE (VSAM KEY SEQUENCED)   -TSGVSAM1*GC0062  
00014 *                2. RELEASED ALL LEVEL TABULAR FILE     -GC0062A *GC0062  
00015 *                                                                *GC0062  
00016 * OUTPUT FILE:   INTERMEDIATE ALL LEVEL TABULAR FILE  TO BE      *GC0062  
00017 *                SORTED  -SEQUENTIAL                    -GC0062B *GC0062  
00018 *                                                                *GC0062  
00019 *                                                                *GC0062  
00020 *   PROCESSING FUNCTIONS:                                        *GC0062  
00021 *   --------------------                                         *GC0062  
00022 *   1. THIS PROGRAM READS THE RELEASED ALL LEVEL TABULAR FILE    *GC0062  
00023 *      CREATED FROM THE WORK-FILE.  THE TYPES OF ALL LEVEL       *GC0062  
00024 *      TABULARS ON THIS FILE ARE:                                *GC0062  
00025 *                          #AAR, #ACON, #ACOS, #ADIP, #ADOP      *GC0062  
00026 *                                                                *GC0062  
00027 *   2. A SEQUENTIAL OUTPUT FILE IS CREATED CONTAINING ONLY       *GC0062  
00028 *      THE FOLLOWING ALL LEVEL TABULARS RECORDS:                 *GC0062  
00029 *                          #AAR, #ACON, #ACOS, #ADIP, #ADOP      *GC0062  
00030 *                                                                *GC0062  
00031 *      A RECORD IS ALSO CREATED CONTAINING THE TABULAR-ID,       *GC0062  
00032 *      THE (LAST) SLOT-NUMBER, AND LOW-VALUES IN THE BODY OF     *GC0062  
00033 *      THE RECORD FOR EACH OF THE FOLLOWING ALL LEVEL TABULARS:  *GC0062  
00034 *                          #AAR, #ACON, #ACOS, #ADIP, #ADOP      *GC0062  
00035 *                                                                *GC0062  
00036 *                                                                *GC0062  
00037 *   MODULES CALLED:                                              *GC0062  
00038 *   ---------------                                              *GC0062  
00039 *      'TSGEND'   - ABEND ROUTINE                                *GC0062  
00040 *                                                                *GC0062  
00041 ******************************************************************GC0062  
00042 ******************************************************************GC0062  
00043 *                                                                *GC0062  
00044 *       ***-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0062  
00045 *       *-*         U P D A T E   H I S T O R Y         *-*      *GC0062  
00046 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0062  
00047 *                                                                *GC0062  
00048 *                                                                *GC0062  
00049 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION--------------- *GC0062  
00050 *                                                                *GC0062  
00051 *   D1009    12/10/88  FRY   CREATED THIS PROGRAM.......         *GC0062  
00052 *                                                                *GC0062  
00053 * 11154    2/21/91  NE   -CHANGE ACCUM TABULAR RECORD LEN FROM   *GC0062  
00054 *                         4000 TO 7805.                          *GC0062  
00055 *                                                                 GC0062  
00056 *   11154     3/06/91  FRY   INCREASE RECORD AREAS IN FILE       *GC0062  
00057 *                            SECTION:                            *GC0062  
00058 *                 OUTPUT-RECORD  PIC  X(4064)  CHANGED TO  7869  *GC0062  
00059 *                                                                *GC0062  
00060 * D-356A   5/08/03  GTF  RECOMPILE FOR COPYBK CHANGES #ACON,     *GC0062  
00061 *                        #ACOS, #ADIP, #ADOP.                    *GC0062  
00062 ******************************************************************GC0062  
00063 ******************************************************************GC0062  
00064  ENVIRONMENT DIVISION.                                            GC0062  
00065                                                                   GC0062  
00066  CONFIGURATION SECTION.                                           GC0062  
00067  SOURCE-COMPUTER.  IBM-370.                                       GC0062  
00068  OBJECT-COMPUTER.  IBM-370.                                       GC0062  
00069                                                                   GC0062  
00070                                                                   GC0062  
00071  INPUT-OUTPUT SECTION.                                            GC0062  
00072                                                                   GC0062  
00073  FILE-CONTROL.                                                    GC0062  
00074      SELECT   INPUT-FILE          ASSIGN  TO   UT-S-GC0062A.      GC0062  
00075      SELECT   OUTPUT-FILE         ASSIGN  TO   UT-S-GC0062B.      GC0062  
00076                                                                   GC0062  
00077                                                                   GC0062  
00078  DATA DIVISION.                                                   GC0062  
00079                                                                   GC0062  
00080  FILE SECTION.                                                    GC0062  
00081  FD  INPUT-FILE                                                   GC0062  
00082          LABEL RECORDS ARE STANDARD                               GC0062  
00083          RECORDING MODE IS V                                      GC0062  
00084          BLOCK CONTAINS  0  RECORDS.                              GC0062  
00085                                                                   GC0062  
00086  01  INPUT-RECORD.                                                GC0062  
00087      05  FILLER                            PIC  X(64).            GC0062  
00088      05  WRK-BEN-TAB-PROV-DATA.                                   GC0062  
00089          10  WRK-TAB-PROV-ID               PIC  X(6).             GC0062  
00090          10  FILLER                        PIC  X(7799).          GC0062  
00091 /                                                                 GC0062  
00092                                                                   GC0062  
00093  FD  OUTPUT-FILE                                                  GC0062  
00094          LABEL RECORDS ARE STANDARD                               GC0062  
00095          RECORDING MODE IS V                                      GC0062  
00096          BLOCK CONTAINS  0  RECORDS.                              GC0062  
00097                                                                   GC0062  
00098  01  OUTPUT-RECORD                         PIC  X(7869).          GC0062  
00099                                                                   GC0062  
00100  01  OUTPUT-LAST-RECORD.                                          GC0062  
00101      05  FILLER                            PIC  X(64).            GC0062  
00102      05  LAST-RECORD-KEY.                                         GC0062  
00103         10  LAST-KEY-ID                    PIC  X(06).            GC0062  
00104         10  LAST-KEY-SLOT-NO               PIC S9(07)   COMP-3.   GC0062  
00105                                                                   GC0062  
00106                                                                   GC0062  
00107  01  OUTPUT-AAR-RECORD.                                           GC0062  
00108      05  FILLER                            PIC  X(64).            GC0062  
00109 *    05  GAE-RECORD.                                              GC0062  
00110      COPY  GCTAARC.                                               GC0062  
00111 /                                                                 GC0062  
00112                                                                   GC0062  
00113  01  OUTPUT-ACON-RECORD.                                          GC0062  
00114      05  FILLER                            PIC  X(64).            GC0062  
00115 *    05  GAI-RECORD.                                              GC0062  
00116      COPY  GCTACONC.                                              GC0062  
00117 /                                                                 GC0062  
00118                                                                   GC0062  
00119  01  OUTPUT-ACOS-RECORD.                                          GC0062  
00120      05  FILLER                            PIC  X(64).            GC0062  
00121 *    05  GAJ-RECORD.                                              GC0062  
00122      COPY  GCTACOSC.                                              GC0062  
00123 /                                                                 GC0062  
00124                                                                   GC0062  
00125  01  OUTPUT-ADIP-RECORD.                                          GC0062  
00126      05  FILLER                            PIC  X(64).            GC0062  
00127 *    05  GAG-RECORD.                                              GC0062  
00128      COPY  GCTADIPC.                                              GC0062  
00129 /                                                                 GC0062  
00130                                                                   GC0062  
00131                                                                   GC0062  
00132  01  OUTPUT-ADOP-RECORD.                                          GC0062  
00133      05  FILLER                            PIC  X(64).            GC0062  
00134 *    05  GAH-RECORD.                                              GC0062  
00135      COPY  GCTADOPC.                                              GC0062  
00136 /                                                                 GC0062  
00137                                                                   GC0062  
00138  WORKING-STORAGE SECTION.                                         GC0062  
00139                                                                   GC0062  
00140  77  FILLER                           PIC  X(26)    VALUE         GC0062  
00141                                      '* GC0062 WORKING STORAGE *'.GC0062  
00142                                                                   GC0062  
00143  01  WS-HOLD-AREAS.                                               GC0062  
00144      05  FILLER                       PIC  X(29)    VALUE         GC0062  
00145                                       '***  ABEND CODE  ***'.     GC0062  
00146      05  WS-ABEND-CODE                PIC  9(04)    VALUE 0  COMP.GC0062  
00147                                                                   GC0062  
00148                                                                   GC0062  
00149  01  WS-WORK-AREAS.                                               GC0062  
00150      05  FILLER                       PIC  X(28)    VALUE         GC0062  
00151                                       '***  WORK AREA  ***'.      GC0062  
00152      05  WS-LAST-SLOT       COMP-3    PIC S9(07)    VALUE +0.     GC0062  
00153      05  WS-INPUT-RELEASED-RECORDS    PIC  9(08)    VALUE ZEROES. GC0062  
00154      05  WS-INPUT-VSAM-RECORDS        PIC  9(08)    VALUE ZEROES. GC0062  
00155      05  WS-OUTPUT-RECORDS            PIC  9(08)    VALUE ZEROES. GC0062  
00156                                                                   GC0062  
00157                                                                   GC0062  
00158  01  WS-SWITCHES.                                                 GC0062  
00159      05  FILLER                       PIC  X(27)    VALUE         GC0062  
00160                                       '***  SWITCHES  ***'.       GC0062  
00161      05  WS-END-OF-INPUT-FILE-SW      PIC  X(01)    VALUE '0'.    GC0062  
00162          88  WS-END-OF-INPUT-FILE-SW-ON             VALUE '1'.    GC0062  
00163      05  WS-VSAM-READ-SWITCH          PIC  X(01)    VALUE '0'.    GC0062  
00164      05  WS-AAR-SWITCH                PIC  X(01)    VALUE '0'.    GC0062  
00165      05  WS-ACON-SWITCH               PIC  X(01)    VALUE '0'.    GC0062  
00166      05  WS-ACOS-SWITCH               PIC  X(01)    VALUE '0'.    GC0062  
00167      05  WS-ADIP-SWITCH               PIC  X(01)    VALUE '0'.    GC0062  
00168      05  WS-ADOP-SWITCH               PIC  X(01)    VALUE '0'.    GC0062  
00169 /                                                                 GC0062  
00170 ******************************************************************GC0062  
00171 **                                                                GC0062  
00172 **     PARAMETERS FOR THE TABULAR FILE        TSGVSAM1            GC0062  
00173 **                                                                GC0062  
00174 ******************************************************************GC0062  
00175  01  FILLER                            PIC  X(22)  VALUE          GC0062  
00176                                        '***  TABULAR FILE  ***'.  GC0062  
00177                                                                   GC0062  
00178  01  PARM-SET.                                                    GC0062  
00179      05  SET-VSAM-RDW.                                            GC0062  
00180         10 SET-VSAM-RECORD-LENGTH      PIC 9(04)     COMP.        GC0062  
00181         10 SET-VSAM-FEEDBACK-CODE      PIC 9(04)     COMP.        GC0062  
00182      05  SET-VSAM-VALUE                PIC 9(08)     COMP.        GC0062  
00183                                                                   GC0062  
00184                                                                   GC0062  
00185  01  PARM-TAB-ONE-A.                                              GC0062  
00186      05  RESERVED-FLDS                 PIC  9(08) VALUE 0   COMP. GC0062  
00187      05  RESERVED-ONE     REDEFINES     RESERVED-FLDS.            GC0062  
00188         10  VSAM-REQUEST-TYPE          PIC  X(01).                GC0062  
00189         10  FILLER                     PIC  X(03).                GC0062  
00190                                                                   GC0062  
00191                                                                   GC0062  
00192  01  PARM-TAB-ONE-B.                                              GC0062  
00193      05  VSAM-RDW.                                                GC0062  
00194         10  VSAM-RECORD-LENGTH         PIC  9(04)    COMP.        GC0062  
00195         10  VSAM-FEEDBACK-CODE         PIC  9(04)    COMP.        GC0062  
00196      05  VSAM-RECORD-AREA              PIC  X(7805).              GC0062  
00197      05  VSAM-RECORD-A       REDEFINES     VSAM-RECORD-AREA.      GC0062  
00198          10  VSAM-KEY-FIELD.                                      GC0062  
00199             15  VSAM-KEY-ID            PIC  X(06).                GC0062  
00200             15  VSAM-KEY-SLOT          PIC S9(07)    COMP-3.      GC0062  
00201          10  VSAM-KEY-DATA.                                       GC0062  
00202             15  FILLER                 PIC  X(27).                GC0062  
00203             15  VSAM-KEY-ENTRY-COUNT   PIC S9(05)  COMP-3.        GC0062  
00204             15  FILLER                 PIC  X(7765).              GC0062  
00205                                                                   GC0062  
00206 /                                                                 GC0062  
00207  PROCEDURE DIVISION.                                              GC0062  
00208                                                                   GC0062  
00209 ******************************************************************GC0062  
00210 **                                                                GC0062  
00211 **               P R O C E S S    C O N T R O L                   GC0062  
00212 **                                                                GC0062  
00213 ******************************************************************GC0062  
00214  0000-MAINLINE.                                                   GC0062  
00215                                                                   GC0062  
00216      PERFORM 1000-OPEN-THE-FILES  THRU  1000-EXIT.                GC0062  
00217                                                                   GC0062  
00218      PERFORM 2000-READ-AND-PROCESS-TABULARS  THRU  2000-EXIT      GC0062  
00219          UNTIL  WS-END-OF-INPUT-FILE-SW-ON.                       GC0062  
00220                                                                   GC0062  
00221      PERFORM 9000-CLOSE-THE-FILES  THRU  9000-EXIT.               GC0062  
00222                                                                   GC0062  
00223      STOP RUN.                                                    GC0062  
00224                                                                   GC0062  
00225  0000-EXIT.                                                       GC0062  
00226      EXIT.                                                        GC0062  
00227 /                                                                 GC0062  
00228 ******************************************************************GC0062  
00229 **                                                                GC0062  
00230 **                O P E N   T H E   F I L E S                     GC0062  
00231 **                                                                GC0062  
00232 ******************************************************************GC0062  
00233  1000-OPEN-THE-FILES.                                             GC0062  
00234                                                                   GC0062  
00235      OPEN INPUT  INPUT-FILE,                                      GC0062  
00236           OUTPUT OUTPUT-FILE.                                     GC0062  
00237                                                                   GC0062  
00238                                                                   GC0062  
00239      MOVE  'S'          TO  VSAM-REQUEST-TYPE.                    GC0062  
00240      MOVE   8           TO  SET-VSAM-RECORD-LENGTH.               GC0062  
00241      MOVE   3           TO  SET-VSAM-VALUE.                       GC0062  
00242      CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-SET.          GC0062  
00243                                                                   GC0062  
00244      IF  VSAM-REQUEST-TYPE   NOT =   'S'                          GC0062  
00245          DISPLAY 'BAD SET IN GC0062    1000-OPEN-THE-FILES'       GC0062  
00246          MOVE  SET-VSAM-FEEDBACK-CODE  TO  WS-ABEND-CODE          GC0062  
00247          GO TO  9999-ERROR-RTN.                                   GC0062  
00248                                                                   GC0062  
00249  1000-EXIT.                                                       GC0062  
00250      EXIT.                                                        GC0062  
00251 /                                                                 GC0062  
00252 ******************************************************************GC0062  
00253 **                                                                GC0062  
00254 **   R E A D   T H E   S E Q U E N T I A L   I N P U T   F I L E  GC0062  
00255 **                                                                GC0062  
00256 ******************************************************************GC0062  
00257  2000-READ-AND-PROCESS-TABULARS.                                  GC0062  
00258                                                                   GC0062  
00259 **--READ THE SEQUENTIAL INPUT FILE.                               GC0062  
00260 **                                                                GC0062  
00261      READ INPUT-FILE                                              GC0062  
00262          AT END                                                   GC0062  
00263              MOVE '1'  TO   WS-END-OF-INPUT-FILE-SW               GC0062  
00264              DISPLAY '  '                                         GC0062  
00265              DISPLAY ' RELEASED RECORDS READ     =   '            GC0062  
00266                                        WS-INPUT-RELEASED-RECORDS  GC0062  
00267              DISPLAY ' VSAM RECORDS READ         =   '            GC0062  
00268                                            WS-INPUT-VSAM-RECORDS  GC0062  
00269              DISPLAY ' RECORDS WRITTEN           =   '            GC0062  
00270                                                WS-OUTPUT-RECORDS  GC0062  
00271              GO TO 2000-EXIT.                                     GC0062  
00272                                                                   GC0062  
00273                                                                   GC0062  
00274      ADD  1  TO  WS-INPUT-RELEASED-RECORDS.                       GC0062  
00275                                                                   GC0062  
00276                                                                   GC0062  
00277 **--PROCESS  \
00278 **                                                                GC0062  
00279      IF WRK-TAB-PROV-ID   EQUAL   '#AAR  '                        GC0062  
00280          IF WS-AAR-SWITCH    >   '0'                              GC0062  
00281              GO TO 2000-EXIT                                      GC0062  
00282          ELSE                                                     GC0062  
00283             MOVE  '1'   TO   WS-AAR-SWITCH                        GC0062  
00284             PERFORM 3000-PROCESS-ALL-LEVL-TABULARS THRU 3000-EXIT.GC0062  
00285                                                                   GC0062  
00286                                                                   GC0062  
00287      IF WRK-TAB-PROV-ID   EQUAL   '#ACON '                        GC0062  
00288          IF WS-ACON-SWITCH   >   '0'                              GC0062  
00289              GO TO 2000-EXIT                                      GC0062  
00290          ELSE                                                     GC0062  
00291             MOVE  '1'   TO   WS-ACON-SWITCH                       GC0062  
00292             PERFORM 3000-PROCESS-ALL-LEVL-TABULARS THRU 3000-EXIT.GC0062  
00293                                                                   GC0062  
00294                                                                   GC0062  
00295      IF WRK-TAB-PROV-ID   EQUAL   '#ACOS '                        GC0062  
00296          IF WS-ACOS-SWITCH   >   '0'                              GC0062  
00297              GO TO 2000-EXIT                                      GC0062  
00298          ELSE                                                     GC0062  
00299             MOVE  '1'   TO   WS-ACOS-SWITCH                       GC0062  
00300             PERFORM 3000-PROCESS-ALL-LEVL-TABULARS THRU 3000-EXIT.GC0062  
00301                                                                   GC0062  
00302                                                                   GC0062  
00303      IF WRK-TAB-PROV-ID   EQUAL   '#ADIP '                        GC0062  
00304          IF WS-ADIP-SWITCH   >   '0'                              GC0062  
00305              GO TO 2000-EXIT                                      GC0062  
00306          ELSE                                                     GC0062  
00307             MOVE  '1'   TO   WS-ADIP-SWITCH                       GC0062  
00308             PERFORM 3000-PROCESS-ALL-LEVL-TABULARS THRU 3000-EXIT.GC0062  
00309                                                                   GC0062  
00310                                                                   GC0062  
00311      IF WRK-TAB-PROV-ID   EQUAL   '#ADOP '                        GC0062  
00312          IF WS-ADOP-SWITCH   >   '0'                              GC0062  
00313              GO TO 2000-EXIT                                      GC0062  
00314          ELSE                                                     GC0062  
00315             MOVE  '1'   TO   WS-ADOP-SWITCH                       GC0062  
00316             PERFORM 3000-PROCESS-ALL-LEVL-TABULARS THRU 3000-EXIT.GC0062  
00317                                                                   GC0062  
00318                                                                   GC0062  
00319      PERFORM 7000-CHECK-ALL-SWITCHES THRU 7000-EXIT.              GC0062  
00320                                                                   GC0062  
00321  2000-EXIT.                                                       GC0062  
00322      EXIT.                                                        GC0062  
00323 /                                                                 GC0062  
00324 ******************************************************************GC0062  
00325 **                                                                GC0062  
00326 **          PROCESS ONLY THE  \
00327 **                                                                GC0062  
00328 ******************************************************************GC0062  
00329  3000-PROCESS-ALL-LEVL-TABULARS.                                  GC0062  
00330                                                                   GC0062  
00331 **-- INITIALIZE LAST SLOT NUMBER                                  GC0062  
00332 **                                                                GC0062  
00333      MOVE  +10               TO  WS-LAST-SLOT.                    GC0062  
00334                                                                   GC0062  
00335 **-- BUILD KEY FOR TABULAR FILE                                   GC0062  
00336 **                                                                GC0062  
00337      MOVE  WRK-TAB-PROV-ID   TO  VSAM-KEY-ID.                     GC0062  
00338      MOVE  +11               TO  VSAM-KEY-SLOT.                   GC0062  
00339      MOVE  14                TO  VSAM-RECORD-LENGTH.              GC0062  
00340      MOVE  'P'               TO  VSAM-REQUEST-TYPE.               GC0062  
00341                                                                   GC0062  
00342      PERFORM 4000-LOCATE-BEGIN-TABULAR THRU 4000-EXIT.            GC0062  
00343                                                                   GC0062  
00344                                                                   GC0062  
00345 **--IF THE RECORD IS NOT ON THE FILE, CREATE A RECORD WITH        GC0062  
00346 *   THE TABULAR-ID AND ASSIGN A SLOT NUMBER OF 10.                GC0062  
00347 *                                                                 GC0062  
00348      IF  VSAM-REQUEST-TYPE     EQUAL   '2'                        GC0062  
00349          MOVE  LOW-VALUES        TO  OUTPUT-RECORD                GC0062  
00350          MOVE  WRK-TAB-PROV-ID   TO  LAST-KEY-ID                  GC0062  
00351          MOVE  WS-LAST-SLOT      TO  LAST-KEY-SLOT-NO             GC0062  
00352          WRITE OUTPUT-LAST-RECORD                                 GC0062  
00353          ADD  1  TO  WS-OUTPUT-RECORDS                            GC0062  
00354          GO TO 3000-EXIT.                                         GC0062  
00355                                                                   GC0062  
00356                                                                   GC0062  
00357                                                                   GC0062  
00358 **--INITIALIZE THE SWITCH WHEN THE TABULAR-ID CHANGES             GC0062  
00359 *                                                                 GC0062  
00360      MOVE  '0'  TO  WS-VSAM-READ-SWITCH.                          GC0062  
00361                                                                   GC0062  
00362                                                                   GC0062  
00363      PERFORM 5000-READNEXT-VSAM-RECORD THRU 5000-EXIT             GC0062  
00364          UNTIL   WS-VSAM-READ-SWITCH   >   '0'.                   GC0062  
00365                                                                   GC0062  
00366                                                                   GC0062  
00367                                                                   GC0062  
00368 **--CREATE A RECORD FOR EACH TYPE OF TABULAR, CONTAINING          GC0062  
00369 **  THE VSAM-KEY-ID , (LAST) TAB-SLOT-NUMBER, AND LOW-VALUES      GC0062  
00370 **  IN THE BODY OF THE RECORD.                                    GC0062  
00371                                                                   GC0062  
00372      MOVE  LOW-VALUES        TO  OUTPUT-RECORD.                   GC0062  
00373      MOVE  WRK-TAB-PROV-ID   TO  LAST-KEY-ID.                     GC0062  
00374      MOVE  WS-LAST-SLOT      TO  LAST-KEY-SLOT-NO.                GC0062  
00375                                                                   GC0062  
00376      WRITE OUTPUT-LAST-RECORD.                                    GC0062  
00377      ADD  1  TO  WS-OUTPUT-RECORDS.                               GC0062  
00378                                                                   GC0062  
00379  3000-EXIT.                                                       GC0062  
00380      EXIT.                                                        GC0062  
00381 /                                                                 GC0062  
00382 ******************************************************************GC0062  
00383 **                                                                GC0062  
00384 **             V S A M   T A B U L A R   F I L E                  GC0062  
00385 **                                                                GC0062  
00386 ******************************************************************GC0062  
00387  4000-LOCATE-BEGIN-TABULAR.                                       GC0062  
00388                                                                   GC0062  
00389      CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    GC0062  
00390                                                                   GC0062  
00391                                                                   GC0062  
00392      IF  VSAM-REQUEST-TYPE     EQUAL   '2'                        GC0062  
00393          GO TO 4000-EXIT.                                         GC0062  
00394                                                                   GC0062  
00395                                                                   GC0062  
00396      IF  VSAM-REQUEST-TYPE     NOT =   'P'                        GC0062  
00397          DISPLAY 'BAD POINT IN GC0062   4000-LOCATE-BEGIN-TABULAR'GC0062  
00398          MOVE  VSAM-FEEDBACK-CODE   TO   WS-ABEND-CODE            GC0062  
00399          GO TO  9999-ERROR-RTN.                                   GC0062  
00400                                                                   GC0062  
00401  4000-EXIT.                                                       GC0062  
00402      EXIT.                                                        GC0062  
00403 /                                                                 GC0062  
00404 ******************************************************************GC0062  
00405 **                                                                GC0062  
00406 **       R E A D    V S A M    T A B U L A R    F I L E           GC0062  
00407 **                                                                GC0062  
00408 ******************************************************************GC0062  
00409  5000-READNEXT-VSAM-RECORD.                                       GC0062  
00410                                                                   GC0062  
00411      MOVE  'G'          TO  VSAM-REQUEST-TYPE.                    GC0062  
00412      CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    GC0062  
00413                                                                   GC0062  
00414                                                                   GC0062  
00415 **--SET THE SWITCH IF END OF VSAM FILE                            GC0062  
00416 *                                                                 GC0062  
00417      IF  VSAM-REQUEST-TYPE   EQUAL   '2'                          GC0062  
00418          MOVE '1' TO WS-VSAM-READ-SWITCH                          GC0062  
00419          GO TO 5000-EXIT.                                         GC0062  
00420                                                                   GC0062  
00421                                                                   GC0062  
00422      IF  VSAM-REQUEST-TYPE   NOT =   'G'                          GC0062  
00423          DISPLAY 'BAD GET IN GC0062     5000-READNEXT-VSAM-RECORD'GC0062  
00424          MOVE  VSAM-FEEDBACK-CODE   TO   WS-ABEND-CODE            GC0062  
00425          GO TO  9999-ERROR-RTN.                                   GC0062  
00426                                                                   GC0062  
00427                                                                   GC0062  
00428      IF VSAM-KEY-ID    NOT EQUAL    WRK-TAB-PROV-ID               GC0062  
00429          MOVE '1'  TO  WS-VSAM-READ-SWITCH                        GC0062  
00430          GO TO 5000-EXIT.                                         GC0062  
00431                                                                   GC0062  
00432                                                                   GC0062  
00433      ADD  1               TO  WS-INPUT-VSAM-RECORDS.              GC0062  
00434      MOVE VSAM-KEY-SLOT   TO  WS-LAST-SLOT.                       GC0062  
00435      MOVE LOW-VALUES      TO  OUTPUT-RECORD.                      GC0062  
00436                                                                   GC0062  
00437      PERFORM 6000-WRITE-ALL-LEVEL-TABULARS  THRU  6000-EXIT.      GC0062  
00438                                                                   GC0062  
00439  5000-EXIT.                                                       GC0062  
00440      EXIT.                                                        GC0062  
00441 /                                                                 GC0062  
00442 ******************************************************************GC0062  
00443 **                                                                GC0062  
00444 **     CREATE A RECORD FOR EACH TYPE OF  \
00445 **                                                                GC0062  
00446 ******************************************************************GC0062  
00447  6000-WRITE-ALL-LEVEL-TABULARS.                                   GC0062  
00448                                                                   GC0062  
00449      IF VSAM-KEY-ID    EQUAL    '#AAR '                           GC0062  
00450          MOVE  VSAM-KEY-ENTRY-COUNT  TO GAE-ENTRY-COUNT           GC0062  
00451          MOVE  VSAM-RECORD-AREA      TO GAE-RECORD                GC0062  
00452          WRITE OUTPUT-AAR-RECORD                                  GC0062  
00453          ADD  1  TO  WS-OUTPUT-RECORDS                            GC0062  
00454          GO TO 6000-EXIT.                                         GC0062  
00455                                                                   GC0062  
00456                                                                   GC0062  
00457      IF VSAM-KEY-ID    EQUAL    '#ACON '                          GC0062  
00458          MOVE  VSAM-KEY-ENTRY-COUNT  TO GAI-ENTRY-COUNT           GC0062  
00459          MOVE  VSAM-RECORD-AREA      TO GAI-RECORD                GC0062  
00460          WRITE OUTPUT-ACON-RECORD                                 GC0062  
00461          ADD  1  TO  WS-OUTPUT-RECORDS                            GC0062  
00462          GO TO 6000-EXIT.                                         GC0062  
00463                                                                   GC0062  
00464                                                                   GC0062  
00465      IF VSAM-KEY-ID    EQUAL    '#ACOS '                          GC0062  
00466          MOVE  VSAM-KEY-ENTRY-COUNT  TO GAJ-ENTRY-COUNT           GC0062  
00467          MOVE  VSAM-RECORD-AREA      TO GAJ-RECORD                GC0062  
00468          WRITE OUTPUT-ACOS-RECORD                                 GC0062  
00469          ADD  1  TO  WS-OUTPUT-RECORDS                            GC0062  
00470          GO TO 6000-EXIT.                                         GC0062  
00471                                                                   GC0062  
00472                                                                   GC0062  
00473      IF VSAM-KEY-ID    EQUAL    '#ADIP '                          GC0062  
00474          MOVE  VSAM-KEY-ENTRY-COUNT  TO GAG-ENTRY-COUNT           GC0062  
00475          MOVE  VSAM-RECORD-AREA      TO GAG-RECORD                GC0062  
00476          WRITE OUTPUT-ADIP-RECORD                                 GC0062  
00477          ADD  1  TO  WS-OUTPUT-RECORDS                            GC0062  
00478          GO TO 6000-EXIT.                                         GC0062  
00479                                                                   GC0062  
00480                                                                   GC0062  
00481      IF VSAM-KEY-ID    EQUAL    '#ADOP '                          GC0062  
00482          MOVE  VSAM-KEY-ENTRY-COUNT  TO GAH-ENTRY-COUNT           GC0062  
00483          MOVE  VSAM-RECORD-AREA      TO GAH-RECORD                GC0062  
00484          WRITE OUTPUT-ADOP-RECORD                                 GC0062  
00485          ADD  1  TO  WS-OUTPUT-RECORDS.                           GC0062  
00486                                                                   GC0062  
00487  6000-EXIT.                                                       GC0062  
00488      EXIT.                                                        GC0062  
00489 /                                                                 GC0062  
00490 ******************************************************************GC0062  
00491 **                                                                GC0062  
00492 **    IF ALL OF THE FIVE TYPES OF TABULARS HAVE BEEN READ,        GC0062  
00493 **    SET THE END OF FILE SWITCH FOR THE SEQUENTIAL INPUT         GC0062  
00494 **    (RELEASED ALL LEVEL TABULAR) FILE.                          GC0062  
00495 **                                                                GC0062  
00496 ******************************************************************GC0062  
00497  7000-CHECK-ALL-SWITCHES.                                         GC0062  
00498                                                                   GC0062  
00499      IF WS-AAR-SWITCH    >   '0'                                  GC0062  
00500        AND                                                        GC0062  
00501         WS-ACON-SWITCH   >   '0'                                  GC0062  
00502        AND                                                        GC0062  
00503         WS-ACOS-SWITCH   >   '0'                                  GC0062  
00504        AND                                                        GC0062  
00505         WS-ADIP-SWITCH   >   '0'                                  GC0062  
00506        AND                                                        GC0062  
00507         WS-ADOP-SWITCH   >   '0'                                  GC0062  
00508          MOVE  '1'   TO   WS-END-OF-INPUT-FILE-SW.                GC0062  
00509                                                                   GC0062  
00510  7000-EXIT.                                                       GC0062  
00511      EXIT.                                                        GC0062  
00512 /                                                                 GC0062  
00513 ******************************************************************GC0062  
00514 **                                                                GC0062  
00515 **            C L O S E   T H E   F I L E S                       GC0062  
00516 **                                                                GC0062  
00517 ******************************************************************GC0062  
00518  9000-CLOSE-THE-FILES.                                            GC0062  
00519                                                                   GC0062  
00520      CLOSE INPUT-FILE,                                            GC0062  
00521            OUTPUT-FILE.                                           GC0062  
00522                                                                   GC0062  
00523                                                                   GC0062  
00524      MOVE     'C'           TO   VSAM-REQUEST-TYPE.               GC0062  
00525      CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    GC0062  
00526                                                                   GC0062  
00527      IF  VSAM-REQUEST-TYPE    NOT =   'C'                         GC0062  
00528          DISPLAY 'BAD CLOSE IN GC0062 AT 9000-CLOSE-THE-FILES'    GC0062  
00529          MOVE  VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE             GC0062  
00530          GO TO  9999-ERROR-RTN.                                   GC0062  
00531                                                                   GC0062  
00532  9000-EXIT.                                                       GC0062  
00533      EXIT.                                                        GC0062  
00534 /                                                                 GC0062  
00535 ******************************************************************GC0062  
00536 **                                                                GC0062  
00537 **                       A B E N D                                GC0062  
00538 **                                                                GC0062  
00539 ******************************************************************GC0062  
00540  9999-ERROR-RTN.                                                  GC0062  
00541                                                                   GC0062  
00542      CALL  'TSGEND' USING  WS-ABEND-CODE.                         GC0062  
00543                                                                   GC0062  
00544  9999-EXIT.                                                       GC0062  
00545      EXIT.                                                        GC0062  
00546 /                                                                 GC0062  
