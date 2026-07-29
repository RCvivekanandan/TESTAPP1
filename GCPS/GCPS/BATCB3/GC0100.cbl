00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID.       GC0100.                                        GC0100  
00003  AUTHOR.           DELORES FRY.                                      LV002
00004  INSTALLATION.     HCSC.                                          GC0100  
00005  DATE-WRITTEN.     DECEMBER 1987.                                 GC0100  
00006                                                                   GC0100  
00007 ******************************************************************GC0100  
00008 ******************************************************************GC0100  
00009 *                                                                *GC0100  
00010 *          GENERIC CONTRACT PROCESSING SYSTEM (GCPS)             *GC0100  
00011 *                                                                *GC0100  
00012 *                                                                *GC0100  
00013 * INPUT FILES:   1. TABULAR FILE (VSAM KEY SEQUENCED) -TSGVSAM1  *GC0100  
00014 *                2. RELEASED PROVISION TABULAR FILE   -GC0100A   *GC0100  
00015 *                                                                *GC0100  
00016 * OUTPUT FILE:   INTERMEDIATE PROVISION TABULAR FILE  TO BE      *GC0100  
00017 *                SORTED  -SEQUENTIAL                  -GC0100B   *GC0100  
00018 *                                                                *GC0100  
00019 *                                                                *GC0100  
00020 *   PROCESSING FUNCTIONS:                                        *GC0100  
00021 *   --------------------                                         *GC0100  
00022 *   1. THIS PROGRAM READS THE RELEASED PROVISION TABULAR FILE    *GC0100  
00023 *      CREATED FROM THE WORK-FILE.  THE TYPES OF BENEFIT         *GC0100  
00024 *      PROVISION TABULARS ON THIS FILE ARE:                      *GC0100  
00025 *                                  #PAQ, #PCX, #PDR, #PPF,       *GC0100  
00026 *                                  #PRR, #PRV, #PSC, #PVE        *GC0100  
00027 *                                                                *GC0100  
00028 *   2. A SEQUENTIAL OUTPUT FILE IS CREATED CONTAINING ONLY       *GC0100  
00029 *      THE FOLLOWING PROVISION TABULARS RECORDS:                 *GC0100  
00030 *                                  #PAQ, #PCX, #PDR, #PPF,       *GC0100  
00031 *                                  #PRR, #PRV, #PSC, #PVE        *GC0100  
00032 *                                                                *GC0100  
00033 *      A RECORD IS ALSO CREATED CONTAINING THE TABULAR-ID,       *GC0100  
00034 *      THE (LAST) SLOT-NUMBER, AND LOW-VALUES IN THE BODY OF     *GC0100  
00035 *      THE RECORD FOR EACH OF THE FOLLOWING TABULARS:            *GC0100  
00036 *                                  #PAQ, #PCX, #PDR, #PPF,       *GC0100  
00037 *                                  #PRR, #PRV, #PSC, #PVE        *GC0100  
00038 *                                                                *GC0100  
00039 *                                                                *GC0100  
00040 *   MODULES CALLED:                                              *GC0100  
00041 *   ---------------                                              *GC0100  
00042 *      'TSGEND'   - ABEND ROUTINE                                *GC0100  
00043 *                                                                *GC0100  
00044 ******************************************************************GC0100  
00045 ******************************************************************GC0100  
00046 *                                                                *GC0100  
00047 *       ***-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0100  
00048 *       *-*         U P D A T E   H I S T O R Y         *-*      *GC0100  
00049 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0100  
00050 *                                                                *GC0100  
00051 *                                                                *GC0100  
00052 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION--------------- *GC0100  
00053 *                                                                *GC0100  
00054 *   D1009    12/21/87  FRY   CREATED THIS PROGRAM.......         *GC0100  
00055 *                                                                *GC0100  
00056 *   11154     3/06/91  FRY   INCREASE RECORD AREAS IN:           *GC0100  
00057 *             FILE SECTION:                                      *GC0100  
00058 *               INPUT-RECORD                                     *GC0100  
00059 *                       FILLER   PIC  X(3994)  CHANGED TO  7799  *GC0100  
00060 *                 OUTPUT-RECORD  PIC  X(4064)  CHANGED TO  7869  *GC0100  
00061 *             WORKING STORAGE SECTION:                           *GC0100  
00062 *               VSAM-RECORD-AREA PIC  X(4000)  CHANGED TO  7805  *GC0100  
00063 *                   FILLER       PIC  X(3960)  CHANGDE TO  7765  *GC0100  
00064 *                                                                *GC0100  
00065 * D-356A   5/08/03  GTF  RECOMPILE FOR COPYBK CHANGES #PAQ, #PDR,*GC0100  
00066 *                        #PRR, #PRV.                             *GC0100  
00067 ******************************************************************GC0100  
00068 ******************************************************************GC0100  
00069  ENVIRONMENT DIVISION.                                            GC0100  
00070                                                                   GC0100  
00071  CONFIGURATION SECTION.                                           GC0100  
00072  SOURCE-COMPUTER.  IBM-370.                                       GC0100  
00073  OBJECT-COMPUTER.  IBM-370.                                       GC0100  
00074                                                                   GC0100  
00075                                                                   GC0100  
00076  INPUT-OUTPUT SECTION.                                            GC0100  
00077                                                                   GC0100  
00078  FILE-CONTROL.                                                    GC0100  
00079      SELECT   INPUT-FILE          ASSIGN  TO   UT-S-GC0100A.      GC0100  
00080      SELECT   OUTPUT-FILE         ASSIGN  TO   UT-S-GC0100B.      GC0100  
00081                                                                   GC0100  
00082                                                                   GC0100  
00083  DATA DIVISION.                                                   GC0100  
00084                                                                   GC0100  
00085  FILE SECTION.                                                    GC0100  
00086  FD  INPUT-FILE                                                   GC0100  
00087          LABEL RECORDS ARE STANDARD                               GC0100  
00088          RECORDING MODE IS V                                      GC0100  
00089          BLOCK CONTAINS  0  RECORDS.                              GC0100  
00090                                                                   GC0100  
00091  01  INPUT-RECORD.                                                GC0100  
00092      05  FILLER                            PIC  X(64).            GC0100  
00093      05  WRK-BEN-TAB-PROV-DATA.                                   GC0100  
00094          10  WRK-TAB-PROV-ID               PIC  X(6).             GC0100  
00095          10  FILLER                        PIC  X(7799).          GC0100  
00096 /                                                                 GC0100  
00097                                                                   GC0100  
00098  FD  OUTPUT-FILE                                                  GC0100  
00099          LABEL RECORDS ARE STANDARD                               GC0100  
00100          RECORDING MODE IS V                                      GC0100  
00101          BLOCK CONTAINS  0  RECORDS.                              GC0100  
00102                                                                   GC0100  
00103  01  OUTPUT-RECORD                         PIC  X(7869).          GC0100  
00104                                                                   GC0100  
00105  01  OUTPUT-LAST-RECORD.                                          GC0100  
00106      05  FILLER                            PIC  X(64).            GC0100  
00107      05  LAST-RECORD-KEY.                                         GC0100  
00108         10  LAST-KEY-ID                    PIC  X(06).            GC0100  
00109         10  LAST-KEY-SLOT-NO               PIC S9(07)   COMP-3.   GC0100  
00110                                                                   GC0100  
00111                                                                   GC0100  
00112  01  OUTPUT-PAQ-RECORD.                                           GC0100  
00113      05  FILLER                            PIC  X(64).            GC0100  
00114 /                                                                 GC0100  
00115 *    05  GBA-RECORD.                                              GC0100  
00116      COPY  GCTPAQC.                                               GC0100  
00117 /                                                                 GC0100  
00118                                                                   GC0100  
00119  01  OUTPUT-PCX-RECORD.                                           GC0100  
00120      05  FILLER                            PIC  X(64).            GC0100  
00121 /                                                                 GC0100  
00122 *    05  GBD-RECORD.                                              GC0100  
00123      COPY  GCTPCXC.                                               GC0100  
00124 /                                                                 GC0100  
00125                                                                   GC0100  
00126  01  OUTPUT-PDR-RECORD.                                           GC0100  
00127      05  FILLER                            PIC  X(64).            GC0100  
00128 /                                                                 GC0100  
00129 *    05  GBG-RECORD.                                              GC0100  
00130      COPY  GCTPDRC.                                               GC0100  
00131 /                                                                 GC0100  
00132                                                                   GC0100  
00133  01  OUTPUT-PPF-RECORD.                                           GC0100  
00134      05  FILLER                            PIC  X(64).            GC0100  
00135 /                                                                 GC0100  
00136 *    05  GBB-RECORD.                                              GC0100  
00137      COPY  GCTPPFC.                                               GC0100  
00138 /                                                                 GC0100  
00139                                                                   GC0100  
00140  01  OUTPUT-PRR-RECORD.                                           GC0100  
00141      05  FILLER                            PIC  X(64).            GC0100  
00142 /                                                                 GC0100  
00143 *    05  GBE-RECORD.                                              GC0100  
00144      COPY  GCTPRRC.                                               GC0100  
00145 /                                                                 GC0100  
00146                                                                   GC0100  
00147  01  OUTPUT-PRV-RECORD.                                           GC0100  
00148      05  FILLER                            PIC  X(64).            GC0100  
00149 /                                                                 GC0100  
00150 *    05  GBH-RECORD.                                              GC0100  
00151      COPY  GCTPRVC.                                               GC0100  
00152 /                                                                 GC0100  
00153                                                                   GC0100  
00154  01  OUTPUT-PSC-RECORD.                                           GC0100  
00155      05  FILLER                            PIC  X(64).            GC0100  
00156 /                                                                 GC0100  
00157 *    05  GBI-RECORD.                                              GC0100  
00158      COPY  GCTPSCC.                                               GC0100  
00159 /                                                                 GC0100  
00160                                                                   GC0100  
00161  01  OUTPUT-PVE-RECORD.                                           GC0100  
00162      05  FILLER                            PIC  X(64).            GC0100  
00163 /                                                                 GC0100  
00164 *    05  GBJ-RECORD.                                              GC0100  
00165      COPY  GCTPVEC.                                               GC0100  
00166 /                                                                 GC0100  
00167                                                                   GC0100  
00168  WORKING-STORAGE SECTION.                                         GC0100  
00169                                                                   GC0100  
00170  77  FILLER                           PIC  X(27)    VALUE         GC0100  
00171                                     '* GC0100 WORKING STORAGE *'. GC0100  
00172                                                                   GC0100  
00173  01  WS-HOLD-AREAS.                                               GC0100  
00174      05  FILLER                       PIC  X(29)    VALUE         GC0100  
00175                                       '***  ABEND CODE  ***'.     GC0100  
00176      05  WS-ABEND-CODE                PIC  9(04)    VALUE 0  COMP.GC0100  
00177                                                                   GC0100  
00178                                                                   GC0100  
00179  01  WS-WORK-AREAS.                                               GC0100  
00180      05  FILLER                       PIC  X(28)    VALUE         GC0100  
00181                                       '***  WORK AREA  ***'.      GC0100  
00182      05  WS-LAST-SLOT       COMP-3    PIC S9(07)    VALUE +0.     GC0100  
00183      05  WS-INPUT-RELEASED-RECORDS    PIC  9(08)    VALUE ZEROES. GC0100  
00184      05  WS-INPUT-VSAM-RECORDS        PIC  9(08)    VALUE ZEROES. GC0100  
00185      05  WS-OUTPUT-RECORDS            PIC  9(08)    VALUE ZEROES. GC0100  
00186                                                                   GC0100  
00187                                                                   GC0100  
00188  01  WS-SWITCHES.                                                 GC0100  
00189      05  FILLER                       PIC  X(27)    VALUE         GC0100  
00190                                       '***  SWITCHES  ***'.       GC0100  
00191      05  WS-END-OF-INPUT-FILE-SW      PIC  X(01)    VALUE '0'.    GC0100  
00192          88  WS-END-OF-INPUT-FILE-SW-ON             VALUE '1'.    GC0100  
00193      05  WS-VSAM-READ-SWITCH          PIC  X(01)    VALUE '0'.    GC0100  
00194      05  WS-PAQ-SWITCH                PIC  X(01)    VALUE '0'.    GC0100  
00195      05  WS-PCX-SWITCH                PIC  X(01)    VALUE '0'.    GC0100  
00196      05  WS-PDR-SWITCH                PIC  X(01)    VALUE '0'.    GC0100  
00197      05  WS-PPF-SWITCH                PIC  X(01)    VALUE '0'.    GC0100  
00198      05  WS-PRR-SWITCH                PIC  X(01)    VALUE '0'.    GC0100  
00199      05  WS-PRV-SWITCH                PIC  X(01)    VALUE '0'.    GC0100  
00200      05  WS-PSC-SWITCH                PIC  X(01)    VALUE '0'.    GC0100  
00201      05  WS-PVE-SWITCH                PIC  X(01)    VALUE '0'.    GC0100  
00202 /                                                                 GC0100  
00203 ******************************************************************GC0100  
00204 **                                                                GC0100  
00205 **     PARAMETERS FOR THE TABULAR FILE        TSGVSAM1            GC0100  
00206 **                                                                GC0100  
00207 ******************************************************************GC0100  
00208  01  FILLER                            PIC  X(22)  VALUE          GC0100  
00209                                        '***  TABULAR FILE  ***'.  GC0100  
00210                                                                   GC0100  
00211  01  PARM-SET.                                                    GC0100  
00212      05  SET-VSAM-RDW.                                            GC0100  
00213         10 SET-VSAM-RECORD-LENGTH      PIC 9(04)     COMP.        GC0100  
00214         10 SET-VSAM-FEEDBACK-CODE      PIC 9(04)     COMP.        GC0100  
00215      05  SET-VSAM-VALUE                PIC 9(08)     COMP.        GC0100  
00216                                                                   GC0100  
00217                                                                   GC0100  
00218  01  PARM-TAB-ONE-A.                                              GC0100  
00219      05  RESERVED-FLDS                 PIC  9(08) VALUE 0   COMP. GC0100  
00220      05  RESERVED-ONE     REDEFINES     RESERVED-FLDS.            GC0100  
00221         10  VSAM-REQUEST-TYPE          PIC  X(01).                GC0100  
00222         10  FILLER                     PIC  X(03).                GC0100  
00223                                                                   GC0100  
00224                                                                   GC0100  
00225  01  PARM-TAB-ONE-B.                                              GC0100  
00226      05  VSAM-RDW.                                                GC0100  
00227         10  VSAM-RECORD-LENGTH         PIC  9(04)    COMP.        GC0100  
00228         10  VSAM-FEEDBACK-CODE         PIC  9(04)    COMP.        GC0100  
00229      05  VSAM-RECORD-AREA              PIC  X(7805).              GC0100  
00230      05  VSAM-RECORD-A       REDEFINES     VSAM-RECORD-AREA.      GC0100  
00231          10  VSAM-KEY-FIELD.                                      GC0100  
00232             15  VSAM-KEY-ID            PIC  X(06).                GC0100  
00233             15  VSAM-KEY-SLOT          PIC S9(07)    COMP-3.      GC0100  
00234          10  VSAM-KEY-DATA.                                       GC0100  
00235             15  FILLER                 PIC  X(27).                GC0100  
00236             15  VSAM-KEY-ENTRY-COUNT   PIC S9(05)  COMP-3.        GC0100  
00237             15  FILLER                 PIC  X(7765).              GC0100  
00238                                                                   GC0100  
00239 /                                                                 GC0100  
00240  PROCEDURE DIVISION.                                              GC0100  
00241                                                                   GC0100  
00242 ******************************************************************GC0100  
00243 **                                                                GC0100  
00244 **               P R O C E S S    C O N T R O L                   GC0100  
00245 **                                                                GC0100  
00246 ******************************************************************GC0100  
00247  0000-MAINLINE.                                                   GC0100  
00248                                                                   GC0100  
00249      PERFORM 1000-OPEN-THE-FILES  THRU  1000-EXIT.                GC0100  
00250                                                                   GC0100  
00251      PERFORM 2000-READ-AND-PROCESS-TABULARS  THRU  2000-EXIT      GC0100  
00252          UNTIL  WS-END-OF-INPUT-FILE-SW-ON.                       GC0100  
00253                                                                   GC0100  
00254      PERFORM 9000-CLOSE-THE-FILES  THRU  9000-EXIT.               GC0100  
00255                                                                   GC0100  
00256      STOP RUN.                                                    GC0100  
00257                                                                   GC0100  
00258  0000-EXIT.                                                       GC0100  
00259      EXIT.                                                        GC0100  
00260 /                                                                 GC0100  
00261 ******************************************************************GC0100  
00262 **                                                                GC0100  
00263 **                O P E N   T H E   F I L E S                     GC0100  
00264 **                                                                GC0100  
00265 ******************************************************************GC0100  
00266  1000-OPEN-THE-FILES.                                             GC0100  
00267                                                                   GC0100  
00268      OPEN INPUT  INPUT-FILE,                                      GC0100  
00269           OUTPUT OUTPUT-FILE.                                     GC0100  
00270                                                                   GC0100  
00271                                                                   GC0100  
00272      MOVE  'S'          TO  VSAM-REQUEST-TYPE.                    GC0100  
00273      MOVE   8           TO  SET-VSAM-RECORD-LENGTH.               GC0100  
00274      MOVE   3           TO  SET-VSAM-VALUE.                       GC0100  
00275      CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A    PARM-SET.         GC0100  
00276                                                                   GC0100  
00277                                                                   GC0100  
00278      IF  VSAM-REQUEST-TYPE   NOT =   'S'                          GC0100  
00279          DISPLAY 'BAD SET IN GC0100    1000-OPEN-THE-FILES'       GC0100  
00280          MOVE  SET-VSAM-FEEDBACK-CODE  TO  WS-ABEND-CODE          GC0100  
00281          GO TO  9999-ERROR-RTN.                                   GC0100  
00282                                                                   GC0100  
00283  1000-EXIT.                                                       GC0100  
00284      EXIT.                                                        GC0100  
00285 /                                                                 GC0100  
00286 ******************************************************************GC0100  
00287 **                                                                GC0100  
00288 **   R E A D   T H E   S E Q U E N T I A L   I N P U T   F I L E  GC0100  
00289 **                                                                GC0100  
00290 ******************************************************************GC0100  
00291  2000-READ-AND-PROCESS-TABULARS.                                  GC0100  
00292                                                                   GC0100  
00293 **--READ THE SEQUENTIAL INPUT FILE.                               GC0100  
00294 **                                                                GC0100  
00295      READ INPUT-FILE                                              GC0100  
00296          AT END                                                   GC0100  
00297              MOVE '1'  TO   WS-END-OF-INPUT-FILE-SW               GC0100  
00298              DISPLAY  '  '                                        GC0100  
00299              DISPLAY  ' RELEASED RECORDS READ    =  '             GC0100  
00300                                     WS-INPUT-RELEASED-RECORDS     GC0100  
00301              DISPLAY  ' VSAM RECORDS READ        =  '             GC0100  
00302                                         WS-INPUT-VSAM-RECORDS     GC0100  
00303              DISPLAY  ' RECORDS WRITTEN          =  '             GC0100  
00304                                             WS-OUTPUT-RECORDS     GC0100  
00305              GO TO 2000-EXIT.                                     GC0100  
00306                                                                   GC0100  
00307                                                                   GC0100  
00308      ADD  1  TO   WS-INPUT-RELEASED-RECORDS.                      GC0100  
00309                                                                   GC0100  
00310                                                                   GC0100  
00311 **--PROCESS BENEFIT PROVISION TABULARS ONLY                       GC0100  
00312 **                                                                GC0100  
00313      IF WRK-TAB-PROV-ID   EQUAL   '#PAQ  '                        GC0100  
00314          IF WS-PAQ-SWITCH    >   '0'                              GC0100  
00315              GO TO 2000-EXIT                                      GC0100  
00316          ELSE                                                     GC0100  
00317             MOVE  '1'   TO   WS-PAQ-SWITCH                        GC0100  
00318             PERFORM 3000-PROCESS-BEN-PROV-TABULARS THRU 3000-EXIT.GC0100  
00319                                                                   GC0100  
00320                                                                   GC0100  
00321      IF WRK-TAB-PROV-ID   EQUAL   '#PCX '                         GC0100  
00322          IF WS-PCX-SWITCH    >   '0'                              GC0100  
00323              GO TO 2000-EXIT                                      GC0100  
00324          ELSE                                                     GC0100  
00325             MOVE  '1'   TO   WS-PCX-SWITCH                        GC0100  
00326             PERFORM 3000-PROCESS-BEN-PROV-TABULARS THRU 3000-EXIT.GC0100  
00327                                                                   GC0100  
00328                                                                   GC0100  
00329      IF WRK-TAB-PROV-ID   EQUAL   '#PDR '                         GC0100  
00330          IF WS-PDR-SWITCH    >   '0'                              GC0100  
00331              GO TO 2000-EXIT                                      GC0100  
00332          ELSE                                                     GC0100  
00333             MOVE  '1'   TO   WS-PDR-SWITCH                        GC0100  
00334             PERFORM 3000-PROCESS-BEN-PROV-TABULARS THRU 3000-EXIT.GC0100  
00335                                                                   GC0100  
00336                                                                   GC0100  
00337      IF WRK-TAB-PROV-ID   EQUAL   '#PPF '                         GC0100  
00338          IF WS-PPF-SWITCH    >   '0'                              GC0100  
00339              GO TO 2000-EXIT                                      GC0100  
00340          ELSE                                                     GC0100  
00341             MOVE  '1'   TO   WS-PPF-SWITCH                        GC0100  
00342             PERFORM 3000-PROCESS-BEN-PROV-TABULARS THRU 3000-EXIT.GC0100  
00343                                                                   GC0100  
00344                                                                   GC0100  
00345      IF WRK-TAB-PROV-ID   EQUAL   '#PRR '                         GC0100  
00346          IF WS-PRR-SWITCH    >   '0'                              GC0100  
00347              GO TO 2000-EXIT                                      GC0100  
00348          ELSE                                                     GC0100  
00349             MOVE  '1'   TO   WS-PRR-SWITCH                        GC0100  
00350             PERFORM 3000-PROCESS-BEN-PROV-TABULARS THRU 3000-EXIT.GC0100  
00351                                                                   GC0100  
00352                                                                   GC0100  
00353      IF WRK-TAB-PROV-ID   EQUAL   '#PRV '                         GC0100  
00354          IF WS-PRV-SWITCH    >   '0'                              GC0100  
00355              GO TO 2000-EXIT                                      GC0100  
00356          ELSE                                                     GC0100  
00357             MOVE  '1'   TO   WS-PRV-SWITCH                        GC0100  
00358             PERFORM 3000-PROCESS-BEN-PROV-TABULARS THRU 3000-EXIT.GC0100  
00359                                                                   GC0100  
00360                                                                   GC0100  
00361      IF WRK-TAB-PROV-ID   EQUAL   '#PSC '                         GC0100  
00362          IF WS-PSC-SWITCH    >   '0'                              GC0100  
00363              GO TO 2000-EXIT                                      GC0100  
00364          ELSE                                                     GC0100  
00365             MOVE  '1'   TO   WS-PSC-SWITCH                        GC0100  
00366             PERFORM 3000-PROCESS-BEN-PROV-TABULARS THRU 3000-EXIT.GC0100  
00367                                                                   GC0100  
00368                                                                   GC0100  
00369      IF WRK-TAB-PROV-ID   EQUAL   '#PVE '                         GC0100  
00370          IF WS-PVE-SWITCH    >   '0'                              GC0100  
00371              GO TO 2000-EXIT                                      GC0100  
00372          ELSE                                                     GC0100  
00373             MOVE  '1'   TO   WS-PVE-SWITCH                        GC0100  
00374             PERFORM 3000-PROCESS-BEN-PROV-TABULARS THRU 3000-EXIT.GC0100  
00375                                                                   GC0100  
00376                                                                   GC0100  
00377      PERFORM 7000-CHECK-ALL-SWITCHES THRU 7000-EXIT.              GC0100  
00378                                                                   GC0100  
00379  2000-EXIT.                                                       GC0100  
00380      EXIT.                                                        GC0100  
00381 /                                                                 GC0100  
00382 ******************************************************************GC0100  
00383 **                                                                GC0100  
00384 **     PROCESS ONLY THE  BENEIFIT PROVISION TABULARS              GC0100  
00385 **                                                                GC0100  
00386 ******************************************************************GC0100  
00387  3000-PROCESS-BEN-PROV-TABULARS.                                  GC0100  
00388                                                                   GC0100  
00389 *-- INITIALIZE LAST SLOT.                                         GC0100  
00390      MOVE  +10               TO  WS-LAST-SLOT.                    GC0100  
00391                                                                   GC0100  
00392                                                                   GC0100  
00393 **-- BUILD KEY FOR TABULAR FILE                                   GC0100  
00394 **                                                                GC0100  
00395      MOVE  WRK-TAB-PROV-ID   TO  VSAM-KEY-ID.                     GC0100  
00396      MOVE  +11               TO  VSAM-KEY-SLOT.                   GC0100  
00397      MOVE  14                TO  VSAM-RECORD-LENGTH.              GC0100  
00398      MOVE  'P'               TO  VSAM-REQUEST-TYPE.               GC0100  
00399                                                                   GC0100  
00400      PERFORM 4000-LOCATE-BEGIN-TABULAR THRU 4000-EXIT.            GC0100  
00401                                                                   GC0100  
00402                                                                   GC0100  
00403 **--IF THE RECORD IS NOT ON THE FILE, CREATE A RECORD WITH        GC0100  
00404 **--THE TABULAR-ID AND ASSIGN A SLOT NUMBER OF 10.                GC0100  
00405 *                                                                 GC0100  
00406      IF  VSAM-REQUEST-TYPE   EQUAL   '2'                          GC0100  
00407          MOVE  LOW-VALUES        TO  OUTPUT-RECORD                GC0100  
00408          MOVE  WRK-TAB-PROV-ID   TO  LAST-KEY-ID                  GC0100  
00409          MOVE  WS-LAST-SLOT      TO  LAST-KEY-SLOT-NO             GC0100  
00410          WRITE OUTPUT-LAST-RECORD                                 GC0100  
00411          ADD  1  TO   WS-OUTPUT-RECORDS                           GC0100  
00412          GO TO 3000-EXIT.                                         GC0100  
00413                                                                   GC0100  
00414                                                                   GC0100  
00415 **--INITIALIZE THE SWITCH WHEN THE TABULAR-ID CHANGES             GC0100  
00416 *                                                                 GC0100  
00417      MOVE  '0'  TO  WS-VSAM-READ-SWITCH.                          GC0100  
00418                                                                   GC0100  
00419                                                                   GC0100  
00420      PERFORM 5000-READNEXT-VSAM-RECORD THRU 5000-EXIT             GC0100  
00421          UNTIL   WS-VSAM-READ-SWITCH   >   '0'.                   GC0100  
00422                                                                   GC0100  
00423                                                                   GC0100  
00424                                                                   GC0100  
00425 **--CREATE A RECORD FOR EACH TYPE OF TABULAR, CONTAINING          GC0100  
00426 **  THE VSAM-KEY-ID , (LAST) TAB-SLOT-NUMBER, AND LOW-VALUES      GC0100  
00427 **  IN THE BODY OF THE RECORD.                                    GC0100  
00428                                                                   GC0100  
00429      MOVE  LOW-VALUES        TO  OUTPUT-RECORD.                   GC0100  
00430      MOVE  WRK-TAB-PROV-ID   TO  LAST-KEY-ID.                     GC0100  
00431      MOVE  WS-LAST-SLOT      TO  LAST-KEY-SLOT-NO.                GC0100  
00432                                                                   GC0100  
00433      WRITE OUTPUT-LAST-RECORD.                                    GC0100  
00434      ADD  1  TO   WS-OUTPUT-RECORDS.                              GC0100  
00435                                                                   GC0100  
00436  3000-EXIT.                                                       GC0100  
00437      EXIT.                                                        GC0100  
00438 /                                                                 GC0100  
00439 ******************************************************************GC0100  
00440 **                                                                GC0100  
00441 **             V S A M   T A B U L A R   F I L E                  GC0100  
00442 **                                                                GC0100  
00443 ******************************************************************GC0100  
00444  4000-LOCATE-BEGIN-TABULAR.                                       GC0100  
00445                                                                   GC0100  
00446      CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    GC0100  
00447                                                                   GC0100  
00448                                                                   GC0100  
00449      IF  VSAM-REQUEST-TYPE     EQUAL   '2'                        GC0100  
00450          GO TO 4000-EXIT.                                         GC0100  
00451                                                                   GC0100  
00452                                                                   GC0100  
00453      IF  VSAM-REQUEST-TYPE     NOT =   'P'                        GC0100  
00454          DISPLAY 'BAD POINT IN GC0100  4000-LOCATE-BEGIN-TABULAR' GC0100  
00455          MOVE  VSAM-FEEDBACK-CODE   TO   WS-ABEND-CODE            GC0100  
00456          GO TO  9999-ERROR-RTN.                                   GC0100  
00457                                                                   GC0100  
00458  4000-EXIT.                                                       GC0100  
00459      EXIT.                                                        GC0100  
00460 /                                                                 GC0100  
00461 ******************************************************************GC0100  
00462 **                                                                GC0100  
00463 **       R E A D    V S A M    T A B U L A R    F I L E           GC0100  
00464 **                                                                GC0100  
00465 ******************************************************************GC0100  
00466  5000-READNEXT-VSAM-RECORD.                                       GC0100  
00467                                                                   GC0100  
00468      MOVE  'G'          TO  VSAM-REQUEST-TYPE.                    GC0100  
00469      CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    GC0100  
00470                                                                   GC0100  
00471                                                                   GC0100  
00472 **--SET THE SWITCH IF END OF VSAM FILE                            GC0100  
00473 *                                                                 GC0100  
00474      IF  VSAM-REQUEST-TYPE   EQUAL   '2'                          GC0100  
00475          MOVE '1' TO WS-VSAM-READ-SWITCH                          GC0100  
00476          GO TO 5000-EXIT.                                         GC0100  
00477                                                                   GC0100  
00478                                                                   GC0100  
00479      IF  VSAM-REQUEST-TYPE   NOT =   'G'                          GC0100  
00480          DISPLAY 'BAD GET IN GC0100    5000-READNEXT-VSAM-RECORD' GC0100  
00481          MOVE  VSAM-FEEDBACK-CODE   TO   WS-ABEND-CODE            GC0100  
00482          GO TO  9999-ERROR-RTN.                                   GC0100  
00483                                                                   GC0100  
00484                                                                   GC0100  
00485      IF VSAM-KEY-ID    NOT EQUAL    WRK-TAB-PROV-ID               GC0100  
00486          MOVE '1'  TO  WS-VSAM-READ-SWITCH                        GC0100  
00487          GO TO 5000-EXIT.                                         GC0100  
00488                                                                   GC0100  
00489                                                                   GC0100  
00490      ADD  1               TO  WS-INPUT-VSAM-RECORDS.              GC0100  
00491      MOVE VSAM-KEY-SLOT   TO  WS-LAST-SLOT.                       GC0100  
00492      MOVE LOW-VALUES      TO  OUTPUT-RECORD.                      GC0100  
00493                                                                   GC0100  
00494      PERFORM 6000-WRITE-BEN-PROV-TABULARS   THRU  6000-EXIT.      GC0100  
00495                                                                   GC0100  
00496  5000-EXIT.                                                       GC0100  
00497      EXIT.                                                        GC0100  
00498 /                                                                 GC0100  
00499 ******************************************************************GC0100  
00500 **                                                                GC0100  
00501 **   CREATE A RECORD FOR EACH TYPE OF BENEFIT PROVISION TABULAR   GC0100  
00502 **                                                                GC0100  
00503 ******************************************************************GC0100  
00504  6000-WRITE-BEN-PROV-TABULARS.                                    GC0100  
00505                                                                   GC0100  
00506      IF VSAM-KEY-ID    EQUAL    '#PAQ '                           GC0100  
00507          MOVE  VSAM-KEY-ENTRY-COUNT  TO GBA-ENTRY-COUNT           GC0100  
00508          MOVE  VSAM-RECORD-AREA      TO GBA-RECORD                GC0100  
00509          WRITE OUTPUT-PAQ-RECORD                                  GC0100  
00510          ADD  1  TO   WS-OUTPUT-RECORDS                           GC0100  
00511          GO TO 6000-EXIT.                                         GC0100  
00512                                                                   GC0100  
00513                                                                   GC0100  
00514      IF VSAM-KEY-ID    EQUAL    '#PCX '                           GC0100  
00515          MOVE  VSAM-KEY-ENTRY-COUNT  TO GBD-ENTRY-COUNT           GC0100  
00516          MOVE  VSAM-RECORD-AREA      TO GBD-RECORD                GC0100  
00517          WRITE OUTPUT-PCX-RECORD                                  GC0100  
00518          ADD  1  TO   WS-OUTPUT-RECORDS                           GC0100  
00519          GO TO 6000-EXIT.                                         GC0100  
00520                                                                   GC0100  
00521                                                                   GC0100  
00522      IF VSAM-KEY-ID    EQUAL    '#PDR '                           GC0100  
00523          MOVE  VSAM-KEY-ENTRY-COUNT  TO GBG-ENTRY-COUNT           GC0100  
00524          MOVE  VSAM-RECORD-AREA      TO GBG-RECORD                GC0100  
00525          WRITE OUTPUT-PDR-RECORD                                  GC0100  
00526          ADD  1  TO   WS-OUTPUT-RECORDS                           GC0100  
00527          GO TO 6000-EXIT.                                         GC0100  
00528                                                                   GC0100  
00529                                                                   GC0100  
00530      IF VSAM-KEY-ID    EQUAL    '#PPF '                           GC0100  
00531          MOVE  VSAM-KEY-ENTRY-COUNT  TO GBB-ENTRY-COUNT           GC0100  
00532          MOVE  VSAM-RECORD-AREA      TO GBB-RECORD                GC0100  
00533          WRITE OUTPUT-PPF-RECORD                                  GC0100  
00534          ADD  1  TO   WS-OUTPUT-RECORDS                           GC0100  
00535          GO TO 6000-EXIT.                                         GC0100  
00536                                                                   GC0100  
00537                                                                   GC0100  
00538      IF VSAM-KEY-ID    EQUAL    '#PRR '                           GC0100  
00539          MOVE  VSAM-KEY-ENTRY-COUNT  TO GBE-ENTRY-COUNT           GC0100  
00540          MOVE  VSAM-RECORD-AREA      TO GBE-RECORD                GC0100  
00541          WRITE OUTPUT-PRR-RECORD                                  GC0100  
00542          ADD  1  TO   WS-OUTPUT-RECORDS                           GC0100  
00543          GO TO 6000-EXIT.                                         GC0100  
00544                                                                   GC0100  
00545                                                                   GC0100  
00546      IF VSAM-KEY-ID    EQUAL    '#PRV '                           GC0100  
00547          MOVE  VSAM-KEY-ENTRY-COUNT  TO GBH-ENTRY-COUNT           GC0100  
00548          MOVE  VSAM-RECORD-AREA      TO GBH-RECORD                GC0100  
00549          WRITE OUTPUT-PRV-RECORD                                  GC0100  
00550          ADD  1  TO   WS-OUTPUT-RECORDS                           GC0100  
00551          GO TO 6000-EXIT.                                         GC0100  
00552                                                                   GC0100  
00553                                                                   GC0100  
00554      IF VSAM-KEY-ID    EQUAL    '#PSC '                           GC0100  
00555          MOVE  VSAM-KEY-ENTRY-COUNT  TO GBI-ENTRY-COUNT           GC0100  
00556          MOVE  VSAM-RECORD-AREA      TO GBI-RECORD                GC0100  
00557          WRITE OUTPUT-PSC-RECORD                                  GC0100  
00558          ADD  1  TO   WS-OUTPUT-RECORDS                           GC0100  
00559          GO TO 6000-EXIT.                                         GC0100  
00560                                                                   GC0100  
00561                                                                   GC0100  
00562      IF VSAM-KEY-ID    EQUAL    '#PVE '                           GC0100  
00563          MOVE  VSAM-KEY-ENTRY-COUNT  TO GBJ-ENTRY-COUNT           GC0100  
00564          MOVE  VSAM-RECORD-AREA      TO GBJ-RECORD                GC0100  
00565          WRITE OUTPUT-PVE-RECORD                                  GC0100  
00566          ADD  1  TO   WS-OUTPUT-RECORDS.                          GC0100  
00567                                                                   GC0100  
00568                                                                   GC0100  
00569  6000-EXIT.                                                       GC0100  
00570      EXIT.                                                        GC0100  
00571 /                                                                 GC0100  
00572 ******************************************************************GC0100  
00573 **                                                                GC0100  
00574 **    IF ALL OF THE BENEIFIT PROVISION TABULARS HAVE BEEN READ,   GC0100  
00575 **    SET THE END OF FILE SWITCH FOR THE SEQUENTIAL INPUT         GC0100  
00576 **    (RELEASED BENEFIT PROVISION TABULAR) FILE.                  GC0100  
00577 **                                                                GC0100  
00578 ******************************************************************GC0100  
00579  7000-CHECK-ALL-SWITCHES.                                         GC0100  
00580                                                                   GC0100  
00581      IF WS-PAQ-SWITCH    >   '0'                                  GC0100  
00582        AND                                                        GC0100  
00583         WS-PCX-SWITCH    >   '0'                                  GC0100  
00584        AND                                                        GC0100  
00585         WS-PDR-SWITCH    >   '0'                                  GC0100  
00586        AND                                                        GC0100  
00587         WS-PPF-SWITCH    >   '0'                                  GC0100  
00588        AND                                                        GC0100  
00589         WS-PRR-SWITCH    >   '0'                                  GC0100  
00590        AND                                                        GC0100  
00591         WS-PRV-SWITCH    >   '0'                                  GC0100  
00592        AND                                                        GC0100  
00593         WS-PSC-SWITCH    >   '0'                                  GC0100  
00594        AND                                                        GC0100  
00595         WS-PVE-SWITCH    >   '0'                                  GC0100  
00596          MOVE  '1'   TO   WS-END-OF-INPUT-FILE-SW.                GC0100  
00597                                                                   GC0100  
00598  7000-EXIT.                                                       GC0100  
00599      EXIT.                                                        GC0100  
00600 /                                                                 GC0100  
00601 ******************************************************************GC0100  
00602 **                                                                GC0100  
00603 **            C L O S E   T H E   F I L E S                       GC0100  
00604 **                                                                GC0100  
00605 ******************************************************************GC0100  
00606  9000-CLOSE-THE-FILES.                                            GC0100  
00607                                                                   GC0100  
00608      CLOSE INPUT-FILE,                                            GC0100  
00609            OUTPUT-FILE.                                           GC0100  
00610                                                                   GC0100  
00611                                                                   GC0100  
00612      MOVE     'C'           TO   VSAM-REQUEST-TYPE.               GC0100  
00613      CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    GC0100  
00614                                                                   GC0100  
00615                                                                   GC0100  
00616      IF  VSAM-REQUEST-TYPE    NOT =   'C'                         GC0100  
00617          DISPLAY 'BAD CLOSE IN GC0100 AT 9000-CLOSE-THE-FILES'    GC0100  
00618          MOVE  VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE             GC0100  
00619          GO TO  9999-ERROR-RTN.                                   GC0100  
00620                                                                   GC0100  
00621  9000-EXIT.                                                       GC0100  
00622      EXIT.                                                        GC0100  
00623 /                                                                 GC0100  
00624 ******************************************************************GC0100  
00625 **                                                                GC0100  
00626 **                       A B E N D                                GC0100  
00627 **                                                                GC0100  
00628 ******************************************************************GC0100  
00629  9999-ERROR-RTN.                                                  GC0100  
00630                                                                   GC0100  
00631      CALL  'TSGEND' USING  WS-ABEND-CODE.                         GC0100  
00632                                                                   GC0100  
00633  9999-EXIT.                                                       GC0100  
00634      EXIT.                                                        GC0100  
00635 /                                                                 GC0100  
