00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID.       GC0160.                                        GC0160  
00003  AUTHOR.           DELORES FRY.                                      LV002
00004  INSTALLATION.     HCSC.                                          GC0160  
00005  DATE-WRITTEN.     DECEMBER 1987.                                 GC0160  
00006                                                                   GC0160  
00007 ******************************************************************GC0160  
00008 ******************************************************************GC0160  
00009 *                                                                *GC0160  
00010 *          GENERIC CONTRACT PROCESSING SYSTEM (GCPS)             *GC0160  
00011 *                                                                *GC0160  
00012 *  THIS PROGRAM CREATES THE CONTRACT TABULAR STRIP-FILE.         *GC0160  
00013 *                                                                *GC0160  
00014 * INPUT FILES:   1. TABULAR FILE (VSAM KEY SEQUENCED)  -TSGVSAM1 *GC0160  
00015 *                2. RELEASED CONTRACT TABULAR FILE     -GC0160A * GC0160  
00016 *                                                                *GC0160  
00017 *                                                                *GC0160  
00018 * OUTPUT FILE:   INTERMEDIATE CONTRACT TABULAR FILE TO BE        *GC0160  
00019 *                SORTED  -SEQUENTIAL                   -GC0160B * GC0160  
00020 *                                                                *GC0160  
00021 *                                                                *GC0160  
00022 *                                                                *GC0160  
00023 *   PROCESSING FUNCTIONS:                                        *GC0160  
00024 *   --------------------                                         *GC0160  
00025 *   1. THIS PROGRAM READS THE RELEASED CONTRACT TABULAR FILE     *GC0160  
00026 *      CREATED FROM THE WORK-FILE.  THE TYPES OF CONTRACT        *GC0160  
00027 *      TABULARS ON THIS FILE ARE:  #CLDR, #CRR                   *GC0160  
00028 *                                                                *GC0160  
00029 *   2. A SEQUENTIAL OUTPUT FILE IS CREATED CONTAINING ONLY       *GC0160  
00030 *      THE FOLLOWING CONTRACT TABULARS RECORDS:                  *GC0160  
00031 *                          #CLDR, #CRR                           *GC0160  
00032 *      A RECORD IS ALSO CREATED CONTAINING THE TABULAR-ID,       *GC0160  
00033 *      THE (LAST) SLOT-NUMBER, AND LOW-VALUES IN THE BODY OF     *GC0160  
00034 *      THE RECORD FOR EACH OF THE FOLLOWING CONTRACT TABULARS:   *GC0160  
00035 *                          #CLDR, #CRR                           *GC0160  
00036 *                                                                *GC0160  
00037 *                                                                *GC0160  
00038 *   MODULES CALLED:                                              *GC0160  
00039 *   ---------------                                              *GC0160  
00040 *      'TSGEND'   - ABEND ROUTINE                                *GC0160  
00041 *                                                                *GC0160  
00042 ******************************************************************GC0160  
00043 ******************************************************************GC0160  
00044 *                                                                *GC0160  
00045 *       ***-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0160  
00046 *       *-*         U P D A T E   H I S T O R Y         *-*      *GC0160  
00047 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0160  
00048 *                                                                *GC0160  
00049 *                                                                *GC0160  
00050 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION--------------- *GC0160  
00051 *                                                                *GC0160  
00052 *                                                                *GC0160  
00053 *    D1009   12/15/87  FRY   CREATED THIS PROGRAM.......         *GC0160  
00054 *                                                                *GC0160  
00055 *    11154    3/06/91  FRY   INCREASE RECORD AREAS IN:           *GC0160  
00056 *           FILE SECTION:                                        *GC0160  
00057 *               INPUT-RECORD.                                    *GC0160  
00058 *                      FILLER   PIC  X(3994)  CHANGED TO  7799   *GC0160  
00059 *               OUTPUT-RECORD   PIC  X(4064)  CHANGED TO  7869   *GC0160  
00060 *           WORKING STORAGE SECTION:                             *GC0160  
00061 *            VSAM-RECORD-AREA   PIC  X(4000)  CHANGED TO  7805   *GC0160  
00062 *            VSAM-KEY-DATA.                                      *GC0160  
00063 *                      FILLER   PIC  X(3960)  CHANGED TO  7765   *GC0160  
00064 *                                                                *GC0160  
00065 * D-356A   5/08/03  GTF  RECOMPILE FOR COPYBK CHANGES #CLDR,     *GC0160  
00066 *                        #CRR.                                   *GC0160  
00067 ******************************************************************GC0160  
00068 ******************************************************************GC0160  
00069  ENVIRONMENT DIVISION.                                            GC0160  
00070                                                                   GC0160  
00071  CONFIGURATION SECTION.                                           GC0160  
00072  SOURCE-COMPUTER.  IBM-370.                                       GC0160  
00073  OBJECT-COMPUTER.  IBM-370.                                       GC0160  
00074                                                                   GC0160  
00075                                                                   GC0160  
00076  INPUT-OUTPUT SECTION.                                            GC0160  
00077                                                                   GC0160  
00078  FILE-CONTROL.                                                    GC0160  
00079      SELECT   INPUT-FILE          ASSIGN  TO     UT-S-GC0160A.    GC0160  
00080      SELECT   OUTPUT-FILE         ASSIGN  TO     UT-S-GC0160B.    GC0160  
00081                                                                   GC0160  
00082                                                                   GC0160  
00083  DATA DIVISION.                                                   GC0160  
00084                                                                   GC0160  
00085  FILE SECTION.                                                    GC0160  
00086  FD  INPUT-FILE                                                   GC0160  
00087          LABEL RECORDS ARE STANDARD                               GC0160  
00088          RECORDING MODE IS V                                      GC0160  
00089          BLOCK CONTAINS  0  RECORDS.                              GC0160  
00090                                                                   GC0160  
00091  01  INPUT-RECORD.                                                GC0160  
00092      05  FILLER                            PIC  X(64).            GC0160  
00093      05  WRK-BEN-TAB-PROV-DATA.                                   GC0160  
00094          10  WRK-TAB-PROV-ID               PIC  X(6).             GC0160  
00095          10  FILLER                        PIC  X(7799).          GC0160  
00096 /                                                                 GC0160  
00097                                                                   GC0160  
00098  FD  OUTPUT-FILE                                                  GC0160  
00099          LABEL RECORDS ARE STANDARD                               GC0160  
00100          RECORDING MODE IS V                                      GC0160  
00101          BLOCK CONTAINS  0  RECORDS.                              GC0160  
00102                                                                   GC0160  
00103  01  OUTPUT-RECORD                         PIC  X(7869).          GC0160  
00104                                                                   GC0160  
00105  01  OUTPUT-LAST-KEY-RECORD.                                      GC0160  
00106      05  FILLER                            PIC  X(64).            GC0160  
00107      05  OUTPUT-KEY.                                              GC0160  
00108         10  OUT-LAST-KEY-ID                PIC  X(06).            GC0160  
00109         10  OUT-LAST-SLOT-NO               PIC S9(07)   COMP-3.   GC0160  
00110                                                                   GC0160  
00111                                                                   GC0160  
00112  01  OUTPUT-CLDR-RECORD.                                          GC0160  
00113      05  FILLER                            PIC  X(64).            GC0160  
00114 *    05  GTB-RECORD.                                              GC0160  
00115      COPY  GCTCLDRC.                                              GC0160  
00116 /                                                                 GC0160  
00117                                                                   GC0160  
00118  01  OUTPUT-CRR-RECORD.                                           GC0160  
00119      05  FILLER                            PIC  X(64).            GC0160  
00120 *    05  GTC-RECORD.                                              GC0160  
00121      COPY  GCTCRRC.                                               GC0160  
00122 /                                                                 GC0160  
00123                                                                   GC0160  
00124                                                                   GC0160  
00125  WORKING-STORAGE SECTION.                                         GC0160  
00126                                                                   GC0160  
00127  77  FILLER                           PIC  X(27)    VALUE         GC0160  
00128                                     '* GC0160 WORKING STORAGE *'. GC0160  
00129                                                                   GC0160  
00130  01  WS-HOLD-AREAS.                                               GC0160  
00131      05  FILLER                       PIC  X(29)    VALUE         GC0160  
00132                                       '***  ABEND CODE  ***'.     GC0160  
00133      05  WS-ABEND-CODE                PIC  9(04)    VALUE 0  COMP.GC0160  
00134                                                                   GC0160  
00135                                                                   GC0160  
00136  01  WS-WORK-AREAS.                                               GC0160  
00137      05  FILLER                       PIC  X(28)    VALUE         GC0160  
00138                                       '***  WORK AREA  ***'.      GC0160  
00139      05  WS-LAST-SLOT       COMP-3    PIC S9(07)    VALUE +0.     GC0160  
00140      05  WS-INPUT-RELEASED-RECORDS    PIC  9(08)    VALUE ZEROES. GC0160  
00141      05  WS-INPUT-VSAM-RECORDS        PIC  9(08)    VALUE ZEROES. GC0160  
00142      05  WS-OUTPUT-RECORDS            PIC  9(08)    VALUE ZEROES. GC0160  
00143                                                                   GC0160  
00144                                                                   GC0160  
00145  01  WS-SWITCHES.                                                 GC0160  
00146      05  FILLER                       PIC  X(27)    VALUE         GC0160  
00147                                       '***  SWITCHES  ***'.       GC0160  
00148      05  WS-END-OF-INPUT-FILE-SW      PIC  X(01)    VALUE '0'.    GC0160  
00149          88  WS-END-OF-INPUT-FILE-SW-ON             VALUE '1'.    GC0160  
00150      05  WS-VSAM-READ-SWITCH          PIC  X(01)    VALUE '0'.    GC0160  
00151      05  WS-CLDR-SWITCH               PIC  X(01)    VALUE '0'.    GC0160  
00152      05  WS-CRR-SWITCH                PIC  X(01)    VALUE '0'.    GC0160  
00153 /                                                                 GC0160  
00154 ******************************************************************GC0160  
00155 **                                                                GC0160  
00156 **     PARAMETERS FOR THE TABULAR FILE        TSGVSAM1            GC0160  
00157 **                                                                GC0160  
00158 ******************************************************************GC0160  
00159  01  FILLER                        PIC  X(22)  VALUE              GC0160  
00160                                    '***  TABULAR FILE  ***'.      GC0160  
00161                                                                   GC0160  
00162  01  PARM-SET.                                                    GC0160  
00163      05 SET-VSAM-RDW.                                             GC0160  
00164         10 SET-VSAM-RECORD-LENGTH      PIC 9(04)     COMP.        GC0160  
00165         10 SET-VSAM-FEEDBACK-CODE      PIC 9(04)     COMP.        GC0160  
00166      05  SET-VSAM-VALUE                PIC 9(08)     COMP.        GC0160  
00167                                                                   GC0160  
00168                                                                   GC0160  
00169  01  PARM-TAB-ONE-A.                                              GC0160  
00170      05 RESERVED-FLDS                  PIC  9(08) VALUE 0  COMP.  GC0160  
00171      05 RESERVED-ONE      REDEFINES      RESERVED-FLDS.           GC0160  
00172         10 VSAM-REQUEST-TYPE           PIC  X(01).                GC0160  
00173         10 FILLER                      PIC  X(03).                GC0160  
00174                                                                   GC0160  
00175                                                                   GC0160  
00176  01  PARM-TAB-ONE-B.                                              GC0160  
00177      05 VSAM-RDW.                                                 GC0160  
00178         10 VSAM-RECORD-LENGTH          PIC  9(04)    COMP.        GC0160  
00179         10 VSAM-FEEDBACK-CODE          PIC  9(04)    COMP.        GC0160  
00180      05  VSAM-RECORD-AREA              PIC  X(7805).              GC0160  
00181      05  VSAM-RECORD-A       REDEFINES     VSAM-RECORD-AREA.      GC0160  
00182          10 VSAM-KEY-FIELD.                                       GC0160  
00183             15  VSAM-KEY-ID            PIC  X(06).                GC0160  
00184             15  VSAM-KEY-SLOT          PIC S9(07)  COMP-3.        GC0160  
00185          10 VSAM-KEY-DATA.                                        GC0160  
00186             15  FILLER                 PIC  X(27).                GC0160  
00187             15  VSAM-KEY-ENTRY-COUNT   PIC S9(05)  COMP-3.        GC0160  
00188             15  FILLER                 PIC  X(7765).              GC0160  
00189                                                                   GC0160  
00190 /                                                                 GC0160  
00191  PROCEDURE DIVISION.                                              GC0160  
00192                                                                   GC0160  
00193 ******************************************************************GC0160  
00194 **                                                                GC0160  
00195 **               P R O C E S S    C O N T R O L                   GC0160  
00196 **                                                                GC0160  
00197 ******************************************************************GC0160  
00198  0000-MAINLINE.                                                   GC0160  
00199                                                                   GC0160  
00200      PERFORM 1000-OPEN-THE-FILES  THRU  1000-EXIT.                GC0160  
00201                                                                   GC0160  
00202      PERFORM 2000-READ-AND-PROCESS-TABULARS  THRU  2000-EXIT      GC0160  
00203          UNTIL  WS-END-OF-INPUT-FILE-SW-ON.                       GC0160  
00204                                                                   GC0160  
00205      PERFORM 9000-CLOSE-THE-FILES  THRU  9000-EXIT.               GC0160  
00206                                                                   GC0160  
00207      STOP RUN.                                                    GC0160  
00208                                                                   GC0160  
00209  0000-EXIT.                                                       GC0160  
00210      EXIT.                                                        GC0160  
00211 /                                                                 GC0160  
00212 ******************************************************************GC0160  
00213 **                                                                GC0160  
00214 **                O P E N   T H E   F I L E S                     GC0160  
00215 **                                                                GC0160  
00216 ******************************************************************GC0160  
00217  1000-OPEN-THE-FILES.                                             GC0160  
00218                                                                   GC0160  
00219      OPEN INPUT  INPUT-FILE,                                      GC0160  
00220           OUTPUT OUTPUT-FILE.                                     GC0160  
00221                                                                   GC0160  
00222                                                                   GC0160  
00223      MOVE  'S'          TO  VSAM-REQUEST-TYPE.                    GC0160  
00224      MOVE   8           TO  SET-VSAM-RECORD-LENGTH.               GC0160  
00225      MOVE   3           TO  SET-VSAM-VALUE.                       GC0160  
00226      CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-SET.          GC0160  
00227                                                                   GC0160  
00228      IF  VSAM-REQUEST-TYPE    NOT EQUAL   'S'                     GC0160  
00229          DISPLAY 'BAD SET IN GC0160    1000-OPEN-THE-FILES'       GC0160  
00230          MOVE  SET-VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE         GC0160  
00231          GO TO  9999-ERROR-RTN.                                   GC0160  
00232                                                                   GC0160  
00233  1000-EXIT.                                                       GC0160  
00234      EXIT.                                                        GC0160  
00235 /                                                                 GC0160  
00236 ******************************************************************GC0160  
00237 **                                                                GC0160  
00238 **   R E A D   T H E   S E Q U E N T I A L   I N P U T   F I L E  GC0160  
00239 **                                                                GC0160  
00240 ******************************************************************GC0160  
00241  2000-READ-AND-PROCESS-TABULARS.                                  GC0160  
00242                                                                   GC0160  
00243 **--READ THE SEQUENTIAL INPUT FILE.                               GC0160  
00244 **                                                                GC0160  
00245      READ INPUT-FILE                                              GC0160  
00246          AT END                                                   GC0160  
00247              MOVE '1'  TO   WS-END-OF-INPUT-FILE-SW               GC0160  
00248              DISPLAY '  '                                         GC0160  
00249              DISPLAY ' RELEASED RECORDS READ   =  '               GC0160  
00250                                        WS-INPUT-RELEASED-RECORDS  GC0160  
00251              DISPLAY ' VSAM RECORDS READ       =  '               GC0160  
00252                                            WS-INPUT-VSAM-RECORDS  GC0160  
00253              DISPLAY ' RECORDS WRITTEN         =  '               GC0160  
00254                                                WS-OUTPUT-RECORDS  GC0160  
00255              GO TO 2000-EXIT.                                     GC0160  
00256                                                                   GC0160  
00257                                                                   GC0160  
00258      ADD  1  TO   WS-INPUT-RELEASED-RECORDS.                      GC0160  
00259                                                                   GC0160  
00260                                                                   GC0160  
00261 **--PROCESS  \
00262 **                                                                GC0160  
00263      IF WRK-TAB-PROV-ID   EQUAL   '#CLDR  '                       GC0160  
00264          IF WS-CLDR-SWITCH   >   '0'                              GC0160  
00265              GO TO 2000-EXIT                                      GC0160  
00266          ELSE                                                     GC0160  
00267             MOVE  '1'   TO   WS-CLDR-SWITCH                       GC0160  
00268             PERFORM 3000-PROCESS-CONTRACT-TABULARS THRU 3000-EXIT.GC0160  
00269                                                                   GC0160  
00270                                                                   GC0160  
00271      IF WRK-TAB-PROV-ID   EQUAL   '#CRR '                         GC0160  
00272          IF WS-CRR-SWITCH    >   '0'                              GC0160  
00273              GO TO 2000-EXIT                                      GC0160  
00274          ELSE                                                     GC0160  
00275             MOVE  '1'   TO   WS-CRR-SWITCH                        GC0160  
00276             PERFORM 3000-PROCESS-CONTRACT-TABULARS THRU 3000-EXIT.GC0160  
00277                                                                   GC0160  
00278                                                                   GC0160  
00279      PERFORM 7000-CHECK-ALL-SWITCHES THRU 7000-EXIT.              GC0160  
00280                                                                   GC0160  
00281  2000-EXIT.                                                       GC0160  
00282      EXIT.                                                        GC0160  
00283 /                                                                 GC0160  
00284 ******************************************************************GC0160  
00285 **                                                                GC0160  
00286 **          PROCESS ONLY THE  \
00287 **                                                                GC0160  
00288 ******************************************************************GC0160  
00289  3000-PROCESS-CONTRACT-TABULARS.                                  GC0160  
00290                                                                   GC0160  
00291 **-- INITIALIZE LAST SLOT NUMBER                                  GC0160  
00292 **                                                                GC0160  
00293      MOVE  +10               TO  WS-LAST-SLOT.                    GC0160  
00294                                                                   GC0160  
00295                                                                   GC0160  
00296 **-- BUILD KEY FOR TABULAR FILE                                   GC0160  
00297 **                                                                GC0160  
00298      MOVE  WRK-TAB-PROV-ID   TO  VSAM-KEY-ID.                     GC0160  
00299      MOVE  +11               TO  VSAM-KEY-SLOT.                   GC0160  
00300      MOVE  14                TO  VSAM-RECORD-LENGTH.              GC0160  
00301      MOVE  'P'               TO  VSAM-REQUEST-TYPE.               GC0160  
00302                                                                   GC0160  
00303      PERFORM 4000-LOCATE-BEGIN-TABULAR THRU 4000-EXIT.            GC0160  
00304                                                                   GC0160  
00305                                                                   GC0160  
00306 **--IF THE RECORD IS NOT ON THE FILE, CREATE A RECORD WITH        GC0160  
00307 *   THE TABULAR-ID AND ASSIGN A SLOT NUMBER OF 10.                GC0160  
00308 *                                                                 GC0160  
00309      IF  VSAM-REQUEST-TYPE    EQUAL   '2'                         GC0160  
00310          MOVE  LOW-VALUES        TO  OUTPUT-RECORD                GC0160  
00311          MOVE  WRK-TAB-PROV-ID   TO  OUT-LAST-KEY-ID              GC0160  
00312          MOVE  WS-LAST-SLOT      TO  OUT-LAST-SLOT-NO             GC0160  
00313          WRITE OUTPUT-LAST-KEY-RECORD                             GC0160  
00314          ADD  1  TO   WS-OUTPUT-RECORDS                           GC0160  
00315          GO TO 3000-EXIT.                                         GC0160  
00316                                                                   GC0160  
00317                                                                   GC0160  
00318                                                                   GC0160  
00319 **--INITIALIZE THE SWITCH WHEN THE TABULAR-ID CHANGES             GC0160  
00320 *                                                                 GC0160  
00321      MOVE  '0'  TO  WS-VSAM-READ-SWITCH.                          GC0160  
00322                                                                   GC0160  
00323                                                                   GC0160  
00324                                                                   GC0160  
00325      PERFORM 5000-READNEXT-VSAM-RECORD THRU 5000-EXIT             GC0160  
00326          UNTIL   WS-VSAM-READ-SWITCH   >   '0'.                   GC0160  
00327                                                                   GC0160  
00328                                                                   GC0160  
00329                                                                   GC0160  
00330 **--CREATE A RECORD FOR EACH TYPE OF TABULAR, CONTAINING          GC0160  
00331 **  THE VSAM-KEY-ID , (LAST) TAB-SLOT-NUMBER, AND LOW-VALUES      GC0160  
00332 **  IN THE BODY OF THE RECORD.                                    GC0160  
00333                                                                   GC0160  
00334      MOVE  LOW-VALUES        TO  OUTPUT-RECORD.                   GC0160  
00335      MOVE  WRK-TAB-PROV-ID   TO  OUT-LAST-KEY-ID.                 GC0160  
00336      MOVE  WS-LAST-SLOT      TO  OUT-LAST-SLOT-NO.                GC0160  
00337                                                                   GC0160  
00338      WRITE OUTPUT-LAST-KEY-RECORD.                                GC0160  
00339      ADD  1  TO   WS-OUTPUT-RECORDS.                              GC0160  
00340                                                                   GC0160  
00341  3000-EXIT.                                                       GC0160  
00342      EXIT.                                                        GC0160  
00343 /                                                                 GC0160  
00344 ******************************************************************GC0160  
00345 **                                                                GC0160  
00346 **             V S A M   T A B U L A R   F I L E                  GC0160  
00347 **                                                                GC0160  
00348 ******************************************************************GC0160  
00349  4000-LOCATE-BEGIN-TABULAR.                                       GC0160  
00350                                                                   GC0160  
00351      CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    GC0160  
00352                                                                   GC0160  
00353                                                                   GC0160  
00354      IF  VSAM-REQUEST-TYPE    EQUAL   '2'                         GC0160  
00355          GO TO 4000-EXIT.                                         GC0160  
00356                                                                   GC0160  
00357                                                                   GC0160  
00358      IF  VSAM-REQUEST-TYPE    NOT EQUAL   'P'                     GC0160  
00359          DISPLAY 'BAD POINT  GC0160    4000-LOCATE-BEGIN-TABULAR' GC0160  
00360          MOVE  VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE             GC0160  
00361          GO TO  9999-ERROR-RTN.                                   GC0160  
00362                                                                   GC0160  
00363  4000-EXIT.                                                       GC0160  
00364      EXIT.                                                        GC0160  
00365 /                                                                 GC0160  
00366 ******************************************************************GC0160  
00367 **                                                                GC0160  
00368 **       R E A D    V S A M    T A B U L A R    F I L E           GC0160  
00369 **                                                                GC0160  
00370 ******************************************************************GC0160  
00371  5000-READNEXT-VSAM-RECORD.                                       GC0160  
00372                                                                   GC0160  
00373      MOVE  'G'          TO  VSAM-REQUEST-TYPE.                    GC0160  
00374      CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    GC0160  
00375                                                                   GC0160  
00376                                                                   GC0160  
00377 **--SET THE SWITCH IF END OF VSAM FILE                            GC0160  
00378 *                                                                 GC0160  
00379      IF  VSAM-REQUEST-TYPE    EQUAL   '2'                         GC0160  
00380          MOVE '1'  TO  WS-VSAM-READ-SWITCH                        GC0160  
00381          GO TO 5000-EXIT.                                         GC0160  
00382                                                                   GC0160  
00383                                                                   GC0160  
00384      IF  VSAM-REQUEST-TYPE    NOT EQUAL   'G'                     GC0160  
00385          DISPLAY 'BAD GET  GC0160      5000-READNEXT-VSAM-RECORD' GC0160  
00386          MOVE  VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE             GC0160  
00387          GO TO  9999-ERROR-RTN.                                   GC0160  
00388                                                                   GC0160  
00389                                                                   GC0160  
00390      IF VSAM-KEY-ID    NOT EQUAL    WRK-TAB-PROV-ID               GC0160  
00391          MOVE '1'  TO  WS-VSAM-READ-SWITCH                        GC0160  
00392          GO TO 5000-EXIT.                                         GC0160  
00393                                                                   GC0160  
00394                                                                   GC0160  
00395      ADD  1                TO  WS-INPUT-VSAM-RECORDS.             GC0160  
00396      MOVE VSAM-KEY-SLOT    TO  WS-LAST-SLOT.                      GC0160  
00397      MOVE LOW-VALUES       TO  OUTPUT-RECORD.                     GC0160  
00398                                                                   GC0160  
00399      PERFORM 6000-WRITE-CONTRACT-TABULARS   THRU  6000-EXIT.      GC0160  
00400                                                                   GC0160  
00401  5000-EXIT.                                                       GC0160  
00402      EXIT.                                                        GC0160  
00403 /                                                                 GC0160  
00404 ******************************************************************GC0160  
00405 **                                                                GC0160  
00406 **     CREATE A RECORD FOR EACH TYPE OF  \
00407 **                                                                GC0160  
00408 ******************************************************************GC0160  
00409  6000-WRITE-CONTRACT-TABULARS.                                    GC0160  
00410                                                                   GC0160  
00411      IF VSAM-KEY-ID    EQUAL   '#CLDR '                           GC0160  
00412          MOVE  VSAM-KEY-ENTRY-COUNT   TO  GTB-ENTRY-COUNT         GC0160  
00413          MOVE  VSAM-RECORD-AREA       TO  GTB-RECORD              GC0160  
00414          WRITE OUTPUT-CLDR-RECORD                                 GC0160  
00415          ADD  1  TO   WS-OUTPUT-RECORDS                           GC0160  
00416          GO TO 6000-EXIT.                                         GC0160  
00417                                                                   GC0160  
00418                                                                   GC0160  
00419      IF VSAM-KEY-ID    EQUAL   '#CRR '                            GC0160  
00420          MOVE  VSAM-KEY-ENTRY-COUNT   TO  GTC-ENTRY-COUNT         GC0160  
00421          MOVE  VSAM-RECORD-AREA       TO  GTC-RECORD              GC0160  
00422          WRITE OUTPUT-CRR-RECORD                                  GC0160  
00423          ADD  1  TO   WS-OUTPUT-RECORDS.                          GC0160  
00424                                                                   GC0160  
00425  6000-EXIT.                                                       GC0160  
00426      EXIT.                                                        GC0160  
00427 /                                                                 GC0160  
00428 ******************************************************************GC0160  
00429 **                                                                GC0160  
00430 **    IF ALL CONTRACT TABULARS HAVE BEEN READ, SET THE            GC0160  
00431 **    END OF FILE SWITCH FOR THE SEQUENTIAL INPUT                 GC0160  
00432 **    (RELEASED CONTRACT TABULAR) FILE.                           GC0160  
00433 **                                                                GC0160  
00434 ******************************************************************GC0160  
00435  7000-CHECK-ALL-SWITCHES.                                         GC0160  
00436                                                                   GC0160  
00437      IF WS-CLDR-SWITCH   >   '0'                                  GC0160  
00438        AND                                                        GC0160  
00439         WS-CRR-SWITCH    >   '0'                                  GC0160  
00440          MOVE  '1'   TO   WS-END-OF-INPUT-FILE-SW.                GC0160  
00441                                                                   GC0160  
00442  7000-EXIT.                                                       GC0160  
00443      EXIT.                                                        GC0160  
00444 /                                                                 GC0160  
00445 ******************************************************************GC0160  
00446 **                                                                GC0160  
00447 **            C L O S E   T H E   F I L E S                       GC0160  
00448 **                                                                GC0160  
00449 ******************************************************************GC0160  
00450  9000-CLOSE-THE-FILES.                                            GC0160  
00451                                                                   GC0160  
00452      CLOSE INPUT-FILE,                                            GC0160  
00453            OUTPUT-FILE.                                           GC0160  
00454                                                                   GC0160  
00455                                                                   GC0160  
00456      MOVE     'C'           TO   VSAM-REQUEST-TYPE.               GC0160  
00457      CALL  'TSGVSAM1'  USING  PARM-TAB-ONE-A   PARM-TAB-ONE-B.    GC0160  
00458                                                                   GC0160  
00459      IF  VSAM-REQUEST-TYPE    NOT EQUAL   'C'                     GC0160  
00460          DISPLAY 'BAD CLOSE IN GC0160 AT 9000-CLOSE-THE-FILES'    GC0160  
00461          MOVE  VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE             GC0160  
00462          GO TO  9999-ERROR-RTN.                                   GC0160  
00463                                                                   GC0160  
00464  9000-EXIT.                                                       GC0160  
00465      EXIT.                                                        GC0160  
00466 /                                                                 GC0160  
00467 ******************************************************************GC0160  
00468 **                                                                GC0160  
00469 **                       A B E N D                                GC0160  
00470 **                                                                GC0160  
00471 ******************************************************************GC0160  
00472  9999-ERROR-RTN.                                                  GC0160  
00473                                                                   GC0160  
00474      CALL  'TSGEND' USING  WS-ABEND-CODE.                         GC0160  
00475                                                                   GC0160  
00476  9999-EXIT.                                                       GC0160  
00477      EXIT.                                                        GC0160  
00478 /                                                                 GC0160  
