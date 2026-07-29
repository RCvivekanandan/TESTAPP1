00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.       GC0130.                                        GC0130  
00003  AUTHOR.           DELORES FRY.                                      LV001
00004  INSTALLATION.     HCSC.                                          GC0130  
00005  DATE-WRITTEN.     JANUARY 1988.                                  GC0130  
00006                                                                   GC0130  
00007 ******************************************************************GC0130  
00008 ******************************************************************GC0130  
00009 *                                                                *GC0130  
00010 *          GENERIC CONTRACT PROCESSING SYSTEM (GCPS)             *GC0130  
00011 *                                                                *GC0130  
00012 *  THIS PROGRAM CREATES THE BENEFIT PROVISION STRIP FILE.        *GC0130  
00013 *                                                                *GC0130  
00014 * INPUT FILES:   1. BENEFIT PROVISION FILE             -TSGVSAM1 *GC0130  
00015 *                2. RELEASED BENEFIT PROVISION FILE    -GC0130A  *GC0130  
00016 *                                                                *GC0130  
00017 *                                                                *GC0130  
00018 * OUTPUT FILE:   INTERMEDIATE BENEFIT PROVISION FILE TO BE       *GC0130  
00019 *                SORTED  -SEQUENTIAL                   -GC0130B * GC0130  
00020 *                                                                *GC0130  
00021 *                                                                *GC0130  
00022 *                                                                *GC0130  
00023 *   PROCESSING FUNCTIONS:                                        *GC0130  
00024 *   --------------------                                         *GC0130  
00025 *   1. THIS PROGRAM READS THE RELEASED BENEIFIT PROVISION FILE   *GC0130  
00026 *      CREATED FROM THE WORK-FILE.                               *GC0130  
00027 *                                                                *GC0130  
00028 *                                                                *GC0130  
00029 *   2. A SEQUENTIAL OUTPUT FILE IS CREATED CONTAINING ALL        *GC0130  
00030 *      BENEIFIT PROVISION RECORDS AND A LAST RECORD FOR EACH     *GC0130  
00031 *      TYPE OF BENEIFT PROVISION-ID CONTAINING:                  *GC0130  
00032 *        A.  TABULAR-ID                                          *GC0130  
00033 *        B.  SLOT-NUMBER (LAST)                                  *GC0130  
00034 *        C.  LOW-VALUES IN THE BODY OF THE RECORD                *GC0130  
00035 *                                                                *GC0130  
00036 *                                                                *GC0130  
00037 *   MODULES CALLED:                                              *GC0130  
00038 *   ---------------                                              *GC0130  
00039 *      'TSGEND'   - ABEND ROUTINE                                *GC0130  
00040 *                                                                *GC0130  
00041 ******************************************************************GC0130  
00042 ******************************************************************GC0130  
00043 *                                                                *GC0130  
00044 *       ***-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0130  
00045 *       *-*         U P D A T E   H I S T O R Y         *-*      *GC0130  
00046 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC0130  
00047 *                                                                *GC0130  
00048 *                                                                *GC0130  
00049 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION--------------- *GC0130  
00050 *                                                                *GC0130  
00051 *   D1009     1/11/88  FRY   CREATED THIS PROGRAM.......         *GC0130  
00052 *                                                                *GC0130  
00053 *                                                                *GC0130  
00054 ******************************************************************GC0130  
00055 ******************************************************************GC0130  
00056  ENVIRONMENT DIVISION.                                            GC0130  
00057                                                                   GC0130  
00058  CONFIGURATION SECTION.                                           GC0130  
00059  SOURCE-COMPUTER.  IBM-370.                                       GC0130  
00060  OBJECT-COMPUTER.  IBM-370.                                       GC0130  
00061                                                                   GC0130  
00062                                                                   GC0130  
00063  INPUT-OUTPUT SECTION.                                            GC0130  
00064                                                                   GC0130  
00065  FILE-CONTROL.                                                    GC0130  
00066      SELECT   INPUT-FILE          ASSIGN  TO     UT-S-GC0130A.    GC0130  
00067      SELECT   OUTPUT-FILE         ASSIGN  TO     UT-S-GC0130B.    GC0130  
00068                                                                   GC0130  
00069                                                                   GC0130  
00070  DATA DIVISION.                                                   GC0130  
00071                                                                   GC0130  
00072  FILE SECTION.                                                    GC0130  
00073  FD  INPUT-FILE                                                   GC0130  
00074          LABEL RECORDS ARE STANDARD                               GC0130  
00075          RECORDING MODE IS V                                      GC0130  
00076          BLOCK CONTAINS  0  RECORDS.                              GC0130  
00077                                                                   GC0130  
00078  01  INPUT-RECORD.                                                GC0130  
00079      05  FILLER                            PIC  X(64).            GC0130  
00080      05  WRK-BEN-PROV-DATA.                                       GC0130  
00081          10  WRK-BEN-PROV-ID               PIC  X(6).             GC0130  
00082          10  FILLER                        PIC  X(389).           GC0130  
00083 /                                                                 GC0130  
00084  FD  OUTPUT-FILE                                                  GC0130  
00085          LABEL RECORDS ARE STANDARD                               GC0130  
00086          RECORDING MODE IS V                                      GC0130  
00087          BLOCK CONTAINS  0  RECORDS.                              GC0130  
00088                                                                   GC0130  
00089  01  OUTPUT-RECORD                         PIC  X(459).           GC0130  
00090                                                                   GC0130  
00091  01  OUTPUT-LAST-KEY-RECORD.                                      GC0130  
00092      05  FILLER                            PIC  X(64).            GC0130  
00093      05  OUTPUT-KEY.                                              GC0130  
00094         10  OUT-LAST-KEY-ID                PIC  X(06).            GC0130  
00095         10  OUT-LAST-SLOT-NO               PIC S9(07)   COMP-3.   GC0130  
00096 /                                                                 GC0130  
00097                                                                   GC0130  
00098  01  OUTPUT-BEN-PROVISION-RECORD.                                 GC0130  
00099      05  FILLER                            PIC  X(64).            GC0130  
00100 /                                                                 GC0130  
00101 *    05  GCP-BEN-PROVN-RECORD.                                    GC0130  
00102      COPY  GCBENPVC.                                              GC0130  
00103 /                                                                 GC0130  
00104  WORKING-STORAGE SECTION.                                         GC0130  
00105                                                                   GC0130  
00106  77  FILLER                           PIC  X(27)    VALUE         GC0130  
00107                                     '* GC0130 WORKING STORAGE *'. GC0130  
00108                                                                   GC0130  
00109  01  WS-HOLD-AREAS.                                               GC0130  
00110      05  FILLER                       PIC  X(29)    VALUE         GC0130  
00111                                       '***  ABEND CODE  ***'.     GC0130  
00112      05  WS-ABEND-CODE                PIC  9(04)    VALUE 0  COMP.GC0130  
00113                                                                   GC0130  
00114                                                                   GC0130  
00115  01  WS-WORK-AREAS.                                               GC0130  
00116      05  FILLER                       PIC  X(28)    VALUE         GC0130  
00117                                       '***  WORK AREA  ***'.      GC0130  
00118      05  WS-LAST-ID                   PIC  X(06)    VALUE SPACES. GC0130  
00119      05  WS-LAST-SLOT       COMP-3    PIC S9(07)    VALUE +0.     GC0130  
00120      05  WS-INPUT-RELEASED-RECORDS    PIC  9(08)    VALUE ZEROES. GC0130  
00121      05  WS-INPUT-VSAM-RECORDS        PIC  9(08)    VALUE ZEROES. GC0130  
00122      05  WS-OUTPUT-RECORDS            PIC  9(08)    VALUE ZEROES. GC0130  
00123                                                                   GC0130  
00124                                                                   GC0130  
00125  01  WS-SWITCHES.                                                 GC0130  
00126      05  FILLER                       PIC  X(27)    VALUE         GC0130  
00127                                       '***  SWITCHES  ***'.       GC0130  
00128      05  WS-END-OF-INPUT-FILE-SW      PIC  X(01)    VALUE '0'.    GC0130  
00129          88  WS-END-OF-INPUT-FILE-SW-ON             VALUE '1'.    GC0130  
00130 /                                                                 GC0130  
00131                                                                   GC0130  
00132 ******************************************************************GC0130  
00133 **                                                                GC0130  
00134 **     PARAMETERS FOR THE BENEFIT PROVISION FILE        TSGVSAM1  GC0130  
00135 **                                                                GC0130  
00136 ******************************************************************GC0130  
00137  01  FILLER                        PIC  X(32)  VALUE              GC0130  
00138                        '***  BENEFIT PROVISION FILE  ***'.        GC0130  
00139                                                                   GC0130  
00140  01  BEN-PARM-SET.                                                GC0130  
00141      05 SET-VSAM-RDW.                                             GC0130  
00142         10 SET-VSAM-RECORD-LENGTH      PIC 9(04)     COMP.        GC0130  
00143         10 SET-VSAM-FEEDBACK-CODE      PIC 9(04)     COMP.        GC0130  
00144      05  SET-VSAM-VALUE                PIC 9(08)     COMP.        GC0130  
00145                                                                   GC0130  
00146                                                                   GC0130  
00147  01  BEN-PARM-ONE.                                                GC0130  
00148      05 RESERVED-FLDS                  PIC  9(08) VALUE 0  COMP.  GC0130  
00149      05 RESERVED-ONE      REDEFINES      RESERVED-FLDS.           GC0130  
00150         10 VSAM-REQUEST-TYPE           PIC  X(01).                GC0130  
00151         10 FILLER                      PIC  X(03).                GC0130  
00152                                                                   GC0130  
00153                                                                   GC0130  
00154  01  BEN-PARM-TWO.                                                GC0130  
00155      05 VSAM-RDW.                                                 GC0130  
00156         10 VSAM-RECORD-LENGTH          PIC  9(04)    COMP.        GC0130  
00157         10 VSAM-FEEDBACK-CODE          PIC  9(04)    COMP.        GC0130  
00158      05  VSAM-RECORD-AREA              PIC  X(395).               GC0130  
00159      05  VSAM-RECORD-A       REDEFINES     VSAM-RECORD-AREA.      GC0130  
00160          10 VSAM-KEY-FIELD.                                       GC0130  
00161             15  VSAM-KEY-ID            PIC  X(06).                GC0130  
00162             15  VSAM-KEY-SLOT          PIC S9(07)  COMP-3.        GC0130  
00163          10 VSAM-KEY-DATA.                                        GC0130  
00164             15  FILLER                 PIC  X(03).                GC0130  
00165             15  VSAM-BEN-COUNT         PIC S9(03)  COMP-3.        GC0130  
00166             15  FILLER                 PIC  X(380).               GC0130  
00167                                                                   GC0130  
00168 /                                                                 GC0130  
00169  PROCEDURE DIVISION.                                              GC0130  
00170                                                                   GC0130  
00171 ******************************************************************GC0130  
00172 **                                                                GC0130  
00173 **               P R O C E S S    C O N T R O L                   GC0130  
00174 **                                                                GC0130  
00175 ******************************************************************GC0130  
00176  0000-MAINLINE.                                                   GC0130  
00177                                                                   GC0130  
00178      PERFORM 1000-OPEN-THE-FILES  THRU  1000-EXIT.                GC0130  
00179                                                                   GC0130  
00180      PERFORM 2000-READ-AND-PROCESS-BEN-PROV  THRU  2000-EXIT      GC0130  
00181          UNTIL  WS-END-OF-INPUT-FILE-SW-ON.                       GC0130  
00182                                                                   GC0130  
00183      PERFORM 9000-CLOSE-THE-FILES  THRU  9000-EXIT.               GC0130  
00184                                                                   GC0130  
00185      STOP RUN.                                                    GC0130  
00186                                                                   GC0130  
00187  0000-EXIT.                                                       GC0130  
00188      EXIT.                                                        GC0130  
00189 /                                                                 GC0130  
00190 ******************************************************************GC0130  
00191 **                                                                GC0130  
00192 **                O P E N   T H E   F I L E S                     GC0130  
00193 **                                                                GC0130  
00194 ******************************************************************GC0130  
00195  1000-OPEN-THE-FILES.                                             GC0130  
00196                                                                   GC0130  
00197      OPEN INPUT  INPUT-FILE,                                      GC0130  
00198           OUTPUT OUTPUT-FILE.                                     GC0130  
00199                                                                   GC0130  
00200                                                                   GC0130  
00201      MOVE  'S'          TO  VSAM-REQUEST-TYPE.                    GC0130  
00202      MOVE   8           TO  SET-VSAM-RECORD-LENGTH.               GC0130  
00203      MOVE   3           TO  SET-VSAM-VALUE.                       GC0130  
00204      CALL  'TSGVSAM1'  USING  BEN-PARM-ONE   BEN-PARM-SET.        GC0130  
00205                                                                   GC0130  
00206      IF  VSAM-REQUEST-TYPE    NOT EQUAL   'S'                     GC0130  
00207          DISPLAY 'BAD SET IN GC0130    1000-OPEN-THE-FILES'       GC0130  
00208          MOVE  SET-VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE         GC0130  
00209          GO TO  9999-ERROR-RTN.                                   GC0130  
00210                                                                   GC0130  
00211  1000-EXIT.                                                       GC0130  
00212      EXIT.                                                        GC0130  
00213 /                                                                 GC0130  
00214 ******************************************************************GC0130  
00215 **                                                                GC0130  
00216 **   R E A D   T H E   S E Q U E N T I A L   I N P U T   F I L E  GC0130  
00217 **                                                                GC0130  
00218 ******************************************************************GC0130  
00219  2000-READ-AND-PROCESS-BEN-PROV.                                  GC0130  
00220                                                                   GC0130  
00221 **--READ THE SEQUENTIAL INPUT FILE.                               GC0130  
00222 **                                                                GC0130  
00223      READ INPUT-FILE                                              GC0130  
00224          AT END                                                   GC0130  
00225              MOVE '1'  TO   WS-END-OF-INPUT-FILE-SW               GC0130  
00226              DISPLAY '  '                                         GC0130  
00227              DISPLAY ' RELEASED RECORDS READ   =   '              GC0130  
00228                                 WS-INPUT-RELEASED-RECORDS         GC0130  
00229              DISPLAY ' VSAM RECORDS READ       =   '              GC0130  
00230                                     WS-INPUT-VSAM-RECORDS         GC0130  
00231              DISPLAY ' RECORDS WRITTEN         =   '              GC0130  
00232                                         WS-OUTPUT-RECORDS         GC0130  
00233              GO TO 2000-EXIT.                                     GC0130  
00234                                                                   GC0130  
00235                                                                   GC0130  
00236      ADD 1  TO   WS-INPUT-RELEASED-RECORDS.                       GC0130  
00237                                                                   GC0130  
00238 **--PROCESS  ALL  BENEIFT PROVISION RECORDS                       GC0130  
00239 **                                                                GC0130  
00240      IF WRK-BEN-PROV-ID    NOT EQUAL   WS-LAST-ID                 GC0130  
00241          MOVE  WRK-BEN-PROV-ID   TO  WS-LAST-ID                   GC0130  
00242          PERFORM 3000-PROCESS-BEN-PROV-RECORD THRU 3000-EXIT.     GC0130  
00243                                                                   GC0130  
00244  2000-EXIT.                                                       GC0130  
00245      EXIT.                                                        GC0130  
00246 /                                                                 GC0130  
00247 ******************************************************************GC0130  
00248 **                                                                GC0130  
00249 **          PROCESS BENEFIT PROVISION RECORDS                     GC0130  
00250 **                                                                GC0130  
00251 ******************************************************************GC0130  
00252  3000-PROCESS-BEN-PROV-RECORD.                                    GC0130  
00253                                                                   GC0130  
00254 **-- INITIALIZE LAST SLOT NUMBER                                  GC0130  
00255 **                                                                GC0130  
00256      MOVE  +10               TO  WS-LAST-SLOT.                    GC0130  
00257                                                                   GC0130  
00258                                                                   GC0130  
00259 **-- BUILD KEY FOR BENEIFIT PROVISION FILE.                       GC0130  
00260 **                                                                GC0130  
00261      MOVE  WRK-BEN-PROV-ID   TO  VSAM-KEY-ID.                     GC0130  
00262      MOVE  +11               TO  VSAM-KEY-SLOT.                   GC0130  
00263      MOVE  14                TO  VSAM-RECORD-LENGTH.              GC0130  
00264      MOVE  'P'               TO  VSAM-REQUEST-TYPE.               GC0130  
00265                                                                   GC0130  
00266      PERFORM 4000-LOCATE-BEGIN-PROVISION THRU 4000-EXIT.          GC0130  
00267                                                                   GC0130  
00268                                                                   GC0130  
00269 **--IF THE RECORD IS NOT ON THE FILE, CREATE A RECORD WITH        GC0130  
00270 *   THE TABULAR-ID AND ASSIGN A SLOT NUMBER OF 10.                GC0130  
00271 *                                                                 GC0130  
00272      IF  VSAM-REQUEST-TYPE    EQUAL  '2'                          GC0130  
00273          MOVE  LOW-VALUES        TO  OUTPUT-RECORD                GC0130  
00274          MOVE  WRK-BEN-PROV-ID   TO  OUT-LAST-KEY-ID              GC0130  
00275          MOVE  WS-LAST-SLOT      TO  OUT-LAST-SLOT-NO             GC0130  
00276          WRITE OUTPUT-LAST-KEY-RECORD                             GC0130  
00277          ADD 1  TO   WS-OUTPUT-RECORDS                            GC0130  
00278          GO TO 3000-EXIT.                                         GC0130  
00279                                                                   GC0130  
00280                                                                   GC0130  
00281                                                                   GC0130  
00282      PERFORM 5000-READNEXT-VSAM-RECORD THRU 5000-EXIT             GC0130  
00283          UNTIL   VSAM-KEY-ID   >   WS-LAST-ID.                    GC0130  
00284                                                                   GC0130  
00285                                                                   GC0130  
00286                                                                   GC0130  
00287 **--CREATE A RECORD FOR EACH TYPE OF TABULAR, CONTAINING          GC0130  
00288 **  THE VSAM-KEY-ID , (LAST) TAB-SLOT-NUMBER, AND LOW-VALUES      GC0130  
00289 **  IN THE BODY OF THE RECORD.                                    GC0130  
00290                                                                   GC0130  
00291      MOVE  LOW-VALUES        TO  OUTPUT-RECORD.                   GC0130  
00292      MOVE  WRK-BEN-PROV-ID   TO  OUT-LAST-KEY-ID.                 GC0130  
00293      MOVE  WS-LAST-SLOT      TO  OUT-LAST-SLOT-NO.                GC0130  
00294                                                                   GC0130  
00295      WRITE OUTPUT-LAST-KEY-RECORD.                                GC0130  
00296      ADD 1  TO   WS-OUTPUT-RECORDS.                               GC0130  
00297                                                                   GC0130  
00298  3000-EXIT.                                                       GC0130  
00299      EXIT.                                                        GC0130  
00300 /                                                                 GC0130  
00301 ******************************************************************GC0130  
00302 **                                                                GC0130  
00303 **    V S A M    B E N E F I T  P R O V I S I O N    F I L E      GC0130  
00304 **                                                                GC0130  
00305 ******************************************************************GC0130  
00306  4000-LOCATE-BEGIN-PROVISION.                                     GC0130  
00307                                                                   GC0130  
00308      CALL  'TSGVSAM1'  USING  BEN-PARM-ONE   BEN-PARM-TWO.        GC0130  
00309                                                                   GC0130  
00310                                                                   GC0130  
00311      IF  VSAM-REQUEST-TYPE    EQUAL  '2'                          GC0130  
00312          GO TO 4000-EXIT.                                         GC0130  
00313                                                                   GC0130  
00314                                                                   GC0130  
00315      IF  VSAM-REQUEST-TYPE    NOT EQUAL   'P'                     GC0130  
00316          DISPLAY 'BAD POINT-GC0130   4000-LOCATE-BEGIN-PROVISION' GC0130  
00317          MOVE  VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE             GC0130  
00318          GO TO  9999-ERROR-RTN.                                   GC0130  
00319                                                                   GC0130  
00320  4000-EXIT.                                                       GC0130  
00321      EXIT.                                                        GC0130  
00322 /                                                                 GC0130  
00323 ******************************************************************GC0130  
00324 **                                                                GC0130  
00325 **       R E A D    V S A M    T A B U L A R    F I L E           GC0130  
00326 **                                                                GC0130  
00327 ******************************************************************GC0130  
00328  5000-READNEXT-VSAM-RECORD.                                       GC0130  
00329                                                                   GC0130  
00330      MOVE  'G'          TO  VSAM-REQUEST-TYPE.                    GC0130  
00331      CALL  'TSGVSAM1'  USING  BEN-PARM-ONE   BEN-PARM-TWO.        GC0130  
00332                                                                   GC0130  
00333                                                                   GC0130  
00334 **--SET THE SWITCH IF END OF VSAM FILE                            GC0130  
00335 *                                                                 GC0130  
00336      IF  VSAM-REQUEST-TYPE    EQUAL  '2'                          GC0130  
00337          MOVE HIGH-VALUES  TO  WS-LAST-ID                         GC0130  
00338          GO TO 5000-EXIT.                                         GC0130  
00339                                                                   GC0130  
00340                                                                   GC0130  
00341      IF  VSAM-REQUEST-TYPE    NOT EQUAL   'G'                     GC0130  
00342          DISPLAY 'BAD GET  GC0130      5000-READNEXT-VSAM-RECORD' GC0130  
00343          MOVE  VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE             GC0130  
00344          GO TO  9999-ERROR-RTN.                                   GC0130  
00345                                                                   GC0130  
00346                                                                   GC0130  
00347      IF VSAM-KEY-ID    NOT EQUAL    WRK-BEN-PROV-ID               GC0130  
00348          GO TO 5000-EXIT.                                         GC0130  
00349                                                                   GC0130  
00350                                                                   GC0130  
00351      ADD  1                TO  WS-INPUT-VSAM-RECORDS.             GC0130  
00352      MOVE VSAM-KEY-SLOT    TO  WS-LAST-SLOT.                      GC0130  
00353      MOVE LOW-VALUES       TO  OUTPUT-RECORD.                     GC0130  
00354                                                                   GC0130  
00355      PERFORM 6000-WRITE-BEN-PROVN-RECORD   THRU  6000-EXIT.       GC0130  
00356                                                                   GC0130  
00357  5000-EXIT.                                                       GC0130  
00358      EXIT.                                                        GC0130  
00359 /                                                                 GC0130  
00360 ******************************************************************GC0130  
00361 **                                                                GC0130  
00362 **     CREATE A RECORD FOR EACH TYPE OF  \
00363 **                                                                GC0130  
00364 ******************************************************************GC0130  
00365  6000-WRITE-BEN-PROVN-RECORD.                                     GC0130  
00366                                                                   GC0130  
00367      MOVE  VSAM-BEN-COUNT     TO GCP-COUNT-TAB-PROVN-POINTERS.    GC0130  
00368      MOVE  VSAM-RECORD-AREA   TO GCP-BEN-PROVN-RECORD.            GC0130  
00369      WRITE OUTPUT-BEN-PROVISION-RECORD.                           GC0130  
00370      ADD 1  TO   WS-OUTPUT-RECORDS.                               GC0130  
00371                                                                   GC0130  
00372  6000-EXIT.                                                       GC0130  
00373      EXIT.                                                        GC0130  
00374 /                                                                 GC0130  
00375 ******************************************************************GC0130  
00376 **                                                                GC0130  
00377 **            C L O S E   T H E   F I L E S                       GC0130  
00378 **                                                                GC0130  
00379 ******************************************************************GC0130  
00380  9000-CLOSE-THE-FILES.                                            GC0130  
00381                                                                   GC0130  
00382      CLOSE INPUT-FILE,                                            GC0130  
00383            OUTPUT-FILE.                                           GC0130  
00384                                                                   GC0130  
00385                                                                   GC0130  
00386      MOVE     'C'           TO   VSAM-REQUEST-TYPE.               GC0130  
00387      CALL  'TSGVSAM1'  USING  BEN-PARM-ONE   BEN-PARM-TWO.        GC0130  
00388                                                                   GC0130  
00389      IF  VSAM-REQUEST-TYPE    NOT EQUAL   'C'                     GC0130  
00390          DISPLAY 'BAD CLOSE IN GC0130 AT 9000-CLOSE-THE-FILES'    GC0130  
00391          MOVE  VSAM-FEEDBACK-CODE   TO  WS-ABEND-CODE             GC0130  
00392          GO TO  9999-ERROR-RTN.                                   GC0130  
00393                                                                   GC0130  
00394  9000-EXIT.                                                       GC0130  
00395      EXIT.                                                        GC0130  
00396 /                                                                 GC0130  
00397 ******************************************************************GC0130  
00398 **                                                                GC0130  
00399 **                       A B E N D                                GC0130  
00400 **                                                                GC0130  
00401 ******************************************************************GC0130  
00402  9999-ERROR-RTN.                                                  GC0130  
00403                                                                   GC0130  
00404      CALL  'TSGEND' USING  WS-ABEND-CODE.                         GC0130  
00405                                                                   GC0130  
00406  9999-EXIT.                                                       GC0130  
00407      EXIT.                                                        GC0130  
00408 /                                                                 GC0130  
