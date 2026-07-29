00001 *      LAST MAINTENANCE TIME: 11.30.29  DATE: 11/05/85            08/21/03
00002  IDENTIFICATION DIVISION.                                         GC0360  
00003  PROGRAM-ID.    GC0360.                                              LV001
00004  AUTHOR.        LUCY TORRES.                                      GC0360  
00005  INSTALLATION.  HSCS - HCMS.                                      GC0360  
00006  DATE-WRITTEN.  OCTOBER 1985.                                     GC0360  
00007  DATE-COMPILED.                                                   GC0360  
00008                                                                   GC0360  
00009 ***************************************************************** GC0360  
00010 *  ABENDS 2040 AND 3040 ARE COMMENTED OUT AT THE TEST LEVEL DUE * GC0360  
00011 *  TO TROUBLE WITH TESTING. PROBABLY SHOULD BE UNCOMMENTED WHEN * GC0360  
00012 *  MOVING UP TO PRODUCTION.                                     * GC0360  
00013 ***************************************************************** GC0360  
00014 ***************************************************************** GC0360  
00015 *-CHG-NUM-* *-DATE-* *-WHO-*  *--------- DESCRIPTION ---------- * GC0360  
00016 *                                                               * GC0360  
00017 * 14726/    12/05/97   DAU    ADDED CODE TO SUPPORT THE YEAR    * GC0360  
00018 * 15057                       2000 AND THE EXPANSION OF THE     * GC0360  
00019 *                             GROUP SPECIFIC AND CONTRACT KEY   * GC0360  
00020 *                             TO SUPPORT THE TEXAS MERGER.      * GC0360  
00021 *                                                               * GC0360  
00022 * 01/25/95  JGR  CONTRACT RECORD EXPANSION.                     * GC0360  
00023 *                                                               * GC0360  
00024 *  1/17/95  EMS  CONVERTED TO COBOL II.                         * GC0360  
00025 *                                                               * GC0360  
00026 * D12009    10/17/91   FRY    INCREASE RECORD AREA IN WORKING   * GC0360  
00027 *                             STORAGE FOR THE CANCEL-RELEASE-   * GC0360  
00028 *                   FROM:                                       * GC0360  
00029 *                    10   C-BEN-PROVN-CNT    PIC S9(03) COMP-3. * GC0360  
00030 *                    10   C-TAB-PROVN-CNT    PIC S9(03) COMP-3. * GC0360  
00031 *                    10   FILLER             PIC  X(7712).      * GC0360  
00032 *                   TO:                                         * GC0360  
00033 *                    10   C-BEN-PROVN-CNT    PIC S9(05) COMP-3. * GC0360  
00034 *                    10   C-TAB-PROVN-CNT    PIC S9(03) COMP-3. * GC0360  
00035 *                    10   FILLER             PIC  X(7711).      * GC0360  
00036 *                                                               * GC0360  
00037 * 09/23/91    TPM    EXPANSION OF THE FAMILY-RELATION FIELD.    * GC0360  
00038 * D12009             REMOVE HARD CODED RECORD LENGTHS FOR       * GC0360  
00039 *                    GCDATES AND INSERTED COPYBOOK MEMBER       * GC0360  
00040 *                    GCCDRLEN  TO BE USED TO INCLUDE NEW RECORD * GC0360  
00041 *                    LENGTHS TO HANDLE THE EXPANSION IN THE     * GC0360  
00042 *                    FAMILY RELATION FIELD.                     * GC0360  
00043 *                                                               * GC0360  
00044 *                    ADJUSTED WRK-REC-DATA-GSAREA TO ACCOMM-    * GC0360  
00045 *                    DATE FOR THE  NEW FAMILY-RELATION FIELD.   * GC0360  
00046 *                    CHANGED THE RECORD LENGTH FROM 21 TO 22    * GC0360  
00047 *                    CHANGED THE RECORD LENGTH FROM 18 TO 19    * GC0360  
00048 *                    WHEN CALLING THE TSGVSAM ROUTINE.          * GC0360  
00049 ******************************************************************GC0360  
00050 *  D12009   09/10/91   GDM    INCREASE HOLD-FAM-REL-LVL TO  2   * GC0360  
00051 *                             POSITIONS                         * GC0360  
00052 *                                                               * GC0360  
00053 *  11154     3/07/91   FRY    INCREASE RECORD AREA IN WORKING   * GC0360  
00054 *                             STORAGE FOR THE CANCEL-RELEASE-   * GC0360  
00055 *                             DELETE FILE.                      * GC0360  
00056 *                 WRK-REC-DATA  PIC  X(5763)  CHANGED TO  7805. * GC0360  
00057 *                       FILLER  PIC  X(5670)  CHANGED TO  7712. * GC0360  
00058 *                       FILLER  PIC  X(5741)  CHANGED TO  7783. * GC0360  
00059 *                                                               * GC0360  
00060 * 11/15/88  RKH  REMOVE TERMINATION DATE FROM COMPARISSION OF THE GC0360  
00061 *                OCCURRANCE WHEN DELETION OF A OCCURRANCE.        GC0360  
00062 * 08/26/88  ENW  FIXED REFERENCE TO BENEFIT PROVISION DEPENDING   GC0360  
00063 *                ON COUNT. PROGRAM WAS USING TABULAR COUNT        GC0360  
00064 *                RATHER THAN THE BEN-PROV COUNT.                  GC0360  
00065 * 03-12-87  ENW  REMOVED CODE FOR VSAM-XREF UPDATE.               GC0360  
00066 *                                                                 GC0360  
00067 * 02-05-87  RKH  CHANGED NAME OF KEY FIELD DELETE                 GC0360  
00068 *                TO WRK-KEY-FLD-DEL-REQ                           GC0360  
00069 * 07-09-86  RKH  ALLOWED FOR 'K' KEY-FIELD DELETES                GC0360  
00070 *                TO BE DELETED.                                   GC0360  
00071 * 02-26-86  RKH  CHANGED ABEND 1024 TO A DISPLAY                  GC0360  
00072 *                STATEMENT.                                       GC0360  
00073 * 01-15-86  LET  REMOVED NUMERIC LITERAL FROM CONTRACT            GC0360  
00074 *                TABULAR LOOPING AND THE FIXED PORTION            GC0360  
00075 *                OF THE CONTRACT RECORD WAS EXPANDED              GC0360  
00076 *                FOR IMPLEMENTATION #9.                           GC0360  
00077 * 10/23/85  LET  INITIAL PROGRAM ADDED TO SYSTEM                  GC0360  
00078 *                                                                 GC0360  
00079 * 08-14-02   GTF   RECOMPILE FOR OPID EXPANSION                   GC0360  
00080 *                                                                 GC0360  
00080 * 09-09-09  JH   DM9441 /                                         GC0360  
00080 *                CHANGED WRK-REC-DATA FROM 8113 TO 31370.         GC0360  
00080 *                CHANGED FILLER IN WRK-REC-DATA-C                 GC0360  
00080 *                FROM 8006 TO 31263.                              GC0360  
00080 *                CHANGED FILLER IN WRK-REC-DATA-GS                GC0360  
00080 *                FROM 8077 TO 31334.                              GC0360  
00080 *                                                                 GC0360  
00081 ****************************************************************  GC0360  
00082 /                                                                 GC0360  
00083                                                                   GC0360  
00084 ****************************************************************  GC0360  
00085 *     THIS PROGRAM DELETES RECORDS OFF PRODUCTION FILES.  WHEN *  GC0360  
00086 * A CERTAIN CONTRACT OR GROUP SPECIFIC RECORD IS NO LONGER     *  GC0360  
00087 * REQUIRED IT WILL BE DELETED AS FOLLOWS.                      *  GC0360  
00088 *                                                              *  GC0360  
00089 * 1)  CONTRACT                                                 *  GC0360  
00090 *     A) RECORD ON THE CONTRACT FILE                           *  GC0360  
00091 *     B) OCCURANCE IN THE RECORD ON THE DATE FILE              *  GC0360  
00092 *                                                              *  GC0360  
00093 * 2)  GROUP SPECIFIC                                           *  GC0360  
00094 *     A) RECORD ON THE GROUP SPECIFIC FILE                     *  GC0360  
00095 *     B) OCCURANCE IN THE RECORD ON THE DATE FILE              *  GC0360  
00096 *                                                              *  GC0360  
00097 * ABEND CODES ARE AS FOLLOWS:                                  *  GC0360  
00098 *    1000 - SET ERROR  (TSGVSAM1 - DATE FILE)                  *  GC0360  
00099 *    1010 - OPEN ERROR                                         *  GC0360  
00100 *    1020 - READ ERROR                                         *  GC0360  
00101 *    1024 - RECORD FOUND BUT NO MATCHING OCCURANCE FOUND       *  GC0360  
00102 *    1030 - WRITE-ERROR                                        *  GC0360  
00103 *    1040 - DELETE ERROR                                       *  GC0360  
00104 *    1050 - CLOSE ERROR                                        *  GC0360  
00105 *    2000 - SET ERROR  (TSGVSAM2 - CONTRACT FILE)              *  GC0360  
00106 *    2010 - OPEN ERROR                                         *  GC0360  
00107 *    2040 - DELETE ERROR                                       *  GC0360  
00108 *    2050 - CLOSE ERROR                                        *  GC0360  
00109 *    3000 - SET ERROR  (TSGVSAM3 - GROUP SPECIFIC FILE)        *  GC0360  
00110 *    3010 - OPEN ERROR                                         *  GC0360  
00111 *    3040 - DELETE ERROR                                       *  GC0360  
00112 *    3050 - CLOSE ERROR                                        *  GC0360  
00113 ****************************************************************  GC0360  
00114                                                                   GC0360  
00115 /                                                                 GC0360  
00116  ENVIRONMENT DIVISION.                                            GC0360  
00117  CONFIGURATION SECTION.                                           GC0360  
00118  SOURCE-COMPUTER.  IBM-370.                                       GC0360  
00119  OBJECT-COMPUTER.  IBM-370.                                       GC0360  
00120  DATA DIVISION.                                                   GC0360  
00121 /                                                                 GC0360  
00122  WORKING-STORAGE SECTION.                                         GC0360  
00123  01  W-S                         PIC  X(24) VALUE                 GC0360  
00124      'WORKING STORAGE GC0360RL'.                                  GC0360  
00125  01  SW-ON                       PIC  X(01) VALUE '1'.            GC0360  
00126  01  SW-OFF                      PIC  X(01) VALUE '0'.            GC0360  
00127                                                                   GC0360  
00128  01  SWITCHES.                                                    GC0360  
00129      05  EOI-SW                  PIC  X(01) VALUE '0'.            GC0360  
00130          88  END-OF-INPUT                   VALUE '1'.            GC0360  
00131      05  DATE-MATCH-SW           PIC  X(01) VALUE '0'.            GC0360  
00132          88  DATE-MATCH-FOUND               VALUE '1'.            GC0360  
00133 /                                                                 GC0360  
00134 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GC0360  
00135 *               SET PARAMETER FOR ALL FILES                     * GC0360  
00136 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GC0360  
00137                                                                   GC0360  
00138  01  PARM-SET.                                                    GC0360  
00139      05  SET-RDW.                                                 GC0360  
00140          10  SET-REC-LEN         PIC  9(04) VALUE 8     COMP.     GC0360  
00141          10  SET-FEEDBACK-CODE   PIC  9(04) VALUE ZEROS COMP.     GC0360  
00142      05  SET-VALUE               PIC  9(08) VALUE 3     COMP.     GC0360  
00143                                                                   GC0360  
00144 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GC0360  
00145 *               PARAMETERS FOR THE DATE FILE                    * GC0360  
00146 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GC0360  
00147                                                                   GC0360  
00148  01  PARM-ONE-1.                                                  GC0360  
00149      05  RESERVED-1              PIC  9(05) COMP VALUE 0.         GC0360  
00150      05  RESERVED-1X REDEFINES RESERVED-1.                        GC0360  
00151          10  REQUEST-TYPE-1      PIC  X(01).                      GC0360  
00152                                                                   GC0360  
00153  01  PARM-TWO-1.                                                  GC0360  
00154      02  RDW-1.                                                   GC0360  
00155          10  REC-LEN-1           PIC  9(04) COMP VALUE 0.         GC0360  
00156          10  FEEDBACK-CODE-1     PIC  9(04) COMP VALUE 0.         GC0360  
00157      02  REC-AREA-1.                                              GC0360  
00158      COPY GCDATESC.                                               GC0360  
00159                                                                   GC0360  
00160 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GC0360  
00161 *               PARAMETERS FOR THE CONTRACT FILE                * GC0360  
00162 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GC0360  
00163                                                                   GC0360  
00164  01  PARM-ONE-2.                                                  GC0360  
00165      05  RESERVED-2              PIC  9(05) COMP VALUE 0.         GC0360  
00166      05  RESERVED-2X REDEFINES RESERVED-2.                        GC0360  
00167          10  REQUEST-TYPE-2      PIC  X(01).                      GC0360  
00168                                                                   GC0360  
00169  01  PARM-TWO-2.                                                  GC0360  
00170      02  RDW-2.                                                   GC0360  
00171          10  REC-LEN-2           PIC  9(04) COMP VALUE 0.         GC0360  
00172          10  FEEDBACK-CODE-2     PIC  9(04) COMP VALUE 0.         GC0360  
00173      02  REC-AREA-2.                                              GC0360  
00174      COPY GCCONTR2.                                               GC0360  
00175                                                                   GC0360  
00176 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GC0360  
00177 *             PARAMETERS FOR THE GROUP SPECIFIC FILE            * GC0360  
00178 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GC0360  
00179                                                                   GC0360  
00180  01  PARM-ONE-3.                                                  GC0360  
00181      05  RESERVED-3              PIC  9(05) COMP VALUE 0.         GC0360  
00182      05  RESERVED-3X REDEFINES RESERVED-3.                        GC0360  
00183          10  REQUEST-TYPE-3      PIC  X(01).                      GC0360  
00184                                                                   GC0360  
00185  01  PARM-TWO-3.                                                  GC0360  
00186      02  RDW-3.                                                   GC0360  
00187          10  REC-LEN-3           PIC  9(04) COMP VALUE 0.         GC0360  
00188          10  FEEDBACK-CODE-3     PIC  9(04) COMP VALUE 0.         GC0360  
00189      02  REC-AREA-3.                                              GC0360  
00190      COPY GCGROUP2.                                               GC0360  
00191                                                                   GC0360  
00192 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GC0360  
00193 *           PARAMETERS FOR THE CANCEL-RELEASE-DELETE FILE       * GC0360  
00194 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GC0360  
00195                                                                   GC0360  
00196  01  IOSW                        PIC  X(01)           VALUE '0'.  GC0360  
00197  01  IOFILE                      PIC  9(04) COMP SYNC VALUE ZEROS.GC0360  
00198  01  INLENGTH                    PIC  9(04) COMP SYNC VALUE ZEROS.GC0360  
00199  01  OUTLENGTH                   PIC  9(04) COMP SYNC VALUE ZEROS.GC0360  
00200  01  IAREA.                                                       GC0360  
00201      COPY GCWRKDCC.                                               GC0360  
00202      05  WRK-REC-DATA            PIC  X(31370).                   GC0360  
00203      05  WRK-REC-DATA-C  REDEFINES  WRK-REC-DATA.                 GC0360  
00204          10   FILLER             PIC  X(102).                     GC0360  
00205          10   C-BEN-PROVN-CNT    PIC S9(05) COMP-3.               GC0360  
00206          10   C-TAB-PROVN-CNT    PIC S9(03) COMP-3.               GC0360  
00207          10   FILLER             PIC  X(31263).                   GC0360  
00208      05  WRK-REC-DATA-GS  REDEFINES  WRK-REC-DATA.                GC0360  
00209          10   FILLER             PIC  X(34).                      GC0360  
00210          10   GS-TAB-PROVN-CNT   PIC S9(03) COMP-3.               GC0360  
00211          10   FILLER             PIC  X(31334).                   GC0360  
00212  01  OAREA                       PIC  X(01).                      GC0360  
00213  01  INPDDNAME                   PIC  X(08) VALUE 'GC0360A '.     GC0360  
00214  01  OUTDDNAME                   PIC  X(08) VALUE 'BALXOIFL'.     GC0360  
00215 /                                                                 GC0360  
00216 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GC0360  
00217 *          HOLD AREA, WORK AREA,                                * GC0360  
00218 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GC0360  
00219                                                                   GC0360  
00220  01  ABEND-CODE                  PIC  9(04) VALUE ZEROS COMP.     GC0360  
00221  01  TOTAL-TABS                  PIC  9(02) VALUE ZEROS COMP-3.   GC0360  
00222  01  TOTAL-DATES                 PIC  9(04) VALUE ZEROS COMP-3.   GC0360  
00223  01  NEW-REC-LEN                 PIC  9(04) VALUE ZEROS COMP-3.   GC0360  
00224                                                                   GC0360  
00225  01  HOLD-PROV-CTL               PIC  X(02) VALUE SPACES.         GC0360  
00226  01  HOLD-FAM-REL-LVL            PIC  X(02) VALUE SPACES.         GC0360  
00227  01  HOLD-EFFDT-CEN              PIC S9(07) VALUE ZEROS COMP-3.   GC0360  
00228  01  HOLD-TERMDT-CEN             PIC S9(07) VALUE ZEROS COMP-3.   GC0360  
00229  01  HOLD-DATE-PTR               PIC  9(03) VALUE ZEROS COMP-3.   GC0360  
00230 /                                                                 GC0360  
00231  01  WS-REC-LENGTHS.                                              GC0360  
00232      COPY GCCDRLEN.                                               GC0360  
00233 /                                                                 GC0360  
00234  01  HOLD-WRK-CONTR.                                              GC0360  
00235      COPY GCCONTRC.                                               GC0360  
00236 /                                                                 GC0360  
00237  01  HOLD-WRK-GRP-SPEC.                                           GC0360  
00238      COPY GCGROUPC.                                               GC0360  
00239 /                                                                 GC0360  
00240  PROCEDURE DIVISION.                                              GC0360  
00241                                                                   GC0360  
00242  0000-MAINLINE.                                                   GC0360  
00243                                                                   GC0360  
00244      PERFORM 1000-OPEN-VSAM-FILES THRU 1000-EXIT.                 GC0360  
00245                                                                   GC0360  
00246      PERFORM 2000-PROCESS-INPUT THRU 2000-EXIT                    GC0360  
00247          UNTIL END-OF-INPUT.                                      GC0360  
00248                                                                   GC0360  
00249      PERFORM 3000-CLOSE-VSAM-FILES THRU 3000-EXIT.                GC0360  
00250                                                                   GC0360  
00251      GOBACK.                                                      GC0360  
00252                                                                   GC0360  
00253  0000-EXIT.  EXIT.                                                GC0360  
00254 /                                                                 GC0360  
00255  1000-OPEN-VSAM-FILES.                                            GC0360  
00256                                                                   GC0360  
00257      MOVE 'S'  TO  REQUEST-TYPE-1.                                GC0360  
00258      CALL 'TSGVSAM1'  USING PARM-ONE-1, PARM-SET.                 GC0360  
00259      IF REQUEST-TYPE-1  NOT  =  'S'                               GC0360  
00260          DISPLAY ' FEEDBACK CODE IS ' SET-FEEDBACK-CODE           GC0360  
00261          MOVE 1000  TO  ABEND-CODE                                GC0360  
00262          PERFORM 9999-ABEND.                                      GC0360  
00263                                                                   GC0360  
00264      MOVE 'O'  TO  REQUEST-TYPE-1.                                GC0360  
00265      CALL 'TSGVSAM1'  USING PARM-ONE-1, PARM-TWO-1.               GC0360  
00266      IF REQUEST-TYPE-1  NOT  =  'O'                               GC0360  
00267          DISPLAY ' FEEDBACK CODE IS ' FEEDBACK-CODE-1             GC0360  
00268          MOVE 1010  TO  ABEND-CODE                                GC0360  
00269          PERFORM 9999-ABEND.                                      GC0360  
00270                                                                   GC0360  
00271      MOVE 'S'  TO  REQUEST-TYPE-2.                                GC0360  
00272      CALL 'TSGVSAM2'  USING PARM-ONE-2, PARM-SET.                 GC0360  
00273      IF REQUEST-TYPE-2  NOT  =  'S'                               GC0360  
00274          DISPLAY ' FEEDBACK CODE IS ' SET-FEEDBACK-CODE           GC0360  
00275          MOVE 2000  TO  ABEND-CODE                                GC0360  
00276          PERFORM 9999-ABEND.                                      GC0360  
00277                                                                   GC0360  
00278      MOVE 'O'  TO  REQUEST-TYPE-2.                                GC0360  
00279      CALL 'TSGVSAM2'  USING PARM-ONE-2, PARM-TWO-2.               GC0360  
00280      IF REQUEST-TYPE-2  NOT  =  'O'                               GC0360  
00281          DISPLAY ' FEEDBACK CODE IS ' FEEDBACK-CODE-2             GC0360  
00282          MOVE 2010  TO  ABEND-CODE                                GC0360  
00283          PERFORM 9999-ABEND.                                      GC0360  
00284                                                                   GC0360  
00285      MOVE 'S'  TO  REQUEST-TYPE-3.                                GC0360  
00286      CALL 'TSGVSAM3'  USING PARM-ONE-3, PARM-SET.                 GC0360  
00287      IF REQUEST-TYPE-3  NOT  =  'S'                               GC0360  
00288          DISPLAY ' FEEDBACK CODE IS ' SET-FEEDBACK-CODE           GC0360  
00289          MOVE 3000  TO  ABEND-CODE                                GC0360  
00290          PERFORM 9999-ABEND.                                      GC0360  
00291                                                                   GC0360  
00292      MOVE 'O'  TO  REQUEST-TYPE-3.                                GC0360  
00293      CALL 'TSGVSAM3'  USING PARM-ONE-3, PARM-TWO-3.               GC0360  
00294      IF REQUEST-TYPE-3  NOT  =  'O'                               GC0360  
00295          DISPLAY ' FEEDBACK CODE IS ' FEEDBACK-CODE-3             GC0360  
00296          MOVE 3010  TO  ABEND-CODE                                GC0360  
00297          PERFORM 9999-ABEND.                                      GC0360  
00298                                                                   GC0360  
00299                                                                   GC0360  
00300  1000-EXIT.  EXIT.                                                GC0360  
00301 /                                                                 GC0360  
00302  2000-PROCESS-INPUT.                                              GC0360  
00303                                                                   GC0360  
00304      PERFORM 9100-READ-INPUT THRU 9100-EXIT.                      GC0360  
00305                                                                   GC0360  
00306      IF (WRK-DELETE-REQUEST OR                                    GC0360  
00307          WRK-KEY-FLD-DEL-REQ)                                     GC0360  
00308          NEXT SENTENCE                                            GC0360  
00309      ELSE                                                         GC0360  
00310          GO TO 2000-EXIT.                                         GC0360  
00311                                                                   GC0360  
00312      IF WRK-REC-CONT-CON                                          GC0360  
00313          PERFORM 2100-CONTRACT-DELETE-PROCESS THRU 2100-EXIT      GC0360  
00314          GO TO 2000-EXIT.                                         GC0360  
00315                                                                   GC0360  
00316      IF WRK-REC-GROUP-SPEC-CTL                                    GC0360  
00317          PERFORM 2200-GROUP-SPEC-DELETE-PROCESS THRU 2200-EXIT.   GC0360  
00318                                                                   GC0360  
00319  2000-EXIT.  EXIT.                                                GC0360  
00320 /                                                                 GC0360  
00321  2100-CONTRACT-DELETE-PROCESS.                                    GC0360  
00322                                                                   GC0360  
00323      PERFORM 9100-READ-INPUT THRU 9100-EXIT.                      GC0360  
00324                                                                   GC0360  
00325      IF END-OF-INPUT                                              GC0360  
00326          GO TO 2100-EXIT.                                         GC0360  
00327                                                                   GC0360  
00328      IF WRK-REC-CONT                                              GC0360  
00329          NEXT SENTENCE                                            GC0360  
00330      ELSE                                                         GC0360  
00331          GO TO 2100-EXIT.                                         GC0360  
00332                                                                   GC0360  
00333      DISPLAY 'CONT KEY IS'    GCT-CONTRACT-ID.                    GC0360  
00334      MOVE GC-GCDATES-VARY-MAX-OCUR TO DTE-ENTRY-COUNT.            GC0360  
00335      MOVE LOW-VALUES          TO  DTE-RECORD.                     GC0360  
00336      MOVE C-BEN-PROVN-CNT     TO  GCT-COUNT-BEN-PROVN-POINTERS.   GC0360  
00337      MOVE WRK-REC-DATA        TO  HOLD-WRK-CONTR.                 GC0360  
00338      MOVE GCT-PLAN-CODE       TO  DTE-CONTR-PLAN-CODE.            GC0360  
00339      MOVE GCT-GROUP-NUM       TO  DTE-CONTR-GROUP-NUM.            GC0360  
00340      MOVE GCT-SECTION-NUM     TO  DTE-CONTR-SECTION-NUM.          GC0360  
00341      MOVE GCT-PKG-CODE        TO  DTE-CONTR-PKG-CODE.             GC0360  
00342      MOVE GCT-L-O-B           TO  DTE-CONTR-L-O-B.                GC0360  
00343      MOVE 'C'                 TO  DTE-FILE-REF-IND.               GC0360  
00344      MOVE GCT-PROVDR-CONTROL  TO  HOLD-PROV-CTL.                  GC0360  
00345      MOVE GCT-FAM-REL-LVL     TO  HOLD-FAM-REL-LVL.               GC0360  
00346      MOVE GCT-EFFDT-CEN       TO  HOLD-EFFDT-CEN.                 GC0360  
00347      MOVE GCT-TERMDT-CEN      TO  HOLD-TERMDT-CEN.                GC0360  
00348      MOVE GC-GCCONTR-VARY-MAX-OCUR TO                             GC0360  
00349                               GCT2-COUNT-BEN-PROVN-POINTERS.      GC0360  
00350      MOVE LOW-VALUES          TO  GCT2-CONTRACT-RECORD.           GC0360  
00351      MOVE GCT-CONTRACT-ID     TO  GCT2-CONTRACT-ID.               GC0360  
00352                                                                   GC0360  
00353      PERFORM 9200-SEARCH-N-DELETE-DATE-FILE THRU 9200-EXIT.       GC0360  
00354                                                                   GC0360  
00355      PERFORM 2110-DELETE-CONTRACT-REC THRU 2110-EXIT.             GC0360  
00356                                                                   GC0360  
00357  2100-EXIT.  EXIT.                                                GC0360  
00358 /                                                                 GC0360  
00359 ****************************************************************  GC0360  
00360 *     DELETE THE SPECIFIED CONTRACT RECORD OFF OF THE CONTRACT *  GC0360  
00361 * FILE.  IF THIS DELETION FAILS, THE PROGRAM WILL ABEND.       *  GC0360  
00362 ****************************************************************  GC0360  
00363                                                                   GC0360  
00364  2110-DELETE-CONTRACT-REC.                                        GC0360  
00365                                                                   GC0360  
00366      MOVE 33   TO  REC-LEN-2.                                     GC0360  
00367      MOVE 'D'  TO  REQUEST-TYPE-2.                                GC0360  
00368      CALL 'TSGVSAM2'  USING  PARM-ONE-2, PARM-TWO-2.              GC0360  
00369                                                                   GC0360  
00370  2110-EXIT.  EXIT.                                                GC0360  
00371                                                                   GC0360  
00372 /                                                                 GC0360  
00373  2200-GROUP-SPEC-DELETE-PROCESS.                                  GC0360  
00374                                                                   GC0360  
00375      PERFORM 9100-READ-INPUT THRU 9100-EXIT.                      GC0360  
00376                                                                   GC0360  
00377      IF END-OF-INPUT                                              GC0360  
00378          GO TO 2200-EXIT.                                         GC0360  
00379                                                                   GC0360  
00380      IF WRK-REC-GROUP-SPEC                                        GC0360  
00381          NEXT SENTENCE                                            GC0360  
00382      ELSE                                                         GC0360  
00383          GO TO 2200-EXIT.                                         GC0360  
00384                                                                   GC0360  
00385      DISPLAY 'GROUP KEY IS'   GCG-GRP-SPECIF-ID.                  GC0360  
00386      MOVE GC-GCDATES-VARY-MAX-OCUR TO DTE-ENTRY-COUNT.            GC0360  
00387      MOVE LOW-VALUES          TO  DTE-RECORD.                     GC0360  
00388      MOVE GS-TAB-PROVN-CNT    TO  GCG-COUNT-TAB-PROVN-POINTERS.   GC0360  
00389      MOVE WRK-REC-DATA        TO  HOLD-WRK-GRP-SPEC.              GC0360  
00390      MOVE GCG-PLAN-CODE       TO  DTE-GROUP-PLAN-CODE.            GC0360  
00391      MOVE GCG-GROUP-NUM       TO  DTE-GROUP-GROUP-NUM.            GC0360  
00392      MOVE GCG-SECTION-NUM     TO  DTE-GROUP-SECTION-NUM.          GC0360  
00393      MOVE GCG-PKG-CODE        TO  DTE-GROUP-PKG-CODE.             GC0360  
00394      MOVE LOW-VALUES          TO  DTE-GRPSPC-FILLER.              GC0360  
00395      MOVE 'G'                 TO  DTE-FILE-REF-IND.               GC0360  
00396      MOVE GCG-FAM-REL-LVL     TO  HOLD-FAM-REL-LVL.               GC0360  
00397      MOVE GCG-EFFDT-CEN       TO  HOLD-EFFDT-CEN.                 GC0360  
00398      MOVE GCG-TERMDT-CEN      TO  HOLD-TERMDT-CEN.                GC0360  
00399      MOVE GC-GCGRPSPC-VARY-MAX-OCUR TO                            GC0360  
00400                               GCG2-COUNT-TAB-PROVN-POINTERS.      GC0360  
00401      MOVE LOW-VALUES          TO  GCG2-GRP-SPEC-RECORD.           GC0360  
00402      MOVE GCG-GRP-SPECIF-ID   TO  GCG2-GRP-SPECIF-ID.             GC0360  
00403                                                                   GC0360  
00404      PERFORM 9200-SEARCH-N-DELETE-DATE-FILE THRU 9200-EXIT.       GC0360  
00405                                                                   GC0360  
00406      PERFORM 2210-DELETE-GRP-SPEC-REC THRU 2210-EXIT.             GC0360  
00407                                                                   GC0360  
00408  2200-EXIT.  EXIT.                                                GC0360  
00409 /                                                                 GC0360  
00410 ****************************************************************  GC0360  
00411 *     DELETE THE SPECIFIED GROUP SPECIFIC RECORD OFF THE GROUP *  GC0360  
00412 * SPECIFIC FILE.  IF THE DELETION FAILS, THE PROGRAM WILL ABEND*  GC0360  
00413 ****************************************************************  GC0360  
00414                                                                   GC0360  
00415  2210-DELETE-GRP-SPEC-REC.                                        GC0360  
00416                                                                   GC0360  
00417      MOVE 30   TO  REC-LEN-3.                                     GC0360  
00418      MOVE 'D'  TO  REQUEST-TYPE-3.                                GC0360  
00419      CALL 'TSGVSAM3'  USING  PARM-ONE-3, PARM-TWO-3.              GC0360  
00420                                                                   GC0360  
00421                                                                   GC0360  
00422  2210-EXIT.  EXIT.                                                GC0360  
00423                                                                   GC0360  
00424 /                                                                 GC0360  
00425  3000-CLOSE-VSAM-FILES.                                           GC0360  
00426                                                                   GC0360  
00427      MOVE 'C'  TO  REQUEST-TYPE-1.                                GC0360  
00428      CALL 'TSGVSAM1'  USING PARM-ONE-1, PARM-TWO-1.               GC0360  
00429      IF REQUEST-TYPE-1  NOT  =  'C'                               GC0360  
00430          DISPLAY 'FEEDBACK CODE IS ' FEEDBACK-CODE-1              GC0360  
00431          MOVE 1050  TO  ABEND-CODE                                GC0360  
00432          PERFORM 9999-ABEND.                                      GC0360  
00433                                                                   GC0360  
00434      MOVE 'C'  TO  REQUEST-TYPE-2.                                GC0360  
00435      CALL 'TSGVSAM2'  USING PARM-ONE-2, PARM-TWO-2.               GC0360  
00436      IF REQUEST-TYPE-2  NOT  =  'C'                               GC0360  
00437          DISPLAY 'FEEDBACK CODE IS ' FEEDBACK-CODE-2              GC0360  
00438          MOVE 2050  TO  ABEND-CODE                                GC0360  
00439          PERFORM 9999-ABEND.                                      GC0360  
00440                                                                   GC0360  
00441      MOVE 'C'  TO  REQUEST-TYPE-3.                                GC0360  
00442      CALL 'TSGVSAM3'  USING PARM-ONE-3, PARM-TWO-3.               GC0360  
00443      IF REQUEST-TYPE-3  NOT  =  'C'                               GC0360  
00444          DISPLAY 'FEEDBACK CODE IS ' FEEDBACK-CODE-3              GC0360  
00445          MOVE 3050  TO  ABEND-CODE                                GC0360  
00446          PERFORM 9999-ABEND.                                      GC0360  
00447                                                                   GC0360  
00448  3000-EXIT.  EXIT.                                                GC0360  
00449 /                                                                 GC0360  
00450  9100-READ-INPUT.                                                 GC0360  
00451      MOVE '1'    TO  IOSW.                                        GC0360  
00452      MOVE ZEROS  TO  IOFILE.                                      GC0360  
00453                                                                   GC0360  
00454      CALL 'BALX' USING  IOSW                                      GC0360  
00455                         IOFILE                                    GC0360  
00456                         IAREA                                     GC0360  
00457                         INLENGTH                                  GC0360  
00458                         OAREA                                     GC0360  
00459                         OUTLENGTH                                 GC0360  
00460                         INPDDNAME                                 GC0360  
00461                         OUTDDNAME.                                GC0360  
00462                                                                   GC0360  
00463      IF IOSW  =  '5'                                              GC0360  
00464          MOVE SW-ON  TO  EOI-SW.                                  GC0360  
00465                                                                   GC0360  
00466  9100-EXIT.  EXIT.                                                GC0360  
00467 /                                                                 GC0360  
00468 ****************************************************************  GC0360  
00469 *     READ THE DATE FILE FOR THE SPECIFIED RECORD AND THEN     *  GC0360  
00470 * SEARCH THE OCCURANCES FOR THE CORRECT ENTRY.  AFTER THE ENTRY*  GC0360  
00471 * IS FOUND ONE OF THE FOLLOWING WILL OCCUR.                    *  GC0360  
00472 *      1)  IF IT'S THE ONLY REAL OCCURANCE ON THE RECORD, THE  *  GC0360  
00473 *             RECORD WILL BE DELETED FROM THE FILE.            *  GC0360  
00474 *                                                              *  GC0360  
00475 *      2)  THE ENTRY WILL BE WIPED OUT BY MOVING ALL OF THE    *  GC0360  
00476 *             OCCURANCES BELOW THIS ONE UP ONE POSITION, AND   *  GC0360  
00477 *             THE RECORD WILL BE UPDATE TO THE FILE.           *  GC0360  
00478 *                                                              *  GC0360  
00479 *      3)  IF THE RECORD IS NOT FOUND THE PROGRAM WILL ABEND.  *  GC0360  
00480 *          IF THE OCCURANCE IS NOT FOUND THE RECORD WILL BE    *  GC0360  
00481 *             DISPLAYED FOR PROGRAMMERS USE.                   *  GC0360  
00482 ****************************************************************  GC0360  
00483                                                                   GC0360  
00484  9200-SEARCH-N-DELETE-DATE-FILE.                                  GC0360  
00485                                                                   GC0360  
00486      PERFORM 9210-READ-DATE-FILE THRU 9210-EXIT.                  GC0360  
00487                                                                   GC0360  
00488      MOVE ZEROS   TO  HOLD-DATE-PTR                               GC0360  
00489      MOVE SW-OFF  TO  DATE-MATCH-SW.                              GC0360  
00490      COMPUTE TOTAL-DATES  =  DTE-ENTRY-COUNT  -  1.               GC0360  
00491                                                                   GC0360  
00492      PERFORM 9220-CHECK-OCCURANCES THRU 9220-EXIT                 GC0360  
00493         VARYING DTE-INDEX FROM 1 BY 1                             GC0360  
00494         UNTIL   DTE-INDEX > TOTAL-DATES                           GC0360  
00495              OR                                                   GC0360  
00496                 DATE-MATCH-FOUND.                                 GC0360  
00497                                                                   GC0360  
00498      IF DATE-MATCH-FOUND                                          GC0360  
00499          IF DTE-ENTRY-COUNT  =  2                                 GC0360  
00500              PERFORM 9230-DELETE-DATE-RECORD THRU 9230-EXIT       GC0360  
00501              GO TO 9200-EXIT                                      GC0360  
00502          ELSE                                                     GC0360  
00503              PERFORM 9240-WIPE-OUT-OCCURANCE THRU 9240-EXIT       GC0360  
00504      ELSE                                                         GC0360  
00505          DISPLAY ' DATE OCCURANCE MISSING FOR ',                  GC0360  
00506          DTE-RECORD-KEY                                           GC0360  
00507          GO TO 9200-EXIT.                                         GC0360  
00508                                                                   GC0360  
00509      PERFORM 9250-UPDATE-DATE-FILE THRU 9250-EXIT.                GC0360  
00510                                                                   GC0360  
00511  9200-EXIT.  EXIT.                                                GC0360  
00512 /                                                                 GC0360  
00513  9210-READ-DATE-FILE.                                             GC0360  
00514                                                                   GC0360  
00515      MOVE 36   TO  REC-LEN-1.                                     GC0360  
00516      MOVE 'R'  TO  REQUEST-TYPE-1.                                GC0360  
00517      CALL 'TSGVSAM1'  USING  PARM-ONE-1, PARM-TWO-1.              GC0360  
00518                                                                   GC0360  
00519      IF REQUEST-TYPE-1  NOT  =  'R'                               GC0360  
00520          DISPLAY ' FEEDBACK CODE IS ' FEEDBACK-CODE-1             GC0360  
00521          MOVE 1020  TO  ABEND-CODE                                GC0360  
00522          PERFORM 9999-ABEND.                                      GC0360  
00523                                                                   GC0360  
00524  9210-EXIT.  EXIT.                                                GC0360  
00525                                                                   GC0360  
00526 /                                                                 GC0360  
00527                                                                   GC0360  
00528  9220-CHECK-OCCURANCES.                                           GC0360  
00529                                                                   GC0360  
00530      IF WRK-REC-GROUP-SPEC                                        GC0360  
00531         PERFORM 9221-CHECK-GRP-OCCURS  THRU  9221-EXIT.           GC0360  
00532                                                                   GC0360  
00533      IF WRK-REC-CONT                                              GC0360  
00534         PERFORM 9223-CHECK-CONT-OCCURS  THRU  9223-EXIT.          GC0360  
00535                                                                   GC0360  
00536  9220-EXIT.  EXIT.                                                GC0360  
00537 /                                                                 GC0360  
00538  9221-CHECK-GRP-OCCURS.                                           GC0360  
00539      IF DTE-EFFDT-CEN (DTE-INDEX)    =  HOLD-EFFDT-CEN  AND       GC0360  
00540         DTE-FAMILY-RELAT-LEVEL (DTE-INDEX)  =  HOLD-FAM-REL-LVL   GC0360  
00541            SET HOLD-DATE-PTR  TO  DTE-INDEX                       GC0360  
00542            MOVE SW-ON         TO  DATE-MATCH-SW.                  GC0360  
00543                                                                   GC0360  
00544  9221-EXIT.  EXIT.                                                GC0360  
00545                                                                   GC0360  
00546  9223-CHECK-CONT-OCCURS.                                          GC0360  
00547      IF DTE-EFFDT-CEN (DTE-INDEX)       =  HOLD-EFFDT-CEN  AND    GC0360  
00548         DTE-PROVIDER-CNTRL (DTE-INDEX)  =  HOLD-PROV-CTL   AND    GC0360  
00549         DTE-FAMILY-RELAT-LEVEL (DTE-INDEX)  =  HOLD-FAM-REL-LVL   GC0360  
00550            SET HOLD-DATE-PTR  TO  DTE-INDEX                       GC0360  
00551            MOVE SW-ON         TO  DATE-MATCH-SW.                  GC0360  
00552                                                                   GC0360  
00553  9223-EXIT.  EXIT.                                                GC0360  
00554 /                                                                 GC0360  
00555                                                                   GC0360  
00556  9230-DELETE-DATE-RECORD.                                         GC0360  
00557                                                                   GC0360  
00558      MOVE 'D'  TO  REQUEST-TYPE-1.                                GC0360  
00559      CALL 'TSGVSAM1'  USING  PARM-ONE-1, PARM-TWO-1.              GC0360  
00560                                                                   GC0360  
00561      IF REQUEST-TYPE-1  NOT  =  'D'                               GC0360  
00562          DISPLAY 'FEEDBACK CODE IS ' FEEDBACK-CODE-1              GC0360  
00563          MOVE 1040  TO  ABEND-CODE                                GC0360  
00564          PERFORM 9999-ABEND.                                      GC0360  
00565                                                                   GC0360  
00566  9230-EXIT.  EXIT.                                                GC0360  
00567 /                                                                 GC0360  
00568  9240-WIPE-OUT-OCCURANCE.                                         GC0360  
00569                                                                   GC0360  
00570      IF HOLD-DATE-PTR  =  TOTAL-DATES                             GC0360  
00571          SET DTE-INDEX     TO  HOLD-DATE-PTR                      GC0360  
00572          MOVE HIGH-VALUES  TO  DTE-EFF-TERM (DTE-INDEX)           GC0360  
00573      ELSE                                                         GC0360  
00574          PERFORM 9242-MOVE-UP-OCCURANCES THRU 9242-EXIT           GC0360  
00575             VARYING DTE-INDEX FROM HOLD-DATE-PTR BY 1             GC0360  
00576             UNTIL   DTE-INDEX > TOTAL-DATES.                      GC0360  
00577                                                                   GC0360  
00578      MOVE TOTAL-DATES TO  DTE-ENTRY-COUNT.                        GC0360  
00579                                                                   GC0360  
00580  9240-EXIT.  EXIT.                                                GC0360  
00581                                                                   GC0360  
00582  9242-MOVE-UP-OCCURANCES.                                         GC0360  
00583                                                                   GC0360  
00584      SET  DTE-INDEX2                 TO  DTE-INDEX.               GC0360  
00585      SET  DTE-INDEX2                 UP  BY  +1.                  GC0360  
00586      MOVE DTE-EFF-TERM (DTE-INDEX2)  TO  DTE-EFF-TERM (DTE-INDEX).GC0360  
00587                                                                   GC0360  
00588  9242-EXIT.  EXIT.                                                GC0360  
00589                                                                   GC0360  
00590  9250-UPDATE-DATE-FILE.                                           GC0360  
00591                                                                   GC0360  
00592      COMPUTE NEW-REC-LEN  =  4 + GC-GCDATES-FIXED-LEN +           GC0360  
00593                 (DTE-ENTRY-COUNT * GC-GCDATES-VARY-LEN).          GC0360  
00594      MOVE NEW-REC-LEN  TO  REC-LEN-1.                             GC0360  
00595      MOVE 'W'          TO  REQUEST-TYPE-1.                        GC0360  
00596      CALL 'TSGVSAM1'  USING  PARM-ONE-1, PARM-TWO-1.              GC0360  
00597                                                                   GC0360  
00598      IF REQUEST-TYPE-1  NOT  =  'W'                               GC0360  
00599          DISPLAY 'FEEDBACK CODE IS ' FEEDBACK-CODE-1              GC0360  
00600          MOVE 1030  TO  ABEND-CODE                                GC0360  
00601          PERFORM 9999-ABEND.                                      GC0360  
00602                                                                   GC0360  
00603  9250-EXIT.  EXIT.                                                GC0360  
00604 /                                                                 GC0360  
00605  9999-ABEND.                                                      GC0360  
00606                                                                   GC0360  
00607      CALL 'TSGEND'  USING ABEND-CODE.                             GC0360  
00608                                                                   GC0360  
00609  9999-EXIT.  EXIT.                                                GC0360  
