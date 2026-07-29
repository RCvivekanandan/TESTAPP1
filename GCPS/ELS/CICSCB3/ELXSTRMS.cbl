00001  ID DIVISION.                                                     06/29/02
00002 ********* THIS IS A COBOL/2 PROGRAM ******                        ELXSTRMS
00003  PROGRAM-ID.     ELXSTRMS.                                           LV001
00004  AUTHOR.         DIANE FLOWERS.                                   ELXSTRMS
00005  DATE-WRITTEN.   03/02/00.                                        ELXSTRMS
00006  DATE-COMPILED.                                                   ELXSTRMS
00007 ******************************************************************ELXSTRMS
00008 *                                                                *ELXSTRMS
00009 *        M A I N T E N A N C E     L O G                         *ELXSTRMS
00010 *                                                                *ELXSTRMS
00011 *                                                                *ELXSTRMS
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELXSTRMS
00013 *                                                                *ELXSTRMS
00014 * 16614      03/02/00  DAF  CREATED SCREEN TO TEST BLUESTORM     *ELXSTRMS
00015 *                                                                *ELXSTRMS
00016 /*****************************************************************ELXSTRMS
00017 *      P R O G R A M   N A R R A T I V E                         *ELXSTRMS
00018 ******************************************************************ELXSTRMS
00019 *                                                                *ELXSTRMS
00020 *   TRANSID:      ELBS                                           *ELXSTRMS
00021 *   PROGRAM:      ELXSTRMS                                       *ELXSTRMS
00022 *   MAPSET:       ELBSSETC                                       *ELXSTRMS
00023 *                                                                *ELXSTRMS
00024 *                                                                *ELXSTRMS
00025 *   PURPOSE:   A) TO ALLOW BLUESTORM TESTING WITHOUT GOING       *ELXSTRMS
00026 *              THROUGH THE INTERNET.                             *ELXSTRMS
00027 *                                                                *ELXSTRMS
00028 *   FUNCTIONS: THIS MODULE IS DESIGNED SO THAT WHAT BLUESTORM    *ELXSTRMS
00029 *              WILL BE PASSING US CAN BE ENTERED WITHOUT         *ELXSTRMS
00030 *              GOING THROUGH BLUESTROM.  THE FIELDS THAT PMCI    *ELXSTRMS
00031 *              PASSES BACK CAN BE DISPLAYED ON THE SCREEN.       *ELXSTRMS
00032 *                                                                *ELXSTRMS
00033 *               1. INITIAL ENTRY:                                *ELXSTRMS
00034 *                  A) SEND MENU SCREEN (MAPONLY).                *ELXSTRMS
00035 *                  B) EXIT MODULE.                               *ELXSTRMS
00036 *                                                                *ELXSTRMS
00037 *               2. SUBSEQUENT ENTRY:                             *ELXSTRMS
00038 *                                                                *ELXSTRMS
00039 *                  A) MAP IN SCREEN.                             *ELXSTRMS
00040 *                     1) IF MAPFAIL, RETURN ERROR MESSAGE.       *ELXSTRMS
00041 *                                                                *ELXSTRMS
00042 *                  B) VALIDATE SCREEN INPUT.                     *ELXSTRMS
00043 *                                                                *ELXSTRMS
00044 *                                                                *ELXSTRMS
00045 ******************************************************************ELXSTRMS
00046 /                                                                 ELXSTRMS
00047  ENVIRONMENT DIVISION.                                            ELXSTRMS
00048  DATA DIVISION.                                                   ELXSTRMS
00049                                                                   ELXSTRMS
00050  WORKING-STORAGE SECTION.                                         ELXSTRMS
00051  01  WS-BEGIN                    PIC X(58) VALUE                  ELXSTRMS
00052      '*** ELXSTRMS WORKING-STORAGE BEGINS HERE ***'.              ELXSTRMS
00053                                                                   ELXSTRMS
00054 /---------- MIL DATE ROUTINE COMMAREA ---------------------------*ELXSTRMS
00055  COPY MLDATE01.                                                   ELXSTRMS
00056                                                                   ELXSTRMS
00057  01  WS-01-ABEND-AREA.                                            ELXSTRMS
00058      05  FILLER                   PIC X(16)  VALUE                ELXSTRMS
00059          '** ABEND AREA **'.                                      ELXSTRMS
00060                                                                   ELXSTRMS
00061      05  WS-01-ABEND-CODES-AND-MSG.                               ELXSTRMS
00062          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. ELXSTRMS
00063          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. ELXSTRMS
00064                                                                   ELXSTRMS
00065          10  WS-01-ABCODE-ELBS          PIC X(04)  VALUE  'ELBS'. ELXSTRMS
00066          10  WS-01-ABCODE-ELBS-MSG      PIC X(44)  VALUE          ELXSTRMS
00067             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          ELXSTRMS
00068                                                                   ELXSTRMS
00069 /------------ HEX VALUES COPYMEMBER ---------------------------*  ELXSTRMS
00070  COPY HEXCOBOL.                                                   ELXSTRMS
00071                                                                   ELXSTRMS
00072  01  WS-02-AREA.                                                  ELXSTRMS
00073      05  FILLER                   PIC X(16)  VALUE                ELXSTRMS
00074          '** WS-02-AREA **'.                                      ELXSTRMS
00075                                                                   ELXSTRMS
00076 *-------- EIBTRNID SAVED HERE -----------------------------------*ELXSTRMS
00077                                                                   ELXSTRMS
00078      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. ELXSTRMS
00079          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         ELXSTRMS
00080                                                    'ELBS'.        ELXSTRMS
00081                                                                   ELXSTRMS
00082 *-------- CHAR BY CHAR (ELIMINATE PRECEDING ZEROS) --------------*ELXSTRMS
00083      05  PLN-IN.                                                  ELXSTRMS
00084          10  PLN-IN-ITEM OCCURS 3 TIMES                           ELXSTRMS
00085              INDEXED BY PLN-IN-INDEX                              ELXSTRMS
00086                                  PIC X.                           ELXSTRMS
00087                                                                   ELXSTRMS
00088      05  PLN-OUT.                                                 ELXSTRMS
00089          10  PLN-OUT-ITEM OCCURS 3 TIMES                          ELXSTRMS
00090              INDEXED BY PLN-OUT-INDEX                             ELXSTRMS
00091                                  PIC X.                           ELXSTRMS
00092                                                                   ELXSTRMS
00093      05  GRP-IN.                                                  ELXSTRMS
00094          10  GRP-IN-ITEM OCCURS 9 TIMES                           ELXSTRMS
00095              INDEXED BY GRP-IN-INDEX                              ELXSTRMS
00096                                  PIC X.                           ELXSTRMS
00097                                                                   ELXSTRMS
00098      05  GRP-OUT.                                                 ELXSTRMS
00099          10  GRP-OUT-ITEM OCCURS 9 TIMES                          ELXSTRMS
00100              INDEXED BY GRP-OUT-INDEX                             ELXSTRMS
00101                                  PIC X.                           ELXSTRMS
00102                                                                   ELXSTRMS
00103      05  SEC-IN.                                                  ELXSTRMS
00104          10  SEC-IN-ITEM OCCURS 5 TIMES                           ELXSTRMS
00105              INDEXED BY SEC-IN-INDEX                              ELXSTRMS
00106                                  PIC X.                           ELXSTRMS
00107                                                                   ELXSTRMS
00108      05  SEC-OUT.                                                 ELXSTRMS
00109          10  SEC-OUT-ITEM OCCURS 5 TIMES                          ELXSTRMS
00110              INDEXED BY SEC-OUT-INDEX                             ELXSTRMS
00111                                  PIC X.                           ELXSTRMS
00112                                                                   ELXSTRMS
00113      05  PKG-IN.                                                  ELXSTRMS
00114          10  PKG-IN-ITEM OCCURS 3 TIMES                           ELXSTRMS
00115              INDEXED BY PKG-IN-INDEX                              ELXSTRMS
00116                                  PIC X.                           ELXSTRMS
00117                                                                   ELXSTRMS
00118      05  PKG-OUT.                                                 ELXSTRMS
00119          10  PKG-OUT-ITEM OCCURS 3 TIMES                          ELXSTRMS
00120              INDEXED BY PKG-OUT-INDEX                             ELXSTRMS
00121                                  PIC X.                           ELXSTRMS
00122                                                                   ELXSTRMS
00123      05  SUB-IN.                                                  ELXSTRMS
00124          10  SUB-IN-ITEM OCCURS 16 TIMES                          ELXSTRMS
00125              INDEXED BY SUB-IN-INDEX                              ELXSTRMS
00126                                  PIC X.                           ELXSTRMS
00127                                                                   ELXSTRMS
00128      05  SUB-OUT.                                                 ELXSTRMS
00129          10  SUB-OUT-ITEM OCCURS 16 TIMES                         ELXSTRMS
00130              INDEXED BY SUB-OUT-INDEX                             ELXSTRMS
00131                                  PIC X.                           ELXSTRMS
00132                                                                   ELXSTRMS
00133 *-------- SCREEN ERROR SWITCH INDICATOR -------------------------*ELXSTRMS
00134                                                                   ELXSTRMS
00135 /                                                                 ELXSTRMS
00136  01  WS-03-AREA.                                                  ELXSTRMS
00137      05  FILLER                   PIC X(16)  VALUE                ELXSTRMS
00138          '** WS-03-AREA **'.                                      ELXSTRMS
00139                                                                   ELXSTRMS
00140      05  WS-03-ELBS-SCREEN-AREA.                                  ELXSTRMS
00141          10  WS-03-ELBS-TRANSID       PIC X(04).                  ELXSTRMS
00142          10  FILLER                   PIC X(3196).                ELXSTRMS
00143 /                                                                 ELXSTRMS
00144  01  WT-00-ELXSTRMC-TABLES.                                       ELXSTRMS
00145      05  FILLER                   PIC X(17)  VALUE                ELXSTRMS
00146          '*ELXSTRMC TABLES*'.                                     ELXSTRMS
00147                                                                   ELXSTRMS
00148  01  WT-01-TABLE.                                                 ELXSTRMS
00149      05  FILLER                  PIC X(16) VALUE                  ELXSTRMS
00150          '* WT-01-TABLE  *'.                                      ELXSTRMS
00151 ******************************************************************ELXSTRMS
00152 *    WT-01   MESSAGE TABLE                                       *ELXSTRMS
00153 ******************************************************************ELXSTRMS
00154  01  FILLER.                                                      ELXSTRMS
00155      05  WT-01-MESSAGE-VALUES.                                    ELXSTRMS
00156                                                                   ELXSTRMS
00157 *----------------------------------------------------------------*ELXSTRMS
00158          10  WT-01-ENTRY-001.                                     ELXSTRMS
00159              15  FILLER              PIC X(2)  VALUE '¬>'.        ELXSTRMS
00160              15  WT-01-MESSAGE-TEXT-001.                          ELXSTRMS
00161                  20  FILLER          PIC X(4)  VALUE  'ELBS'.     ELXSTRMS
00162                  20  FILLER          PIC X(1)  VALUE  '-'.        ELXSTRMS
00163                  20  FILLER          PIC X(3)  VALUE  '001'.      ELXSTRMS
00164                  20  FILLER          PIC X(1)  VALUE  ' '.        ELXSTRMS
00165                  20  FILLER          PIC X(70) VALUE              ELXSTRMS
00166                      'INVALID PFKEY SELECTION                     ELXSTRMS
00167 -                    '                         '.                 ELXSTRMS
00168              15  FILLER              PIC X(2)  VALUE '<¬'.        ELXSTRMS
00169                                                                   ELXSTRMS
00170 *----------------------------------------------------------------*ELXSTRMS
00171          10  WT-01-ENTRY-002.                                     ELXSTRMS
00172              15  FILLER              PIC X(2)  VALUE '¬>'.        ELXSTRMS
00173              15  WT-01-MESSAGE-TEXT-002.                          ELXSTRMS
00174                  20  FILLER          PIC X(4)  VALUE  'ELBS'.     ELXSTRMS
00175                  20  FILLER          PIC X(1)  VALUE  '-'.        ELXSTRMS
00176                  20  FILLER          PIC X(3)  VALUE  '002'.      ELXSTRMS
00177                  20  FILLER          PIC X(1)  VALUE  ' '.        ELXSTRMS
00178                  20  FILLER          PIC X(70) VALUE              ELXSTRMS
00179                      'NOT PROPER LENGTH OR EQUAL SPACES           ELXSTRMS
00180 -                    '                         '.                 ELXSTRMS
00181              15  FILLER              PIC X(2)  VALUE '<¬'.        ELXSTRMS
00182                                                                   ELXSTRMS
00183 *----------------------------------------------------------------*ELXSTRMS
00184          10  WT-01-ENTRY-003.                                     ELXSTRMS
00185              15  FILLER              PIC X(2)  VALUE '¬>'.        ELXSTRMS
00186              15  WT-01-MESSAGE-TEXT-003.                          ELXSTRMS
00187                  20  FILLER          PIC X(4)  VALUE  'ELBS'.     ELXSTRMS
00188                  20  FILLER          PIC X(1)  VALUE  '-'.        ELXSTRMS
00189                  20  FILLER          PIC X(3)  VALUE  '003'.      ELXSTRMS
00190                  20  FILLER          PIC X(1)  VALUE  ' '.        ELXSTRMS
00191                  20  FILLER          PIC X(70) VALUE              ELXSTRMS
00192                      'PMCI CALL WAS NOT SUCCESSFUL                ELXSTRMS
00193 -                    '                         '.                 ELXSTRMS
00194              15  FILLER              PIC X(2)  VALUE '<¬'.        ELXSTRMS
00195                                                                   ELXSTRMS
00196 *----------------------------------------------------------------*ELXSTRMS
00197          10  WT-01-ENTRY-004.                                     ELXSTRMS
00198              15  FILLER              PIC X(2)  VALUE '¬>'.        ELXSTRMS
00199              15  WT-01-MESSAGE-TEXT-003.                          ELXSTRMS
00200                  20  FILLER          PIC X(4)  VALUE  'ELBS'.     ELXSTRMS
00201                  20  FILLER          PIC X(1)  VALUE  '-'.        ELXSTRMS
00202                  20  FILLER          PIC X(3)  VALUE  '004'.      ELXSTRMS
00203                  20  FILLER          PIC X(1)  VALUE  ' '.        ELXSTRMS
00204                  20  FILLER          PIC X(70) VALUE              ELXSTRMS
00205                      'ENTER DATE AGAIN IN MMDDCCYY FORMAT         ELXSTRMS
00206 -                    '                         '.                 ELXSTRMS
00207              15  FILLER              PIC X(2)  VALUE '<¬'.        ELXSTRMS
00208                                                                   ELXSTRMS
00209                                                                   ELXSTRMS
00210      05  WT-01-MESSAGE-TABLE         REDEFINES                    ELXSTRMS
00211          WT-01-MESSAGE-VALUES        OCCURS 004 TIMES             ELXSTRMS
00212                                      INDEXED BY WT-01-INDEX.      ELXSTRMS
00213          10  WT-01-ENTRY.                                         ELXSTRMS
00214              15  FILLER              PIC X(02).                   ELXSTRMS
00215              15  WT-01-MESSAGE-TEXT  PIC X(79).                   ELXSTRMS
00216              15  FILLER              PIC X(02).                   ELXSTRMS
00217                                                                   ELXSTRMS
00218                                                                   ELXSTRMS
00219 /-------------- MAP FIELD ATTRIBUTES ----------------------------*ELXSTRMS
00220  COPY DFHBMSCA.                                                   ELXSTRMS
00221 *                         AUTOSKIP, BRIGHT, FSET                  ELXSTRMS
00222      02  DFHBMABF         PIC X  VALUE 'Z'.                       ELXSTRMS
00223                                                                   ELXSTRMS
00224 /-------------- ATTENTION KEYS ----------------------------------*ELXSTRMS
00225  COPY DFHAID.                                                     ELXSTRMS
00226                                                                   ELXSTRMS
00227 /------------- PMCI INTERFACE TESTING SCREEN --------------------*ELXSTRMS
00228  COPY ELBSSETC.                                                   ELXSTRMS
00229                                                                   ELXSTRMS
00230  01  WS-ELXSTRMI-COMMAREA.                                        ELXSTRMS
00231      05  STRMI-PLAN-CODE               PIC X(3).                  ELXSTRMS
00232      05  STRMI-GROUP-NBR               PIC X(9).                  ELXSTRMS
00233      05  STRMI-SECT-NUM                PIC X(5).                  ELXSTRMS
00234      05  STRMI-PACKAGE-CODE            PIC X(3).                  ELXSTRMS
00235      05  STRMI-SUBSCRIBER-NBR          PIC X(16).                 ELXSTRMS
00236      05  STRMI-ADS-PROG-TYPE           PIC X(2).                  ELXSTRMS
00237      05  STRMI-PAT-BIRTH-DATE          PIC X(8).                  ELXSTRMS
00238      05  STRMI-PAT-RELATIONSHIP        PIC X.                     ELXSTRMS
00239      05  STRMI-MEDICARE-ELIGIBILITY    PIC X.                     ELXSTRMS
00240      05  STRMI-DATE-OF-SERVICE         PIC 9(8).                  ELXSTRMS
00241      05  STRMI-PROVIDER-INDICATOR      PIC X.                     ELXSTRMS
00242      05  STRMI-IP-OR-OP-INQUIRY        PIC X.                     ELXSTRMS
00243      05  STRMI-ERROR-CODE              PIC 9(2).                  ELXSTRMS
00244      05  STRMI-ERROR-DESCRIPTION       PIC X(60).                 ELXSTRMS
00245      05  STRMI-FOOTNOTE-COUNT          PIC S99  COMP-3.           ELXSTRMS
00246      05  STRMI-FOOTNOTES OCCURS 50 TIMES                          ELXSTRMS
00247              INDEXED BY STRMI-FOOT-IDX.                           ELXSTRMS
00248          10  STRMI-FOOTNOTE            PIC X(100).                ELXSTRMS
00249          10  STRMI-NOTE-FORMAT         PIC X(2).                  ELXSTRMS
00250      05  STRMI-BEN-POINTERS            PIC S999 COMP-3.           ELXSTRMS
00251      05  STRMI-BENEFIT-ENTRIES.                                   ELXSTRMS
00252          10  STRMI-BENEFITS OCCURS 1 TO 250 TIMES                 ELXSTRMS
00253                  DEPENDING ON STRMI-BEN-POINTERS                  ELXSTRMS
00254                  INDEXED BY STRMI-BEN-IDX.                        ELXSTRMS
00255              15  STRMI-BENEFIT-DESC    PIC X(50).                 ELXSTRMS
00256              15  STRMI-DESC-FORMAT     PIC X(2).                  ELXSTRMS
00257              15  STRMI-BEN-COLS OCCURS 2 TIMES                    ELXSTRMS
00258                    INDEXED BY STRMI-COL-IDX.                      ELXSTRMS
00259                  20  STRMI-BENEFIT     PIC X(25).                 ELXSTRMS
00260                  20  STRMI-BEN-FORMAT  PIC X(2).                  ELXSTRMS
00261                                                                   ELXSTRMS
00262  01  WS-END                       PIC X(58) VALUE                 ELXSTRMS
00263      '*** ELXSTRMS WORKING-STORAGE ENDS HERE ***'.                ELXSTRMS
00264                                                                   ELXSTRMS
00265  LINKAGE SECTION.                                                 ELXSTRMS
00266                                                                   ELXSTRMS
00267 /*****************************************************************ELXSTRMS
00268 *    INCOMING/OUTGOING DFHCOMMAREA FOR ELXSTRMI                  *ELXSTRMS
00269 ******************************************************************ELXSTRMS
00270  01  DFHCOMMAREA.                                                 ELXSTRMS
00271      COPY  PMCSTRMC.                                              ELXSTRMS
00272 /                                                                 ELXSTRMS
00273  PROCEDURE DIVISION.                                              ELXSTRMS
00274                                                                   ELXSTRMS
00275 ****************************************************************  ELXSTRMS
00276 *                                                              *  ELXSTRMS
00277 *           P R O C E S S     C O N T R O L                    *  ELXSTRMS
00278 *                                                              *  ELXSTRMS
00279 ****************************************************************  ELXSTRMS
00280  0000-000-PROCESS-CONTROL       SECTION.                          ELXSTRMS
00281  0000-010.                                                        ELXSTRMS
00282                                                                   ELXSTRMS
00283      MOVE EIBTRNID TO WS-02-EIBTRNID.                             ELXSTRMS
00284                                                                   ELXSTRMS
00285      IF  WS-02-VALID-ENTRY-EIBTRNID                               ELXSTRMS
00286      THEN                                                         ELXSTRMS
00287          PERFORM  2000-000-PROCESS-INPUT                          ELXSTRMS
00288      ELSE                                                         ELXSTRMS
00289          PERFORM  1000-000-DISPLAY-SCREEN.                        ELXSTRMS
00290                                                                   ELXSTRMS
00291 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*ELXSTRMS
00292                                                                   ELXSTRMS
00293      MOVE WS-01-ABCODE-ELBS     TO WS-01-ABCODE                   ELXSTRMS
00294      MOVE WS-01-ABCODE-ELBS-MSG TO WS-01-ABCODE-MSG               ELXSTRMS
00295      PERFORM  9999-000-ABEND-THE-TASK.                            ELXSTRMS
00296                                                                   ELXSTRMS
00297      GOBACK.                                                      ELXSTRMS
00298                                                                   ELXSTRMS
00299                                                                   ELXSTRMS
00300  0000-900-EXIT.                                                   ELXSTRMS
00301      EXIT.                                                        ELXSTRMS
00302 /***************************************************************  ELXSTRMS
00303 *                                                              *  ELXSTRMS
00304 * 1000  DISPLAY INITIAL SCREEN                                 *  ELXSTRMS
00305 *                                                              *  ELXSTRMS
00306 ****************************************************************  ELXSTRMS
00307  1000-000-DISPLAY-SCREEN        SECTION.                          ELXSTRMS
00308  1000-010.                                                        ELXSTRMS
00309                                                                   ELXSTRMS
00310 *------- SEND INITIAL SCREEN ------------------------------------*ELXSTRMS
00311                                                                   ELXSTRMS
00312      MOVE LOW-VALUES TO ELBSI01O.                                 ELXSTRMS
00313      MOVE -1 TO BSPLANL.                                          ELXSTRMS
00314      PERFORM 9200-000-SEND-THEN-RETURN.                           ELXSTRMS
00315                                                                   ELXSTRMS
00316                                                                   ELXSTRMS
00317  1000-900-EXIT.                                                   ELXSTRMS
00318      EXIT.                                                        ELXSTRMS
00319 /***************************************************************  ELXSTRMS
00320 *                                                              *  ELXSTRMS
00321 * 2000    P R O C E S S    I N P U T                           *  ELXSTRMS
00322 *                                                              *  ELXSTRMS
00323 ****************************************************************  ELXSTRMS
00324  2000-000-PROCESS-INPUT         SECTION.                          ELXSTRMS
00325  2000-010.                                                        ELXSTRMS
00326                                                                   ELXSTRMS
00327 *------ VALIDATE PFKEY USAGE ------------------------------------*ELXSTRMS
00328                                                                   ELXSTRMS
00329      IF  EIBAID NOT = DFHENTER                                    ELXSTRMS
00330          MOVE  -1 TO  BSERMSGL                                    ELXSTRMS
00331          SET WT-01-INDEX TO +01                                   ELXSTRMS
00332          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00333          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXSTRMS
00334                                                                   ELXSTRMS
00335      EXEC CICS                                                    ELXSTRMS
00336           RECEIVE INTO(WS-03-ELBS-SCREEN-AREA)                    ELXSTRMS
00337      END-EXEC.                                                    ELXSTRMS
00338                                                                   ELXSTRMS
00339      EXEC CICS  HANDLE CONDITION                                  ELXSTRMS
00340                        MAPFAIL(9200-000-SEND-THEN-RETURN)         ELXSTRMS
00341                        END-EXEC.                                  ELXSTRMS
00342                                                                   ELXSTRMS
00343      EXEC CICS  RECEIVE MAP   ('ELBSI01')                         ELXSTRMS
00344                         FROM(WS-03-ELBS-SCREEN-AREA)              ELXSTRMS
00345                         MAPSET('ELBSSET')                         ELXSTRMS
00346                         INTO  (ELBSI01I)                          ELXSTRMS
00347                         END-EXEC.                                 ELXSTRMS
00348                                                                   ELXSTRMS
00349      IF BSFUNCNI NOT = 'ELBS' OR                                  ELXSTRMS
00350         BSSCRNI  NOT = 'ELBS00'                                   ELXSTRMS
00351          MOVE  -1 TO  BSERMSGL                                    ELXSTRMS
00352          PERFORM 9200-000-SEND-THEN-RETURN.                       ELXSTRMS
00353                                                                   ELXSTRMS
00354 *--- PROCESS SCREEN FIELDS --------------------------------------*ELXSTRMS
00355                                                                   ELXSTRMS
00356      PERFORM 2100-000-FIELD-EDITS.                                ELXSTRMS
00357                                                                   ELXSTRMS
00358      PERFORM 9400-000-LINK-TO-ELXSTRMI.                           ELXSTRMS
00359      IF STRMI-ERROR-CODE = 01 OR 02 OR 03 OR 04 OR 05             ELXSTRMS
00360          MOVE -1 TO BSERMSGL                                      ELXSTRMS
00361          SET WT-01-INDEX TO +03                                   ELXSTRMS
00362          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00363          PERFORM 9300-000-SEND-THEN-RETURN                        ELXSTRMS
00364          GO TO 2000-900-EXIT                                      ELXSTRMS
00365      ELSE                                                         ELXSTRMS
00366          MOVE -1 TO BSPLANL                                       ELXSTRMS
00367          PERFORM 9300-000-SEND-THEN-RETURN                        ELXSTRMS
00368          GO TO 2000-900-EXIT.                                     ELXSTRMS
00369                                                                   ELXSTRMS
00370  2000-900-EXIT.                                                   ELXSTRMS
00371      EXIT.                                                        ELXSTRMS
00372 /***************************************************************  ELXSTRMS
00373 *                                                              *  ELXSTRMS
00374 * 2100  DO SCREEN FIELD EDITS                                  *  ELXSTRMS
00375 *                                                              *  ELXSTRMS
00376 ****************************************************************  ELXSTRMS
00377  2100-000-FIELD-EDITS           SECTION.                          ELXSTRMS
00378  2100-010.                                                        ELXSTRMS
00379                                                                   ELXSTRMS
00380 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*ELXSTRMS
00381                                                                   ELXSTRMS
00382      MOVE DFHBMUNF TO  BSPLANA                                    ELXSTRMS
00383                        BSGRPA                                     ELXSTRMS
00384                        BSSECA                                     ELXSTRMS
00385                        BSPKGA                                     ELXSTRMS
00386                        BSSUBA                                     ELXSTRMS
00387                        BSBDATEA                                   ELXSTRMS
00388                        BSPTRELA                                   ELXSTRMS
00389                        BSMEDA                                     ELXSTRMS
00390                        BSSERVA                                    ELXSTRMS
00391                        BSPROVA                                    ELXSTRMS
00392                        BSINOUTA                                   ELXSTRMS
00393                        BSADSA.                                    ELXSTRMS
00394                                                                   ELXSTRMS
00395      INSPECT BSPLANI   REPLACING ALL '_' BY SPACES.               ELXSTRMS
00396      INSPECT BSGRPI    REPLACING ALL '_' BY SPACES.               ELXSTRMS
00397      INSPECT BSSECI    REPLACING ALL '_' BY SPACES.               ELXSTRMS
00398      INSPECT BSPKGI    REPLACING ALL '_' BY SPACES.               ELXSTRMS
00399      INSPECT BSSUBI    REPLACING ALL '_' BY SPACES.               ELXSTRMS
00400      INSPECT BSBDATEI  REPLACING ALL '_' BY SPACES.               ELXSTRMS
00401      INSPECT BSPTRELI  REPLACING ALL '_' BY SPACES.               ELXSTRMS
00402      INSPECT BSMEDI    REPLACING ALL '_' BY SPACES.               ELXSTRMS
00403      INSPECT BSSERVI   REPLACING ALL '_' BY SPACES.               ELXSTRMS
00404      INSPECT BSPROVI   REPLACING ALL '_' BY SPACES.               ELXSTRMS
00405      INSPECT BSINOUTI  REPLACING ALL '_' BY SPACES.               ELXSTRMS
00406      INSPECT BSADSI    REPLACING ALL '_' BY SPACES.               ELXSTRMS
00407                                                                   ELXSTRMS
00408      IF (BSPLANI = SPACES) OR                                     ELXSTRMS
00409         (BSPLANL < 3)                                             ELXSTRMS
00410          MOVE -1 TO BSPLANL                                       ELXSTRMS
00411          MOVE DFHBMUBF TO BSPLANA                                 ELXSTRMS
00412          SET WT-01-INDEX TO +02                                   ELXSTRMS
00413          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00414          PERFORM 9300-000-SEND-THEN-RETURN                        ELXSTRMS
00415      ELSE                                                         ELXSTRMS
00416          MOVE ZEROES TO PLN-OUT                                   ELXSTRMS
00417          MOVE BSPLANI TO PLN-IN                                   ELXSTRMS
00418          PERFORM 2400-RIGHT-JUSTIFY-PLN                           ELXSTRMS
00419          MOVE PLN-OUT TO BSPLANI.                                 ELXSTRMS
00420                                                                   ELXSTRMS
00421      IF (BSGRPI = SPACES) OR                                      ELXSTRMS
00422         (BSGRPL < 1)                                              ELXSTRMS
00423          MOVE -1 TO BSGRPL                                        ELXSTRMS
00424          MOVE DFHBMUBF TO BSGRPA                                  ELXSTRMS
00425          SET WT-01-INDEX TO +02                                   ELXSTRMS
00426          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00427          PERFORM 9300-000-SEND-THEN-RETURN                        ELXSTRMS
00428      ELSE                                                         ELXSTRMS
00429          MOVE ZEROES TO GRP-OUT                                   ELXSTRMS
00430          MOVE BSGRPI TO GRP-IN                                    ELXSTRMS
00431          PERFORM 2500-RIGHT-JUSTIFY-GRP                           ELXSTRMS
00432          MOVE GRP-OUT TO BSGRPI.                                  ELXSTRMS
00433                                                                   ELXSTRMS
00434      IF (BSSECI = SPACES) OR                                      ELXSTRMS
00435         (BSSECL < 1)                                              ELXSTRMS
00436          MOVE -1 TO BSSECL                                        ELXSTRMS
00437          MOVE DFHBMUBF TO BSSECA                                  ELXSTRMS
00438          SET WT-01-INDEX TO +02                                   ELXSTRMS
00439          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00440          PERFORM 9300-000-SEND-THEN-RETURN                        ELXSTRMS
00441      ELSE                                                         ELXSTRMS
00442          MOVE ZEROES TO SEC-OUT                                   ELXSTRMS
00443          MOVE BSSECI TO SEC-IN                                    ELXSTRMS
00444          PERFORM 2600-RIGHT-JUSTIFY-SEC                           ELXSTRMS
00445          MOVE SEC-OUT TO BSSECI.                                  ELXSTRMS
00446                                                                   ELXSTRMS
00447      IF (BSPKGI = SPACES) OR                                      ELXSTRMS
00448         (BSPKGL < 3)                                              ELXSTRMS
00449          MOVE -1 TO BSPKGL                                        ELXSTRMS
00450          MOVE DFHBMUBF TO BSPKGA                                  ELXSTRMS
00451          SET WT-01-INDEX TO +02                                   ELXSTRMS
00452          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00453          PERFORM 9300-000-SEND-THEN-RETURN                        ELXSTRMS
00454      ELSE                                                         ELXSTRMS
00455          MOVE ZEROES TO PKG-OUT                                   ELXSTRMS
00456          MOVE BSPKGI TO PKG-IN                                    ELXSTRMS
00457          PERFORM 2700-RIGHT-JUSTIFY-PKG                           ELXSTRMS
00458          MOVE PKG-OUT TO BSPKGI.                                  ELXSTRMS
00459                                                                   ELXSTRMS
00460      IF (BSSUBI = SPACES) OR                                      ELXSTRMS
00461         (BSSUBL < 9)                                              ELXSTRMS
00462          MOVE -1 TO BSSUBL                                        ELXSTRMS
00463          MOVE DFHBMUBF TO BSSUBA                                  ELXSTRMS
00464          SET WT-01-INDEX TO +02                                   ELXSTRMS
00465          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00466          PERFORM 9300-000-SEND-THEN-RETURN                        ELXSTRMS
00467      ELSE                                                         ELXSTRMS
00468          MOVE ZEROES TO SUB-OUT                                   ELXSTRMS
00469          MOVE BSSUBI TO SUB-IN                                    ELXSTRMS
00470          PERFORM 2800-RIGHT-JUSTIFY-SUB                           ELXSTRMS
00471          MOVE SUB-OUT TO BSSUBI.                                  ELXSTRMS
00472                                                                   ELXSTRMS
00473      IF (BSBDATEI = SPACES) OR                                    ELXSTRMS
00474         (BSBDATEL < 8)                                            ELXSTRMS
00475          MOVE -1 TO BSBDATEL                                      ELXSTRMS
00476          MOVE DFHBMUBF TO BSBDATEA                                ELXSTRMS
00477          SET WT-01-INDEX TO +02                                   ELXSTRMS
00478          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00479          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXSTRMS
00480                                                                   ELXSTRMS
00481      IF (BSPTRELI = SPACES) OR                                    ELXSTRMS
00482         (BSPTRELL < 1)                                            ELXSTRMS
00483          MOVE -1 TO BSPTRELL                                      ELXSTRMS
00484          MOVE DFHBMUBF TO BSPTRELA                                ELXSTRMS
00485          SET WT-01-INDEX TO +02                                   ELXSTRMS
00486          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00487          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXSTRMS
00488                                                                   ELXSTRMS
00489      IF (BSMEDI = SPACES) OR                                      ELXSTRMS
00490         (BSMEDL < 1)                                              ELXSTRMS
00491          MOVE -1 TO BSMEDL                                        ELXSTRMS
00492          MOVE DFHBMUBF TO BSMEDA                                  ELXSTRMS
00493          SET WT-01-INDEX TO +02                                   ELXSTRMS
00494          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00495          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXSTRMS
00496                                                                   ELXSTRMS
00497      IF (BSSERVI = SPACES) OR                                     ELXSTRMS
00498         (BSSERVL < 8)                                             ELXSTRMS
00499          MOVE -1 TO BSSERVL                                       ELXSTRMS
00500          MOVE DFHBMUBF TO BSSERVA                                 ELXSTRMS
00501          SET WT-01-INDEX TO +02                                   ELXSTRMS
00502          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00503          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXSTRMS
00504                                                                   ELXSTRMS
00505      IF (BSPROVI = SPACES) OR                                     ELXSTRMS
00506         (BSPROVL < 1)                                             ELXSTRMS
00507          MOVE -1 TO BSPROVL                                       ELXSTRMS
00508          MOVE DFHBMUBF TO BSPROVA                                 ELXSTRMS
00509          SET WT-01-INDEX TO +02                                   ELXSTRMS
00510          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00511          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXSTRMS
00512                                                                   ELXSTRMS
00513      IF (BSINOUTI = SPACES) OR                                    ELXSTRMS
00514         (BSINOUTL < 1)                                            ELXSTRMS
00515          MOVE -1 TO BSINOUTL                                      ELXSTRMS
00516          MOVE DFHBMUBF TO BSINOUTA                                ELXSTRMS
00517          SET WT-01-INDEX TO +02                                   ELXSTRMS
00518          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00519          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXSTRMS
00520                                                                   ELXSTRMS
00521      IF (BSADSI = SPACES) OR                                      ELXSTRMS
00522         (BSADSL < 1)                                              ELXSTRMS
00523          MOVE -1 TO BSADSL                                        ELXSTRMS
00524          MOVE DFHBMUBF TO BSADSA                                  ELXSTRMS
00525          SET WT-01-INDEX TO +02                                   ELXSTRMS
00526          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00527          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXSTRMS
00528                                                                   ELXSTRMS
00529      MOVE BSPLANI  TO STRMI-PLAN-CODE.                            ELXSTRMS
00530      MOVE BSGRPI   TO STRMI-GROUP-NBR.                            ELXSTRMS
00531      MOVE BSSECI   TO STRMI-SECT-NUM.                             ELXSTRMS
00532      MOVE BSPKGI   TO STRMI-PACKAGE-CODE.                         ELXSTRMS
00533      MOVE BSSUBI   TO STRMI-SUBSCRIBER-NBR.                       ELXSTRMS
00534      MOVE BSBDATEI TO MLDATE-DATE1.                               ELXSTRMS
00535      PERFORM 9800-000-CNV-TO-CCYYMMDD.                            ELXSTRMS
00536      IF MLDATE-RETURN = ZEROS                                     ELXSTRMS
00537          MOVE MLDATE-DATE2 TO STRMI-PAT-BIRTH-DATE                ELXSTRMS
00538      ELSE                                                         ELXSTRMS
00539          MOVE -1 TO BSBDATEL                                      ELXSTRMS
00540          MOVE DFHBMUBF TO BSBDATEA                                ELXSTRMS
00541          SET WT-01-INDEX TO +04                                   ELXSTRMS
00542          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00543          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXSTRMS
00544      MOVE BSPTRELI TO STRMI-PAT-RELATIONSHIP.                     ELXSTRMS
00545      MOVE BSMEDI   TO STRMI-MEDICARE-ELIGIBILITY.                 ELXSTRMS
00546      MOVE BSSERVI  TO MLDATE-DATE1.                               ELXSTRMS
00547      PERFORM 9800-000-CNV-TO-CCYYMMDD.                            ELXSTRMS
00548      IF MLDATE-RETURN = ZEROS                                     ELXSTRMS
00549          MOVE MLDATE-DATE2 TO STRMI-DATE-OF-SERVICE               ELXSTRMS
00550      ELSE                                                         ELXSTRMS
00551          MOVE -1 TO BSSERVL                                       ELXSTRMS
00552          MOVE DFHBMUBF TO BSSERVA                                 ELXSTRMS
00553          SET WT-01-INDEX TO +04                                   ELXSTRMS
00554          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXSTRMS
00555          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXSTRMS
00556      MOVE BSPROVI  TO STRMI-PROVIDER-INDICATOR.                   ELXSTRMS
00557      MOVE BSINOUTI TO STRMI-IP-OR-OP-INQUIRY.                     ELXSTRMS
00558      MOVE BSADSI   TO STRMI-ADS-PROG-TYPE.                        ELXSTRMS
00559      MOVE +5       TO STRMI-BEN-POINTERS.                         ELXSTRMS
00560                                                                   ELXSTRMS
00561  2100-900-EXIT.                                                   ELXSTRMS
00562      EXIT.                                                        ELXSTRMS
00563 ************************************************************      ELXSTRMS
00564 *                                                          *      ELXSTRMS
00565 *        STRIP PLAN                                        *      ELXSTRMS
00566 *                                                          *      ELXSTRMS
00567 ************************************************************      ELXSTRMS
00568  2400-RIGHT-JUSTIFY-PLN  SECTION.                                 ELXSTRMS
00569  2400-010.                                                        ELXSTRMS
00570      SET PLN-OUT-INDEX TO 3.                                      ELXSTRMS
00571      PERFORM 2410-STRIP-PLN-INPUT                                 ELXSTRMS
00572          VARYING PLN-IN-INDEX FROM 3 BY -1                        ELXSTRMS
00573          UNTIL PLN-IN-INDEX < 1.                                  ELXSTRMS
00574                                                                   ELXSTRMS
00575  2400-900-EXIT.                                                   ELXSTRMS
00576      EXIT.                                                        ELXSTRMS
00577                                                                   ELXSTRMS
00578 ************************************************************      ELXSTRMS
00579 *                                                          *      ELXSTRMS
00580 *        STRIP PLAN INPUT                                  *      ELXSTRMS
00581 *                                                          *      ELXSTRMS
00582 ************************************************************      ELXSTRMS
00583  2410-STRIP-PLN-INPUT  SECTION.                                   ELXSTRMS
00584  2410-010.                                                        ELXSTRMS
00585      IF PLN-IN-ITEM (PLN-IN-INDEX) IS NUMERIC                     ELXSTRMS
00586           OR PLN-IN-ITEM (PLN-IN-INDEX) IS ALPHABETIC             ELXSTRMS
00587          PERFORM 2420-MOVE-PLN-CHAR.                              ELXSTRMS
00588                                                                   ELXSTRMS
00589  2410-900-EXIT.                                                   ELXSTRMS
00590      EXIT.                                                        ELXSTRMS
00591                                                                   ELXSTRMS
00592 ************************************************************      ELXSTRMS
00593 *                                                          *      ELXSTRMS
00594 *        MOVE PLAN CHAR                                    *      ELXSTRMS
00595 *                                                          *      ELXSTRMS
00596 ************************************************************      ELXSTRMS
00597  2420-MOVE-PLN-CHAR  SECTION.                                     ELXSTRMS
00598  2420-010.                                                        ELXSTRMS
00599      IF  PLN-IN-ITEM (PLN-IN-INDEX) NOT = SPACE                   ELXSTRMS
00600          PERFORM 2430-MOVE-PLN-CHAR-NSPC.                         ELXSTRMS
00601                                                                   ELXSTRMS
00602  2420-900-EXIT.                                                   ELXSTRMS
00603      EXIT.                                                        ELXSTRMS
00604                                                                   ELXSTRMS
00605 ************************************************************      ELXSTRMS
00606 *                                                          *      ELXSTRMS
00607 *        MOVE PLAN CHAR NSPC                               *      ELXSTRMS
00608 *                                                          *      ELXSTRMS
00609 ************************************************************      ELXSTRMS
00610  2430-MOVE-PLN-CHAR-NSPC  SECTION.                                ELXSTRMS
00611  2430-010.                                                        ELXSTRMS
00612      MOVE PLN-IN-ITEM (PLN-IN-INDEX)                              ELXSTRMS
00613                             TO PLN-OUT-ITEM (PLN-OUT-INDEX).      ELXSTRMS
00614      SET PLN-OUT-INDEX DOWN BY 1.                                 ELXSTRMS
00615                                                                   ELXSTRMS
00616  2430-900-EXIT.                                                   ELXSTRMS
00617      EXIT.                                                        ELXSTRMS
00618                                                                   ELXSTRMS
00619 ************************************************************      ELXSTRMS
00620 *                                                          *      ELXSTRMS
00621 *        STRIP GROUP                                       *      ELXSTRMS
00622 *                                                          *      ELXSTRMS
00623 ************************************************************      ELXSTRMS
00624  2500-RIGHT-JUSTIFY-GRP  SECTION.                                 ELXSTRMS
00625  2500-010.                                                        ELXSTRMS
00626      SET GRP-OUT-INDEX TO 9.                                      ELXSTRMS
00627      PERFORM 2510-STRIP-GRP-INPUT                                 ELXSTRMS
00628          VARYING GRP-IN-INDEX FROM 9 BY -1                        ELXSTRMS
00629          UNTIL GRP-IN-INDEX < 1.                                  ELXSTRMS
00630                                                                   ELXSTRMS
00631  2500-900-EXIT.                                                   ELXSTRMS
00632      EXIT.                                                        ELXSTRMS
00633                                                                   ELXSTRMS
00634 ************************************************************      ELXSTRMS
00635 *                                                          *      ELXSTRMS
00636 *        STRIP GROUP INPUT                                 *      ELXSTRMS
00637 *                                                          *      ELXSTRMS
00638 ************************************************************      ELXSTRMS
00639  2510-STRIP-GRP-INPUT  SECTION.                                   ELXSTRMS
00640  2510-010.                                                        ELXSTRMS
00641      IF GRP-IN-ITEM (GRP-IN-INDEX) IS NUMERIC                     ELXSTRMS
00642           OR GRP-IN-ITEM (GRP-IN-INDEX) IS ALPHABETIC             ELXSTRMS
00643          PERFORM 2520-MOVE-GRP-CHAR.                              ELXSTRMS
00644                                                                   ELXSTRMS
00645  2510-900-EXIT.                                                   ELXSTRMS
00646      EXIT.                                                        ELXSTRMS
00647                                                                   ELXSTRMS
00648 ************************************************************      ELXSTRMS
00649 *                                                          *      ELXSTRMS
00650 *        MOVE GROUP CHAR                                   *      ELXSTRMS
00651 *                                                          *      ELXSTRMS
00652 ************************************************************      ELXSTRMS
00653  2520-MOVE-GRP-CHAR  SECTION.                                     ELXSTRMS
00654  2520-010.                                                        ELXSTRMS
00655      IF  GRP-IN-ITEM (GRP-IN-INDEX) NOT = SPACE                   ELXSTRMS
00656          PERFORM 2530-MOVE-GRP-CHAR-NSPC.                         ELXSTRMS
00657                                                                   ELXSTRMS
00658  2520-900-EXIT.                                                   ELXSTRMS
00659      EXIT.                                                        ELXSTRMS
00660                                                                   ELXSTRMS
00661 ************************************************************      ELXSTRMS
00662 *                                                          *      ELXSTRMS
00663 *        MOVE GROUP CHAR NSPC                              *      ELXSTRMS
00664 *                                                          *      ELXSTRMS
00665 ************************************************************      ELXSTRMS
00666  2530-MOVE-GRP-CHAR-NSPC  SECTION.                                ELXSTRMS
00667  2530-010.                                                        ELXSTRMS
00668      MOVE GRP-IN-ITEM (GRP-IN-INDEX)                              ELXSTRMS
00669                             TO GRP-OUT-ITEM (GRP-OUT-INDEX).      ELXSTRMS
00670      SET GRP-OUT-INDEX DOWN BY 1.                                 ELXSTRMS
00671                                                                   ELXSTRMS
00672  2530-900-EXIT.                                                   ELXSTRMS
00673      EXIT.                                                        ELXSTRMS
00674                                                                   ELXSTRMS
00675 ************************************************************      ELXSTRMS
00676 *                                                          *      ELXSTRMS
00677 *        STRIP SECTION                                     *      ELXSTRMS
00678 *                                                          *      ELXSTRMS
00679 ************************************************************      ELXSTRMS
00680  2600-RIGHT-JUSTIFY-SEC  SECTION.                                 ELXSTRMS
00681  2600-010.                                                        ELXSTRMS
00682      SET SEC-OUT-INDEX TO 5.                                      ELXSTRMS
00683      PERFORM 2610-STRIP-SEC-INPUT                                 ELXSTRMS
00684          VARYING SEC-IN-INDEX FROM 5 BY -1                        ELXSTRMS
00685          UNTIL SEC-IN-INDEX < 1.                                  ELXSTRMS
00686                                                                   ELXSTRMS
00687  2600-900-EXIT.                                                   ELXSTRMS
00688      EXIT.                                                        ELXSTRMS
00689                                                                   ELXSTRMS
00690 ************************************************************      ELXSTRMS
00691 *                                                          *      ELXSTRMS
00692 *        STRIP SECTION INPUT                               *      ELXSTRMS
00693 *                                                          *      ELXSTRMS
00694 ************************************************************      ELXSTRMS
00695  2610-STRIP-SEC-INPUT  SECTION.                                   ELXSTRMS
00696  2610-010.                                                        ELXSTRMS
00697      IF SEC-IN-ITEM (SEC-IN-INDEX) IS NUMERIC                     ELXSTRMS
00698           OR SEC-IN-ITEM (SEC-IN-INDEX) IS ALPHABETIC             ELXSTRMS
00699          PERFORM 2620-MOVE-SEC-CHAR.                              ELXSTRMS
00700                                                                   ELXSTRMS
00701  2610-900-EXIT.                                                   ELXSTRMS
00702      EXIT.                                                        ELXSTRMS
00703                                                                   ELXSTRMS
00704 ************************************************************      ELXSTRMS
00705 *                                                          *      ELXSTRMS
00706 *        MOVE SECTION CHAR                                 *      ELXSTRMS
00707 *                                                          *      ELXSTRMS
00708 ************************************************************      ELXSTRMS
00709  2620-MOVE-SEC-CHAR  SECTION.                                     ELXSTRMS
00710  2620-010.                                                        ELXSTRMS
00711      IF  SEC-IN-ITEM (SEC-IN-INDEX) NOT = SPACE                   ELXSTRMS
00712          PERFORM 2630-MOVE-SEC-CHAR-NSPC.                         ELXSTRMS
00713                                                                   ELXSTRMS
00714  2620-900-EXIT.                                                   ELXSTRMS
00715      EXIT.                                                        ELXSTRMS
00716                                                                   ELXSTRMS
00717 ************************************************************      ELXSTRMS
00718 *                                                          *      ELXSTRMS
00719 *        MOVE SECTION CHAR NSPC                            *      ELXSTRMS
00720 *                                                          *      ELXSTRMS
00721 ************************************************************      ELXSTRMS
00722  2630-MOVE-SEC-CHAR-NSPC  SECTION.                                ELXSTRMS
00723  2630-010.                                                        ELXSTRMS
00724      MOVE SEC-IN-ITEM (SEC-IN-INDEX)                              ELXSTRMS
00725                             TO SEC-OUT-ITEM (SEC-OUT-INDEX).      ELXSTRMS
00726      SET SEC-OUT-INDEX DOWN BY 1.                                 ELXSTRMS
00727                                                                   ELXSTRMS
00728  2630-900-EXIT.                                                   ELXSTRMS
00729      EXIT.                                                        ELXSTRMS
00730                                                                   ELXSTRMS
00731 ************************************************************      ELXSTRMS
00732 *                                                          *      ELXSTRMS
00733 *        STRIP PACKAGE                                     *      ELXSTRMS
00734 *                                                          *      ELXSTRMS
00735 ************************************************************      ELXSTRMS
00736  2700-RIGHT-JUSTIFY-PKG  SECTION.                                 ELXSTRMS
00737  2700-010.                                                        ELXSTRMS
00738      SET PKG-OUT-INDEX TO 3.                                      ELXSTRMS
00739      PERFORM 2710-STRIP-PKG-INPUT                                 ELXSTRMS
00740          VARYING PKG-IN-INDEX FROM 3 BY -1                        ELXSTRMS
00741          UNTIL PKG-IN-INDEX < 1.                                  ELXSTRMS
00742                                                                   ELXSTRMS
00743  2700-900-EXIT.                                                   ELXSTRMS
00744      EXIT.                                                        ELXSTRMS
00745                                                                   ELXSTRMS
00746 ************************************************************      ELXSTRMS
00747 *                                                          *      ELXSTRMS
00748 *        STRIP PACKAGE INPUT                               *      ELXSTRMS
00749 *                                                          *      ELXSTRMS
00750 ************************************************************      ELXSTRMS
00751  2710-STRIP-PKG-INPUT  SECTION.                                   ELXSTRMS
00752  2710-010.                                                        ELXSTRMS
00753      IF PKG-IN-ITEM (PKG-IN-INDEX) IS NUMERIC                     ELXSTRMS
00754           OR PKG-IN-ITEM (PKG-IN-INDEX) IS ALPHABETIC             ELXSTRMS
00755          PERFORM 2720-MOVE-PKG-CHAR.                              ELXSTRMS
00756                                                                   ELXSTRMS
00757  2710-900-EXIT.                                                   ELXSTRMS
00758      EXIT.                                                        ELXSTRMS
00759                                                                   ELXSTRMS
00760 ************************************************************      ELXSTRMS
00761 *                                                          *      ELXSTRMS
00762 *        MOVE PACKAGE CHAR                                 *      ELXSTRMS
00763 *                                                          *      ELXSTRMS
00764 ************************************************************      ELXSTRMS
00765  2720-MOVE-PKG-CHAR  SECTION.                                     ELXSTRMS
00766  2720-010.                                                        ELXSTRMS
00767      IF  PKG-IN-ITEM (PKG-IN-INDEX) NOT = SPACE                   ELXSTRMS
00768          PERFORM 2730-MOVE-PKG-CHAR-NSPC.                         ELXSTRMS
00769                                                                   ELXSTRMS
00770  2720-900-EXIT.                                                   ELXSTRMS
00771      EXIT.                                                        ELXSTRMS
00772                                                                   ELXSTRMS
00773 ************************************************************      ELXSTRMS
00774 *                                                          *      ELXSTRMS
00775 *        MOVE PACKAGE CHAR NSPC                            *      ELXSTRMS
00776 *                                                          *      ELXSTRMS
00777 ************************************************************      ELXSTRMS
00778  2730-MOVE-PKG-CHAR-NSPC  SECTION.                                ELXSTRMS
00779  2730-010.                                                        ELXSTRMS
00780      MOVE PKG-IN-ITEM (PKG-IN-INDEX)                              ELXSTRMS
00781                             TO PKG-OUT-ITEM (PKG-OUT-INDEX).      ELXSTRMS
00782      SET PKG-OUT-INDEX DOWN BY 1.                                 ELXSTRMS
00783                                                                   ELXSTRMS
00784  2730-900-EXIT.                                                   ELXSTRMS
00785      EXIT.                                                        ELXSTRMS
00786                                                                   ELXSTRMS
00787 ************************************************************      ELXSTRMS
00788 *                                                          *      ELXSTRMS
00789 *        STRIP SUBSCRIBER                                  *      ELXSTRMS
00790 *                                                          *      ELXSTRMS
00791 ************************************************************      ELXSTRMS
00792  2800-RIGHT-JUSTIFY-SUB  SECTION.                                 ELXSTRMS
00793  2800-010.                                                        ELXSTRMS
00794      SET SUB-OUT-INDEX TO 16.                                     ELXSTRMS
00795      PERFORM 2810-STRIP-SUB-INPUT                                 ELXSTRMS
00796          VARYING SUB-IN-INDEX FROM 16 BY -1                       ELXSTRMS
00797          UNTIL SUB-IN-INDEX < 1.                                  ELXSTRMS
00798                                                                   ELXSTRMS
00799  2800-900-EXIT.                                                   ELXSTRMS
00800      EXIT.                                                        ELXSTRMS
00801                                                                   ELXSTRMS
00802 ************************************************************      ELXSTRMS
00803 *                                                          *      ELXSTRMS
00804 *        STRIP SUBSCRIBER INPUT                            *      ELXSTRMS
00805 *                                                          *      ELXSTRMS
00806 ************************************************************      ELXSTRMS
00807  2810-STRIP-SUB-INPUT  SECTION.                                   ELXSTRMS
00808  2810-010.                                                        ELXSTRMS
00809      IF SUB-IN-ITEM (SUB-IN-INDEX) IS NUMERIC                     ELXSTRMS
00810           OR SUB-IN-ITEM (SUB-IN-INDEX) IS ALPHABETIC             ELXSTRMS
00811          PERFORM 2820-MOVE-SUB-CHAR.                              ELXSTRMS
00812                                                                   ELXSTRMS
00813  2810-900-EXIT.                                                   ELXSTRMS
00814      EXIT.                                                        ELXSTRMS
00815                                                                   ELXSTRMS
00816 ************************************************************      ELXSTRMS
00817 *                                                          *      ELXSTRMS
00818 *        MOVE SUBSCRIBER CHAR                              *      ELXSTRMS
00819 *                                                          *      ELXSTRMS
00820 ************************************************************      ELXSTRMS
00821  2820-MOVE-SUB-CHAR  SECTION.                                     ELXSTRMS
00822  2820-010.                                                        ELXSTRMS
00823      IF  SUB-IN-ITEM (SUB-IN-INDEX) NOT = SPACE                   ELXSTRMS
00824          PERFORM 2830-MOVE-SUB-CHAR-NSPC.                         ELXSTRMS
00825                                                                   ELXSTRMS
00826  2820-900-EXIT.                                                   ELXSTRMS
00827      EXIT.                                                        ELXSTRMS
00828                                                                   ELXSTRMS
00829 ************************************************************      ELXSTRMS
00830 *                                                          *      ELXSTRMS
00831 *        MOVE SUBSCRIBER CHAR NSPC                         *      ELXSTRMS
00832 *                                                          *      ELXSTRMS
00833 ************************************************************      ELXSTRMS
00834  2830-MOVE-SUB-CHAR-NSPC  SECTION.                                ELXSTRMS
00835  2830-010.                                                        ELXSTRMS
00836      MOVE SUB-IN-ITEM (SUB-IN-INDEX)                              ELXSTRMS
00837                             TO SUB-OUT-ITEM (SUB-OUT-INDEX).      ELXSTRMS
00838      SET SUB-OUT-INDEX DOWN BY 1.                                 ELXSTRMS
00839                                                                   ELXSTRMS
00840  2830-900-EXIT.                                                   ELXSTRMS
00841      EXIT.                                                        ELXSTRMS
00842 /***************************************************************  ELXSTRMS
00843 *                                                              *  ELXSTRMS
00844 * 9000   MOVE MESSAGE TO SCREEN                                *  ELXSTRMS
00845 *                                                              *  ELXSTRMS
00846 ****************************************************************  ELXSTRMS
00847  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          ELXSTRMS
00848  9000-010.                                                        ELXSTRMS
00849                                                                   ELXSTRMS
00850      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)                         ELXSTRMS
00851                    TO BSERMSGO.                                   ELXSTRMS
00852                                                                   ELXSTRMS
00853  9000-900-EXIT.                                                   ELXSTRMS
00854      EXIT.                                                        ELXSTRMS
00855 /***************************************************************  ELXSTRMS
00856 *                                                              *  ELXSTRMS
00857 * 9200 SEND SCREEN AND RETURN                                  *  ELXSTRMS
00858 *                                                              *  ELXSTRMS
00859 ****************************************************************  ELXSTRMS
00860  9200-000-SEND-THEN-RETURN      SECTION.                          ELXSTRMS
00861  9200-010.                                                        ELXSTRMS
00862                                                                   ELXSTRMS
00863      EXEC CICS  SEND MAP   ('ELBSI01')                            ELXSTRMS
00864                      MAPSET('ELBSSET')                            ELXSTRMS
00865                      MAPONLY                                      ELXSTRMS
00866                      ERASE                                        ELXSTRMS
00867                      END-EXEC.                                    ELXSTRMS
00868                                                                   ELXSTRMS
00869      EXEC CICS  RETURN                                            ELXSTRMS
00870               END-EXEC.                                           ELXSTRMS
00871                                                                   ELXSTRMS
00872                                                                   ELXSTRMS
00873  9200-900-EXIT.                                                   ELXSTRMS
00874      EXIT.                                                        ELXSTRMS
00875 /***************************************************************  ELXSTRMS
00876 *                                                              *  ELXSTRMS
00877 * 9300 SEND SCREEN AND RETURN                                  *  ELXSTRMS
00878 *                                                              *  ELXSTRMS
00879 ****************************************************************  ELXSTRMS
00880  9300-000-SEND-THEN-RETURN      SECTION.                          ELXSTRMS
00881  9300-010.                                                        ELXSTRMS
00882                                                                   ELXSTRMS
00883      EXEC CICS  SEND MAP   ('ELBSI01')                            ELXSTRMS
00884                      MAPSET('ELBSSET')                            ELXSTRMS
00885                      ERASE                                        ELXSTRMS
00886                      FROM (ELBSI01O)                              ELXSTRMS
00887                      CURSOR                                       ELXSTRMS
00888                      END-EXEC.                                    ELXSTRMS
00889                                                                   ELXSTRMS
00890      EXEC CICS  RETURN                                            ELXSTRMS
00891               END-EXEC.                                           ELXSTRMS
00892                                                                   ELXSTRMS
00893                                                                   ELXSTRMS
00894  9300-900-EXIT.                                                   ELXSTRMS
00895      EXIT.                                                        ELXSTRMS
00896 /*****************************************************************ELXSTRMS
00897 *                                                                *ELXSTRMS
00898 * 9400    LINK TO ELXSTRMI                                       *ELXSTRMS
00899 *                                                                *ELXSTRMS
00900 *                                                                *ELXSTRMS
00901 ******************************************************************ELXSTRMS
00902  9400-000-LINK-TO-ELXSTRMI      SECTION.                          ELXSTRMS
00903  9400-010.                                                        ELXSTRMS
00904                                                                   ELXSTRMS
00905      EXEC CICS  LINK  PROGRAM ('ELXSTRMI')                        ELXSTRMS
00906                       COMMAREA(WS-ELXSTRMI-COMMAREA)              ELXSTRMS
00907                       LENGTH  (LENGTH OF WS-ELXSTRMI-COMMAREA)    ELXSTRMS
00908                       END-EXEC.                                   ELXSTRMS
00909                                                                   ELXSTRMS
00910      MOVE STRMI-PAT-BIRTH-DATE TO MLDATE-DATE1.                   ELXSTRMS
00911      PERFORM 9810-000-CNV-TO-MMDDCCYY.                            ELXSTRMS
00912      IF MLDATE-RETURN = ZEROS                                     ELXSTRMS
00913          MOVE MLDATE-DATE2 TO BSBDATEI.                           ELXSTRMS
00914      MOVE STRMI-DATE-OF-SERVICE TO MLDATE-DATE1.                  ELXSTRMS
00915      PERFORM 9810-000-CNV-TO-MMDDCCYY.                            ELXSTRMS
00916      IF MLDATE-RETURN = ZEROS                                     ELXSTRMS
00917          MOVE MLDATE-DATE2 TO BSSERVI.                            ELXSTRMS
00918      MOVE STRMI-BENEFIT(3 1)           TO BSDEDIFO.               ELXSTRMS
00919      MOVE STRMI-BENEFIT(3 2)           TO BSDEDOFO.               ELXSTRMS
00920      MOVE STRMI-BENEFIT(4 1)           TO BSDEDIIO.               ELXSTRMS
00921      MOVE STRMI-BENEFIT(4 2)           TO BSDEDOIO.               ELXSTRMS
00922      MOVE STRMI-BENEFIT(2 1)           TO BSCOIIO.                ELXSTRMS
00923      MOVE STRMI-BENEFIT(2 2)           TO BSCOIOO.                ELXSTRMS
00924      MOVE STRMI-BENEFIT(5 1)           TO BSLIFEIO.               ELXSTRMS
00925      MOVE STRMI-BENEFIT(5 2)           TO BSLIFEOO.               ELXSTRMS
00926      MOVE STRMI-BENEFIT(6 1)           TO BSOOPIO.                ELXSTRMS
00927      MOVE STRMI-BENEFIT(6 2)           TO BSOOPOO.                ELXSTRMS
00928                                                                   ELXSTRMS
00929  9400-900-EXIT.                                                   ELXSTRMS
00930      EXIT.                                                        ELXSTRMS
00931 /*****************************************************************ELXSTRMS
00932 *                                                                *ELXSTRMS
00933 * 9800                                                           *ELXSTRMS
00934 *                                                                *ELXSTRMS
00935 *   CONVERT GREGORIAN DATE (MMDDCCYY) TO GREG DATE (CCYYMMDD)    *ELXSTRMS
00936 *                                                                *ELXSTRMS
00937 ******************************************************************ELXSTRMS
00938  9800-000-CNV-TO-CCYYMMDD       SECTION.                          ELXSTRMS
00939  9800-010.                                                        ELXSTRMS
00940                                                                   ELXSTRMS
00941      MOVE 'CNV' TO  MLDATE-FUNC.                                  ELXSTRMS
00942      MOVE 'M'   TO  MLDATE-FORM1.                                 ELXSTRMS
00943      MOVE 'Y'   TO  MLDATE-FORM2.                                 ELXSTRMS
00944      MOVE ZEROS TO  MLDATE-RETURN                                 ELXSTRMS
00945                     MLDATE-AMOUNT.                                ELXSTRMS
00946      EXEC CICS LINK PROGRAM ('MLDATEC')                           ELXSTRMS
00947                     COMMAREA(MLDATE01)                            ELXSTRMS
00948                     LENGTH  (LENGTH OF MLDATE01)                  ELXSTRMS
00949                     END-EXEC.                                     ELXSTRMS
00950                                                                   ELXSTRMS
00951  9800-900-900-EXIT.                                               ELXSTRMS
00952      EXIT.                                                        ELXSTRMS
00953 /*****************************************************************ELXSTRMS
00954 *                                                                *ELXSTRMS
00955 * 9810                                                           *ELXSTRMS
00956 *                                                                *ELXSTRMS
00957 *   CONVERT GREGORIAN DATE (CCYYMMDD) TO GREG DATE (MMDDCCYY)    *ELXSTRMS
00958 *                                                                *ELXSTRMS
00959 ******************************************************************ELXSTRMS
00960  9810-000-CNV-TO-MMDDCCYY       SECTION.                          ELXSTRMS
00961  9810-010.                                                        ELXSTRMS
00962                                                                   ELXSTRMS
00963      MOVE 'CNV' TO  MLDATE-FUNC.                                  ELXSTRMS
00964      MOVE 'Y'   TO  MLDATE-FORM1.                                 ELXSTRMS
00965      MOVE 'M'   TO  MLDATE-FORM2.                                 ELXSTRMS
00966      MOVE ZEROS TO  MLDATE-RETURN                                 ELXSTRMS
00967                     MLDATE-AMOUNT.                                ELXSTRMS
00968      EXEC CICS LINK PROGRAM ('MLDATEC')                           ELXSTRMS
00969                     COMMAREA(MLDATE01)                            ELXSTRMS
00970                     LENGTH  (LENGTH OF MLDATE01)                  ELXSTRMS
00971                     END-EXEC.                                     ELXSTRMS
00972                                                                   ELXSTRMS
00973  9810-900-900-EXIT.                                               ELXSTRMS
00974      EXIT.                                                        ELXSTRMS
00975 /***************************************************************  ELXSTRMS
00976 *                                                              *  ELXSTRMS
00977 * 9999  ABEND THE TASK                                         *  ELXSTRMS
00978 *                                                              *  ELXSTRMS
00979 ****************************************************************  ELXSTRMS
00980  9999-000-ABEND-THE-TASK        SECTION.                          ELXSTRMS
00981  9999-010.                                                        ELXSTRMS
00982                                                                   ELXSTRMS
00983      EXEC CICS  ABEND                                             ELXSTRMS
00984                 ABCODE(WS-01-ABCODE)                              ELXSTRMS
00985                 END-EXEC.                                         ELXSTRMS
00986                                                                   ELXSTRMS
00987  9999-900-EXIT.                                                   ELXSTRMS
00988      EXIT.                                                        ELXSTRMS
