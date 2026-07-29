00001  ID DIVISION.                                                     09/03/03
00002 ********* THIS IS A COBOL/2 PROGRAM ******                        ELXRMEDS
00003  PROGRAM-ID.     ELXRMEDS.                                           LV002
00004  AUTHOR.         DIANE FLOWERS.                                   ELXRMEDS
00005  DATE-WRITTEN.   09/15/00.                                        ELXRMEDS
00006  DATE-COMPILED.                                                   ELXRMEDS
00007 ******************************************************************ELXRMEDS
00008 *                                                                *ELXRMEDS
00009 *        M A I N T E N A N C E     L O G                         *ELXRMEDS
00010 *                                                                *ELXRMEDS
00011 *                                                                *ELXRMEDS
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELXRMEDS
00013 *                                                                *ELXRMEDS
00014 * P0345      09/15/00  DAF  CREATED SCREEN TO TEST REALMED       *ELXRMEDS
00015 * P03424     03/14/03  DAF  ADDED PACKAGE CODE FOR TEXAS AND     *ELXRMEDS
00016 *                           GENERATED PROGRAM ABOVE THE LINE.    *ELXRMEDS
00017 /*****************************************************************ELXRMEDS
00018 *      P R O G R A M   N A R R A T I V E                         *ELXRMEDS
00019 ******************************************************************ELXRMEDS
00020 *                                                                *ELXRMEDS
00021 *   TRANSID:      ELRM                                           *ELXRMEDS
00022 *   PROGRAM:      ELXRMEDS                                       *ELXRMEDS
00023 *   MAPSET:       ELRMSETC                                       *ELXRMEDS
00024 *                                                                *ELXRMEDS
00025 *                                                                *ELXRMEDS
00026 *   PURPOSE:   A) TO ALLOW REALMED TESTING WITHOUT GOING         *ELXRMEDS
00027 *              THROUGH REALMED SCREENS.                          *ELXRMEDS
00028 *                                                                *ELXRMEDS
00029 *   FUNCTIONS: THIS MODULE IS DESIGNED SO THAT WHAT REALMED      *ELXRMEDS
00030 *              WILL BE PASSING US CAN BE ENTERED WITHOUT         *ELXRMEDS
00031 *              GOING THROUGH REALMED.  THE FIELDS THAT PMCI AND  *ELXRMEDS
00032 *              THE ACCUM INTERFACE PASSES BACK CAN BE DISPLAYED  *ELXRMEDS
00033 *              ON THE SCREEN.                                    *ELXRMEDS
00034 *                                                                *ELXRMEDS
00035 *               1. INITIAL ENTRY:                                *ELXRMEDS
00036 *                  A) SEND MENU SCREEN (MAPONLY).                *ELXRMEDS
00037 *                  B) EXIT MODULE.                               *ELXRMEDS
00038 *                                                                *ELXRMEDS
00039 *               2. SUBSEQUENT ENTRY:                             *ELXRMEDS
00040 *                                                                *ELXRMEDS
00041 *                  A) MAP IN SCREEN.                             *ELXRMEDS
00042 *                     1) IF MAPFAIL, RETURN ERROR MESSAGE.       *ELXRMEDS
00043 *                                                                *ELXRMEDS
00044 *                  B) VALIDATE SCREEN INPUT.                     *ELXRMEDS
00045 *                                                                *ELXRMEDS
00046 *                                                                *ELXRMEDS
00047 ******************************************************************ELXRMEDS
00048 /                                                                 ELXRMEDS
00049  ENVIRONMENT DIVISION.                                            ELXRMEDS
00050  DATA DIVISION.                                                   ELXRMEDS
00051                                                                   ELXRMEDS
00052  WORKING-STORAGE SECTION.                                         ELXRMEDS
00053  01  WS-BEGIN                    PIC X(58) VALUE                  ELXRMEDS
00054      '*** ELXRMEDS WORKING-STORAGE BEGINS HERE ***'.              ELXRMEDS
00055                                                                   ELXRMEDS
00056 /---------- MIL DATE ROUTINE COMMAREA ---------------------------*ELXRMEDS
00057  COPY MLDATE01.                                                   ELXRMEDS
00058                                                                   ELXRMEDS
00059  01  WS-01-ABEND-AREA.                                            ELXRMEDS
00060      05  FILLER                   PIC X(16)  VALUE                ELXRMEDS
00061          '** ABEND AREA **'.                                      ELXRMEDS
00062                                                                   ELXRMEDS
00063      05  WS-01-ABEND-CODES-AND-MSG.                               ELXRMEDS
00064          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. ELXRMEDS
00065          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. ELXRMEDS
00066                                                                   ELXRMEDS
00067          10  WS-01-ABCODE-ELRM          PIC X(04)  VALUE  'ELBS'. ELXRMEDS
00068          10  WS-01-ABCODE-ELRM-MSG      PIC X(44)  VALUE          ELXRMEDS
00069             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          ELXRMEDS
00070                                                                   ELXRMEDS
00071 /------------ HEX VALUES COPYMEMBER ---------------------------*  ELXRMEDS
00072  COPY HEXCOBOL.                                                   ELXRMEDS
00073                                                                   ELXRMEDS
00074  01  WS-02-AREA.                                                  ELXRMEDS
00075      05  FILLER                   PIC X(16)  VALUE                ELXRMEDS
00076          '** WS-02-AREA **'.                                      ELXRMEDS
00077                                                                   ELXRMEDS
00078 *-------- EIBTRNID SAVED HERE -----------------------------------*ELXRMEDS
00079                                                                   ELXRMEDS
00080      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. ELXRMEDS
00081          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         ELXRMEDS
00082                                                    'ELRM'.        ELXRMEDS
00083                                                                   ELXRMEDS
00084 *-------- CHAR BY CHAR (ELIMINATE PRECEDING ZEROS) --------------*ELXRMEDS
00085      05  GRP-IN.                                                  ELXRMEDS
00086          10  GRP-IN-ITEM OCCURS 9 TIMES                           ELXRMEDS
00087              INDEXED BY GRP-IN-INDEX                              ELXRMEDS
00088                                  PIC X.                           ELXRMEDS
00089                                                                   ELXRMEDS
00090      05  GRP-OUT.                                                 ELXRMEDS
00091          10  GRP-OUT-ITEM OCCURS 9 TIMES                          ELXRMEDS
00092              INDEXED BY GRP-OUT-INDEX                             ELXRMEDS
00093                                  PIC X.                           ELXRMEDS
00094                                                                   ELXRMEDS
00095      05  SEC-IN.                                                  ELXRMEDS
00096          10  SEC-IN-ITEM OCCURS 5 TIMES                           ELXRMEDS
00097              INDEXED BY SEC-IN-INDEX                              ELXRMEDS
00098                                  PIC X.                           ELXRMEDS
00099                                                                   ELXRMEDS
00100      05  SEC-OUT.                                                 ELXRMEDS
00101          10  SEC-OUT-ITEM OCCURS 5 TIMES                          ELXRMEDS
00102              INDEXED BY SEC-OUT-INDEX                             ELXRMEDS
00103                                  PIC X.                           ELXRMEDS
00104                                                                   ELXRMEDS
00105 /                                                                 ELXRMEDS
00106  01  WS-03-AREA.                                                  ELXRMEDS
00107      05  FILLER                   PIC X(17)  VALUE                ELXRMEDS
00108          '** WS-03-AREA **'.                                      ELXRMEDS
00109      05  WS-03-ELRM-SCREEN-AREA.                                  ELXRMEDS
00110          10  WS-03-ELRM-TRANSID         PIC X(04).                ELXRMEDS
00111          10  FILLER                     PIC X(3196).              ELXRMEDS
00112 /                                                                 ELXRMEDS
00113  01  WT-00-ELXRMEDC-TABLES.                                       ELXRMEDS
00114      05  FILLER                   PIC X(17)  VALUE                ELXRMEDS
00115          '*ELXRMEDC TABLES*'.                                     ELXRMEDS
00116                                                                   ELXRMEDS
00117  01  WT-01-TABLE.                                                 ELXRMEDS
00118      05  FILLER                  PIC X(16) VALUE                  ELXRMEDS
00119          '* WT-01-TABLE  *'.                                      ELXRMEDS
00120 ******************************************************************ELXRMEDS
00121 *    WT-01   MESSAGE TABLE                                       *ELXRMEDS
00122 ******************************************************************ELXRMEDS
00123  01  FILLER.                                                      ELXRMEDS
00124      05  WT-01-MESSAGE-VALUES.                                    ELXRMEDS
00125                                                                   ELXRMEDS
00126 *----------------------------------------------------------------*ELXRMEDS
00127          10  WT-01-ENTRY-001.                                     ELXRMEDS
00128              15  FILLER              PIC X(2)  VALUE '¬>'.        ELXRMEDS
00129              15  WT-01-MESSAGE-TEXT-001.                          ELXRMEDS
00130                  20  FILLER          PIC X(4)  VALUE  'ELRM'.     ELXRMEDS
00131                  20  FILLER          PIC X(1)  VALUE  '-'.        ELXRMEDS
00132                  20  FILLER          PIC X(3)  VALUE  '001'.      ELXRMEDS
00133                  20  FILLER          PIC X(1)  VALUE  ' '.        ELXRMEDS
00134                  20  FILLER          PIC X(70) VALUE              ELXRMEDS
00135                      'INVALID PFKEY SELECTION                     ELXRMEDS
00136 -                    '                         '.                 ELXRMEDS
00137              15  FILLER              PIC X(2)  VALUE '<¬'.        ELXRMEDS
00138                                                                   ELXRMEDS
00139 *----------------------------------------------------------------*ELXRMEDS
00140          10  WT-01-ENTRY-002.                                     ELXRMEDS
00141              15  FILLER              PIC X(2)  VALUE '¬>'.        ELXRMEDS
00142              15  WT-01-MESSAGE-TEXT-002.                          ELXRMEDS
00143                  20  FILLER          PIC X(4)  VALUE  'ELRM'.     ELXRMEDS
00144                  20  FILLER          PIC X(1)  VALUE  '-'.        ELXRMEDS
00145                  20  FILLER          PIC X(3)  VALUE  '002'.      ELXRMEDS
00146                  20  FILLER          PIC X(1)  VALUE  ' '.        ELXRMEDS
00147                  20  FILLER          PIC X(70) VALUE              ELXRMEDS
00148                      'NOT PROPER LENGTH OR EQUAL SPACES           ELXRMEDS
00149 -                    '                         '.                 ELXRMEDS
00150              15  FILLER              PIC X(2)  VALUE '<¬'.        ELXRMEDS
00151                                                                   ELXRMEDS
00152 *----------------------------------------------------------------*ELXRMEDS
00153          10  WT-01-ENTRY-003.                                     ELXRMEDS
00154              15  FILLER              PIC X(2)  VALUE '¬>'.        ELXRMEDS
00155              15  WT-01-MESSAGE-TEXT-003.                          ELXRMEDS
00156                  20  FILLER          PIC X(4)  VALUE  'ELRM'.     ELXRMEDS
00157                  20  FILLER          PIC X(1)  VALUE  '-'.        ELXRMEDS
00158                  20  FILLER          PIC X(3)  VALUE  '003'.      ELXRMEDS
00159                  20  FILLER          PIC X(1)  VALUE  ' '.        ELXRMEDS
00160                  20  FILLER          PIC X(70) VALUE              ELXRMEDS
00161                      'PMCI CALL WAS NOT SUCCESSFUL                ELXRMEDS
00162 -                    '                         '.                 ELXRMEDS
00163              15  FILLER              PIC X(2)  VALUE '<¬'.        ELXRMEDS
00164                                                                   ELXRMEDS
00165 *----------------------------------------------------------------*ELXRMEDS
00166          10  WT-01-ENTRY-004.                                     ELXRMEDS
00167              15  FILLER              PIC X(2)  VALUE '¬>'.        ELXRMEDS
00168              15  WT-01-MESSAGE-TEXT-003.                          ELXRMEDS
00169                  20  FILLER          PIC X(4)  VALUE  'ELRM'.     ELXRMEDS
00170                  20  FILLER          PIC X(1)  VALUE  '-'.        ELXRMEDS
00171                  20  FILLER          PIC X(3)  VALUE  '004'.      ELXRMEDS
00172                  20  FILLER          PIC X(1)  VALUE  ' '.        ELXRMEDS
00173                  20  FILLER          PIC X(70) VALUE              ELXRMEDS
00174                      'ENTER DATE AGAIN IN MMDDCCYY FORMAT         ELXRMEDS
00175 -                    '                         '.                 ELXRMEDS
00176              15  FILLER              PIC X(2)  VALUE '<¬'.        ELXRMEDS
00177                                                                   ELXRMEDS
00178                                                                   ELXRMEDS
00179      05  WT-01-MESSAGE-TABLE         REDEFINES                    ELXRMEDS
00180          WT-01-MESSAGE-VALUES        OCCURS 004 TIMES             ELXRMEDS
00181                                      INDEXED BY WT-01-INDEX.      ELXRMEDS
00182          10  WT-01-ENTRY.                                         ELXRMEDS
00183              15  FILLER              PIC X(02).                   ELXRMEDS
00184              15  WT-01-MESSAGE-TEXT  PIC X(79).                   ELXRMEDS
00185              15  FILLER              PIC X(02).                   ELXRMEDS
00186                                                                   ELXRMEDS
00187                                                                   ELXRMEDS
00188 /-------------- MAP FIELD ATTRIBUTES ----------------------------*ELXRMEDS
00189  COPY DFHBMSCA.                                                   ELXRMEDS
00190 *                         AUTOSKIP, BRIGHT, FSET                  ELXRMEDS
00191      02  DFHBMABF         PIC X  VALUE 'Z'.                       ELXRMEDS
00192                                                                   ELXRMEDS
00193 /-------------- ATTENTION KEYS ----------------------------------*ELXRMEDS
00194  COPY DFHAID.                                                     ELXRMEDS
00195                                                                   ELXRMEDS
00196 /---------- REALMED INTERFACE TESTING SCREEN --------------------*ELXRMEDS
00197  COPY ELRMSETC.                                                   ELXRMEDS
00198                                                                   ELXRMEDS
00199  01  WS-ELXRMEDI-COMMAREA.                                        ELXRMEDS
00200      05  RMED-GROUP-NBR               PIC X(9).                   ELXRMEDS
00201      05  RMED-SECT-NUM                PIC X(5).                   ELXRMEDS
00202      05  RMED-SUBSCRIBER-NBR          PIC X(9).                   ELXRMEDS
00203      05  RMED-PRODUCT                 PIC X(2).                   ELXRMEDS
00204      05  RMED-DATE-OF-SERVICE         PIC X(8).                   ELXRMEDS
00205      05  RMED-LAST-NAME               PIC X(5).                   ELXRMEDS
00206      05  RMED-FIRST-NAME              PIC X(9).                   ELXRMEDS
00207      05  RMED-SEX                     PIC X(2).                   ELXRMEDS
00208      05  RMED-BIRTH-DATE              PIC X(8).                   ELXRMEDS
00209      05  RMED-RELATIONSHIP            PIC X.                      ELXRMEDS
00210      05  RMED-PKG-CODE                PIC X(3).                   ELXRMEDS
00211      05  RMED-ERROR-CODE              PIC 9(2).                   ELXRMEDS
00212      05  RMED-ERROR-DESCRIPTION       PIC X(60).                  ELXRMEDS
00213      05  RMED-IND-DED-IN              PIC X(25).                  ELXRMEDS
00214      05  RMED-IND-DED-IN-MET          PIC X(25).                  ELXRMEDS
00215      05  RMED-IND-DED-OUT             PIC X(25).                  ELXRMEDS
00216      05  RMED-IND-DED-OUT-MET         PIC X(25).                  ELXRMEDS
00217      05  RMED-FAM-DED-IN              PIC X(25).                  ELXRMEDS
00218      05  RMED-FAM-DED-IN-MET          PIC X(25).                  ELXRMEDS
00219      05  RMED-FAM-DED-OUT             PIC X(25).                  ELXRMEDS
00220      05  RMED-FAM-DED-OUT-MET         PIC X(25).                  ELXRMEDS
00221      05  RMED-COPAY-IN                PIC X(25).                  ELXRMEDS
00222      05  RMED-COPAY-OUT               PIC X(25).                  ELXRMEDS
00223      05  RMED-COINSURANCE-IN          PIC X(25).                  ELXRMEDS
00224      05  RMED-COINSURANCE-OUT         PIC X(25).                  ELXRMEDS
00225                                                                   ELXRMEDS
00226  01  WS-END                       PIC X(58) VALUE                 ELXRMEDS
00227      '*** ELXRMEDS WORKING-STORAGE ENDS HERE ***'.                ELXRMEDS
00228                                                                   ELXRMEDS
00229 /                                                                 ELXRMEDS
00230  PROCEDURE DIVISION.                                              ELXRMEDS
00231                                                                   ELXRMEDS
00232 ****************************************************************  ELXRMEDS
00233 *                                                              *  ELXRMEDS
00234 *           P R O C E S S     C O N T R O L                    *  ELXRMEDS
00235 *                                                              *  ELXRMEDS
00236 ****************************************************************  ELXRMEDS
00237  0000-000-PROCESS-CONTROL       SECTION.                          ELXRMEDS
00238  0000-010.                                                        ELXRMEDS
00239                                                                   ELXRMEDS
00240      MOVE EIBTRNID TO WS-02-EIBTRNID.                             ELXRMEDS
00241                                                                   ELXRMEDS
00242      IF  WS-02-VALID-ENTRY-EIBTRNID                               ELXRMEDS
00243      THEN                                                         ELXRMEDS
00244          PERFORM  2000-000-PROCESS-INPUT                          ELXRMEDS
00245      ELSE                                                         ELXRMEDS
00246          PERFORM  1000-000-DISPLAY-SCREEN.                        ELXRMEDS
00247                                                                   ELXRMEDS
00248 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*ELXRMEDS
00249                                                                   ELXRMEDS
00250      MOVE WS-01-ABCODE-ELRM     TO WS-01-ABCODE                   ELXRMEDS
00251      MOVE WS-01-ABCODE-ELRM-MSG TO WS-01-ABCODE-MSG               ELXRMEDS
00252      PERFORM  9999-000-ABEND-THE-TASK.                            ELXRMEDS
00253                                                                   ELXRMEDS
00254      GOBACK.                                                      ELXRMEDS
00255                                                                   ELXRMEDS
00256                                                                   ELXRMEDS
00257  0000-900-EXIT.                                                   ELXRMEDS
00258      EXIT.                                                        ELXRMEDS
00259 /***************************************************************  ELXRMEDS
00260 *                                                              *  ELXRMEDS
00261 * 1000  DISPLAY INITIAL SCREEN                                 *  ELXRMEDS
00262 *                                                              *  ELXRMEDS
00263 ****************************************************************  ELXRMEDS
00264  1000-000-DISPLAY-SCREEN        SECTION.                          ELXRMEDS
00265  1000-010.                                                        ELXRMEDS
00266                                                                   ELXRMEDS
00267 *------- SEND INITIAL SCREEN ------------------------------------*ELXRMEDS
00268                                                                   ELXRMEDS
00269      MOVE LOW-VALUES TO ELRMI01O.                                 ELXRMEDS
00270      MOVE  -1 TO  RMGRPL.                                         ELXRMEDS
00271      PERFORM 9200-000-SEND-THEN-RETURN.                           ELXRMEDS
00272                                                                   ELXRMEDS
00273                                                                   ELXRMEDS
00274  1000-900-EXIT.                                                   ELXRMEDS
00275      EXIT.                                                        ELXRMEDS
00276 /***************************************************************  ELXRMEDS
00277 *                                                              *  ELXRMEDS
00278 * 2000    P R O C E S S    I N P U T                           *  ELXRMEDS
00279 *                                                              *  ELXRMEDS
00280 ****************************************************************  ELXRMEDS
00281  2000-000-PROCESS-INPUT         SECTION.                          ELXRMEDS
00282  2000-010.                                                        ELXRMEDS
00283                                                                   ELXRMEDS
00284 *------ VALIDATE PFKEY USAGE ------------------------------------*ELXRMEDS
00285                                                                   ELXRMEDS
00286      IF  EIBAID NOT = DFHENTER                                    ELXRMEDS
00287          MOVE  -1 TO  RMERMSGL                                    ELXRMEDS
00288          SET WT-01-INDEX TO +01                                   ELXRMEDS
00289          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00290          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXRMEDS
00291                                                                   ELXRMEDS
00292      EXEC CICS                                                    ELXRMEDS
00293           RECEIVE INTO(WS-03-ELRM-SCREEN-AREA)                    ELXRMEDS
00294      END-EXEC.                                                    ELXRMEDS
00295                                                                   ELXRMEDS
00296      EXEC CICS  HANDLE CONDITION                                  ELXRMEDS
00297                        MAPFAIL(9200-000-SEND-THEN-RETURN)         ELXRMEDS
00298                        END-EXEC.                                  ELXRMEDS
00299                                                                   ELXRMEDS
00300      EXEC CICS  RECEIVE MAP   ('ELRMI01')                         ELXRMEDS
00301                         FROM(WS-03-ELRM-SCREEN-AREA)              ELXRMEDS
00302                         MAPSET('ELRMSET')                         ELXRMEDS
00303                         INTO(ELRMI01I)                            ELXRMEDS
00304                         END-EXEC.                                 ELXRMEDS
00305                                                                   ELXRMEDS
00306      IF RMFUNCNI NOT = 'ELRM' OR                                  ELXRMEDS
00307         RMSCRNI  NOT = 'ELRM00'                                   ELXRMEDS
00308          MOVE  -1 TO  RMERMSGL                                    ELXRMEDS
00309          PERFORM 9200-000-SEND-THEN-RETURN.                       ELXRMEDS
00310                                                                   ELXRMEDS
00311 *--- PROCESS SCREEN FIELDS --------------------------------------*ELXRMEDS
00312                                                                   ELXRMEDS
00313      PERFORM 2100-000-FIELD-EDITS.                                ELXRMEDS
00314                                                                   ELXRMEDS
00315      PERFORM 9400-000-LINK-TO-ELXSTRMI.                           ELXRMEDS
00316      IF RMED-ERROR-CODE = 01 OR 02 OR 03 OR 04 OR 05              ELXRMEDS
00317          MOVE -1 TO RMERMSGL                                      ELXRMEDS
00318          SET WT-01-INDEX TO +03                                   ELXRMEDS
00319          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00320          PERFORM 9300-000-SEND-THEN-RETURN                        ELXRMEDS
00321          GO TO 2000-900-EXIT                                      ELXRMEDS
00322      ELSE                                                         ELXRMEDS
00323          MOVE -1 TO RMGRPL                                        ELXRMEDS
00324          PERFORM 9300-000-SEND-THEN-RETURN                        ELXRMEDS
00325          GO TO 2000-900-EXIT.                                     ELXRMEDS
00326                                                                   ELXRMEDS
00327  2000-900-EXIT.                                                   ELXRMEDS
00328      EXIT.                                                        ELXRMEDS
00329 /***************************************************************  ELXRMEDS
00330 *                                                              *  ELXRMEDS
00331 * 2100  DO SCREEN FIELD EDITS                                  *  ELXRMEDS
00332 *                                                              *  ELXRMEDS
00333 ****************************************************************  ELXRMEDS
00334  2100-000-FIELD-EDITS           SECTION.                          ELXRMEDS
00335  2100-010.                                                        ELXRMEDS
00336                                                                   ELXRMEDS
00337 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*ELXRMEDS
00338                                                                   ELXRMEDS
00339      MOVE DFHBMUNF TO  RMGRPA                                     ELXRMEDS
00340                        RMSECA                                     ELXRMEDS
00341                        RMSUBA                                     ELXRMEDS
00342                        RMPRODA                                    ELXRMEDS
00343                        RMSRVDTA                                   ELXRMEDS
00344                        RMLNAMEA                                   ELXRMEDS
00345                        RMFNAMEA                                   ELXRMEDS
00346                        RMSEXA                                     ELXRMEDS
00347                        RMBDATEA                                   ELXRMEDS
00348                        RMRELA                                     ELXRMEDS
00349                        RMPKGA.                                    ELXRMEDS
00350                                                                   ELXRMEDS
00351      INSPECT RMGRPI    REPLACING ALL '_' BY SPACES.               ELXRMEDS
00352      INSPECT RMSECI    REPLACING ALL '_' BY SPACES.               ELXRMEDS
00353      INSPECT RMSUBI    REPLACING ALL '_' BY SPACES.               ELXRMEDS
00354      INSPECT RMPRODI   REPLACING ALL '_' BY SPACES.               ELXRMEDS
00355      INSPECT RMSRVDTI  REPLACING ALL '_' BY SPACES.               ELXRMEDS
00356      INSPECT RMLNAMEI  REPLACING ALL '_' BY SPACES.               ELXRMEDS
00357      INSPECT RMFNAMEI  REPLACING ALL '_' BY SPACES.               ELXRMEDS
00358      INSPECT RMSEXI    REPLACING ALL '_' BY SPACES.               ELXRMEDS
00359      INSPECT RMBDATEI  REPLACING ALL '_' BY SPACES.               ELXRMEDS
00360      INSPECT RMRELI    REPLACING ALL '_' BY SPACES.               ELXRMEDS
00361      INSPECT RMPKGI    REPLACING ALL '_' BY SPACES.               ELXRMEDS
00362                                                                   ELXRMEDS
00363      IF (RMGRPI = SPACES) OR                                      ELXRMEDS
00364         (RMGRPL < 1)                                              ELXRMEDS
00365          MOVE -1 TO RMGRPL                                        ELXRMEDS
00366          MOVE DFHBMUBF TO RMGRPA                                  ELXRMEDS
00367          SET WT-01-INDEX TO +02                                   ELXRMEDS
00368          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00369          PERFORM 9300-000-SEND-THEN-RETURN                        ELXRMEDS
00370      ELSE                                                         ELXRMEDS
00371          MOVE ZEROES TO GRP-OUT                                   ELXRMEDS
00372          MOVE RMGRPI TO GRP-IN                                    ELXRMEDS
00373          PERFORM 2500-RIGHT-JUSTIFY-GRP                           ELXRMEDS
00374          MOVE GRP-OUT TO RMGRPI.                                  ELXRMEDS
00375                                                                   ELXRMEDS
00376      IF (RMSECI = SPACES) OR                                      ELXRMEDS
00377         (RMSECL < 1)                                              ELXRMEDS
00378          MOVE -1 TO RMSECL                                        ELXRMEDS
00379          MOVE DFHBMUBF TO RMSECA                                  ELXRMEDS
00380          SET WT-01-INDEX TO +02                                   ELXRMEDS
00381          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00382          PERFORM 9300-000-SEND-THEN-RETURN                        ELXRMEDS
00383      ELSE                                                         ELXRMEDS
00384          MOVE ZEROES TO SEC-OUT                                   ELXRMEDS
00385          MOVE RMSECI TO SEC-IN                                    ELXRMEDS
00386          PERFORM 2600-RIGHT-JUSTIFY-SEC                           ELXRMEDS
00387          MOVE SEC-OUT TO RMSECI.                                  ELXRMEDS
00388                                                                   ELXRMEDS
00389      IF (RMSUBI = SPACES) OR                                      ELXRMEDS
00390         (RMSUBL < 9)                                              ELXRMEDS
00391          MOVE -1 TO RMSUBL                                        ELXRMEDS
00392          MOVE DFHBMUBF TO RMSUBA                                  ELXRMEDS
00393          SET WT-01-INDEX TO +02                                   ELXRMEDS
00394          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00395          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXRMEDS
00396                                                                   ELXRMEDS
00397      IF (RMPRODI = SPACES) OR                                     ELXRMEDS
00398         (RMPRODL < 2)                                             ELXRMEDS
00399          MOVE -1 TO RMPRODL                                       ELXRMEDS
00400          MOVE DFHBMUBF TO RMPRODA                                 ELXRMEDS
00401          SET WT-01-INDEX TO +02                                   ELXRMEDS
00402          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00403          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXRMEDS
00404                                                                   ELXRMEDS
00405      IF (RMSRVDTI = SPACES) OR                                    ELXRMEDS
00406         (RMSRVDTL < 8)                                            ELXRMEDS
00407          MOVE -1 TO RMSRVDTL                                      ELXRMEDS
00408          MOVE DFHBMUBF TO RMSRVDTA                                ELXRMEDS
00409          SET WT-01-INDEX TO +02                                   ELXRMEDS
00410          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00411          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXRMEDS
00412                                                                   ELXRMEDS
00413      IF (RMLNAMEI = SPACES) OR                                    ELXRMEDS
00414         (RMLNAMEL < 5)                                            ELXRMEDS
00415          MOVE -1 TO RMLNAMEL                                      ELXRMEDS
00416          MOVE DFHBMUBF TO RMLNAMEA                                ELXRMEDS
00417          SET WT-01-INDEX TO +02                                   ELXRMEDS
00418          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00419          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXRMEDS
00420                                                                   ELXRMEDS
00421      IF (RMFNAMEI = SPACES) OR                                    ELXRMEDS
00422         (RMFNAMEL < 1)                                            ELXRMEDS
00423          MOVE -1 TO RMFNAMEL                                      ELXRMEDS
00424          MOVE DFHBMUBF TO RMFNAMEA                                ELXRMEDS
00425          SET WT-01-INDEX TO +02                                   ELXRMEDS
00426          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00427          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXRMEDS
00428                                                                   ELXRMEDS
00429      IF (RMSEXI = SPACES) OR                                      ELXRMEDS
00430         (RMSEXL < 1)                                              ELXRMEDS
00431          MOVE -1 TO RMSEXL                                        ELXRMEDS
00432          MOVE DFHBMUBF TO RMSEXA                                  ELXRMEDS
00433          SET WT-01-INDEX TO +02                                   ELXRMEDS
00434          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00435          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXRMEDS
00436                                                                   ELXRMEDS
00437      IF (RMBDATEI = SPACES) OR                                    ELXRMEDS
00438         (RMBDATEL < 8)                                            ELXRMEDS
00439          MOVE -1 TO RMBDATEL                                      ELXRMEDS
00440          MOVE DFHBMUBF TO RMBDATEA                                ELXRMEDS
00441          SET WT-01-INDEX TO +02                                   ELXRMEDS
00442          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00443          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXRMEDS
00444                                                                   ELXRMEDS
00445      IF (RMRELI = SPACES) OR                                      ELXRMEDS
00446         (RMRELL < 1)                                              ELXRMEDS
00447          MOVE -1 TO RMRELL                                        ELXRMEDS
00448          MOVE DFHBMUBF TO RMRELA                                  ELXRMEDS
00449          SET WT-01-INDEX TO +02                                   ELXRMEDS
00450          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00451          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXRMEDS
00452                                                                   ELXRMEDS
00453      MOVE RMGRPI   TO RMED-GROUP-NBR.                             ELXRMEDS
00454      MOVE RMSECI   TO RMED-SECT-NUM.                              ELXRMEDS
00455      MOVE RMSUBI   TO RMED-SUBSCRIBER-NBR.                        ELXRMEDS
00456      MOVE RMBDATEI TO MLDATE-DATE1.                               ELXRMEDS
00457      PERFORM 9800-000-CNV-TO-CCYYMMDD.                            ELXRMEDS
00458      IF MLDATE-RETURN = ZEROS                                     ELXRMEDS
00459          MOVE MLDATE-DATE2 TO RMED-BIRTH-DATE                     ELXRMEDS
00460      ELSE                                                         ELXRMEDS
00461          MOVE -1 TO RMBDATEL                                      ELXRMEDS
00462          MOVE DFHBMUBF TO RMBDATEA                                ELXRMEDS
00463          SET WT-01-INDEX TO +04                                   ELXRMEDS
00464          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00465          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXRMEDS
00466      MOVE RMRELI   TO RMED-RELATIONSHIP.                          ELXRMEDS
00467      IF RMPKGI = SPACES                                           ELXRMEDS
00468          MOVE ZEROES TO RMED-PKG-CODE                             ELXRMEDS
00469      ELSE                                                         ELXRMEDS
00470          MOVE RMPKGI TO RMED-PKG-CODE.                            ELXRMEDS
00471      MOVE RMSRVDTI TO MLDATE-DATE1.                               ELXRMEDS
00472      PERFORM 9800-000-CNV-TO-CCYYMMDD.                            ELXRMEDS
00473      IF MLDATE-RETURN = ZEROS                                     ELXRMEDS
00474          MOVE MLDATE-DATE2 TO RMED-DATE-OF-SERVICE                ELXRMEDS
00475      ELSE                                                         ELXRMEDS
00476          MOVE -1 TO RMSRVDTL                                      ELXRMEDS
00477          MOVE DFHBMUBF TO RMSRVDTA                                ELXRMEDS
00478          SET WT-01-INDEX TO +04                                   ELXRMEDS
00479          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      ELXRMEDS
00480          PERFORM 9300-000-SEND-THEN-RETURN.                       ELXRMEDS
00481      MOVE RMPRODI  TO RMED-PRODUCT.                               ELXRMEDS
00482                                                                   ELXRMEDS
00483  2100-900-EXIT.                                                   ELXRMEDS
00484      EXIT.                                                        ELXRMEDS
00485                                                                   ELXRMEDS
00486 ************************************************************      ELXRMEDS
00487 *                                                          *      ELXRMEDS
00488 *        STRIP GROUP                                       *      ELXRMEDS
00489 *                                                          *      ELXRMEDS
00490 ************************************************************      ELXRMEDS
00491  2500-RIGHT-JUSTIFY-GRP  SECTION.                                 ELXRMEDS
00492  2500-010.                                                        ELXRMEDS
00493      SET GRP-OUT-INDEX TO 9.                                      ELXRMEDS
00494      PERFORM 2510-STRIP-GRP-INPUT                                 ELXRMEDS
00495          VARYING GRP-IN-INDEX FROM 9 BY -1                        ELXRMEDS
00496          UNTIL GRP-IN-INDEX < 1.                                  ELXRMEDS
00497                                                                   ELXRMEDS
00498  2500-900-EXIT.                                                   ELXRMEDS
00499      EXIT.                                                        ELXRMEDS
00500                                                                   ELXRMEDS
00501 ************************************************************      ELXRMEDS
00502 *                                                          *      ELXRMEDS
00503 *        STRIP GROUP INPUT                                 *      ELXRMEDS
00504 *                                                          *      ELXRMEDS
00505 ************************************************************      ELXRMEDS
00506  2510-STRIP-GRP-INPUT  SECTION.                                   ELXRMEDS
00507  2510-010.                                                        ELXRMEDS
00508      IF GRP-IN-ITEM (GRP-IN-INDEX) IS NUMERIC                     ELXRMEDS
00509           OR GRP-IN-ITEM (GRP-IN-INDEX) IS ALPHABETIC             ELXRMEDS
00510          PERFORM 2520-MOVE-GRP-CHAR.                              ELXRMEDS
00511                                                                   ELXRMEDS
00512  2510-900-EXIT.                                                   ELXRMEDS
00513      EXIT.                                                        ELXRMEDS
00514                                                                   ELXRMEDS
00515 ************************************************************      ELXRMEDS
00516 *                                                          *      ELXRMEDS
00517 *        MOVE GROUP CHAR                                   *      ELXRMEDS
00518 *                                                          *      ELXRMEDS
00519 ************************************************************      ELXRMEDS
00520  2520-MOVE-GRP-CHAR  SECTION.                                     ELXRMEDS
00521  2520-010.                                                        ELXRMEDS
00522      IF  GRP-IN-ITEM (GRP-IN-INDEX) NOT = SPACE                   ELXRMEDS
00523          PERFORM 2530-MOVE-GRP-CHAR-NSPC.                         ELXRMEDS
00524                                                                   ELXRMEDS
00525  2520-900-EXIT.                                                   ELXRMEDS
00526      EXIT.                                                        ELXRMEDS
00527                                                                   ELXRMEDS
00528 ************************************************************      ELXRMEDS
00529 *                                                          *      ELXRMEDS
00530 *        MOVE GROUP CHAR NSPC                              *      ELXRMEDS
00531 *                                                          *      ELXRMEDS
00532 ************************************************************      ELXRMEDS
00533  2530-MOVE-GRP-CHAR-NSPC  SECTION.                                ELXRMEDS
00534  2530-010.                                                        ELXRMEDS
00535      MOVE GRP-IN-ITEM (GRP-IN-INDEX)                              ELXRMEDS
00536                             TO GRP-OUT-ITEM (GRP-OUT-INDEX).      ELXRMEDS
00537      SET GRP-OUT-INDEX DOWN BY 1.                                 ELXRMEDS
00538                                                                   ELXRMEDS
00539  2530-900-EXIT.                                                   ELXRMEDS
00540      EXIT.                                                        ELXRMEDS
00541                                                                   ELXRMEDS
00542 ************************************************************      ELXRMEDS
00543 *                                                          *      ELXRMEDS
00544 *        STRIP SECTION                                     *      ELXRMEDS
00545 *                                                          *      ELXRMEDS
00546 ************************************************************      ELXRMEDS
00547  2600-RIGHT-JUSTIFY-SEC  SECTION.                                 ELXRMEDS
00548  2600-010.                                                        ELXRMEDS
00549      SET SEC-OUT-INDEX TO 5.                                      ELXRMEDS
00550      PERFORM 2610-STRIP-SEC-INPUT                                 ELXRMEDS
00551          VARYING SEC-IN-INDEX FROM 5 BY -1                        ELXRMEDS
00552          UNTIL SEC-IN-INDEX < 1.                                  ELXRMEDS
00553                                                                   ELXRMEDS
00554  2600-900-EXIT.                                                   ELXRMEDS
00555      EXIT.                                                        ELXRMEDS
00556                                                                   ELXRMEDS
00557 ************************************************************      ELXRMEDS
00558 *                                                          *      ELXRMEDS
00559 *        STRIP SECTION INPUT                               *      ELXRMEDS
00560 *                                                          *      ELXRMEDS
00561 ************************************************************      ELXRMEDS
00562  2610-STRIP-SEC-INPUT  SECTION.                                   ELXRMEDS
00563  2610-010.                                                        ELXRMEDS
00564      IF SEC-IN-ITEM (SEC-IN-INDEX) IS NUMERIC                     ELXRMEDS
00565           OR SEC-IN-ITEM (SEC-IN-INDEX) IS ALPHABETIC             ELXRMEDS
00566          PERFORM 2620-MOVE-SEC-CHAR.                              ELXRMEDS
00567                                                                   ELXRMEDS
00568  2610-900-EXIT.                                                   ELXRMEDS
00569      EXIT.                                                        ELXRMEDS
00570                                                                   ELXRMEDS
00571 ************************************************************      ELXRMEDS
00572 *                                                          *      ELXRMEDS
00573 *        MOVE SECTION CHAR                                 *      ELXRMEDS
00574 *                                                          *      ELXRMEDS
00575 ************************************************************      ELXRMEDS
00576  2620-MOVE-SEC-CHAR  SECTION.                                     ELXRMEDS
00577  2620-010.                                                        ELXRMEDS
00578      IF  SEC-IN-ITEM (SEC-IN-INDEX) NOT = SPACE                   ELXRMEDS
00579          PERFORM 2630-MOVE-SEC-CHAR-NSPC.                         ELXRMEDS
00580                                                                   ELXRMEDS
00581  2620-900-EXIT.                                                   ELXRMEDS
00582      EXIT.                                                        ELXRMEDS
00583                                                                   ELXRMEDS
00584 ************************************************************      ELXRMEDS
00585 *                                                          *      ELXRMEDS
00586 *        MOVE SECTION CHAR NSPC                            *      ELXRMEDS
00587 *                                                          *      ELXRMEDS
00588 ************************************************************      ELXRMEDS
00589  2630-MOVE-SEC-CHAR-NSPC  SECTION.                                ELXRMEDS
00590  2630-010.                                                        ELXRMEDS
00591      MOVE SEC-IN-ITEM (SEC-IN-INDEX)                              ELXRMEDS
00592                             TO SEC-OUT-ITEM (SEC-OUT-INDEX).      ELXRMEDS
00593      SET SEC-OUT-INDEX DOWN BY 1.                                 ELXRMEDS
00594                                                                   ELXRMEDS
00595  2630-900-EXIT.                                                   ELXRMEDS
00596      EXIT.                                                        ELXRMEDS
00597                                                                   ELXRMEDS
00598 /***************************************************************  ELXRMEDS
00599 *                                                              *  ELXRMEDS
00600 * 9000   MOVE MESSAGE TO SCREEN                                *  ELXRMEDS
00601 *                                                              *  ELXRMEDS
00602 ****************************************************************  ELXRMEDS
00603  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          ELXRMEDS
00604  9000-010.                                                        ELXRMEDS
00605                                                                   ELXRMEDS
00606      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)                         ELXRMEDS
00607                    TO RMERMSGO.                                   ELXRMEDS
00608                                                                   ELXRMEDS
00609  9000-900-EXIT.                                                   ELXRMEDS
00610      EXIT.                                                        ELXRMEDS
00611 /***************************************************************  ELXRMEDS
00612 *                                                              *  ELXRMEDS
00613 * 9200 SEND SCREEN AND RETURN                                  *  ELXRMEDS
00614 *                                                              *  ELXRMEDS
00615 ****************************************************************  ELXRMEDS
00616  9200-000-SEND-THEN-RETURN      SECTION.                          ELXRMEDS
00617  9200-010.                                                        ELXRMEDS
00618                                                                   ELXRMEDS
00619      EXEC CICS  SEND MAP   ('ELRMI01')                            ELXRMEDS
00620                      MAPSET('ELRMSET')                            ELXRMEDS
00621                      MAPONLY                                      ELXRMEDS
00622                      ERASE                                        ELXRMEDS
00623                      END-EXEC.                                    ELXRMEDS
00624                                                                   ELXRMEDS
00625      EXEC CICS  RETURN                                            ELXRMEDS
00626               END-EXEC.                                           ELXRMEDS
00627                                                                   ELXRMEDS
00628                                                                   ELXRMEDS
00629  9200-900-EXIT.                                                   ELXRMEDS
00630      EXIT.                                                        ELXRMEDS
00631 /***************************************************************  ELXRMEDS
00632 *                                                              *  ELXRMEDS
00633 * 9300 SEND SCREEN AND RETURN                                  *  ELXRMEDS
00634 *                                                              *  ELXRMEDS
00635 ****************************************************************  ELXRMEDS
00636  9300-000-SEND-THEN-RETURN      SECTION.                          ELXRMEDS
00637  9300-010.                                                        ELXRMEDS
00638                                                                   ELXRMEDS
00639      EXEC CICS  SEND MAP   ('ELRMI01')                            ELXRMEDS
00640                      MAPSET('ELRMSET')                            ELXRMEDS
00641                      ERASE                                        ELXRMEDS
00642                      FROM (ELRMI01O)                              ELXRMEDS
00643                      CURSOR                                       ELXRMEDS
00644                      END-EXEC.                                    ELXRMEDS
00645                                                                   ELXRMEDS
00646      EXEC CICS  RETURN                                            ELXRMEDS
00647               END-EXEC.                                           ELXRMEDS
00648                                                                   ELXRMEDS
00649                                                                   ELXRMEDS
00650  9300-900-EXIT.                                                   ELXRMEDS
00651      EXIT.                                                        ELXRMEDS
00652 /*****************************************************************ELXRMEDS
00653 *                                                                *ELXRMEDS
00654 * 9400    LINK TO ELXSTRMI                                       *ELXRMEDS
00655 *                                                                *ELXRMEDS
00656 *                                                                *ELXRMEDS
00657 ******************************************************************ELXRMEDS
00658  9400-000-LINK-TO-ELXSTRMI      SECTION.                          ELXRMEDS
00659  9400-010.                                                        ELXRMEDS
00660                                                                   ELXRMEDS
00661      EXEC CICS  LINK  PROGRAM ('ELXRMEDI')                        ELXRMEDS
00662                       COMMAREA(WS-ELXRMEDI-COMMAREA)              ELXRMEDS
00663                       LENGTH  (LENGTH OF WS-ELXRMEDI-COMMAREA)    ELXRMEDS
00664                       END-EXEC.                                   ELXRMEDS
00665                                                                   ELXRMEDS
00666      MOVE RMED-BIRTH-DATE TO MLDATE-DATE1.                        ELXRMEDS
00667      PERFORM 9810-000-CNV-TO-MMDDCCYY.                            ELXRMEDS
00668      IF MLDATE-RETURN = ZEROS                                     ELXRMEDS
00669          MOVE MLDATE-DATE2 TO RMBDATEI.                           ELXRMEDS
00670      MOVE RMED-DATE-OF-SERVICE TO MLDATE-DATE1.                   ELXRMEDS
00671      PERFORM 9810-000-CNV-TO-MMDDCCYY.                            ELXRMEDS
00672      IF MLDATE-RETURN = ZEROS                                     ELXRMEDS
00673          MOVE MLDATE-DATE2 TO RMSRVDTI.                           ELXRMEDS
00674      MOVE RMED-IND-DED-IN              TO RMDEDIIO.               ELXRMEDS
00675      MOVE RMED-IND-DED-IN-MET          TO RMDEIIMO.               ELXRMEDS
00676      MOVE RMED-IND-DED-OUT             TO RMDEDIOO.               ELXRMEDS
00677      MOVE RMED-IND-DED-OUT-MET         TO RMDEIOMO.               ELXRMEDS
00678      MOVE RMED-FAM-DED-IN              TO RMDEDFIO.               ELXRMEDS
00679      MOVE RMED-FAM-DED-IN-MET          TO RMDEFIMO.               ELXRMEDS
00680      MOVE RMED-FAM-DED-OUT             TO RMDEDFOO.               ELXRMEDS
00681      MOVE RMED-FAM-DED-OUT-MET         TO RMDEFOMO.               ELXRMEDS
00682      MOVE RMED-COPAY-IN                TO RMCOPYIO.               ELXRMEDS
00683      MOVE RMED-COPAY-OUT               TO RMCOPYOO.               ELXRMEDS
00684      MOVE RMED-COINSURANCE-IN          TO RMCOINIO.               ELXRMEDS
00685      MOVE RMED-COINSURANCE-OUT         TO RMCOINOO.               ELXRMEDS
00686                                                                   ELXRMEDS
00687  9400-900-EXIT.                                                   ELXRMEDS
00688      EXIT.                                                        ELXRMEDS
00689 /*****************************************************************ELXRMEDS
00690 *                                                                *ELXRMEDS
00691 * 9800                                                           *ELXRMEDS
00692 *                                                                *ELXRMEDS
00693 *   CONVERT GREGORIAN DATE (MMDDCCYY) TO GREG DATE (CCYYMMDD)    *ELXRMEDS
00694 *                                                                *ELXRMEDS
00695 ******************************************************************ELXRMEDS
00696  9800-000-CNV-TO-CCYYMMDD       SECTION.                          ELXRMEDS
00697  9800-010.                                                        ELXRMEDS
00698                                                                   ELXRMEDS
00699      MOVE 'CNV' TO  MLDATE-FUNC.                                  ELXRMEDS
00700      MOVE 'M'   TO  MLDATE-FORM1.                                 ELXRMEDS
00701      MOVE 'Y'   TO  MLDATE-FORM2.                                 ELXRMEDS
00702      MOVE ZEROS TO  MLDATE-RETURN                                 ELXRMEDS
00703                     MLDATE-AMOUNT.                                ELXRMEDS
00704      EXEC CICS LINK PROGRAM ('MLDATEC')                           ELXRMEDS
00705                     COMMAREA(MLDATE01)                            ELXRMEDS
00706                     LENGTH  (LENGTH OF MLDATE01)                  ELXRMEDS
00707                     END-EXEC.                                     ELXRMEDS
00708                                                                   ELXRMEDS
00709  9800-900-900-EXIT.                                               ELXRMEDS
00710      EXIT.                                                        ELXRMEDS
00711 /*****************************************************************ELXRMEDS
00712 *                                                                *ELXRMEDS
00713 * 9810                                                           *ELXRMEDS
00714 *                                                                *ELXRMEDS
00715 *   CONVERT GREGORIAN DATE (CCYYMMDD) TO GREG DATE (MMDDCCYY)    *ELXRMEDS
00716 *                                                                *ELXRMEDS
00717 ******************************************************************ELXRMEDS
00718  9810-000-CNV-TO-MMDDCCYY       SECTION.                          ELXRMEDS
00719  9810-010.                                                        ELXRMEDS
00720                                                                   ELXRMEDS
00721      MOVE 'CNV' TO  MLDATE-FUNC.                                  ELXRMEDS
00722      MOVE 'Y'   TO  MLDATE-FORM1.                                 ELXRMEDS
00723      MOVE 'M'   TO  MLDATE-FORM2.                                 ELXRMEDS
00724      MOVE ZEROS TO  MLDATE-RETURN                                 ELXRMEDS
00725                     MLDATE-AMOUNT.                                ELXRMEDS
00726      EXEC CICS LINK PROGRAM ('MLDATEC')                           ELXRMEDS
00727                     COMMAREA(MLDATE01)                            ELXRMEDS
00728                     LENGTH  (LENGTH OF MLDATE01)                  ELXRMEDS
00729                     END-EXEC.                                     ELXRMEDS
00730                                                                   ELXRMEDS
00731  9810-900-900-EXIT.                                               ELXRMEDS
00732      EXIT.                                                        ELXRMEDS
00733 /***************************************************************  ELXRMEDS
00734 *                                                              *  ELXRMEDS
00735 * 9999  ABEND THE TASK                                         *  ELXRMEDS
00736 *                                                              *  ELXRMEDS
00737 ****************************************************************  ELXRMEDS
00738  9999-000-ABEND-THE-TASK        SECTION.                          ELXRMEDS
00739  9999-010.                                                        ELXRMEDS
00740                                                                   ELXRMEDS
00741      EXEC CICS  ABEND                                             ELXRMEDS
00742                 ABCODE(WS-01-ABCODE)                              ELXRMEDS
00743                 END-EXEC.                                         ELXRMEDS
00744                                                                   ELXRMEDS
00745  9999-900-EXIT.                                                   ELXRMEDS
00746      EXIT.                                                        ELXRMEDS
