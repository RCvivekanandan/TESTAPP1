       IDENTIFICATION DIVISION.                                         00000100
       PROGRAM-ID. GAGSAPGM.                                            00000200
       DATE-WRITTEN. 01/02/2015.                                        00000300
       DATE-COMPILED.                                                   00000400
           SKIP3                                                        00000500
      ******************************************************************00000600
      *** GAGSAPGM    ATB - MULTIPLE GROUP AND SECTION NUMBERS.         00000700
      *                                                                 00000800
      *   THIS PROGRAM IS DESIGNED TO ALLOW THE OPERATOR TO PERFORM     00000900
      *   AN ATB (ACROSS THE BOARD) CHANGE OF NETWORK SET ID FOR        00001000
      *   MULIPLE GROUP SECTIONS.                                       00001100
      *                                                                 00001200
      ******************************************************************00001300
      *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00001400
      *       *-*         U P D A T E   H I S T O R Y         *-*      *00001500
      *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *00001600
      *                                                                 00001700
      **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*00001800
      *                                                                *00001900
      *           01/04/2015 I331809  INITIAL PROGRAM INSTALLATION     *00002000
013584*           01/21/2015 I331520  NETWORK SET ID \
      *                               \
      *                                CORP ENTITY CODE                *00002030
JJ0123* GCPS00250 01/23/2015 I334539  VALIDATE SCREEN DATA WHEN ENTER  *00002040
JJ0123*                               KEY PRESSED ALSO. QC DEFECT 13584*00002050
RB0123* DEF.13683 01/23/2015 I331809  CHANGE CORP ENT CD TO LOOK AT    *00002060
RB0123*                               COPYBOOK GNTW002 FOR VALID VALUES*00002070
      *SVC0000011 08/17/2015 I031306  MOVE CWAPEARL TO DYN-PEARL-IND   *00002080
      ******************************************************************00002100
           SKIP3                                                        00002200
       ENVIRONMENT DIVISION.                                            00002300
                                                                        00002400
       DATA DIVISION.                                                   00002500
                                                                        00002600
       WORKING-STORAGE SECTION.                                         00002700
                                                                        00002800
       01  WS-MISC-FIELDS.                                              00002900
           05  WS-TRAN-ID              PIC X(04)  VALUE 'GAGS'.         00003000
           05  WS-GAGSAPGM             PIC X(08)  VALUE 'GAGSAPGM'.     00003100
           05  WS-GCPSDAS              PIC X(8)   VALUE 'GCPSDAS '.     00003200
           05  WS-WORK-INSTANCE-NM     PIC X(60)  VALUE SPACES.         00003300
                                                                        00003400
      *    '***GAGSAPGM WS BEGINS***'.                                  00003500
           05  WS-PARA-ID              PIC X(15) VALUE SPACES.          00003600
           05  WS-AUD-PROC             PIC X(30) VALUE SPACES.          00003700
           05  WS-ABEND-CODE           PIC X(4) VALUE 'XXXX'.           00003800
           05  WS-DFHRESP              PIC S9(08) COMP.                 00003900
           05  WS-PSEUDO-EDIT-IND      PIC X(1) VALUE SPACES.           00004000
           05  WS-INT-REL-CODE.                                         00004200
               10  WS-INT-REL-CODE-POS1 PIC X(01) VALUE SPACES.         00004300
                   88  TRUST-GROUP                VALUE '9'.            00004400
               10  FILLER               PIC X(29) VALUE SPACES.         00004500
           05  WS-H-JULIAN-TERMDT       PIC S9(7) COMP-3 VALUE ZEROS.   00004600
           05  WS-H-JULIAN-EFFDT        PIC S9(5) COMP-3 VALUE ZEROS.   00004700
           05  WS-H-JULIAN-EFFDT-CEN.                                   00004800
               10 WS-EFFDT-CC           PIC X.                          00004900
               10 WS-EFFDT              PIC S9(5) COMP-3.               00005000
           05 WS-EFFDT-COMPARE REDEFINES                                00005100
                 WS-H-JULIAN-EFFDT-CEN  PIC S9(7) COMP-3.               00005200
                                                                        00005300
           05  WS-H-JUL-TERM-DATE.                                      00005400
               10  WS-H-JULIAN-TERMDT-CC   PIC X.                       00005500
               10  WS-H-JULIAN-TERMDT      PIC S9(5) COMP-3.            00005600
           05  WS-H-JUL-TERMDT-CEN REDEFINES                            00005700
                  WS-H-JUL-TERM-DATE       PIC S9(7) COMP-3.            00005800
                                                                        00005900
           05  WS-H-JUL-EFF-DATE.                                       00006000
               10  WS-H-JULIAN-EFFDT-CC    PIC X.                       00006100
               10  WS-H-JULIAN-EFFDT       PIC S9(5) COMP-3.            00006200
           05  WS-H-JUL-EFFDT-CEN REDEFINES                             00006300
                  WS-H-JUL-EFF-DATE        PIC S9(7) COMP-3.            00006400
                                                                        00006500
           05  WS-H-JULIAN-COMPARE-DATE.                                00006600
               10 WS-H-JUL-COMP-CC      PIC X.                          00006700
               10 WS-H-JUL-COMP-DT      PIC S9(5) COMP-3.               00006800
           05  WS-H-JUL-COMP-CEN REDEFINES                              00006900
                 WS-H-JULIAN-COMPARE-DATE PIC S9(7) COMP-3.             00007000
                                                                        00007100
           05  WS-JUL-DATE              PIC 9(7).                       00007200
013584     05 WS-NSB-FROM-CORP-ENT-CD        PIC X(03).                 00007210
013584     05 WS-NSB-TO-CORP-ENT-CD          PIC X(03).                 00007220
                                                                        00007300
      /       A T T R I B U T E S                                       00007400
       COPY DFHBMSCA.                                                   00007500
           02  DFHBMABF                     PIC X VALUE 'Z'.            00007600
                                                                        00007700
       COPY GNTW003.                                                    00007800
                                                                        00007900
      ** DYNAMIC ROUTING COPYBOOK                                       00008000
       01  COPY-DYNROUTC.                                               00008100
           COPY DYNROUTC.                                               00008200
                                                                        00008300
      * GNTW002 - COPYBOOK FOR TRANSLATION VALUES                       00008400
        COPY GNTW002.                                                   00008500
                                                                        00008600
      * GNTW001 - COPYBOOK FOR COMMAREA                                 00008700
        COPY GNTW001.                                                   00008800
      *** DATE                                                          00008900
        COPY MLDATE01.                                                  00009000
                                                                        00009100
      /       A T T E N T I O N   I D E N T I F I E R S                 00009200
           COPY DFHAID.                                                 00009300
                                                                        00009400
       01  HOST-VARIABLES.                                              00009500
           05  HV-NTWK-SET-ID     PIC S9(9) VALUE +0 USAGE COMP.        00009600
           05  HV-TOSETI          PIC S9(9) VALUE +0 USAGE COMP.        00009700
           05  HV-FRSETI          PIC S9(9) VALUE +0 USAGE COMP.        00009800
           05  HV-CPNTYI          PIC X(03) VALUE SPACES.               00009900
           05  HV-GROUPI          PIC X(10) VALUE SPACES.               00010000
           05  HV-SECT-NBR        PIC X(10) VALUE SPACES.               00010100
           05  HV-FMRELI          PIC X(02) VALUE SPACES.               00010200
           05  HV-DB2-DATE        PIC X(10) VALUE SPACES.               00010300
013584     05  HV-NSB-CORP-ENT-CD            PIC X(03).                 00010310
                                                                        00010400
       01 GCPS-CHANNEL-AND-CONTAINERS.                                  00010500
          05  GCPS-IO-CHANNEL        PIC X(16) VALUE 'GCPS_IO_CHANNEL'. 00010600
          05  GCPS-DFHROUTE-CONT     PIC X(16) VALUE 'DFHROUTE'.        00010700
          05  GCPS-PARM-CONT         PIC X(16) VALUE 'PARM_CONT'.       00010800
          05  GCPS-GAGS-HOSTV-CONT   PIC X(16) VALUE 'GAGS_HOSTV_CONT'. 00010900
          05  GCPS-GAGS-NTWSET-CONT  PIC X(16) VALUE 'GAGS_NTWSET_CNT'. 00011000
          05  GCPS-GAGS-GSNTW-CONT   PIC X(16) VALUE 'GAGS_GSNTW_CNT'.  00011100
          05  GCPS-GAGS-GSNTWP-CONT  PIC X(16) VALUE 'GAGS_GSNTWP_CNT'. 00011200
                                                                        00011300
       01 GCPS-DB2-IO-PARMS.                                            00011400
          05 GCPS-DB2-IO-FUNCTION.                                      00011500
             10 GCPS-DB2-IO-CALLER-PGM       PIC X(08) VALUE SPACES.    00011600
             10 GCPS-DB2-IO-FUNCTION-CODE    PIC X(03) VALUE SPACES.    00011700
          05 GCPS-DB2-IO-RET-SQLCODE         PIC S9(9) COMP-5 VALUE 0.  00011800
          05 GCPS-DB2-IO-RET-RC              PIC S9(4) VALUE 0.         00011900
          05 GCPS-DB2-IO-RET-RC-MESSAGE      PIC X(80) VALUE SPACES.    00012000
                                                                        00012100
       01  WS-TBL-SEARCH-VARIABLES.                                     00012200
           05  WS-TOSETI          PIC S9(9) USAGE COMP.                 00012300
           05  WS-EFFDTI          PIC X(10) VALUE SPACES.               00012400
           05  WS-EFFDT-DB2       PIC X(10) VALUE SPACES.               00012500
           05  WS-ENDDTI          PIC X(10) VALUE SPACES.               00012600
           05  WS-ENDDTI-DB2      PIC X(10) VALUE SPACES.               00012700
           05  WS-CPNTYI          PIC X(03) VALUE SPACES.               00012800
           05  WS-GROUPI          PIC X(10) VALUE SPACES.               00012900
           05  WS-SECTI           PIC X(10) VALUE SPACES.               00013000
           05  WS-FROMSETI        PIC S9(9) USAGE COMP.                 00013100
           05  WS-FMRELI          PIC X(02) VALUE SPACES.               00013200
      /       P N T R S   &   R E C O R D   L E N G T H S               00013300
       01  WS-PNTRS-RECORD-LENGTHS.                                     00013400
           05  WS-SCREEN-PNTR               POINTER.                    00013500
           05  WS-SCREEN-COMP            REDEFINES                      00013600
               WS-SCREEN-PNTR               PIC S9(8) COMP.             00013700
           05  WS-IO-MODEL-GRP-SPEC-PNTR    POINTER.                    00013800
           05  WS-IO-PROD-GSUB-PNTR         POINTER.                    00013900
           05  WS-IO-NEW-GRP-SPEC-PNTR      POINTER.                    00014000
           05  WS-IO-NEW-GRP-SPEC-COMP   REDEFINES                      00014100
               WS-IO-NEW-GRP-SPEC-PNTR      PIC S9(8) COMP.             00014200
           05  WS-IO-NEW-GSUB-PNTR          POINTER.                    00014300
           05  WS-IO-NEW-GSUB-COMP       REDEFINES                      00014400
               WS-IO-NEW-GSUB-PNTR          PIC S9(8) COMP.             00014500
           05  WS-TCTUA-PNTR                POINTER.                    00014600
           05  WS-IO-PARM-GRP-SPEC-LEN      PIC S9(4) COMP.             00014700
           05  WS-IO-PARM-GSUB-LEN          PIC S9(4) COMP VALUE +0.    00014800
           05  WS-IO-PARM-NEW-GSUB-LEN      PIC S9(4) COMP VALUE +0.    00014900
           05  WS-SCREEN-LEN                PIC S9(4) COMP VALUE +2048. 00015000
           05  WS-GCPSEUDO-GETMAIN-LEN      PIC S9(4) COMP VALUE +0.    00015100
           COPY GCCDRLEN.                                               00015200
                                                                        00015300
      /*-----  O P E R A T I N G    A C T I V I T Y    L O G   --------*00015400
                                                                        00015500
      /      W O R K F I E L D S ,   A N D   S W I T C H E S            00015600
       01  WS-WORK-FIELDS.                                              00015700
           05  WS-MAP                       PIC S99  COMP-3.            00015800
           05 WS-FROM-NKSET-ID              PIC X(05).                  00015900
           05 WS-FROM-NTWK-SET-ID     PIC X(05).                        00016000
           05 WS-FROM-NTWK-SET-ID-NUM REDEFINES                         00016100
                 WS-FROM-NTWK-SET-ID        PIC 9(05).                  00016200
           05 WS-TO-NKSET-ID              PIC X(05).                    00016300
           05 WS-TO-NTWK-SET-ID     PIC X(05).                          00016400
           05 WS-TO-NTWK-SET-ID-NUM REDEFINES                           00016500
                  WS-TO-NTWK-SET-ID     PIC 9(05).                      00016600
           05 WS-SECT-NBR-BV               PIC X(5).                    00016700
           05 WS-TOSETID                    PIC S9(9) USAGE COMP.       00016800
           05 WS-PND-TO-NTWK-SET-ID         PIC S9(9) USAGE COMP.       00016900
           05 WS-FROM-SET-ID                PIC S9(9) USAGE COMP.       00017000
           05 WS-TO-SET-ID                  PIC S9(9) USAGE COMP.       00017100
           05 WS-SQLCODE                    PIC +999.                   00017200
           05 WS-TO-NKSET-ID                PIC X(05).                  00017300
           05 WS-TO-NKSET-ID-NUM   REDEFINES                            00017400
              WS-TO-NKSET-ID                PIC 9(05).                  00017500
           05 WS-GROUP                      PIC X(09).                  00017600
           05 WS-SECTION                    PIC X(05).                  00017700
           05 WS-SPACE-SW             PIC X(01) VALUE 'N'.              00017800
              88 WS-SPACE-FOUND       VALUE 'Y'.                        00017900
              88 WS-SPACE-NOT-FOUND   VALUE 'N'.                        00018000
JJ0123     05 WS-CRSR-SET-SW          PIC X(01) VALUE 'N'.              00018010
JJ0123        88 WS-CRSR-SET-NO       VALUE 'N'.                        00018020
JJ0123        88 WS-CRSR-SET-YES      VALUE 'Y'.                        00018030
           05 WS-UNDERSCORE-CNT             PIC 9(02) VALUE ZEROES.     00018100
           05  WS-HOLD-GRP-SECT-NTWK-SET-ID PIC S9(9) USAGE COMP.       00018200
           05  WS-PEND-GRP-SECT-NTWK-SET-ID     PIC S9(9) USAGE COMP.   00018300
           05  WS-PEND-REC-END-DT            PIC X(10) VALUE SPACES.    00018400
           05  WS-PEND-NTWK-SET-ID          PIC S9(9) USAGE COMP.       00018500
           05  WS-GOOD-FROM-NTWKSET-ID-SW  PIC X(1) VALUE SPACE.        00018600
           05  WS-GOOD-TO-NTWKSET-ID-SW   PIC X(1) VALUE SPACE.         00018700
                                                                        00018800
       01 CM-CONFIRMATION-MESSAGES.                                     00018900
          05  CM-FREEFORM-001.                                          00019000
              10 FILLER               PIC X(27) VALUE                   00019100
              'THE ATB WAS SUCCESSFUL FOR '.                            00019200
              10 CM-SUCCESSFUL        PIC 9(02) VALUE 0.                00019300
              10 FILLER               PIC X(08) VALUE                   00019400
              ' OUT OF '.                                               00019500
              10 CM-TOTAL             PIC 9(02) VALUE 0.                00019600
              10 FILLER               PIC X(6) VALUE ' ROWS.'.          00019700
              10 FILLER               PIC X(28) VALUE SPACES.           00019800
                                                                        00019900
          05  ED-FREEFORM-ERROR-001.                                    00020000
              10 FILLER                   PIC X(79) VALUE               00020100
              'THE FROM NTWK SET ID IS NOT VALID. PLEASE ENTER A VALID V00020200
      -       'ALUE.'.                                                  00020300
          05  ED-FREEFORM-ERROR-002.                                    00020400
              10 FILLER                   PIC X(73) VALUE               00020500
              'THE TO NTWK SET ID IS NOT VALID. PLEASE ENTER A VALID VAL00020600
      -       'UE.'.                                                    00020700
          05  ED-FREEFORM-ERROR-004.                                    00020800
              10 FILLER                   PIC X(73) VALUE               00020900
              'THIS PFKEY IS INVALID FOR THIS SCREEN'.                  00021000
          05  ED-FREEFORM-ERROR-005.                                    00021100
              10 FILLER                   PIC X(73) VALUE               00021200
              'INVALID DATE FORMAT.  PLEASE USE MM/DD/YYYY FORMAT'.     00021300
          05  ED-FREEFORM-ERROR-006.                                    00021400
              10 FILLER                   PIC X(73) VALUE               00021500
              'INVALID CORPORATE ENTITY CODE'.                          00021600
          05  ED-FREEFORM-ERROR-007.                                    00021700
              10 FILLER                   PIC X(73) VALUE               00021800
            'THE GROUP NUMBER IS NOT VALID. PLEASE ENTER A VALID VALUE'.00021900
          05  ED-FREEFORM-ERROR-008.                                    00022000
              10 FILLER                   PIC X(73) VALUE               00022100
            'THE SECT NUMBER IS NOT VALID. PLEASE ENTER A VALID VALUE.'.00022200
          05  ED-FREEFORM-ERROR-009.                                    00022300
              10 FILLER                   PIC X(73) VALUE               00022400
              'NO EFFECTIVE DATE. PLEASE ENTER VALID EFFECTIVE DATE.'.  00022500
          05  ED-FREEFORM-ERROR-010.                                    00022600
              10 FILLER                   PIC X(80) VALUE               00022700
              'THE CORP ENTITY CODE VALUE ISN''T VALID. PLEASE ENTER A V00022800
      -       'ALID VALUE TO CONTINUE.'.                                00022810
          05  ED-FREEFORM-ERROR-011.                                    00022900
              10 FILLER                   PIC X(73) VALUE               00023000
              'PLEASE ENTER A VALID FAMILY REL LEVEL CODE.'.            00023100
          05  ED-FREEFORM-ERROR-012.                                    00023200
              10 FILLER                   PIC X(73) VALUE               00023300
              'RECORD DOES NOT EXIST IN THE BASE TABLE.   '.            00023400
          05  ED-FREEFORM-ERROR-013.                                    00023500
              10 FILLER                   PIC X(73) VALUE               00023600
              'CONTAINER ERR IN 8001-PUT-DFHROUTE-CONTUT  '.            00023700
          05  ED-FREEFORM-ERROR-014.                                    00023800
              10 FILLER                   PIC X(73) VALUE               00023900
              'GAGSAPGM CONTAINER ERR IN PARA 8002       '.             00024000
          05  ED-FREEFORM-ERROR-015.                                    00024100
              10 FILLER                   PIC X(73) VALUE               00024200
              'GAGSAPGM CONTAINER ERR IN PARA 8004        '.            00024300
          05  ED-FREEFORM-ERROR-016.                                    00024400
              10 FILLER                   PIC X(73) VALUE               00024500
              'CONTAINER ERR IN 8004-PUT-GAGS-NTWSET-CONT '.            00024600
          05  ED-FREEFORM-ERROR-017.                                    00024700
              10 FILLER                   PIC X(73) VALUE               00024800
              'GAGSAPGM CONTAINER ERR IN PARA 8005        '.            00024900
          05  ED-FREEFORM-ERROR-018.                                    00025000
              10 FILLER                   PIC X(73) VALUE               00025100
              'GAGSAPGM CONTAINER ERR IN PARA 8006        '.            00025200
          05  ED-FREEFORM-ERROR-019.                                    00025300
              10 FILLER                   PIC X(73) VALUE               00025400
              'LINK TO GCPSDASD FAILED IN PARA 1125       '.            00025500
          05  ED-FREEFORM-ERROR-020.                                    00025600
              10 FILLER                   PIC X(73) VALUE               00025700
              'GAGSAPGM LINK TO GCPSDASD FAILED PARA 1130 '.            00025800
          05  ED-FREEFORM-ERROR-021.                                    00025900
              10 FILLER                   PIC X(73) VALUE               00026000
              'GAGSAPGM LINK TO GCPSDASD FAILED PARA 1135 '.            00026100
          05  ED-FREEFORM-ERROR-022.                                    00026200
              10 FILLER                   PIC X(73) VALUE               00026300
              'GAGSAPGM GET CONTAINER FAILED PARA 9004    '.            00026400
          05  ED-FREEFORM-ERROR-023.                                    00026500
              10 FILLER                   PIC X(73) VALUE               00026600
              'GAGSAPGM GET CONTAINER FAILED PARA 9005    '.            00026700
          05  ED-FREEFORM-ERROR-024.                                    00026800
              10 FILLER                   PIC X(73) VALUE               00026900
              'GAGSAPGM GET CONTAINER FAILED PARA 9001    '.            00027000
          05  ED-FREEFORM-ERROR-025.                                    00027100
              10 FILLER                   PIC X(73) VALUE               00027200
              'GAGSAPGM GET CONTAINER FAILED PARA 9002    '.            00027300
          05  ED-FREEFORM-ERROR-026.                                    00027400
              10 FILLER                   PIC X(73) VALUE               00027500
           'THE ATB EFF DATE IS NOT VALID. PLEASE ENTER A VALID VALUE.'.00027600
013584    05  ED-FREEFORM-ERROR-027.                                    00027610
013584        10 FILLER                   PIC X(79) VALUE               00027620
013584     'BOTH \
      -    'ER A VALID VALUE.'.                                         00027700
       01  WS-DB2-DATE                    PIC X(10) VALUE SPACES.       00027800
                                                                        00027900
       COPY GAGSSETC.                                                   00028000
                                                                        00028100
      ******************************************************************00028200
      *    D B 2   D E C L A R E S   A R E A                            00028300
      ******************************************************************00028400
               EXEC SQL                                                 00028500
                 INCLUDE SQLCA                                          00028600
               END-EXEC.                                                00028700
      *                                                                 00028800
      * DCLGEN FOR GRP_SECT_NTWK_SET_PND                                00028900
               EXEC SQL                                                 00029000
                 INCLUDE GSNWSETP                                       00029100
               END-EXEC.                                                00029200
      * DCLGEN FOR GRP_SECT_NTWK_SET                                    00029300
               EXEC SQL                                                 00029400
                 INCLUDE GSNTWSET                                       00029500
               END-EXEC.                                                00029600
      * DCLGEN FOR NETWK_SET                                            00029700
               EXEC SQL                                                 00029800
                 INCLUDE NTWSET                                         00029900
               END-EXEC.                                                00030000
                                                                        00030100
       01  FILLER                      PIC X(16)  VALUE                 00030200
           '*** W/S ENDS ***'.                                          00030300
                                                                        00030400
      ***************************************************************   00030500
       LINKAGE SECTION.                                                 00030600
                                                                        00030700
       COPY CWACOBOL.                                                   00030800
                                                                        00030900
       PROCEDURE DIVISION.                                              00031000
           MOVE '0000'  TO  WS-PARA-ID.                                 00031100
                                                                        00031200
       0000-MAINLINE.                                                   00031300
           MOVE '0000-MAIN' TO WS-PARA-ID                               00031400
                                                                        00031500
           EXEC CICS                                                    00031600
                ADDRESS                                                 00031700
                CWA   (ADDRESS OF CWACOBOL)                             00031800
                RESP  (WS-DFHRESP)                                      00031900
           END-EXEC                                                     00032000
                                                                        00032100
           IF EIBCALEN = +0                                             00032200
              PERFORM 7100-XCTL-TO-CALLING-PROGRAM THRU 7100-EXIT       00032300
           END-IF                                                       00032400
                                                                        00032500
           IF EIBAID   = DFHPF3                                         00032600
              PERFORM 7100-XCTL-TO-CALLING-PROGRAM THRU 7100-EXIT       00032700
           END-IF                                                       00032800
                                                                        00032900
           IF    EIBTRNID = 'GAGS'                                      00033000
              OR EIBTRNID = 'GNAH'                                      00033100
               CONTINUE                                                 00033200
           ELSE                                                         00033300
              PERFORM 7100-XCTL-TO-CALLING-PROGRAM THRU 7100-EXIT       00033400
           END-IF                                                       00033500
                                                                        00033600
           IF (EIBTRNID = 'GNAH')                                       00033700
           OR (EIBAID   = DFHPF9)                                       00033800
              PERFORM 2500-SEND-MAP-ERASE       THRU 2500-EXIT          00033900
              PERFORM 9990-RETURN-TRANSID       THRU 9990-EXIT          00034000
           END-IF                                                       00034100
                                                                        00034200
           EXEC CICS RECEIVE                                            00034300
                MAP('GAGSI01')                                          00034400
                MAPSET('GAGSSET')                                       00034500
                INTO(GAGSI01I)                                          00034600
           END-EXEC                                                     00034700
                                                                        00034800
           PERFORM 1000-RESET-ATTRIBUTES     THRU 1000-EXIT             00034900
                                                                        00035000
           IF    EIBAID = DFHPF10                                       00035100
              OR EIBAID = DFHPF9                                        00035200
              OR EIBAID = DFHPF3                                        00035300
              OR EIBAID = DFHENTER                                      00035400
                 CONTINUE                                               00035500
           ELSE                                                         00035600
               MOVE ED-FREEFORM-ERROR-004 TO G1MSGO                     00035700
               PERFORM 8000-PROCESS-ERROR                               00035900
           END-IF                                                       00036000
                                                                        00036100
JJ0123*    IF EIBAID = DFHPF10                                          00036200
JJ0123     IF EIBAID = DFHPF10 OR DFHENTER                              00036300
              PERFORM 1100-EDIT-SCREEN-VALUES   THRU 1100-EXIT          00036400
           END-IF                                                       00036500
                                                                        00036600
           IF (EIBAID   = DFHPF9                                        00036700
                OR EIBTRNID = 'GNAH')                                   00036800
              PERFORM 2500-SEND-MAP-ERASE       THRU 2500-EXIT          00036900
              PERFORM 9990-RETURN-TRANSID       THRU 9990-EXIT          00037000
           END-IF                                                       00037100
                                                                        00037200
           PERFORM 2510-SEND-MAP-DATA-ONLY   THRU 2510-EXIT             00037300
           PERFORM 9990-RETURN-TRANSID       THRU 9990-EXIT.            00037400
                                                                        00037500
       0000-EXIT. EXIT.                                                 00037600
                                                                        00037700
       1000-RESET-ATTRIBUTES.                                           00037800
           MOVE '1000-RESET' TO WS-PARA-ID.                             00037900
                                                                        00038000
           MOVE DFHBMUNF TO FROMSETA                                    00038100
                            TOSETA                                      00038200
                            GROUPA(1)                                   00038300
                            SECTA(1)                                    00038400
                            EFFDTA(1)                                   00038500
                            CPNTYA(1)                                   00038600
                            FMRELA(1)                                   00038700
                            ERRLNA(1)                                   00038800
                            GROUPA(2)                                   00038900
                            SECTA(2)                                    00039000
                            EFFDTA(2)                                   00039100
                            CPNTYA(2)                                   00039200
                            FMRELA(2)                                   00039300
                            ERRLNA(2)                                   00039400
                            GROUPA(3)                                   00039500
                            SECTA(3)                                    00039600
                            EFFDTA(3)                                   00039700
                            CPNTYA(3)                                   00039800
                            FMRELA(3)                                   00039900
                            ERRLNA(3)                                   00040000
                            GROUPA(4)                                   00040100
                            SECTA(4)                                    00040200
                            EFFDTA(4)                                   00040300
                            CPNTYA(4)                                   00040400
                            FMRELA(4)                                   00040500
                            ERRLNA(4)                                   00040600
                            GROUPA(5)                                   00040700
                            SECTA(5)                                    00040800
                            EFFDTA(5)                                   00040900
                            CPNTYA(5)                                   00041000
                            FMRELA(5)                                   00041100
                            ERRLNA(5)                                   00041200
                            GROUPA(6)                                   00041300
                            SECTA(6)                                    00041400
                            EFFDTA(6)                                   00041500
                            CPNTYA(6)                                   00041600
                            FMRELA(6)                                   00041700
                            ERRLNA(6)                                   00041800
                            GROUPA(7)                                   00041900
                            SECTA(7)                                    00042000
                            EFFDTA(7)                                   00042100
                            CPNTYA(7)                                   00042200
                            FMRELA(7)                                   00042300
                            ERRLNA(7)                                   00042400
                            GROUPA(8)                                   00042500
                            SECTA(8)                                    00042600
                            EFFDTA(8)                                   00042700
                            CPNTYA(8)                                   00042800
                            FMRELA(8)                                   00042900
                            ERRLNA(8)                                   00043000
                            GROUPA(9)                                   00043100
                            SECTA(9)                                    00043200
                            EFFDTA(9)                                   00043300
                            CPNTYA(9)                                   00043400
                            FMRELA(9)                                   00043500
                            ERRLNA(9)                                   00043600
                            GROUPA(10)                                  00043700
                            SECTA(10)                                   00043800
                            EFFDTA(10)                                  00043900
                            CPNTYA(10)                                  00044000
                            FMRELA(10)                                  00044100
                            ERRLNA(10)                                  00044200
                            GROUPA(11)                                  00044300
                            SECTA(11)                                   00044400
                            EFFDTA(11)                                  00044500
                            CPNTYA(11)                                  00044600
                            FMRELA(11)                                  00044700
                            ERRLNA(11)                                  00044800
                            GROUPA(12)                                  00044900
                            SECTA(12)                                   00045000
                            EFFDTA(12)                                  00045100
                            CPNTYA(12)                                  00045200
                            FMRELA(12)                                  00045300
                            ERRLNA(12)                                  00045400
                            GROUPA(13)                                  00045500
                            SECTA(13)                                   00045600
                            EFFDTA(13)                                  00045700
                            CPNTYA(13)                                  00045800
                            FMRELA(13)                                  00045900
                            ERRLNA(13)                                  00046000
           MOVE DFHBMASD  TO ERRLNA(1) ERRLNA(2) ERRLNA(3) ERRLNA(4)    00046100
                   ERRLNA(5) ERRLNA(6) ERRLNA(7) ERRLNA(8) ERRLNA(9)    00046200
                   ERRLNA(10) ERRLNA(11) ERRLNA(12) ERRLNA(13)          00046300
                                                                        00046400
           MOVE SPACES  TO G1MSGO.                                      00046500
                                                                        00046600
           EXEC CICS                                                    00046700
                ASSIGN USERID(GPN-AUD-USR-TEXT)                         00046800
            END-EXEC                                                    00046900
                                                                        00047000
           MOVE +50              TO GPN-AUD-USR-LEN.                    00047100
                                                                        00047200
       1000-EXIT. EXIT.                                                 00047300
                                                                        00047400
       1100-EDIT-SCREEN-VALUES.                                         00047500
           MOVE '1100-EDIT'  TO WS-PARA-ID.                             00047600
                                                                        00047700
           IF CWA-PROD-SYSTEM                                           00047800
              MOVE 'PROD' TO WS-AUD-PROC                                00047900
           ELSE                                                         00048000
              MOVE 'TEST' TO WS-AUD-PROC                                00048100
           END-IF                                                       00048200
                                                                        00048300
           MOVE +4         TO GSB-AUD-PROC-LEN,                         00048400
                              GPN-AUD-PROC-LEN                          00048500
      ********************************************                      00048600
      *    EDIT 'FROM' NETWORK SET ID                                   00048700
      ********************************************                      00048800
           MOVE 'N' TO WS-GOOD-FROM-NTWKSET-ID-SW                       00048900
           PERFORM 1110-VALIDATE-FROM-SET-ID  THRU 1110-EXIT            00049000
           IF WS-GOOD-FROM-NTWKSET-ID-SW = 'Y'                          00049100
               CONTINUE                                                 00049200
           ELSE                                                         00049300
               MOVE -1                    TO FROMSETL                   00049500
               MOVE ED-FREEFORM-ERROR-001 TO G1MSGO                     00049600
               MOVE DFHBMUBF  TO FROMSETA                               00049700
               PERFORM 8000-PROCESS-ERROR                               00049800
           END-IF                                                       00049900
      ************************************                              00050000
      *    EDIT 'TO' NETWORK SET ID                                     00050100
      ************************************                              00050200
           MOVE 'N' TO WS-GOOD-TO-NTWKSET-ID-SW                         00050300
           PERFORM 1120-EDIT-TO-NTWK-SET-ID   THRU 1120-EXIT            00050400
013584     IF WS-NSB-FROM-CORP-ENT-CD = WS-NSB-TO-CORP-ENT-CD           00050410
013584        CONTINUE                                                  00050420
013584     ELSE                                                         00050430
013584         MOVE -1                    TO FROMSETL                   00050431
013584         MOVE ED-FREEFORM-ERROR-027 TO G1MSGO                     00050432
013584         MOVE DFHBMUBF  TO FROMSETA                               00050433
013584         MOVE DFHBMUBF  TO TOSETA                                 00050434
013584         PERFORM 8000-PROCESS-ERROR                               00050435
013584     END-IF                                                       00050450
           IF WS-GOOD-TO-NTWKSET-ID-SW = 'Y'                            00050500
               CONTINUE                                                 00050600
           ELSE                                                         00050700
               MOVE -1                    TO TOSETL                     00050900
               MOVE ED-FREEFORM-ERROR-002 TO G1MSGO                     00051000
               MOVE DFHBMUBF  TO TOSETA                                 00051100
               PERFORM 8000-PROCESS-ERROR                               00051200
           END-IF                                                       00051300
      *******************************************                       00051400
      *    EDIT SCREEN BODY (GRP#, SECT#, EFF-DT, CORP-ENT, FAM-REL)    00051500
      *******************************************                       00051600
           MOVE 0 TO CM-SUCCESSFUL                                      00051700
           MOVE 0 TO CM-TOTAL                                           00051800
           INITIALIZE WS-TBL-SEARCH-VARIABLES                           00051900
           MOVE 0 TO WS-MAP                                             00052000
           PERFORM 1130-EDIT-DATA-LINES THRU 1130-EXIT 13 TIMES         00052100
                                                                        00052200
           IF CM-TOTAL > 0                                              00052300
      *        USER ENTERED FROM-SET, TO-SET AND AT LEAST ONE DATA LINE 00052400
               MOVE -1                        TO FROMSETL               00052500
               MOVE CM-FREEFORM-001           TO G1MSGO                 00052600
                                                                        00052700
               IF CM-SUCCESSFUL > 0                                     00052800
                   MOVE DFHBMPRF    TO FROMSETA,                        00052900
                                       TOSETA                           00053000
               END-IF                                                   00053100
                                                                        00053200
JJ0123*        PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT            00053300
JJ0123*        PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT            00053400
JJ0123*    ELSE                                                         00053500
      *        USER ENTERRED FROM-SET, TO-SET BUT NO DATA LINES         00053600
JJ0123*        MOVE -1                        TO GROUPL(1)              00053700
JJ0123*        PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT            00053800
JJ0123*        PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT            00053900
           END-IF.                                                      00054000
                                                                        00054001
JJ0123     PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT.               00054010
JJ0123     PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT.               00054020
                                                                        00054100
       1100-EXIT.   EXIT.                                               00054200
                                                                        00054300
       1110-VALIDATE-FROM-SET-ID.                                       00054400
           MOVE '1110-VALIDATE' TO WS-PARA-ID.                          00054500
                                                                        00054600
           INITIALIZE WS-UNDERSCORE-CNT.                                00054700
           IF FROMSETI NOT = ('_____' AND SPACES AND LOW-VALUES)        00054800
              INSPECT FUNCTION REVERSE(FROMSETI)                        00054900
                      TALLYING WS-UNDERSCORE-CNT                        00055000
                      FOR LEADING '_'                                   00055100
                                                                        00055200
              ADD 1               TO WS-UNDERSCORE-CNT                  00055300
              MOVE '00000'        TO WS-SECTION                         00055400
              MOVE FROMSETI       TO WS-SECTION(WS-UNDERSCORE-CNT:)     00055500
              MOVE WS-SECTION     TO FROMSETO                           00055600
              MOVE FROMSETI       TO WS-FROM-NTWK-SET-ID                00055700
              IF WS-FROM-NTWK-SET-ID-NUM IS NUMERIC                     00055800
                  MOVE WS-FROM-NTWK-SET-ID-NUM   TO NSB-NTWK-SET-ID     00055900
                  PERFORM 1115-VALIDATE-FROM-SET-ID  THRU 1115-EXIT     00056000
              ELSE                                                      00056100
                 MOVE 'N' TO WS-GOOD-FROM-NTWKSET-ID-SW                 00056110
                 MOVE -1                        TO FROMSETL             00056200
                 MOVE DFHBMUBF                  TO FROMSETA             00056300
                 MOVE ED-FREEFORM-ERROR-001     TO G1MSGO               00056400
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00056500
                 PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT          00056600
              END-IF                                                    00056700
           ELSE                                                         00056800
                 MOVE -1                        TO FROMSETL             00056900
                 MOVE 'N' TO WS-GOOD-FROM-NTWKSET-ID-SW                 00056910
                 MOVE DFHBMUBF                  TO FROMSETA             00057000
      * COMMENTED THE BELOW TO AVOID ABEND WHEN PF10 PRESSED            00057001
      *          MOVE FROMSETI                  TO WS-FROMSETI          00057010
                 MOVE ZEROES                    TO WS-FROMSETI          00057020
                 MOVE ED-FREEFORM-ERROR-001     TO G1MSGO               00057200
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00057300
                 PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT          00057400
           END-IF.                                                      00057500
                                                                        00057600
       1110-EXIT.   EXIT.                                               00057700
                                                                        00057800
       1115-VALIDATE-FROM-SET-ID.                                       00057900
            MOVE '1115-VALID' TO WS-PARA-ID.                            00058000
                                                                        00058100
      ****************************************                          00058200
      * CHECK TO SEE IF FROM-NTWKSET IS ON NTWK_SET TABLE               00058300
      ****************************************                          00058400
      *     #1 SQL, FUNCTION CALL = GAGSAPGM001                         00058500
      *             SELECT NTWK_SET, -NTW                               00058600
            MOVE 'GAGSAPGM001'                TO GCPS-DB2-IO-FUNCTION   00058700
                                                                        00058800
            PERFORM 8001-PUT-DFHROUTE-CONT    THRU 8001-EXIT            00058900
            PERFORM 8002-PUT-PARM-CONT        THRU 8002-EXIT            00059000
            MOVE NSB-NTWK-SET-ID              TO HV-NTWK-SET-ID         00059100
                                                                        00059200
            PERFORM 8003-PUT-GAGS-HOSTV-CONT   THRU 8003-EXIT           00059300
            PERFORM 8004-PUT-GAGS-NTWSET-CONT  THRU 8004-EXIT           00059400
                                                                        00059500
            EXEC CICS LINK                                              00059600
               PROGRAM (WS-GCPSDAS)                                     00059700
               CHANNEL (GCPS-IO-CHANNEL)                                00059800
               RESP    (WS-DFHRESP)                                     00059900
            END-EXEC                                                    00060000
                                                                        00060100
            EVALUATE WS-DFHRESP                                         00060200
               WHEN 0                                                   00060300
                  CONTINUE                                              00060400
                                                                        00060500
               WHEN OTHER                                               00060600
                  MOVE 'N' TO WS-GOOD-FROM-NTWKSET-ID-SW                00060610
                  MOVE -1                        TO FROMSETL            00060700
                  MOVE DFHBMUBF                  TO FROMSETA            00060800
                  MOVE ED-FREEFORM-ERROR-001 TO G1MSGO                  00060900
                  PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT             00061000
                END-EVALUATE                                            00061100
                                                                        00061200
           PERFORM 9001-GET-PARM-CONT          THRU 9001-EXIT           00061300
           MOVE GCPS-DB2-IO-RET-SQLCODE        TO SQLCODE               00061400
           PERFORM 9002-GET-GAGS-HOSTV-CONT    THRU 9002-EXIT           00061500
           PERFORM 9003-GET-GAGS-NTWSET-CONT   THRU 9003-EXIT           00061600
                                                                        00061700
      *    REM: SQLCODE SET BY GCPSDAS WHEN GETTING THE PARM CONTAINER  00061800
      *                                                                 00061900
      *    EXEC SQL                                                     00062000
      *        SELECT NTWK_SET_ID INTO :NSB-NTWK-SET-ID                 00062100
      *        FROM NTWK_SET                                            00062200
      *        WHERE NTWK_SET_ID = :NSB-NTWK-SET-ID                     00062300
      *        FETCH FIRST ROW ONLY                                     00062400
      *    END-EXEC                                                     00062500
                                                                        00062600
           EVALUATE SQLCODE                                             00062700
              WHEN +0                                                   00062800
                  MOVE NSB-NTWK-SET-ID TO WS-FROM-SET-ID                00062900
013584            MOVE HV-NSB-CORP-ENT-CD TO WS-NSB-FROM-CORP-ENT-CD    00062910
                  MOVE 'Y' TO WS-GOOD-FROM-NTWKSET-ID-SW                00063000
              WHEN +100                                                 00063100
                  MOVE -1 TO FROMSETL                                   00063200
                  MOVE DFHBMUBF  TO FROMSETA                            00063300
                  MOVE 'N' TO WS-GOOD-FROM-NTWKSET-ID-SW                00063310
                  MOVE ED-FREEFORM-ERROR-001 TO G1MSGO                  00063400
                  PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT         00063500
                  PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT         00063600
              WHEN OTHER                                                00063700
                  MOVE -1 TO FROMSETL                                   00063800
                  MOVE DFHBMUBF  TO FROMSETA                            00063900
                  STRING 'SELECT ERROR IN PARA 1115.  SQLCODE = '       00064000
                      WS-SQLCODE DELIMITED BY SIZE                      00064100
                      INTO G1MSGO                                       00064200
                  END-STRING                                            00064300
                  PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT         00064400
                  PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT         00064500
           END-EVALUATE.                                                00064600
                                                                        00064700
       1115-EXIT.   EXIT.                                               00064800
                                                                        00064900
       1120-EDIT-TO-NTWK-SET-ID.                                        00065000
            MOVE '1120-EDIT' TO WS-PARA-ID.                             00065100
                                                                        00065200
      **************************************                            00065300
      * CHECK TO SEE IF TO-NTWKSET IS ON NTWK_SET TABLE                 00065400
      **************************************                            00065500
           INITIALIZE WS-UNDERSCORE-CNT.                                00065600
           IF TOSETI NOT = ('_____' AND SPACES AND LOW-VALUES)          00065700
              INSPECT FUNCTION REVERSE(TOSETI)                          00065800
                      TALLYING WS-UNDERSCORE-CNT                        00065900
                      FOR LEADING '_'                                   00066000
                                                                        00066100
              ADD 1               TO WS-UNDERSCORE-CNT                  00066200
              MOVE '00000'        TO WS-SECTION                         00066300
              MOVE TOSETI         TO WS-SECTION(WS-UNDERSCORE-CNT:)     00066400
              MOVE WS-SECTION     TO TOSETO                             00066500
              MOVE TOSETI         TO WS-TO-NTWK-SET-ID                  00066600
              IF WS-TO-NTWK-SET-ID-NUM IS NUMERIC                       00066700
                  MOVE WS-TO-NTWK-SET-ID-NUM   TO NSB-NTWK-SET-ID       00066800
                  PERFORM 1125-VALIDATE-TO-SET-ID  THRU 1125-EXIT       00066900
              ELSE                                                      00067000
                 MOVE -1                        TO TOSETL               00067100
                 MOVE DFHBMUBF                  TO TOSETA               00067200
      * COMMENTED THE BELOW CODE FOR FIX WARNING FOR BUILD              00067300
      *          MOVE TOSETI                    TO TOSETO               00067310
                 MOVE ED-FREEFORM-ERROR-002     TO G1MSGO               00067400
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00067500
                 PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT          00067600
              END-IF                                                    00067700
           ELSE                                                         00067800
                 MOVE -1                        TO TOSETL               00067900
                 MOVE DFHBMUBF                  TO TOSETA               00068000
      * COMMENTED THE BELOW CODE FOR FIX WARNING FOR BUILD              00068010
      *          MOVE TOSETI                    TO TOSETO               00068100
                 MOVE ED-FREEFORM-ERROR-002     TO G1MSGO               00068200
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00068300
                 PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT          00068400
           END-IF.                                                      00068500
                                                                        00068600
           INITIALIZE WS-UNDERSCORE-CNT.                                00068700
           IF TOSETI NOT = ('_____' AND SPACES AND LOW-VALUES)          00068800
              INSPECT FUNCTION REVERSE(TOSETI)                          00068900
                      TALLYING WS-UNDERSCORE-CNT                        00069000
                      FOR LEADING '_'                                   00069100
                                                                        00069200
              ADD 1               TO WS-UNDERSCORE-CNT                  00069300
              MOVE '00000'        TO WS-SECTION                         00069400
              MOVE TOSETI       TO WS-SECTION(WS-UNDERSCORE-CNT:)       00069500
              MOVE WS-SECTION     TO TOSETO                             00069600
           ELSE                                                         00069700
              SET WS-SPACE-FOUND             TO TRUE                    00069800
              MOVE -1                        TO TOSETL                  00069900
              MOVE DFHBMUBF                  TO TOSETA                  00070000
      * COMMENTED THE BELOW CODE FOR FIX WARNING FOR BUILD              00070010
      *       MOVE TOSETI                    TO TOSETO                  00070100
              MOVE ED-FREEFORM-ERROR-002     TO G1MSGO                  00070200
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00070300
              PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT             00070400
           END-IF.                                                      00070500
                                                                        00070600
       1120-EXIT.   EXIT.                                               00070700
                                                                        00070800
       1125-VALIDATE-TO-SET-ID.                                         00070900
           MOVE '1125-VALIDATE' TO WS-PARA-ID.                          00071000
                                                                        00071100
      ****************************************************              00071200
      * CHECK TO SEE THAT THE 'TO' SET-ID ESISTS IN NTWK_SET TBL        00071300
      ****************************************************              00071400
      *    #2 SQL, FUNCTION CALL = GAGSAPGM001                          00071500
      *     SAME FUNCTION CALL AS #1, SELECT NTWK_SET, -NTW             00071600
                                                                        00071700
           MOVE 'GAGSAPGM001'                TO GCPS-DB2-IO-FUNCTION    00071800
                                                                        00071900
           PERFORM 8001-PUT-DFHROUTE-CONT    THRU 8001-EXIT             00072000
           PERFORM 8002-PUT-PARM-CONT        THRU 8002-EXIT             00072100
           MOVE NSB-NTWK-SET-ID              TO HV-NTWK-SET-ID          00072200
                                                                        00072300
           PERFORM 8003-PUT-GAGS-HOSTV-CONT   THRU 8003-EXIT            00072400
           PERFORM 8004-PUT-GAGS-NTWSET-CONT  THRU 8004-EXIT            00072500
                                                                        00072600
           EXEC CICS LINK                                               00072700
              PROGRAM (WS-GCPSDAS)                                      00072800
              CHANNEL (GCPS-IO-CHANNEL)                                 00072900
              RESP    (WS-DFHRESP)                                      00073000
           END-EXEC                                                     00073100
                                                                        00073200
           EVALUATE WS-DFHRESP                                          00073300
             WHEN 0                                                     00073400
                CONTINUE                                                00073500
                                                                        00073600
             WHEN OTHER                                                 00073700
                MOVE -1                    TO TOSETL                    00073800
                MOVE DFHBMUBF              TO TOSETA                    00073900
                MOVE ED-FREEFORM-ERROR-019 TO G1MSGO                    00074000
                PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT               00074100
           END-EVALUATE                                                 00074200
                                                                        00074300
           PERFORM 9001-GET-PARM-CONT          THRU 9001-EXIT           00074400
           MOVE GCPS-DB2-IO-RET-SQLCODE        TO SQLCODE               00074500
           PERFORM 9002-GET-GAGS-HOSTV-CONT    THRU 9002-EXIT           00074600
           PERFORM 9003-GET-GAGS-NTWSET-CONT   THRU 9003-EXIT           00074700
      *    REM: SQLCODE SET BY GCPSDAS WHEN GETTING THE PARM CONTAINER  00074800
                                                                        00074900
      *    EXEC SQL                                                     00075000
      *        SELECT NTWK_SET_ID INTO :NSB-NTWK-SET-ID                 00075100
      *        FROM NTWK_SET                                            00075200
      *        WHERE NTWK_SET_ID = :NSB-NTWK-SET-ID                     00075300
      *        FETCH FIRST ROW ONLY                                     00075400
      *    END-EXEC                                                     00075500
                                                                        00075600
           EVALUATE SQLCODE                                             00075700
              WHEN +0                                                   00075800
                  MOVE 'Y' TO WS-GOOD-TO-NTWKSET-ID-SW                  00075900
0112              MOVE NSB-NTWK-SET-ID  TO WS-TO-SET-ID                 00076000
013584            MOVE HV-NSB-CORP-ENT-CD TO WS-NSB-TO-CORP-ENT-CD      00076010
              WHEN OTHER                                                00076100
                  MOVE -1                    TO TOSETL                  00076200
                  MOVE DFHBMUBF              TO TOSETA                  00076300
                  MOVE ED-FREEFORM-ERROR-002 TO G1MSGO                  00076400
                  PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT             00076500
           END-EVALUATE.                                                00076600
                                                                        00076700
       1125-EXIT.   EXIT.                                               00076800
                                                                        00076900
       1130-EDIT-DATA-LINES.                                            00077000
           MOVE '1130-EDIT' TO WS-PARA-ID.                              00077100
                                                                        00077200
      **********************************************                    00077300
      * VALIDATE EACH OF THE DATA LINES ENTERRED BY THE USER*           00077400
      **********************************************                    00077500
                                                                        00077600
           ADD +1 TO WS-MAP                                             00077700
      * IF LINE HAS NO DATA, SKIP IT                                    00077800
           IF  (GROUPI(WS-MAP) = '_________' OR SPACES OR LOW-VALUES)   00077900
           AND (SECTI(WS-MAP)  = '_____' OR SPACES OR LOW-VALUES)       00078000
           AND (EFFDTI(WS-MAP) = '__________' OR SPACES OR LOW-VALUES)  00078100
           AND (CPNTYI(WS-MAP) = '___' OR SPACES OR LOW-VALUES)         00078200
           AND (FMRELI(WS-MAP) = '__' OR SPACES OR LOW-VALUES)          00078300
JJ0123         IF WS-CRSR-SET-NO                                        00078410
JJ0123            SET WS-CRSR-SET-YES TO TRUE                           00078411
JJ0123            MOVE -1             TO GROUPL(WS-MAP)                 00078420
JJ0123         END-IF                                                   00078430
               GO TO 1130-EXIT                                          00078440
           END-IF                                                       00078500
                                                                        00078600
JJ0123*    ADD +1 TO CM-TOTAL                                           00078700
           PERFORM 1140-FORMAT-GROUP-NBR   THRU 1140-EXIT               00078800
           PERFORM 1150-FORMAT-SECT-NBR    THRU 1150-EXIT               00078900
           PERFORM 1160-FORMAT-EFF-DT      THRU 1160-EXIT               00079000
           PERFORM 1170-FORMAT-CORP-ENT    THRU 1170-EXIT               00079100
           PERFORM 1180-FORMAT-FAM-REL     THRU 1180-EXIT               00079200
                                                                        00079300
JJ0123     IF EIBAID = DFHPF10                                          00079310
      * ENSURE SCREEN DATA LINE EXISTS IN GRP_SECT_NTWK_SET (BASE) TBL  00079400
      * FOR THE GRP/ SECT/ CORP-ENT/ FAM-REL/ EFF-DT ON SCREEN LINE.    00079500
      * IF NO RECORD IS FOUND ON BASE TBL FOR THE ROW,(SQLCODE = +100), 00079600
      *     THEN THIS IS AN ERROR.                                      00079700
      * THIS QUERY IS NOT CHECKING THE NTWK_SET_ID - THE NEXT ONE WILL  00079800
                                                                        00079900
      *    #3 SQL, FUNCTION CALL = GAGSAPGM002                          00080000
      *            SELECT GRP_SECT_NTWK_SET, -GSB                       00080100
              MOVE 'GAGSAPGM002'                TO GCPS-DB2-IO-FUNCTION 00080200
                                                                        00080300
              MOVE WS-CPNTYI         TO HV-CPNTYI                       00080400
              MOVE WS-GROUPI         TO HV-GROUPI                       00080500
              MOVE GSB-SECT-NBR      TO HV-SECT-NBR                     00080600
              MOVE WS-FMRELI         TO HV-FMRELI                       00080700
              MOVE WS-DB2-DATE       TO HV-DB2-DATE                     00080800
                                                                        00080900
              PERFORM 8001-PUT-DFHROUTE-CONT    THRU 8001-EXIT          00081000
              PERFORM 8002-PUT-PARM-CONT        THRU 8002-EXIT          00081100
              PERFORM 8003-PUT-GAGS-HOSTV-CONT  THRU 8003-EXIT          00081200
              PERFORM 8005-PUT-GAGS-GSNTW-CONT  THRU 8005-EXIT          00081300
                                                                        00081400
              EXEC CICS LINK                                            00081500
                 PROGRAM (WS-GCPSDAS)                                   00081600
                 CHANNEL (GCPS-IO-CHANNEL)                              00081700
                 RESP    (WS-DFHRESP)                                   00081800
              END-EXEC                                                  00081900
              EVALUATE WS-DFHRESP                                       00082000
                 WHEN 0                                                 00082100
                    CONTINUE                                            00082200
                 WHEN OTHER                                             00082300
                     MOVE ED-FREEFORM-ERROR-020 TO G1MSGO               00082400
                     PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT          00082500
              END-EVALUATE                                              00082600
                                                                        00082700
              PERFORM 9001-GET-PARM-CONT          THRU 9001-EXIT        00082800
              MOVE GCPS-DB2-IO-RET-SQLCODE        TO SQLCODE            00082900
              PERFORM 9002-GET-GAGS-HOSTV-CONT    THRU 9002-EXIT        00083000
              PERFORM 9004-GET-GAGS-GSNTW-CONT    THRU 9004-EXIT        00083100
                                                                        00083200
      *    REM: SQLCODE SET BY GCPSDAS WHEN GETTING THE PARM CONTAINER  00083300
                                                                        00083400
                                                                        00083500
      *    EXEC SQL                                                     00083600
      *        SELECT GRP_SECT_NTWK_SET_ID,                             00083700
      *               CORP_ENT_CD,                                      00083800
      *               GRP_NBR,                                          00083900
      *               SECT_NBR,                                         00084000
      *               NTWK_SET_ID,                                      00084100
      *               FAM_RELSHP_LVL_CD,                                00084200
      *               REC_EFF_DT,                                       00084300
      *               REC_END_DT                                        00084400
      *         INTO :GSB-GRP-SECT-NTWK-SET-ID,                         00084500
      *              :GSB-CORP-ENT-CD,                                  00084600
      *              :GSB-GRP-NBR,                                      00084700
      *              :GSB-SECT-NBR,                                     00084800
      *              :GSB-NTWK-SET-ID,                                  00084900
      *              :GSB-FAM-RELSHP-LVL-CD,                            00085000
      *              :GSB-REC-EFF-DT,                                   00085100
      *              :GSB-REC-END-DT                                    00085200
      *         FROM GRP_SECT_NTWK_SET                                  00085300
      *         WHERE CORP_ENT_CD        = :HV-CPNTYI                   00085400
      *           AND GRP_NBR            = :HV-GROUPI                   00085500
      *           AND SECT_NBR           = :HV-SECT-NBR                 00085600
      *           AND REC_EFF_DT         = :HV-DB2-DATE                 00085700
      *           AND FAM_RELSHP_LVL_CD  = :HV-FMRELI                   00085800
      *    END-EXEC                                                     00085900
                                                                        00086000
              EVALUATE SQLCODE                                          00086100
                 WHEN +0                                                00086200
                     IF GSB-NTWK-SET-ID = WS-TOSETID                    00086300
JJ0123                   ADD +1 TO CM-TOTAL                             00086310
                         PERFORM 8060-PROTECT-DATALINE THRU 8060-EXIT   00086400
                         PERFORM 1135-CHECK-PND-TBL                     00086500
                         GO TO 1130-EXIT                                00086600
                     END-IF                                             00086700
                     IF GSB-NTWK-SET-ID = WS-FROM-SET-ID                00086800
JJ0123                   ADD +1 TO CM-TOTAL                             00086810
                         PERFORM 1250-UPDATE-BASE THRU 1250-EXIT        00086900
                         GO TO 1130-EXIT                                00087000
                     ELSE                                               00087100
                         MOVE -1                    TO GROUPL(WS-MAP)   00087200
                         MOVE DFHBMUBF              TO GROUPA(WS-MAP)   00087300
                         MOVE 'E'                   TO ERRLNO(WS-MAP)   00087400
                     END-IF                                             00087500
                 WHEN +100                                              00087600
                     MOVE -1                        TO GROUPL(WS-MAP)   00087700
                     MOVE DFHBMUBF                  TO GROUPA(WS-MAP)   00087800
                     MOVE 'E'                       TO ERRLNO(WS-MAP)   00087900
                     PERFORM 8050-RED-ERR-LINE      THRU  8050-EXIT     00088000
                 WHEN OTHER                                             00088100
                     STRING 'SELECT ERROR IN PARA 1130.  SQLCODE = '    00088200
                         WS-SQLCODE DELIMITED BY SIZE                   00088300
                         INTO G1MSGO                                    00088400
                     END-STRING                                         00088500
              END-EVALUATE                                              00088600
JJ0123     END-IF.                                                      00088610
                                                                        00088700
       1130-EXIT.   EXIT.                                               00088800
                                                                        00088900
       1135-CHECK-PND-TBL.                                              00089000
           MOVE '1135-CHECK-PND' TO WS-PARA-ID.                         00089100
                                                                        00089200
      * THIS PARA IS PERFORMED TO SEE IF A MATCHING PEND RCD ESISTS     00089300
      * FOR THE GRP/ SECT/ CORP-ENT/ FAM-REL/ EFF-DT ON SCREEN LINE.    00089400
                                                                        00089500
      *    #4 SQL, FUNCTION CALL = GAGSAPGM003                          00089600
      *            SELECT GRP_SECT_NTWK_SET_PND, GPN-                   00089700
           MOVE 'GAGSAPGM003'                TO GCPS-DB2-IO-FUNCTION    00089800
                                                                        00089900
           MOVE WS-CPNTYI         TO HV-CPNTYI                          00090000
           MOVE WS-GROUPI         TO HV-GROUPI                          00090100
           MOVE GSB-SECT-NBR      TO HV-SECT-NBR                        00090200
           MOVE WS-FMRELI         TO HV-FMRELI                          00090300
           MOVE WS-DB2-DATE       TO HV-DB2-DATE                        00090400
                                                                        00090500
           PERFORM 8001-PUT-DFHROUTE-CONT    THRU 8001-EXIT             00090600
           PERFORM 8002-PUT-PARM-CONT        THRU 8002-EXIT             00090700
           PERFORM 8003-PUT-GAGS-HOSTV-CONT  THRU 8003-EXIT             00090800
           PERFORM 8006-PUT-GAGS-GSNTWP-CONT THRU 8006-EXIT             00090900
                                                                        00091000
           EXEC CICS LINK                                               00091100
              PROGRAM (WS-GCPSDAS)                                      00091200
              CHANNEL (GCPS-IO-CHANNEL)                                 00091300
              RESP    (WS-DFHRESP)                                      00091400
           END-EXEC                                                     00091500
                                                                        00091600
           EVALUATE WS-DFHRESP                                          00091700
              WHEN 0                                                    00091800
                  CONTINUE                                              00091900
              WHEN OTHER                                                00092000
                  MOVE ED-FREEFORM-ERROR-020 TO G1MSGO                  00092100
                  PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT             00092200
           END-EVALUATE                                                 00092300
                                                                        00092400
           PERFORM 9001-GET-PARM-CONT          THRU 9001-EXIT           00092500
           MOVE GCPS-DB2-IO-RET-SQLCODE        TO SQLCODE               00092600
           PERFORM 9002-GET-GAGS-HOSTV-CONT    THRU 9002-EXIT           00092700
           PERFORM 9005-GET-GAGS-GSNTWP-CONT   THRU 9005-EXIT           00092800
      *    REM: SQLCODE SET BY GCPSDAS WHEN GETTING THE PARM CONTAINER  00092900
                                                                        00093000
                                                                        00093100
      *    EXEC SQL                                                     00093200
      *        SELECT NTWK_SET_ID,                                      00093300
      *               GRP_SECT_NTWK_SET_ID,                             00093400
      *               REC_END_DT                                        00093500
      *        INTO :GPN-NTWK-SET-ID,                                   00093600
      *             :GPN-GRP-SECT-NTWK-SET-ID,                          00093700
      *             :GPN-REC-END-DT                                     00093800
      *        FROM GRP_SECT_NTWK_SET_PND                               00093900
      *        WHERE CORP_ENT_CD        = :WS-CPNTYI                    00094000
      *          AND GRP_NBR            = :WS-GROUPI                    00094100
      *          AND SECT_NBR           = :GSB-SECT-NBR                 00094200
      *          AND REC_EFF_DT         = :WS-DB2-DATE                  00094300
      *          AND FAM_RELSHP_LVL_CD  = :WS-FMRELI                    00094400
      *    END-EXEC                                                     00094500
                                                                        00094600
           EVALUATE SQLCODE                                             00094700
              WHEN +0                                                   00094800
0112              IF GPN-NTWK-SET-ID          = WS-TO-SET-ID            00095000
                      ADD +1 TO CM-SUCCESSFUL                           00095100
                      PERFORM 8060-PROTECT-DATALINE THRU 8060-EXIT      00095200
                      GO TO 1135-EXIT                                   00095210
                  END-IF                                                00095300
                  IF GPN-NTWK-SET-ID   = WS-FROMSETI                    00095400
                      PERFORM 1255-UPDATE-PND THRU 1255-EXIT            00095500
                      GO TO 1135-EXIT                                   00095510
                  END-IF                                                00095600
              WHEN +100                                                 00095700
                  CONTINUE                                              00095800
              WHEN OTHER                                                00095900
                  STRING 'SELECT ERROR IN PARA 1135.  SQLCODE = '       00096000
                      WS-SQLCODE DELIMITED BY SIZE                      00096100
                      INTO G1MSGO                                       00096200
                  END-STRING                                            00096300
           END-EVALUATE.                                                00096400
                                                                        00096500
       1135-EXIT.   EXIT.                                               00096600
                                                                        00096700
       1140-FORMAT-GROUP-NBR.                                           00096800
           MOVE '1140-FORMAT' TO WS-PARA-ID.                            00096900
                                                                        00097000
           INITIALIZE WS-UNDERSCORE-CNT                                 00097100
           IF GROUPI(WS-MAP) NOT = ( '_________' AND SPACES)            00097200
              INSPECT FUNCTION REVERSE(GROUPI(WS-MAP))                  00097300
                      TALLYING WS-UNDERSCORE-CNT                        00097400
                      FOR LEADING '_'                                   00097500
                                                                        00097600
              ADD 1                TO WS-UNDERSCORE-CNT                 00097700
              MOVE '000000000'     TO WS-GROUP                          00097800
              MOVE GROUPI(WS-MAP)  TO WS-GROUP(WS-UNDERSCORE-CNT:)      00097900
              MOVE WS-GROUP        TO GROUPO(WS-MAP),                   00098000
                                      WS-GROUPI                         00098100
              INSPECT GROUPO(WS-MAP) REPLACING ALL '_' BY ' '           00098200
              INSPECT WS-GROUPI      REPLACING ALL '_' BY ' '           00098300
           ELSE                                                         00098400
              MOVE -1                        TO GROUPL(WS-MAP)          00098500
              MOVE DFHBMUBF                  TO GROUPA(WS-MAP)          00098600
              MOVE ED-FREEFORM-ERROR-007     TO G1MSGO                  00098700
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00098800
              PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT.            00098900
                                                                        00099000
       1140-EXIT.   EXIT.                                               00099100
                                                                        00099200
       1150-FORMAT-SECT-NBR.                                            00099300
           MOVE '1150-FORMAT' TO WS-PARA-ID.                            00099400
                                                                        00099500
           IF SECTI(WS-MAP) = ( '_____' OR SPACES OR LOW-VALUES)        00099600
              MOVE -1                        TO SECTL(WS-MAP)           00099700
              MOVE DFHBMUBF                  TO SECTA(WS-MAP)           00099800
              MOVE SECTI(WS-MAP)             TO SECTO(WS-MAP)           00099900
              INSPECT SECTO(WS-MAP) REPLACING ALL '_' BY ' '            00100000
              MOVE ED-FREEFORM-ERROR-008     TO G1MSGO                  00100100
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00100200
              PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT             00100300
           ELSE                                                         00100400
              INITIALIZE WS-UNDERSCORE-CNT                              00100500
              INSPECT FUNCTION REVERSE(SECTI(WS-MAP))                   00100600
                      TALLYING WS-UNDERSCORE-CNT                        00100700
                      FOR LEADING '_'                                   00100800
                                                                        00100900
              ADD 1               TO WS-UNDERSCORE-CNT                  00101000
              MOVE '00000'        TO WS-SECTION                         00101100
              MOVE SECTI(WS-MAP) TO WS-SECTION(WS-UNDERSCORE-CNT:)      00101200
              INSPECT WS-SECTION    REPLACING ALL '_' BY ' '            00101300
              MOVE WS-SECTION     TO SECTO(WS-MAP)                      00101400
              MOVE WS-SECTION TO GSB-SECT-NBR                           00101500
           END-IF.                                                      00101600
                                                                        00101700
       1150-EXIT.   EXIT.                                               00101800
                                                                        00101900
       1160-FORMAT-EFF-DT.                                              00102000
           MOVE '1160-FORMAT' TO WS-PARA-ID.                            00102100
                                                                        00102200
           MOVE EFFDTI(WS-MAP)   TO WS-EFFDTI                           00102300
                                                                        00102400
           IF EFFDTI(WS-MAP) > SPACES                                   00102500
              IF (WS-EFFDTI(3:1) NOT = '/'                              00102600
              OR  WS-EFFDTI(6:1) NOT = '/'                              00102700
              OR  WS-EFFDTI(1:2) NOT NUMERIC                            00102800
              OR  WS-EFFDTI(4:2) NOT NUMERIC                            00102900
              OR  WS-EFFDTI(7:4) NOT NUMERIC)                           00103000
                 MOVE -1                        TO EFFDTL(WS-MAP)       00103100
                 MOVE DFHBMUBF                  TO EFFDTA(WS-MAP)       00103200
                 MOVE ED-FREEFORM-ERROR-005     TO G1MSGO               00103300
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00103400
                 PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT          00103500
           END-IF                                                       00103600
                                                                        00103700
           MOVE 'CNV'                TO MLDATE-FUNC.                    00103800
           MOVE 'M'                  TO MLDATE-FORM1.                   00103900
           MOVE 'Y'                  TO MLDATE-FORM2.                   00104000
           MOVE EFFDTI(WS-MAP)(1:2) TO MLDATE-DATE1(1:2).               00104100
           MOVE EFFDTI(WS-MAP)(4:2) TO MLDATE-DATE1(3:2).               00104200
           MOVE EFFDTI(WS-MAP)(7:4) TO MLDATE-DATE1(5:4).               00104300
                                                                        00104400
           EXEC CICS LINK                                               00104500
                PROGRAM('MLDATEC')                                      00104600
                COMMAREA(MLDATE01)                                      00104700
                LENGTH(LENGTH OF MLDATE01)                              00104800
           END-EXEC.                                                    00104900
                                                                        00105000
           IF MLDATE-RETURN = '01'                                      00105100
              SET WS-SPACE-FOUND             TO TRUE                    00105200
              MOVE -1                        TO EFFDTL(WS-MAP)          00105300
              MOVE DFHBMUBF                  TO EFFDTA(WS-MAP)          00105400
              MOVE ED-FREEFORM-ERROR-026 TO G1MSGO                      00105500
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00105600
              PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT             00105700
           END-IF                                                       00105800
                                                                        00105900
           IF EFFDTI(WS-MAP) = ( '__________' OR SPACES OR LOW-VALUES)  00106000
              MOVE -1                        TO EFFDTL(WS-MAP)          00106100
              MOVE DFHBMUBF                  TO EFFDTA(WS-MAP)          00106200
              MOVE EFFDTI(WS-MAP)            TO EFFDTO(WS-MAP)          00106300
              MOVE ED-FREEFORM-ERROR-009 TO G1MSGO                      00106400
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00106500
              PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT             00106600
           ELSE                                                         00106700
               MOVE EFFDTI(WS-MAP)   TO WS-EFFDTI                       00106800
               MOVE WS-EFFDTI(7:4)  TO WS-DB2-DATE(1:4)                 00106900
               MOVE '-'             TO WS-DB2-DATE(5:1)                 00107000
               MOVE WS-EFFDTI(1:2)  TO WS-DB2-DATE(6:2)                 00107100
               MOVE '-'             TO WS-DB2-DATE(8:1)                 00107200
               MOVE WS-EFFDTI(4:2)  TO WS-DB2-DATE(9:2)                 00107300
               MOVE WS-EFFDTI       TO WS-EFFDT-DB2                     00107400
           END-IF.                                                      00107500
                                                                        00107600
       1160-EXIT.   EXIT.                                               00107700
                                                                        00107800
       1170-FORMAT-CORP-ENT.                                            00107900
           MOVE '1170-FORMAT' TO WS-PARA-ID.                            00108000
                                                                        00108100
RB0123     MOVE CPNTYI(WS-MAP) TO TVD-VALIDATE-CORP-ENTITY              00108110
RB0123     IF TVD-VALID-CORP-ENTITIES                                   00108120
              CONTINUE                                                  00108130
           ELSE                                                         00108500
               MOVE -1                        TO CPNTYL(WS-MAP)         00108600
               MOVE DFHBMUBF                  TO CPNTYA(WS-MAP)         00108700
               MOVE CPNTYI(WS-MAP)            TO CPNTYO(WS-MAP)         00108800
               MOVE ED-FREEFORM-ERROR-010 TO G1MSGO                     00108900
               PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT            00109000
               PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT            00109100
           END-IF                                                       00109200
                                                                        00109210
           IF CPNTYI(WS-MAP) = ( '___' OR SPACES OR LOW-VALUES)         00109300
               MOVE -1                        TO CPNTYL(WS-MAP)         00109400
               MOVE DFHBMUBF                  TO CPNTYA(WS-MAP)         00109500
               MOVE CPNTYI(WS-MAP)            TO CPNTYO(WS-MAP)         00109600
               MOVE ED-FREEFORM-ERROR-010     TO G1MSGO                 00109700
               PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT            00109800
               PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT            00109900
           ELSE                                                         00110000
               MOVE CPNTYI(WS-MAP) TO WS-CPNTYI,                        00110100
                                     CPNTYO(WS-MAP)                     00110200
           END-IF.                                                      00110300
                                                                        00110400
       1170-EXIT.   EXIT.                                               00110500
                                                                        00110600
       1180-FORMAT-FAM-REL.                                             00110700
           MOVE '1180-FORMAT' TO WS-PARA-ID.                            00110800
                                                                        00110900
                                                                        00111000
           IF FMRELI(WS-MAP) = ( '__' OR SPACES OR LOW-VALUES)          00111100
               MOVE -1                        TO FMRELL(WS-MAP)         00111200
               MOVE DFHBMUBF                  TO FMRELA(WS-MAP)         00111300
               MOVE FMRELI(WS-MAP)            TO FMRELO(WS-MAP)         00111400
               MOVE ED-FREEFORM-ERROR-011 TO G1MSGO                     00111500
               PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT            00111600
               PERFORM 9990-RETURN-TRANSID    THRU 9990-EXIT            00111700
           ELSE                                                         00111800
              MOVE FMRELI(WS-MAP)  TO WS-FMRELI,                        00111900
                                      FMRELO(WS-MAP)                    00112000
           END-IF.                                                      00112100
                                                                        00112200
       1180-EXIT.   EXIT.                                               00112300
                                                                        00112500
       1250-UPDATE-BASE.                                                00112600
           MOVE '1250-UPDATE-BASE' TO WS-PARA-ID.                       00112700
                                                                        00112800
      * THIS PARA IS PERFORMED WHEN A BASE RCD IS FOUND                 00112900
      * BUT WITH A DIFFERENT NTWK-SET-ID.  A PEND RECORD IS CREATED     00113000
      * AND INSERTED INTO THE PEND TABLE.                               00113100
      *    #7 SQL, FUNCTION CALL = GAGSAPGM006                          00113110
      *            SELECT GRP_SECT_NTWK_SET_PND, GPN-                   00113120
                                                                        00113121
           MOVE 'GAGSAPGM006' TO GCPS-DB2-IO-FUNCTION                   00113130
                                                                        00113200
           PERFORM 8001-PUT-DFHROUTE-CONT    THRU 8001-EXIT             00113310
           PERFORM 8002-PUT-PARM-CONT        THRU 8002-EXIT             00113320
                                                                        00113329
           MOVE GSB-CORP-ENT-CD       TO HV-CPNTYI                      00113330
           MOVE GSB-GRP-NBR           TO HV-GROUPI                      00113331
           MOVE GSB-SECT-NBR          TO HV-SECT-NBR                    00113332
           MOVE GSB-FAM-RELSHP-LVL-CD TO HV-FMRELI                      00113340
           MOVE GSB-REC-EFF-DT        TO HV-DB2-DATE                    00113341
           MOVE WS-TO-SET-ID          TO HV-TOSETI                      00113343
                                                                        00113352
           PERFORM 8003-PUT-GAGS-HOSTV-CONT  THRU 8003-EXIT             00113353
 0113      PERFORM 8006-PUT-GAGS-GSNTWP-CONT THRU 8006-EXIT             00113354
                                                                        00113360
 0113      EXEC CICS LINK                                               00113370
 0113         PROGRAM (WS-GCPSDAS)                                      00113380
 0113         CHANNEL (GCPS-IO-CHANNEL)                                 00113390
 0113         RESP    (WS-DFHRESP)                                      00113391
 0113      END-EXEC                                                     00113392
                                                                        00113393
           EVALUATE WS-DFHRESP                                          00113394
              WHEN 0                                                    00113395
                  CONTINUE                                              00113396
              WHEN OTHER                                                00113397
                  MOVE ED-FREEFORM-ERROR-020 TO G1MSGO                  00113398
                  PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT             00113399
           END-EVALUATE                                                 00113400
                                                                        00113401
                                                                        00113417
0112  *    EXEC SQL                                                     00113420
0112  *    SELECT NTWK_SET_ID INTO :WS-PND-TO-NTWK-SET-ID               00113500
0112  *    FROM GRP_SECT_NTWK_SET_PND                                   00113600
0112  *    WHERE CORP_ENT_CD = :GSB-CORP-ENT-CD  AND                    00113700
0112  *          GRP_NBR     = :GSB-GRP-NBR       AND                   00113800
0112  *          SECT_NBR    = :GSB-SECT-NBR      AND                   00113900
0112  *          FAM_RELSHP_LVL_CD = :GSB-FAM-RELSHP-LVL-CD AND         00114000
0112  *          REC_EFF_DT  = :GSB-REC-EFF-DT     AND                  00114100
0112  *          REC_END_DT  = :GSB-REC-END-DT    AND                   00114200
0112  *          NTWK_SET_ID = :WS-TO-SET-ID                            00114300
0112  *    END-EXEC                                                     00114400
                                                                        00114401
           PERFORM 9001-GET-PARM-CONT          THRU 9001-EXIT           00114410
           MOVE GCPS-DB2-IO-RET-SQLCODE        TO SQLCODE               00114420
           PERFORM 9002-GET-GAGS-HOSTV-CONT    THRU 9002-EXIT           00114430
           PERFORM 9003-GET-GAGS-NTWSET-CONT   THRU 9003-EXIT           00114440
                                                                        00114450
0112       EVALUATE SQLCODE                                             00114460
0112          WHEN +0                                                   00114470
                  ADD +1 TO CM-SUCCESSFUL                               00114480
                  PERFORM 8060-PROTECT-DATALINE THRU 8060-EXIT          00114490
0112              GO TO 1250-EXIT                                       00114500
0112          WHEN -811                                                 00114600
                  ADD +1 TO CM-SUCCESSFUL                               00114700
                  PERFORM 8060-PROTECT-DATALINE THRU 8060-EXIT          00114800
0112              GO TO 1250-EXIT                                       00114900
0112          WHEN +100                                                 00115000
0112              CONTINUE                                              00115100
0112          WHEN OTHER                                                00115200
0112              DISPLAY 'DB2 ERR IN PARA 1250'                        00115300
0112              DISPLAY 'SQLCODE = ' SQLCODE                          00115400
0112       END-EVALUATE                                                 00115500
                                                                        00116300
                                                                        00117400
      *    #5 SQL, FUNCTION CALL = GAGSAPGM005                          00117700
      *            INSERT GRP_SECT_NTWK_SET_PND, GPN-                   00117800
                                                                        00117810
           MOVE 'GAGSAPGM005' TO GCPS-DB2-IO-FUNCTION                   00117900
                                                                        00118000
           PERFORM 8001-PUT-DFHROUTE-CONT    THRU 8001-EXIT             00118100
           PERFORM 8002-PUT-PARM-CONT        THRU 8002-EXIT             00118200
                                                                        00118201
      *   MOVE GSB FIELDS (BASE) FIELDS TO GPN (PEND) FIELDS            00118210
           MOVE GSB-GRP-SECT-NTWK-SET-ID  TO GPN-GRP-SECT-NTWK-SET-ID   00118220
           MOVE WS-TO-NTWK-SET-ID         TO GPN-NTWK-SET-ID            00118230
           MOVE GSB-CORP-ENT-CD           TO GPN-CORP-ENT-CD            00118240
           MOVE GSB-GRP-NBR               TO GPN-GRP-NBR                00118250
           MOVE GSB-SECT-NBR              TO GPN-SECT-NBR               00118260
           MOVE GSB-FAM-RELSHP-LVL-CD     TO GPN-FAM-RELSHP-LVL-CD      00118270
           MOVE GSB-REC-EFF-DT            TO GPN-REC-EFF-DT             00118280
           MOVE GSB-REC-END-DT            TO GPN-REC-END-DT             00118290
           MOVE +4                        TO GPN-AUD-PROC-LEN           00118291
           MOVE WS-AUD-PROC               TO GPN-AUD-PROC-TEXT          00118292
                                                                        00118293
           PERFORM 8006-PUT-GAGS-GSNTWP-CONT THRU 8006-EXIT             00118300
                                                                        00118400
           EXEC CICS LINK                                               00118500
              PROGRAM (WS-GCPSDAS)                                      00118600
              CHANNEL (GCPS-IO-CHANNEL)                                 00118700
              RESP    (WS-DFHRESP)                                      00118800
           END-EXEC                                                     00118900
                                                                        00119000
           EVALUATE WS-DFHRESP                                          00119100
              WHEN 0                                                    00119200
                  CONTINUE                                              00119300
              WHEN OTHER                                                00119400
                  MOVE ED-FREEFORM-ERROR-020 TO G1MSGO                  00119500
                  PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT             00119600
           END-EVALUATE                                                 00119700
                                                                        00119800
           PERFORM 9001-GET-PARM-CONT          THRU 9001-EXIT           00119900
           MOVE GCPS-DB2-IO-RET-SQLCODE        TO SQLCODE               00120000
           PERFORM 9002-GET-GAGS-HOSTV-CONT    THRU 9002-EXIT           00120100
           PERFORM 9005-GET-GAGS-GSNTWP-CONT   THRU 9005-EXIT           00120200
                                                                        00120300
      *    EXEC SQL                                                     00120400
      *     INSERT INTO GRP_SECT_NTWK_SET_PND                           00120500
      *      (                                                          00120600
      *         GRP_SECT_NTWK_SET_ID,                                   00120700
      *         CORP_ENT_CD,                                            00120800
      *         NTWK_SET_ID,                                            00120900
      *         GRP_NBR,                                                00121000
      *         SECT_NBR,                                               00121100
      *         FAM_RELSHP_LVL_CD,                                      00121200
      *         REC_EFF_DT,                                             00121300
      *         REC_END_DT,                                             00121400
      *         SRC_REC_STA_CD,                                         00121500
      *         AUD_PROC,                                               00121600
      *         AUD_TS,                                                 00121700
      *         AUD_USR                                                 00121800
      *         )                                                       00121900
      *            VALUES                                               00122000
      *         (                                                       00122100
      *              :GPN-GRP-SECT-NTWK-SET-ID,                         00122200
      *              :GPN-CORP-ENT-CD,                                  00122300
      *              :WS-TOSETID,                                       00122400
      *              :GPN-GRP-NBR,                                      00122500
      *              :GPN-SECT-NBR,                                     00122600
      *              :GPN-FAM-RELSHP-LVL-CD,                            00122700
      *              :GPN-REC-EFF-DT,                                   00122800
      *              :GPN-REC-END-DT,                                   00122900
      *              'I',                                               00123000
      *              :WS-AUD-PROC,                                      00123100
      *              CURRENT TIMESTAMP,                                 00123200
      *              :GPN-AUD-USR                                       00123300
      *         )                                                       00123400
      *    END-EXEC                                                     00123500
                                                                        00123600
           EVALUATE SQLCODE                                             00123700
              WHEN +0                                                   00123800
                  ADD +1 TO CM-SUCCESSFUL                               00123900
                  PERFORM 8060-PROTECT-DATALINE THRU 8060-EXIT          00124000
                  MOVE ' ' TO ERRLNI(WS-MAP)                            00124100
              WHEN +100                                                 00124200
                  CONTINUE                                              00124400
              WHEN OTHER                                                00124500
                  DISPLAY 'DB2 ERR IN PARA 1250'                        00124600
                  DISPLAY 'SQLCODE = ' SQLCODE                          00124700
           END-EVALUATE.                                                00124800
                                                                        00124900
       1250-EXIT.   EXIT.                                               00125000
                                                                        00125100
       1255-UPDATE-PND.                                                 00125200
           MOVE '1255-UPDATE-PND' TO WS-PARA-ID.                        00125300
                                                                        00125400
      *    #6 SQL, FUNCTION CALL = GAGSAPGM004                          00125500
      *            UPDATE GRP_SECT_NTWK_SET_PND, -GPN                   00125600
           MOVE 'GAGSAPGM004'     TO GCPS-DB2-IO-FUNCTION               00125700
                                                                        00125800
           MOVE WS-TOSETI         TO HV-TOSETI                          00125900
           MOVE WS-FROMSETI       TO HV-FRSETI                          00126000
           MOVE WS-CPNTYI         TO HV-CPNTYI                          00126100
           MOVE WS-GROUPI         TO HV-GROUPI                          00126200
           MOVE GSB-SECT-NBR      TO HV-SECT-NBR                        00126300
           MOVE WS-FMRELI         TO HV-FMRELI                          00126400
           MOVE WS-DB2-DATE       TO HV-DB2-DATE                        00126500
                                                                        00126600
           PERFORM 8001-PUT-DFHROUTE-CONT    THRU 8001-EXIT             00126700
           PERFORM 8002-PUT-PARM-CONT        THRU 8002-EXIT             00126800
           PERFORM 8006-PUT-GAGS-GSNTWP-CONT THRU 8006-EXIT             00126900
                                                                        00127000
           EXEC CICS LINK                                               00127100
              PROGRAM (WS-GCPSDAS)                                      00127200
              CHANNEL (GCPS-IO-CHANNEL)                                 00127300
              RESP    (WS-DFHRESP)                                      00127400
           END-EXEC                                                     00127500
                                                                        00127600
           EVALUATE WS-DFHRESP                                          00127700
              WHEN 0                                                    00127800
                  CONTINUE                                              00127900
              WHEN OTHER                                                00128000
                  MOVE ED-FREEFORM-ERROR-020 TO G1MSGO                  00128100
                  PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT             00128200
           END-EVALUATE                                                 00128300
                                                                        00128400
           PERFORM 9001-GET-PARM-CONT          THRU 9001-EXIT           00128500
           MOVE GCPS-DB2-IO-RET-SQLCODE        TO SQLCODE               00128600
           PERFORM 9002-GET-GAGS-HOSTV-CONT    THRU 9002-EXIT           00128700
           PERFORM 9005-GET-GAGS-GSNTWP-CONT   THRU 9005-EXIT           00128800
                                                                        00128900
      *    EXEC SQL                                                     00129000
      *     UPDATE GRP_SECT_NTWK_SET_PND                                00129100
      *         SET NTWK_SET_ID = :WS-TOSETID                           00129200
      *         WHERE CORP_ENT_CD        = :WS-CPNTYI                   00129300
      *           AND GRP_NBR            = :WS-GROUPI                   00129400
      *           AND SECT_NBR           = :GSB-SECT-NBR                00129500
      *           AND REC_EFF_DT         = :WS-DB2-DATE                 00129600
      *           AND NTWK_SET_ID        = :WS-FROMSETI                 00129700
      *           AND FAM_RELSHP_LVL_CD  = :WS-FMRELI                   00129800
      *    END-EXEC                                                     00129900
                                                                        00130000
           EVALUATE SQLCODE                                             00130100
              WHEN +0                                                   00130200
                  ADD +1 TO CM-SUCCESSFUL                               00130300
                  PERFORM 8060-PROTECT-DATALINE THRU 8060-EXIT          00130400
                  MOVE ' ' TO ERRLNI(WS-MAP)                            00130500
              WHEN +100                                                 00130600
                  STRING 'UPDATE ERROR IN PARA 1250.  SQLCODE = '       00130700
                      WS-SQLCODE DELIMITED BY SIZE                      00130800
                      INTO G1MSGO                                       00130900
                  END-STRING                                            00131000
              WHEN OTHER                                                00131100
                  STRING 'UPDATE ERROR IN PARA 1250.  SQLCODE = '       00131200
                      WS-SQLCODE DELIMITED BY SIZE                      00131300
                      INTO G1MSGO                                       00131400
                  END-STRING                                            00131500
           END-EVALUATE.                                                00131600
                                                                        00131700
       1255-EXIT.   EXIT.                                               00131800
                                                                        00131900
       2500-SEND-MAP-ERASE.                                             00132000
           MOVE '2500-SEND'  TO WS-PARA-ID.                             00132100
                                                                        00132200
           MOVE -1 TO FROMSETL                                          00132300
                                                                        00132400
           EXEC CICS                                                    00132500
              SEND   MAP   ('GAGSI01')                                  00132600
                     MAPSET('GAGSSET')                                  00132700
                     ERASE                                              00132800
                     FROM  (GAGSI01O)                                   00132900
                     CURSOR                                             00133000
           END-EXEC.                                                    00133100
                                                                        00133200
       2500-EXIT. EXIT.                                                 00133300
                                                                        00133400
       2510-SEND-MAP-DATA-ONLY.                                         00133500
           MOVE '2510-SEND' TO WS-PARA-ID.                              00133600
                                                                        00133700
           EXEC CICS                                                    00133800
              SEND   MAP   ('GAGSI01')                                  00133900
                     MAPSET('GAGSSET')                                  00134000
                     DATAONLY                                           00134100
                     FROM  (GAGSI01O)                                   00134200
                     CURSOR                                             00134300
           END-EXEC.                                                    00134400
                                                                        00134500
       2510-EXIT. EXIT.                                                 00134600
                                                                        00134700
       7100-XCTL-TO-CALLING-PROGRAM.                                    00134800
           MOVE '7100-XCTL' TO WS-PARA-ID.                              00134900
                                                                        00135000
           EXEC CICS XCTL                                               00135100
                PROGRAM  ('GNAHAPGM')                                   00135200
           END-EXEC.                                                    00135300
                                                                        00135400
       7100-EXIT. EXIT.                                                 00135500
                                                                        00135600
       8000-PROCESS-ERROR.                                              00135700
           MOVE '8000-PROCESS-ERROR' TO WS-PARA-ID.                     00135800
                                                                        00135900
           EXEC CICS SEND                                               00136000
               MAP   ('GAGSI01')                                        00136100
               MAPSET('GAGSSET')                                        00136200
               DATAONLY                                                 00136300
               FROM  (GAGSI01O)                                         00136400
               CURSOR                                                   00136500
           END-EXEC                                                     00136600
                                                                        00136700
           PERFORM 9990-RETURN-TRANSID   THRU 9990-EXIT.                00136800
                                                                        00136900
       8000-EXIT. EXIT.                                                 00137000
      ***************************************************************** 00137100
      *                                                                 00137200
      * 8001 - PUT THE ROUTING CONTAINER ON THE CHANNEL TO ROUTE TO THE 00137300
      *        CORRECT CICS REGION.                                     00137400
      ***************************************************************** 00137500
      *                                                                 00137600
       8001-PUT-DFHROUTE-CONT.                                          00137700
           MOVE '8001-PUT'          TO WS-PARA-ID                       00137800
                                                                        00137900
           MOVE CWABCENV            TO DYN-ENVIR-IND                    00138000
           IF  CWA-PROD-SYSTEM                                          00138100
               MOVE ' '             TO DYN-REL-MAINT-IND                00138200
           ELSE                                                         00138300
               MOVE CWATIND         TO DYN-REL-MAINT-IND                00138400
           END-IF.                                                      00138500
           MOVE 0                   TO DYN-ERROR-CODE                   00138600
           MOVE '<EYU9WRAM>'        TO DYN-ERROR-MESSAGE                00138700
           MOVE WS-GCPSDAS          TO DYN-CALLED-PROGRAM               00138800
           MOVE CWAPEARL            TO DYN-PEARL-IND                    00138900
           MOVE 'GCMI'              TO DYN-TRANSACTION                  00139000
                                                                        00139100
           EXEC CICS PUT                                                00139200
              CONTAINER (GCPS-DFHROUTE-CONT)                            00139300
              CHANNEL   (GCPS-IO-CHANNEL)                               00139400
              FROM      (COPY-DYNROUTC)                                 00139500
              FLENGTH   (LENGTH OF COPY-DYNROUTC)                       00139600
              RESP      (WS-DFHRESP)                                    00139700
           END-EXEC                                                     00139800
                                                                        00139900
           EVALUATE WS-DFHRESP                                          00140000
              WHEN 0                                                    00140100
                 CONTINUE                                               00140200
              WHEN OTHER                                                00140300
                 MOVE ED-FREEFORM-ERROR-013 TO G1MSGO                   00140400
                 PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT              00140500
           END-EVALUATE                                                 00140600
           .                                                            00140700
       8001-EXIT. EXIT.                                                 00140800
                                                                        00140900
      ***************************************************************** 00141000
      *8002 - PUT THE BASIC PARAMETERS EXCHANGED BETWEEN GAGSAPGM AND   00141100
      *       GCPSDAS.                                                  00141200
      ***************************************************************** 00141300
       8002-PUT-PARM-CONT.                                              00141400
           MOVE '8002-PUT'         TO WS-PARA-ID.                       00141500
                                                                        00141600
           MOVE WS-GAGSAPGM        TO GCPS-DB2-IO-CALLER-PGM            00141700
                                                                        00141800
           EXEC CICS PUT                                                00141900
              CONTAINER (GCPS-PARM-CONT)                                00142000
              CHANNEL   (GCPS-IO-CHANNEL)                               00142100
              FROM      (GCPS-DB2-IO-PARMS)                             00142200
              FLENGTH   (LENGTH OF GCPS-DB2-IO-PARMS)                   00142300
              RESP      (WS-DFHRESP)                                    00142400
           END-EXEC                                                     00142500
           EVALUATE WS-DFHRESP                                          00142600
              WHEN 0                                                    00142700
                 CONTINUE                                               00142800
              WHEN OTHER                                                00142900
                 MOVE ED-FREEFORM-ERROR-014 TO G1MSGO                   00143000
                 PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT              00143100
           END-EVALUATE                                                 00143200
           .                                                            00143300
       8002-EXIT. EXIT.                                                 00143400
                                                                        00143500
      ***********************************************************       00143600
      * 8003 - PUT THE HOST VARIABLES CONTAINER ON THE CHANNEL.         00143700
      ***********************************************************       00143800
                                                                        00143900
       8003-PUT-GAGS-HOSTV-CONT.                                        00144000
           MOVE '8003-PUT'                    TO WS-PARA-ID.            00144100
                                                                        00144200
           EXEC CICS PUT                                                00144300
              CONTAINER (GCPS-GAGS-HOSTV-CONT)                          00144400
              CHANNEL   (GCPS-IO-CHANNEL)                               00144500
              FROM      (HOST-VARIABLES)                                00144600
              FLENGTH   (LENGTH OF HOST-VARIABLES)                      00144700
              RESP      (WS-DFHRESP)                                    00144800
           END-EXEC                                                     00144900
                                                                        00145000
           EVALUATE WS-DFHRESP                                          00145100
              WHEN 0                                                    00145200
                 CONTINUE                                               00145300
                                                                        00145400
              WHEN OTHER                                                00145500
                 MOVE ED-FREEFORM-ERROR-015 TO G1MSGO                   00145600
                 PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT              00145700
           END-EVALUATE                                                 00145800
           .                                                            00145900
       8003-EXIT. EXIT.                                                 00146000
                                                                        00146100
      ******************************************************************00146200
      * 8004 - PUT THE NETWORK SET CONTAINER ON THE CHANNEL.            00146300
      ******************************************************************00146400
       8004-PUT-GAGS-NTWSET-CONT.                                       00146500
           MOVE '8004-PUT'                    TO WS-PARA-ID             00146600
                                                                        00146700
           EXEC CICS PUT                                                00146800
              CONTAINER (GCPS-GAGS-NTWSET-CONT)                         00146900
              CHANNEL   (GCPS-IO-CHANNEL)                               00147000
              FROM      (DCLNTWK-SET)                                   00147100
              FLENGTH   (LENGTH OF DCLNTWK-SET)                         00147200
              RESP      (WS-DFHRESP)                                    00147300
           END-EXEC.                                                    00147400
                                                                        00147500
           EVALUATE WS-DFHRESP                                          00147600
              WHEN 0                                                    00147700
                 CONTINUE                                               00147800
              WHEN OTHER                                                00147900
                 MOVE ED-FREEFORM-ERROR-016 TO G1MSGO                   00148000
                 PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT              00148100
           END-EVALUATE                                                 00148200
           .                                                            00148300
       8004-EXIT. EXIT.                                                 00148400
                                                                        00148500
      ******************************************************************00148600
      * 8005 - PUT THE GROUP SECTION NETWORK DCLGEN CONTAINER ON THE    00148700
      *        CHANNEL. COLUMNS PREFIXED WITH 'GSB-'.                   00148800
      ******************************************************************00148900
                                                                        00149000
       8005-PUT-GAGS-GSNTW-CONT.                                        00149100
           MOVE '8005-PUT'                    TO WS-PARA-ID             00149200
                                                                        00149300
           EXEC CICS PUT                                                00149400
              CONTAINER (GCPS-GAGS-GSNTW-CONT)                          00149500
              CHANNEL   (GCPS-IO-CHANNEL)                               00149600
              FROM      (DCLGRP-SECT-NTWK-SET)                          00149700
              FLENGTH   (LENGTH OF DCLGRP-SECT-NTWK-SET)                00149800
              RESP      (WS-DFHRESP)                                    00149900
           END-EXEC                                                     00150000
           EVALUATE WS-DFHRESP                                          00150100
              WHEN 0                                                    00150200
                 CONTINUE                                               00150300
                                                                        00150400
              WHEN OTHER                                                00150500
                 MOVE ED-FREEFORM-ERROR-017 TO G1MSGO                   00150600
                 PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT              00150700
           END-EVALUATE                                                 00150800
           .                                                            00150900
       8005-EXIT. EXIT.                                                 00151000
                                                                        00151100
      ******************************************************************00151200
      * 8006 - PUT THE GROUP SECTION NETWORK PENDING DCLGEN CONTAINER   00151300
      *        ON THE CHANNEL. COLUMNS PREFIXED WITH 'GSN-'.            00151400
      ******************************************************************00151500
       8006-PUT-GAGS-GSNTWP-CONT.                                       00151600
           MOVE '8006-PUT'                    TO WS-PARA-ID             00151700
                                                                        00151800
           EXEC CICS PUT                                                00151900
              CONTAINER (GCPS-GAGS-GSNTWP-CONT)                         00152000
              CHANNEL   (GCPS-IO-CHANNEL)                               00152100
              FROM      (DCLGRP-SECT-NTWK-SET-PND)                      00152200
              FLENGTH   (LENGTH OF DCLGRP-SECT-NTWK-SET-PND)            00152300
              RESP      (WS-DFHRESP)                                    00152400
           END-EXEC.                                                    00152500
           EVALUATE WS-DFHRESP                                          00152600
              WHEN 0                                                    00152700
                 CONTINUE                                               00152800
                                                                        00152900
              WHEN OTHER                                                00153000
                 MOVE ED-FREEFORM-ERROR-018 TO G1MSGO                   00153100
                 PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT              00153200
           END-EVALUATE                                                 00153300
           .                                                            00153400
       8006-EXIT. EXIT.                                                 00153500
                                                                        00153600
       8050-RED-ERR-LINE.                                               00153700
           MOVE '8050-RED-ERR-LINE' TO WS-PARA-ID.                      00153800
      *    HIGHLIGHT ERR LINES IN RED                                   00153900
                                                                        00154000
           MOVE DFHBMUBF   TO GROUPA(WS-MAP),                           00154100
                              SECTA(WS-MAP),                            00154200
                              EFFDTA(WS-MAP),                           00154300
                              CPNTYA(WS-MAP),                           00154400
                              FMRELA(WS-MAP).                           00154500
                                                                        00154600
       8050-EXIT.    EXIT.                                              00154700
                                                                        00154800
       8060-PROTECT-DATALINE.                                           00154900
           MOVE '8060-PROTECT-DATALINE' TO WS-PARA-ID.                  00155000
      *    PROTECT DATA LINES ONCE UPDATE IS SUCCESSFUL.                00155100
                                                                        00155200
           MOVE DFHBMASK TO GROUPA(WS-MAP),                             00155300
                            SECTA(WS-MAP),                              00155400
                            EFFDTA(WS-MAP),                             00155500
                            CPNTYA(WS-MAP),                             00155600
                            FMRELA(WS-MAP).                             00155700
                                                                        00155800
       8060-EXIT.   EXIT.                                               00155900
                                                                        00156000
      ***************************************************************** 00156100
      *                                                                 00156200
      * 9001 - GET THE BASIC PARAMETERS EXCHANGED BETWEEN GAGSAPGM AND  00156300
      *        GCPSDAS.                                                 00156400
      ***************************************************************** 00156500
                                                                        00156600
       9001-GET-PARM-CONT.                                              00156700
           MOVE '9001-GET'                    TO WS-PARA-ID             00156800
                                                                        00156900
           EXEC CICS GET                                                00157000
              CONTAINER (GCPS-PARM-CONT)                                00157100
              CHANNEL   (GCPS-IO-CHANNEL)                               00157200
              INTO      (GCPS-DB2-IO-PARMS)                             00157300
              FLENGTH   (LENGTH OF GCPS-DB2-IO-PARMS)                   00157400
              RESP      (WS-DFHRESP)                                    00157500
           END-EXEC                                                     00157600
                                                                        00157700
           EVALUATE WS-DFHRESP                                          00157800
              WHEN 0                                                    00157900
                 CONTINUE                                               00158000
                                                                        00158100
              WHEN OTHER                                                00158200
                 MOVE ED-FREEFORM-ERROR-024 TO G1MSGO                   00158300
                 PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT              00158400
           END-EVALUATE                                                 00158500
           .                                                            00158600
       9001-EXIT. EXIT.                                                 00158700
                                                                        00158800
      ******************************************************************00158900
      *                                                                 00159000
      * 9002 - GET THE HOST VARIABLES CONTAINER ON THE CHANNEL.         00159100
      *                                                                 00159200
      ******************************************************************00159300
       9002-GET-GAGS-HOSTV-CONT.                                        00159400
           MOVE '9002-GET'                     TO WS-PARA-ID            00159500
                                                                        00159600
           EXEC CICS GET                                                00159700
              CONTAINER (GCPS-GAGS-HOSTV-CONT)                          00159800
              CHANNEL   (GCPS-IO-CHANNEL)                               00159900
              INTO      (HOST-VARIABLES)                                00160000
              FLENGTH   (LENGTH OF HOST-VARIABLES)                      00160100
              RESP      (WS-DFHRESP)                                    00160200
           END-EXEC                                                     00160300
                                                                        00160400
           EVALUATE WS-DFHRESP                                          00160500
              WHEN 0                                                    00160600
                 CONTINUE                                               00160700
                                                                        00160800
              WHEN OTHER                                                00160900
                  MOVE ED-FREEFORM-ERROR-025 TO G1MSGO                  00161000
                  PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT             00161100
           END-EVALUATE                                                 00161200
           .                                                            00161300
       9002-EXIT. EXIT.                                                 00161400
                                                                        00161500
      ******************************************************************00161600
      *                                                                 00161700
      * 9003 - GET THE NETWORK SET DCLGEN CONTAINER ON THE CHANNEL.     00161800
      *                                                                 00161900
      ******************************************************************00162000
       9003-GET-GAGS-NTWSET-CONT.                                       00162100
           MOVE '9003-GET'                     TO WS-PARA-ID            00162200
                                                                        00162300
           EXEC CICS GET                                                00162400
              CONTAINER (GCPS-GAGS-NTWSET-CONT)                         00162500
              CHANNEL   (GCPS-IO-CHANNEL)                               00162600
              INTO      (DCLNTWK-SET)                                   00162700
              FLENGTH   (LENGTH OF DCLNTWK-SET)                         00162800
              RESP      (WS-DFHRESP)                                    00162900
           END-EXEC                                                     00163000
                                                                        00163100
           EVALUATE WS-DFHRESP                                          00163200
              WHEN 0                                                    00163300
                 CONTINUE                                               00163400
              WHEN OTHER                                                00163500
                  MOVE ED-FREEFORM-ERROR-024 TO G1MSGO                  00163600
                  PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT             00163700
           END-EVALUATE                                                 00163800
           .                                                            00163900
       9003-EXIT. EXIT.                                                 00164000
                                                                        00164100
      ******************************************************************00164200
      *                                                                 00164300
      * 9004 - GET THE GROUP SECTION NETWORK DCLGEN CONTAINER ON THE    00164400
      *        CHANNEL. COLUMNS PREFIXED WITH 'GSB-'.                   00164500
      *                                                                 00164600
      ******************************************************************00164700
       9004-GET-GAGS-GSNTW-CONT.                                        00164800
           MOVE '9004-GET'  TO WS-PARA-ID                               00164900
                                                                        00165000
           EXEC CICS GET                                                00165100
              CONTAINER (GCPS-GAGS-GSNTW-CONT)                          00165200
              CHANNEL   (GCPS-IO-CHANNEL)                               00165300
              INTO      (DCLGRP-SECT-NTWK-SET)                          00165400
              FLENGTH   (LENGTH OF DCLGRP-SECT-NTWK-SET)                00165500
              RESP      (WS-DFHRESP)                                    00165600
           END-EXEC                                                     00165700
                                                                        00165800
           EVALUATE WS-DFHRESP                                          00165900
              WHEN 0                                                    00166000
                 CONTINUE                                               00166100
              WHEN OTHER                                                00166200
                  MOVE ED-FREEFORM-ERROR-022 TO G1MSGO                  00166300
                  PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT             00166400
           END-EVALUATE                                                 00166500
           .                                                            00166600
       9004-EXIT. EXIT.                                                 00166700
      ******************************************************************00166800
      *                                                                 00166900
      * 9005 - GET THE GROUP SECTION NETWORK PENDING DCLGEN CONTAINER ON00167000
      *        THE CHANNEL. COLUMNS PREFIXED WITH 'GSN-'.               00167100
      *                                                                 00167200
      ******************************************************************00167300
       9005-GET-GAGS-GSNTWP-CONT.                                       00167400
           MOVE '9005-GET'  TO WS-PARA-ID                               00167500
                                                                        00167600
           EXEC CICS GET                                                00167700
              CONTAINER (GCPS-GAGS-GSNTWP-CONT)                         00167800
              CHANNEL   (GCPS-IO-CHANNEL)                               00167900
              INTO      (DCLGRP-SECT-NTWK-SET-PND)                      00168000
              FLENGTH   (LENGTH OF DCLGRP-SECT-NTWK-SET-PND)            00168100
              RESP      (WS-DFHRESP)                                    00168200
           END-EXEC                                                     00168300
                                                                        00168400
           EVALUATE WS-DFHRESP                                          00168500
              WHEN 0                                                    00168600
                  CONTINUE                                              00168700
              WHEN OTHER                                                00168800
                  MOVE ED-FREEFORM-ERROR-023 TO G1MSGO                  00168900
                  PERFORM 8000-PROCESS-ERROR THRU 8000-EXIT             00169000
           END-EVALUATE                                                 00169100
           .                                                            00169200
       9005-EXIT. EXIT.                                                 00169300
                                                                        00169400
       9200-SEND-MAP-AND-DATA.                                          00169500
           MOVE '9200-SEND-MAP' TO WS-PARA-ID.                          00169600
                                                                        00169700
           EXEC CICS                                                    00169800
              SEND   MAP   ('GAGSI01')                                  00169900
                     MAPSET('GAGSSET')                                  00170000
                     DATAONLY                                           00170100
                     FROM  (GAGSI01O)                                   00170200
                     CURSOR                                             00170300
           END-EXEC.                                                    00170400
                                                                        00170500
       9200-EXIT.   EXIT.                                               00170600
                                                                        00170700
       9800-STD-ERROR-MSG.                                              00170800
           MOVE '9800-STD-ERR' TO WS-PARA-ID.                           00170900
                                                                        00171000
           MOVE DFHBMABF  TO  G1MSGA                                    00171100
           EXEC CICS SEND                                               00171200
                 MAP('GAGSI01')                                         00171300
                 MAPSET('GAGSSET')                                      00171400
                 DATAONLY                                               00171500
                 FROM(GAGSI01O)                                         00171600
                 CURSOR                                                 00171700
           END-EXEC.                                                    00171800
                                                                        00171900
       9800-EXIT.   EXIT.                                               00172000
                                                                        00172100
       9990-RETURN-TRANSID.                                             00172200
           MOVE '9990-RETURN' TO WS-PARA-ID.                            00172300
                                                                        00172400
           EXEC CICS RETURN                                             00172500
                 TRANSID('GAGS')                                        00172600
                 COMMAREA (DFHCOMMAREA)                                 00172700
                 FLENGTH (LENGTH OF DFHCOMMAREA)                        00172800
           END-EXEC.                                                    00172900
                                                                        00173000
       9990-EXIT. EXIT.                                                 00173100
