       ID DIVISION.                                                     00000100
       PROGRAM-ID.   GAGWSPGM.                                          00000200
       AUTHOR.       JABIVULLA JAMAL SHAIK.                             00000300
       DATE-WRITTEN. 12/02/14.                                          00000400
      *                                                                 00000500
      ******************************************************************00000600
      *            M A I N T E N A N C E     L O G                     *00000700
      *----------------------------------------------------------------*00000800
      **-CHG-NUM-* *-DATE-* *-WHO-* *-------DESCRIPTION----------------*00000900
      *            02/16/15 I334539 INITIAL VERSION.                   *00001000
13610 * GCPS00250  01/20/15 I309952 FIX FOR DEFECT 13610, HIGHLIGHTING *00001010
13610 *                             BLUE COLOR FOR SUCCESSFULLY UPDATED*00001011
13610 *                             OR INSERTED RECORDS ON SCREEN      *00001012
13584 * GCPS00250  01/21/15 I334539 FIX FOR DEFECT 13584, VALIDATE BOTH*00001020
13584 *                             \
13584 *                             ASSOCIATED WITH SAME CORP ENTY CODE*00001040
13683 * GCPS00251  01/23/15 I307151 FIX FOR DEFECT 13683, VALIDATE CORP*00001050
13683 *                             ENTITY CODE USING GNTW002 COPYBOOK *00001060
      ******************************************************************00001100
                                                                        00001200
      ******************************************************************00001300
      *    GAGWSPGM - ATB: GROUP WITH MULTIPLE SECTION NUMBERS         *00001400
      *                                                                *00001500
      *    TRANSID: GAGW                                               *00001600
      *    MAPSET:  GAGWSETC                                           *00001700
      *    ENTRY POINT: NETWORK ATB HOME (GNAHAPGM)                    *00001710
      *                                                                *00001800
      *    PROGRAM NARRATIVE:                                          *00001900
      *            THIS PROGRAM WILL ALLOW USER TO DO MASS INSERT(15   *00002000
      *    RECORDS) ON A SINGLE NETWORK SET ID WITH A COMBINATION OF   *00002100
      *    SINGLE GROUP AND MULTIPLE SECTION NUMBERS.                  *00002200
      *                                                                *00002300
      *    FUNCTION KEYS:                                              *00002400
      *        F3  - RETURN TO PREVIOUS MENU                           *00002500
      *        F9  - CLEAR SCREEN DATA                                 *00002600
      *        F10 - SAVE & EXECUTE SCREEN DATA                        *00002700
      ******************************************************************00002800
                                                                        00002900
       ENVIRONMENT DIVISION.                                            00003000
       DATA DIVISION.                                                   00003100
      *                                                                 00003200
       WORKING-STORAGE SECTION.                                         00003300
      *** WORK FIELDS AND SWITCHES                                      00003400
       01 WS-WORK-FIELDS.                                               00003500
          05 WS-EIBTRNID             PIC X(04).                         00003600
             88 WS-VALID-TRAN-ID     VALUE 'GAGW' 'GNAH'.               00003700
             88 WS-TRAN-ID-GNAH      VALUE 'GNAH'.                      00003800
          05 WS-SPACE-SW             PIC X(01) VALUE 'N'.               00003900
             88 WS-SPACE-FOUND       VALUE 'Y'.                         00004000
             88 WS-SPACE-NOT-FOUND   VALUE 'N'.                         00004100
          05 WS-PARA-SW              PIC X(01) VALUE SPACES.            00004200
             88 WS-PARA-2000         VALUE '2'.                         00004300
             88 WS-PARA-3000         VALUE '3'.                         00004400
                                                                        00004500
          05 WS-CNTR                 PIC 9(02) VALUE ZEROES.            00004600
          05 WS-REGION               PIC X(04) VALUE SPACES.            00004700
          05 WS-AUD-USR              PIC X(07) VALUE SPACES.            00004800
          05 WS-TOT-CNTR             PIC 9(02) VALUE ZEROES.            00004900
          05 WS-SUCC-CNTR            PIC 9(02) VALUE ZEROES.            00005000
          05 WS-LOW-VALUES           PIC X(01) VALUE LOW-VALUES.        00005100
          05 WS-UNDERSCORE-CNT       PIC 9(02) VALUE ZEROES.            00005200
          05 WS-GROUP                PIC X(09).                         00005400
          05 WS-SECTION              PIC X(05).                         00005500
          05 WS-SQLCODE              PIC +999.                          00005700
          05 WS-TO-NTWK-SETID        PIC S9(9) USAGE COMP.              00005800
          05 WS-TO-NTWK-SET-ID       PIC X(05).                         00005900
          05 WS-TO-NTWK-SET-ID-NUM   REDEFINES                          00006000
             WS-TO-NTWK-SET-ID       PIC 9(05).                         00006100
          05 WS-FROM-NTWK-SET-ID     PIC X(05).                         00006200
          05 WS-FROM-NTWK-SET-ID-NUM REDEFINES                          00006300
             WS-FROM-NTWK-SET-ID     PIC 9(05).                         00006400
13584     05 WS-TO-CORP-ENT-CD       PIC X(03).                         00006410
13584     05 WS-FROM-CORP-ENT-CD     PIC X(03).                         00006420
          05 WS-EFFDT.                                                  00006500
             10 WS-EFFDT-CCYY        PIC X(04).                         00006600
             10 FILLER               PIC X(01) VALUE '-'.               00006700
             10 WS-EFFDT-MM          PIC X(02).                         00006800
             10 FILLER               PIC X(01) VALUE '-'.               00006900
             10 WS-EFFDT-DD          PIC X(02).                         00007000
          05 WS-DFHRESP              PIC S9(08) COMP VALUE +0.          00007100
          05 WS-DFHRESP2             PIC S9(08) COMP VALUE +0.          00007200
          05 WS-GAGWSPGM             PIC X(8) VALUE 'GAGWSPGM'.         00007300
          05 WS-GCPSDAS              PIC X(8) VALUE 'GCPSDAS '.         00007400
                                                                        00007500
      *** ERROR MESSAGES                                                00007600
       01 WS-MESSAGE-VALUES.                                            00007700
          05 WS-MESSAGE-TEXT-001 PIC X(79) VALUE                        00007800
             'THE GROUP NUMBER IS NOT VALID. PLEASE ENTER A VALID VALUE.00007900
      -      ''.                                                        00008000
          05 WS-MESSAGE-TEXT-002 PIC X(79) VALUE                        00008100
             'THE FROM NTWK SET ID IS NOT VALID. PLEASE ENTER A VALID VA00008200
      -      'LUE.'.                                                    00008300
          05 WS-MESSAGE-TEXT-003 PIC X(79) VALUE                        00008400
             'THE TO NTWK SET ID IS NOT VALID. PLEASE ENTER A VALID VALU00008500
      -      'E.'.                                                      00008600
          05 WS-MESSAGE-TEXT-004 PIC X(79) VALUE                        00008700
             'THE FR VALUE IS NOT VALID. PLEASE ENTER A VALID VALUE.'.  00008800
          05 WS-MESSAGE-TEXT-005 PIC X(79) VALUE                        00008900
             'THE SECT NUMBER IS NOT VALID. PLEASE ENTER A VALID VALUE. 00009000
      -      ''.                                                        00009100
          05 WS-MESSAGE-TEXT-006 PIC X(79) VALUE                        00009200
             'THE ATB EFF DATE IS NOT VALID. PLEASE ENTER A VALID VALUE.00009300
      -      ''.                                                        00009400
          05 WS-MESSAGE-TEXT-007 PIC X(79) VALUE                        00009500
             'THE CORP ENTITY CODE VALUE ISN''T VALID. PLEASE ENTER A VA00009600
      -      'LID VALUE TO CONTINUE.'.                                  00009700
          05 WS-MESSAGE-TEXT-008 PIC X(79) VALUE                        00009800
             'PLEASE CORRECT THE VALUES HIGHLIGHTED IN RED TO CONTINUE. 00009900
      -      ''.                                                        00010000
          05 WS-MESSAGE-TEXT-009 PIC X(79) VALUE                        00010100
             '*** INVALID REQUEST. THE PF KEY USED HAS NO MEANING TO THI00010200
      -      'S PROGRAM ***'.                                           00010300
          05 WS-MESSAGE-TEXT-010.                                       00010400
             10 WS-ERR-MSG10-PGM          PIC X(08).                    00010500
             10 WS-ERR-MSG10-TEXT         PIC X(18)                     00010600
                VALUE ' PUT CONT FAILED: '.                             00010700
             10 WS-ERR-MSG10-CONT-NAME    PIC X(16).                    00010800
             10 FILLER                    PIC X(12)                     00010900
                VALUE ' RESP CODE: '.                                   00011000
             10 WS-ERR-MSG10-RESP-CODE    PIC -9(08).                   00011100
          05 WS-MESSAGE-TEXT-011.                                       00011200
             10 WS-ERR-MSG11-PGM          PIC X(08).                    00011300
             10 WS-ERR-MSG11-TEXT         PIC X(18)                     00011400
                VALUE ' GET CONT FAILED: '.                             00011500
             10 WS-ERR-MSG11-CONT-NAME    PIC X(16).                    00011600
             10 FILLER                    PIC X(12)                     00011700
                VALUE ' RESP CODE: '.                                   00011800
             10 WS-ERR-MSG11-RESP-CODE    PIC -9(08).                   00011900
          05 WS-MESSAGE-TEXT-012.                                       00012000
             10 WS-ERR-MSG12-PGM          PIC X(08).                    00012100
             10 FILLER                    PIC X(01) VALUE SPACE.        00012200
             10 WS-ERR-MSG12-TEXT         PIC X(70).                    00012300
          05 WS-MESSAGE-TEXT-013.                                       00012400
             10 WS-ERR-MSG13-PGM          PIC X(08).                    00012500
             10 WS-ERR-MSG13-TEXT1        PIC X(32)                     00012600
                VALUE ' LINK TO GCPSDAS FAILED IN PARA '.               00012700
             10 WS-ERR-MSG13-PARA-NUMBER  PIC X(04).                    00012800
             10 WS-ERR-MSG13-TEXT2        PIC X(11)                     00012900
                VALUE ' RESP CODE '.                                    00013000
             10 WS-ERR-MSG13-RESP-CODE    PIC -9(08).                   00013100
             10 FILLER                    PIC X(01) VALUE '|'.          00013200
             10 WS-ERR-MSG13-RESP2-CODE   PIC -9(08).                   00013300
13584     05 WS-MESSAGE-TEXT-014 PIC X(79) VALUE                        00013310
13584        'BOTH \
13584 -      'NTER A VALID VALUE.'.                                     00013330
                                                                        00013400
      ***************************************************************** 00013500
      * CONTAINER INFORMATION.                                          00013600
      ***************************************************************** 00013700
      *                                                                 00013800
       01  WS-CHANNEL-AND-CONTAINERS.                                   00013900
           05  GCPS-IO-CHANNEL      PIC X(16) VALUE 'GCPS_IO_CHANNEL'.  00014000
           05  GCPS-DFHROUTE-CONT   PIC X(16) VALUE 'DFHROUTE'.         00014100
           05  GCPS-PARM-CONT       PIC X(16) VALUE 'PARM_CONT'.        00014200
           05  GCPS-DATA-CONT-IN    PIC X(16) VALUE 'DATA_CONT_IN'.     00014300
           05  GCPS-DATA-CONT-OUT   PIC X(16) VALUE 'DATA_CONT_OUT'.    00014400
                                                                        00014500
       01 GCPS-DB2-IO-PARMS.                                            00014600
          05 GCPS-DB2-IO-FUNCTION.                                      00014700
             10 GCPS-DB2-IO-CALLER-PGM       PIC X(08) VALUE SPACES.    00014800
             10 GCPS-DB2-IO-FUNCTION-CODE    PIC X(03) VALUE SPACES.    00014900
          05 GCPS-DB2-IO-RET-SQLCODE         PIC S9(9) COMP-5 VALUE 0.  00015000
          05 GCPS-DB2-IO-RET-RC              PIC S9(4) VALUE 0.         00015100
          05 GCPS-DB2-IO-RET-RC-MESSAGE      PIC X(80) VALUE SPACES.    00015200
                                                                        00015300
       01 GAGW-IN-STORAGE.                                              00015400
          05 GAGW-IN-AUD-PROC.                                          00015500
             49 GAGW-AUD-PROC-LEN          PIC S9(4) USAGE COMP.        00015600
             49 GAGW-AUD-PROC-TEXT         PIC X(30).                   00015700
          05 GAGW-IN-AUD-USR.                                           00015800
             49 GAGW-AUD-USR-LEN           PIC S9(4) USAGE COMP.        00015900
             49 GAGW-AUD-USR-TEXT          PIC X(50).                   00016000
          05 GAGW-IN-GRP-NBR               PIC X(10).                   00016100
          05 GAGW-IN-SECT-NBR              PIC X(10).                   00016200
          05 GAGW-IN-REC-EFF-DT            PIC X(10).                   00016300
          05 GAGW-IN-REC-END-DT            PIC X(10).                   00016400
          05 GAGW-IN-CORP-ENT-CD           PIC X(03).                   00016500
          05 GAGW-IN-SRC-REC-STA-CD        PIC X(01).                   00016600
          05 GAGW-IN-FAM-RELSHP-LVL-CD     PIC X(02).                   00016700
          05 GAGW-IN-NTWK-SET-ID           PIC S9(9) USAGE COMP.        00016800
          05 GAGW-GPN-NTWK-SET-ID          PIC S9(9) USAGE COMP.        00016810
          05 GAGW-IN-TO-NTWK-SETID         PIC S9(9) USAGE COMP.        00016900
          05 GAGW-IN-GRP-SECT-NTWK-SET-ID  PIC S9(9) USAGE COMP.        00017000
      *                                                                 00017100
       01 GAGW-OUT-STORAGE.                                             00017200
          05 GAGW-OUT-REC-END-DT           PIC X(10).                   00017300
13584     05 GAGW-OUT-CORP-ENT-CD          PIC X(03).                   00017301
          05 GAGW-OUT-NTWK-SET-ID          PIC S9(9) USAGE COMP.        00017310
          05 GAGW-OUT-GRP-SECT-NTWK-SET-ID PIC S9(9) USAGE COMP.        00017400
      *                                                                 00017500
      *** GAGWSET SYMBOLIC MAP                                          00017600
       COPY GAGWSETC.                                                   00017700
                                                                        00017800
      *** MAP FIELD ATTRIBUTES                                          00017900
       COPY DFHBMSCA.                                                   00018000
                                                                        00018100
      *** ATTENTION KEYS                                                00018200
       COPY DFHAID.                                                     00018300
                                                                        00018400
      *** DATE                                                          00018500
       COPY MLDATE01.                                                   00018600
                                                                        00018700
13683 * GNTW002 - COPYBOOK FOR TRANSLATION VALUES                       00018710
13683   COPY GNTW002.                                                   00018720
                                                                        00018730
      *** DYNAMIC ROUTING COPYBOOK                                      00018800
       01  COPY-DYNROUTC.                                               00018900
       COPY DYNROUTC.                                                   00019000
                                                                        00019100
      *** SQL COMMUNICATION AREA                                        00019200
           EXEC SQL                                                     00019300
              INCLUDE SQLCA                                             00019400
           END-EXEC.                                                    00019500
                                                                        00019600
      *** NETWORK SET TABLE                                             00019700
           EXEC SQL                                                     00019800
              INCLUDE NTWSET                                            00019900
           END-EXEC.                                                    00020000
                                                                        00020100
      *** GRP,SECT,NETWORK SET TABLE                                    00020200
           EXEC SQL                                                     00020300
              INCLUDE GSNTWSET                                          00020400
           END-EXEC.                                                    00020500
                                                                        00020600
      *** GRP,SECT,NETWORK SET PEND TABLE                               00020700
           EXEC SQL                                                     00020800
              INCLUDE GSNWSETP                                          00020900
           END-EXEC.                                                    00021000
                                                                        00021100
       LINKAGE SECTION.                                                 00021200
          COPY CWACOBOL.                                                00021300
                                                                        00021400
       PROCEDURE DIVISION.                                              00021500
                                                                        00021600
       0000-PROCESS-CONTROL.                                            00021700
           PERFORM 1000-HOUSE-KEEPING    THRU 1000-EXIT.                00021800
           PERFORM 2000-PROCESS-INPUT    THRU 2000-EXIT.                00021900
           PERFORM 3000-PROCESS-PF10     THRU 3000-EXIT.                00022000
           PERFORM 9000-DISPLAY-SUCC-MSG THRU 9000-EXIT.                00022100
           PERFORM 9999-RETURN-TRANSID   THRU 9999-EXIT.                00022200
                                                                        00022300
       0000-EXIT.                                                       00022400
           EXIT.                                                        00022500
                                                                        00022600
       1000-HOUSE-KEEPING.                                              00022700
           EXEC CICS HANDLE CONDITION                                   00022800
                MAPFAIL(9400-XCTL-TO-GCPSPGM)                           00022900
           END-EXEC.                                                    00023000
                                                                        00023100
           EXEC CICS ASSIGN                                             00023200
                USERID(WS-AUD-USR)                                      00023300
           END-EXEC.                                                    00023400
                                                                        00023500
           EXEC CICS ADDRESS                                            00023600
                CWA(ADDRESS OF CWACOBOL)                                00023700
                RESP(WS-DFHRESP)                                        00023800
           END-EXEC.                                                    00023900
                                                                        00024000
           IF CWA-PROD-SYSTEM                                           00024100
              MOVE 'PROD' TO WS-REGION                                  00024200
           ELSE                                                         00024300
              MOVE 'TEST' TO WS-REGION                                  00024400
           END-IF.                                                      00024500
                                                                        00024600
      *** VALIDATE TRANS. ID                                            00024700
           MOVE EIBTRNID  TO WS-EIBTRNID.                               00024800
                                                                        00024900
           IF WS-VALID-TRAN-ID                                          00025000
              IF WS-TRAN-ID-GNAH                                        00025100
                 PERFORM 9100-SEND-MAP-ONLY  THRU 9100-EXIT             00025200
                 PERFORM 9999-RETURN-TRANSID THRU 9999-EXIT             00025300
              END-IF                                                    00025400
           ELSE                                                         00025500
              PERFORM 9400-XCTL-TO-GCPSPGM   THRU 9400-EXIT             00025600
           END-IF.                                                      00025700
                                                                        00025800
      *** VALIDATE PFKEYS                                               00025900
           IF EIBAID = DFHENTER OR DFHCLEAR OR                          00026000
                       DFHPF3   OR DFHPF15  OR                          00026100
                       DFHPF9   OR DFHPF21  OR                          00026200
                       DFHPF10  OR DFHPF22                              00026300
              CONTINUE                                                  00026400
           ELSE                                                         00026500
              MOVE -1                        TO GRPNUML                 00026600
              MOVE WS-MESSAGE-TEXT-009       TO G1MSGO                  00026800
              PERFORM 1001-RESET-ATTRIBUTES  THRU 1001-EXIT             00026900
              PERFORM 9200-SEND-DATA-ONLY    THRU 9200-EXIT             00026910
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00027000
           END-IF.                                                      00027100
                                                                        00027200
      *** REQUEST PREVIOUS MENU                                         00027300
           IF EIBAID = DFHPF3 OR DFHPF15                                00027400
              EXEC CICS XCTL                                            00027500
                   PROGRAM('GNAHAPGM')                                  00027600
              END-EXEC                                                  00027700
           END-IF.                                                      00027800
                                                                        00027900
      *** CLEAR SCREEN DATA                                             00028000
           IF EIBAID = DFHPF9                                           00028100
              PERFORM 9100-SEND-MAP-ONLY  THRU 9100-EXIT                00028200
              PERFORM 9999-RETURN-TRANSID THRU 9999-EXIT                00028300
           END-IF.                                                      00028400
                                                                        00028500
      *** CLEAR SCREEN                                                  00028600
           IF EIBAID = DFHCLEAR                                         00028700
              EXEC CICS SEND                                            00028800
                   FROM(WS-LOW-VALUES)                                  00028900
                   ERASE                                                00029000
              END-EXEC                                                  00029100
                                                                        00029200
              EXEC CICS RETURN END-EXEC                                 00029300
           END-IF.                                                      00029400
                                                                        00029500
       1000-EXIT.                                                       00029600
           EXIT.                                                        00029700
                                                                        00029800
       1001-RESET-ATTRIBUTES.                                           00029900
      *** RESET ATTRIBUTES TO UNPROTECTED TO REVERT RED COLOR           00029901
           MOVE DFHBMUNF TO GRPNUMA                                     00029902
                            NSIDFRMA                                    00029903
                            NSIDTOA                                     00029904
                            FRLA.                                       00029905
                                                                        00029906
           PERFORM VARYING WS-CNTR FROM 1 BY 1 UNTIL WS-CNTR >=15       00029907
              MOVE DFHBMUNF TO SECTA(WS-CNTR)                           00029908
                               EFFDTA(WS-CNTR)                          00029909
                               CRPCDA(WS-CNTR)                          00029910
           END-PERFORM.                                                 00029911
                                                                        00029912
       1001-EXIT.                                                       00029913
           EXIT.                                                        00029920
                                                                        00029930
       2000-PROCESS-INPUT.                                              00029940
      *** RECEIVE MAP DATA                                              00030000
           EXEC CICS RECEIVE                                            00030100
                MAP('GAGWI01')                                          00030200
                MAPSET('GAGWSET')                                       00030300
           END-EXEC.                                                    00030400
                                                                        00030500
           IF EIBAID = DFHENTER OR DFHPF10                              00030600
              PERFORM 2001-VALIDATE-SCRN-DATA         THRU 2001-EXIT    00030700
              MOVE '2'                                TO WS-PARA-SW     00030800
              PERFORM VARYING WS-CNTR FROM 1 BY 1                       00030900
                UNTIL WS-CNTR >= 15   OR WS-SPACE-FOUND                 00031000
                  PERFORM 2300-VALIDATE-SCRN-ROW-DATA THRU 2300-EXIT    00031100
              END-PERFORM                                               00031200
           END-IF.                                                      00031300
                                                                        00031400
       2000-EXIT.                                                       00031500
           EXIT.                                                        00031600
                                                                        00031700
       2001-VALIDATE-SCRN-DATA.                                         00031800
      *** GROUP NUMBER                                                  00031900
           INITIALIZE WS-UNDERSCORE-CNT.                                00032000
           IF GRPNUMI NOT = ( '_________' AND SPACES AND LOW-VALUES)    00032100
              INSPECT GRPNUMI REPLACING ALL '_' BY SPACES               00032210
                                                                        00032211
              INSPECT FUNCTION REVERSE(GRPNUMI)                         00032220
                      TALLYING WS-UNDERSCORE-CNT                        00032300
                      FOR LEADING SPACES                                00032400
                                                                        00032500
              ADD 1            TO WS-UNDERSCORE-CNT                     00032600
              MOVE '000000000' TO WS-GROUP                              00032700
              MOVE GRPNUMI     TO WS-GROUP(WS-UNDERSCORE-CNT:)          00032800
              MOVE WS-GROUP    TO GRPNUMO                               00032900
           ELSE                                                         00033000
              MOVE -1                        TO GRPNUML                 00033100
              MOVE DFHBMUBF                  TO GRPNUMA                 00033200
              MOVE WS-MESSAGE-TEXT-001       TO G1MSGO                  00033300
              PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT             00033400
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00033500
           END-IF.                                                      00033600
                                                                        00033700
      *** FROM SET ID NUMERIC CHECK                                     00033800
           INITIALIZE WS-UNDERSCORE-CNT.                                00033900
           IF NSIDFRMI NOT = ( '_____' AND SPACES AND LOW-VALUES)       00034000
              INSPECT NSIDFRMI REPLACING ALL '_' BY SPACES              00034010
                                                                        00034020
              INSPECT FUNCTION REVERSE(NSIDFRMI)                        00034100
                      TALLYING WS-UNDERSCORE-CNT                        00034200
                      FOR LEADING SPACES                                00034300
                                                                        00034400
              ADD 1         TO WS-UNDERSCORE-CNT                        00034500
              MOVE '00000'  TO WS-FROM-NTWK-SET-ID                      00034600
              MOVE NSIDFRMI TO WS-FROM-NTWK-SET-ID(WS-UNDERSCORE-CNT:)  00034700
                                                                        00034800
              IF WS-FROM-NTWK-SET-ID-NUM IS NUMERIC                     00034900
                 MOVE WS-FROM-NTWK-SET-ID-NUM TO GAGW-IN-NTWK-SET-ID    00035000
                 PERFORM 2100-VALIDATE-FROM-SET-ID THRU 2100-EXIT       00035100
              ELSE                                                      00035200
                 MOVE -1                        TO NSIDFRML             00035300
                 MOVE DFHBMUBF                  TO NSIDFRMA             00035400
                 MOVE WS-MESSAGE-TEXT-002       TO G1MSGO               00035500
                 PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT          00035600
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00035700
              END-IF                                                    00035800
           ELSE                                                         00035900
              MOVE -1                        TO NSIDFRML                00036000
              MOVE DFHBMUBF                  TO NSIDFRMA                00036100
              MOVE WS-MESSAGE-TEXT-002       TO G1MSGO                  00036200
              PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT             00036300
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00036400
           END-IF.                                                      00036500
                                                                        00036600
      *** TO SET ID NUMERIC CHECK                                       00036700
           INITIALIZE WS-UNDERSCORE-CNT.                                00036800
           IF NSIDTOI NOT = ( '_____' AND SPACES AND LOW-VALUES)        00036900
              INSPECT NSIDTOI REPLACING ALL '_' BY SPACES               00036910
                                                                        00036920
              INSPECT FUNCTION REVERSE(NSIDTOI)                         00037000
                      TALLYING WS-UNDERSCORE-CNT                        00037100
                      FOR LEADING SPACES                                00037200
                                                                        00037300
              ADD 1        TO WS-UNDERSCORE-CNT                         00037400
              MOVE '00000' TO WS-TO-NTWK-SET-ID                         00037500
              MOVE NSIDTOI TO WS-TO-NTWK-SET-ID(WS-UNDERSCORE-CNT:)     00037600
                                                                        00037700
              IF WS-TO-NTWK-SET-ID-NUM IS NUMERIC                       00037800
                 MOVE WS-TO-NTWK-SET-ID-NUM TO GAGW-IN-NTWK-SET-ID      00037900
                                               GAGW-IN-TO-NTWK-SETID    00037910
                 PERFORM 2200-VALIDATE-TO-SET-ID   THRU 2200-EXIT       00038000
              ELSE                                                      00038100
                 MOVE -1                        TO NSIDTOL              00038200
                 MOVE DFHBMUBF                  TO NSIDTOA              00038300
                 MOVE WS-MESSAGE-TEXT-003       TO G1MSGO               00038400
                 PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT          00038500
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00038600
              END-IF                                                    00038700
           ELSE                                                         00038800
              MOVE -1                        TO NSIDTOL                 00038900
              MOVE DFHBMUBF                  TO NSIDTOA                 00039000
              MOVE WS-MESSAGE-TEXT-003       TO G1MSGO                  00039100
              PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT             00039200
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00039300
           END-IF.                                                      00039400
                                                                        00039500
13584 *** VALIDATE BOTH 'CHANGE FROM' AND 'CHANGE TO' SET ID'S ARE      00039600
13584 *** ASSOCIATED WITH SAME CORPORATE ENTITY CODE                    00039601
13584      IF WS-FROM-CORP-ENT-CD = WS-TO-CORP-ENT-CD                   00039602
13584         CONTINUE                                                  00039603
13584      ELSE                                                         00039604
13584         MOVE -1                        TO NSIDFRML                00039605
13584         MOVE DFHBMUBF                  TO NSIDFRMA                00039606
13584         MOVE DFHBMUBF                  TO NSIDTOA                 00039607
13584         MOVE WS-MESSAGE-TEXT-014       TO G1MSGO                  00039608
13584         PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT             00039609
13584         PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00039610
13584      END-IF.                                                      00039611
                                                                        00039612
      *** FRL                                                           00039620
           IF FRLI NOT = ( '__' AND SPACES AND LOW-VALUES)              00039700
              CONTINUE                                                  00039800
           ELSE                                                         00039900
              MOVE -1                        TO FRLL                    00040000
              MOVE DFHBMUBF                  TO FRLA                    00040100
              MOVE WS-MESSAGE-TEXT-004       TO G1MSGO                  00040200
              PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT             00040300
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00040400
           END-IF.                                                      00040500
                                                                        00040600
       2001-EXIT.                                                       00040700
           EXIT.                                                        00040800
                                                                        00040900
       2100-VALIDATE-FROM-SET-ID.                                       00041000
           MOVE '001' TO GCPS-DB2-IO-FUNCTION-CODE.                     00041100
           PERFORM 8001-GCPSDAS-DB2-PROCESS THRU 8001-EXIT.             00041200
13584      PERFORM 7006-GET-OUTPUT-CONT     THRU 7006-EXIT.             00041210
                                                                        00041300
           MOVE SQLCODE TO WS-SQLCODE.                                  00041400
           EVALUATE SQLCODE                                             00041500
             WHEN 0                                                     00041600
13584 *         CONTINUE                                                00041700
13584           MOVE GAGW-OUT-CORP-ENT-CD      TO WS-FROM-CORP-ENT-CD   00041710
             WHEN +100                                                  00041800
                MOVE -1                        TO NSIDFRML              00041900
                MOVE DFHBMUBF                  TO NSIDFRMA              00042000
                MOVE WS-MESSAGE-TEXT-002       TO G1MSGO                00042100
                PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT           00042200
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00042300
             WHEN OTHER                                                 00042400
                MOVE -1                        TO NSIDFRML              00042500
                MOVE DFHBMUBF                  TO NSIDFRMA              00042600
                STRING 'SELECT ERROR IN PARA 2100. SQLCODE = '          00042700
                       WS-SQLCODE DELIMITED BY SIZE                     00042800
                  INTO G1MSGO                                           00042900
                END-STRING                                              00043000
                PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT           00043100
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00043200
           END-EVALUATE.                                                00043300
                                                                        00043400
       2100-EXIT.                                                       00043500
           EXIT.                                                        00043600
                                                                        00043700
       2200-VALIDATE-TO-SET-ID.                                         00043800
           MOVE '001' TO GCPS-DB2-IO-FUNCTION-CODE.                     00043900
           PERFORM 8001-GCPSDAS-DB2-PROCESS THRU 8001-EXIT.             00044000
13584      PERFORM 7006-GET-OUTPUT-CONT     THRU 7006-EXIT.             00044010
                                                                        00044100
           MOVE SQLCODE TO WS-SQLCODE.                                  00044200
           EVALUATE SQLCODE                                             00044300
             WHEN 0                                                     00044400
13584 *         CONTINUE                                                00044500
13584           MOVE GAGW-OUT-CORP-ENT-CD      TO WS-TO-CORP-ENT-CD     00044510
             WHEN +100                                                  00044600
                MOVE -1                        TO NSIDTOL               00044700
                MOVE DFHBMUBF                  TO NSIDTOA               00044800
                MOVE WS-MESSAGE-TEXT-003       TO G1MSGO                00044900
                PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT           00045000
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00045100
             WHEN OTHER                                                 00045200
                MOVE -1                        TO NSIDTOL               00045300
                MOVE DFHBMUBF                  TO NSIDTOA               00045400
                STRING 'SELECT ERROR IN PARA 2200. SQLCODE = '          00045500
                       WS-SQLCODE DELIMITED BY SIZE                     00045600
                  INTO G1MSGO                                           00045700
                END-STRING                                              00045800
                PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT           00045900
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00046000
           END-EVALUATE.                                                00046100
                                                                        00046200
       2200-EXIT.                                                       00046300
           EXIT.                                                        00046400
                                                                        00046500
       2300-VALIDATE-SCRN-ROW-DATA.                                     00046600
      *** SKIP THE PROCESS IF NO DATA ENTERED AFTER 1ST ROW             00046700
           IF  SECTI(WS-CNTR)  = ('_____' OR SPACES OR LOW-VALUES)      00046800
           AND EFFDTI(WS-CNTR) = ('__________' OR SPACES OR LOW-VALUES) 00046900
           AND CRPCDI(WS-CNTR) = ('___' OR SPACES OR LOW-VALUES)        00047000
             IF WS-CNTR = 1                                             00047100
                MOVE -1                        TO SECTL(WS-CNTR)        00047200
                MOVE DFHBMUBF                  TO SECTA(WS-CNTR)        00047300
                                                  EFFDTA(WS-CNTR)       00047400
                                                  CRPCDA(WS-CNTR)       00047500
                MOVE WS-MESSAGE-TEXT-008       TO G1MSGO                00047600
                PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT           00047700
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00047800
             ELSE                                                       00047900
                IF WS-PARA-2000                                         00048000
                   GO TO 2000-EXIT                                      00048100
                ELSE                                                    00048200
                   IF WS-PARA-3000                                      00048300
                      GO TO 3000-EXIT                                   00048400
                   END-IF                                               00048500
                END-IF                                                  00048600
             END-IF                                                     00048700
           END-IF.                                                      00048800
                                                                        00048900
      *-------------------------------------------------------------*   00049000
      * CHECK FOR MULTIPLE INVALID VALUES.                          *   00049100
      * VALID   - USER ENTERED SOME VALUE                           *   00049200
      * INVALID - SPACES, LOW-VALUES AND ALL '_'                    *   00049300
      *-------------------------------------------------------------*   00049400
      *** VALID #SECT AND INVALID (#EFF-DATE OR #CORP ENTITY CODE)      00049500
           IF   SECTI(WS-CNTR) NOT = ('_____' AND SPACES AND LOW-VALUES)00049600
           AND (EFFDTI(WS-CNTR) = ('__________' OR SPACES OR LOW-VALUES)00049700
           AND  CRPCDI(WS-CNTR) = ('___' OR SPACES OR LOW-VALUES))      00049800
                SET WS-SPACE-FOUND             TO TRUE                  00049900
                MOVE -1                        TO EFFDTL(WS-CNTR)       00050000
                MOVE DFHBMUBF                  TO EFFDTA(WS-CNTR)       00050100
                                                  CRPCDA(WS-CNTR)       00050200
                MOVE WS-MESSAGE-TEXT-008       TO G1MSGO                00050300
                PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT           00050400
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00050500
           END-IF.                                                      00050600
                                                                        00050700
      *** VALID #EFF-DATE AND INVALID (#SECT OR #CORP ENTITY CODE)      00050800
           IF   EFFDTI(WS-CNTR) NOT = ('__________' AND SPACES AND      00050900
                                        LOW-VALUES)                     00051000
           AND (SECTI(WS-CNTR)   = ('_____' OR SPACES OR LOW-VALUES)    00051100
           AND  CRPCDI(WS-CNTR)  = ('___' OR SPACES OR LOW-VALUES))     00051200
                SET WS-SPACE-FOUND             TO TRUE                  00051300
                MOVE -1                        TO SECTL(WS-CNTR)        00051400
                MOVE DFHBMUBF                  TO SECTA(WS-CNTR)        00051500
                                                  CRPCDA(WS-CNTR)       00051600
                MOVE WS-MESSAGE-TEXT-008       TO G1MSGO                00051700
                PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT           00051800
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00051900
           END-IF.                                                      00052000
                                                                        00052100
      *** VALID #CORP ENTITY CODE AND INVALID (#SECT OR #EFF-DATE)      00052200
           IF   CRPCDI(WS-CNTR) NOT = ('___' AND SPACES AND LOW-VALUES) 00052300
           AND (EFFDTI(WS-CNTR) = ('__________' OR SPACES OR LOW-VALUES)00052400
           AND  SECTI(WS-CNTR)  = ('_____' OR SPACES OR LOW-VALUES))    00052500
                SET WS-SPACE-FOUND             TO TRUE                  00052600
                MOVE -1                        TO SECTL(WS-CNTR)        00052700
                MOVE DFHBMUBF                  TO SECTA(WS-CNTR)        00052800
                                                  EFFDTA(WS-CNTR)       00052900
                MOVE WS-MESSAGE-TEXT-008       TO G1MSGO                00053000
                PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT           00053100
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00053200
           END-IF.                                                      00053300
                                                                        00053400
      *** SECTION                                                       00053500
           INITIALIZE WS-UNDERSCORE-CNT.                                00053600
           IF SECTI(WS-CNTR) NOT = ('_____' AND SPACES AND LOW-VALUES)  00053700
              INSPECT SECTI(WS-CNTR) REPLACING ALL '_' BY SPACES        00053710
                                                                        00053720
              INSPECT FUNCTION REVERSE(SECTI(WS-CNTR))                  00053800
                      TALLYING WS-UNDERSCORE-CNT                        00053900
                      FOR LEADING SPACES                                00054000
                                                                        00054100
              ADD 1               TO WS-UNDERSCORE-CNT                  00054200
              MOVE '00000'        TO WS-SECTION                         00054300
              MOVE SECTI(WS-CNTR) TO WS-SECTION(WS-UNDERSCORE-CNT:)     00054400
              MOVE WS-SECTION     TO SECTO(WS-CNTR)                     00054500
           ELSE                                                         00054600
              SET WS-SPACE-FOUND             TO TRUE                    00054700
              MOVE -1                        TO SECTL(WS-CNTR)          00054800
              MOVE DFHBMUBF                  TO SECTA(WS-CNTR)          00054900
              MOVE WS-MESSAGE-TEXT-005       TO G1MSGO                  00055100
              PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT             00055200
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00055300
           END-IF.                                                      00055400
                                                                        00055500
      *** ATB EFFECTIVE DATE                                            00055600
           IF EFFDTI(WS-CNTR)(1:2) IS NUMERIC AND                       00055700
              EFFDTI(WS-CNTR)(3:1)= '/'       AND                       00055800
              EFFDTI(WS-CNTR)(4:2) IS NUMERIC AND                       00055900
              EFFDTI(WS-CNTR)(6:1)= '/'       AND                       00056000
              EFFDTI(WS-CNTR)(7:4) IS NUMERIC                           00056100
              PERFORM 2310-VALIDATE-EFF-DATE  THRU 2310-EXIT            00056200
           ELSE                                                         00056300
              SET WS-SPACE-FOUND              TO TRUE                   00056400
              MOVE -1                         TO EFFDTL(WS-CNTR)        00056500
              MOVE DFHBMUBF                   TO EFFDTA(WS-CNTR)        00056600
              MOVE WS-MESSAGE-TEXT-006        TO G1MSGO                 00056700
              PERFORM 9300-SEND-MAP-AND-DATA  THRU 9300-EXIT            00056800
              PERFORM 9999-RETURN-TRANSID     THRU 9999-EXIT            00056900
           END-IF.                                                      00057000
                                                                        00057100
      *** CORPORATE ENTITY CODE                                         00057200
13683      MOVE CRPCDI(WS-CNTR) TO TVD-VALIDATE-CORP-ENTITY.            00057210
13683                                                                   00057220
13683      IF TVD-VALID-CORP-ENTITIES                                   00057230
              CONTINUE                                                  00057500
           ELSE                                                         00057600
              SET WS-SPACE-FOUND             TO TRUE                    00057700
              MOVE -1                        TO CRPCDL(WS-CNTR)         00057800
              MOVE DFHBMUBF                  TO CRPCDA(WS-CNTR)         00057900
              MOVE WS-MESSAGE-TEXT-007       TO G1MSGO                  00058000
              PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT             00058100
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00058200
           END-IF.                                                      00058300
                                                                        00058400
       2300-EXIT.                                                       00058500
           EXIT.                                                        00058600
                                                                        00058700
       2310-VALIDATE-EFF-DATE.                                          00058800
      *** EFFECTIVE DATE VALIDATION                                  *  00058900
           MOVE 'CNV'                TO MLDATE-FUNC.                    00059000
           MOVE 'M'                  TO MLDATE-FORM1.                   00059100
           MOVE 'Y'                  TO MLDATE-FORM2.                   00059200
           MOVE EFFDTI(WS-CNTR)(1:2) TO MLDATE-DATE1(1:2).              00059300
           MOVE EFFDTI(WS-CNTR)(4:2) TO MLDATE-DATE1(3:2).              00059400
           MOVE EFFDTI(WS-CNTR)(7:4) TO MLDATE-DATE1(5:4).              00059500
                                                                        00059600
           EXEC CICS LINK                                               00059700
                PROGRAM('MLDATEC')                                      00059800
                COMMAREA(MLDATE01)                                      00059900
                LENGTH(LENGTH OF MLDATE01)                              00060000
           END-EXEC.                                                    00060100
                                                                        00060200
           IF MLDATE-RETURN = '01'                                      00060300
              SET WS-SPACE-FOUND             TO TRUE                    00060400
              MOVE -1                        TO EFFDTL(WS-CNTR)         00060500
              MOVE DFHBMUBF                  TO EFFDTA(WS-CNTR)         00060600
              MOVE WS-MESSAGE-TEXT-006       TO G1MSGO                  00060700
              PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT             00060800
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00060900
           END-IF.                                                      00061000
                                                                        00061100
       2310-EXIT.                                                       00061200
           EXIT.                                                        00061300
                                                                        00061400
       2400-SCRN-ROW-BASE-CHECK.                                        00061500
           MOVE WS-FROM-NTWK-SET-ID-NUM  TO GAGW-IN-NTWK-SET-ID.        00061600
           MOVE '002'                    TO GCPS-DB2-IO-FUNCTION-CODE.  00061700
           PERFORM 8001-GCPSDAS-DB2-PROCESS THRU 8001-EXIT.             00061800
           PERFORM 7006-GET-OUTPUT-CONT     THRU 7006-EXIT.             00061810
                                                                        00061900
           MOVE SQLCODE TO WS-SQLCODE.                                  00062000
           EVALUATE SQLCODE                                             00062100
             WHEN 0                                                     00062200
                ADD 1                          TO WS-TOT-CNTR           00062300
             WHEN +100                                                  00062400
                MOVE -1                        TO SECTL(WS-CNTR)        00062500
                MOVE DFHBMUBF                  TO SECTA(WS-CNTR)        00062600
                                                  EFFDTA(WS-CNTR)       00062700
                                                  CRPCDA(WS-CNTR)       00062800
                MOVE WS-MESSAGE-TEXT-008       TO G1MSGO                00062900
                PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT           00063000
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00063100
             WHEN OTHER                                                 00063200
                MOVE -1                        TO SECTL(WS-CNTR)        00063300
                MOVE DFHBMUBF                  TO SECTA(WS-CNTR)        00063400
                STRING 'SELECT ERROR IN PARA 2400. SQLCODE = '          00063500
                       WS-SQLCODE DELIMITED BY SIZE                     00063600
                  INTO G1MSGO                                           00063700
                END-STRING                                              00063800
                PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT           00063900
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00064000
           END-EVALUATE.                                                00064100
                                                                        00064200
       2400-EXIT.                                                       00064300
           EXIT.                                                        00064400
                                                                        00064500
       2500-SCRN-ROW-PEND-CHECK.                                        00064600
           MOVE '003'                    TO GCPS-DB2-IO-FUNCTION-CODE.  00064700
           PERFORM 8001-GCPSDAS-DB2-PROCESS THRU 8001-EXIT.             00064800
           PERFORM 7006-GET-OUTPUT-CONT     THRU 7006-EXIT.             00064810
                                                                        00064900
           MOVE SQLCODE TO WS-SQLCODE.                                  00065000
           EVALUATE SQLCODE                                             00065100
             WHEN 0                                                     00065200
                IF GAGW-GPN-NTWK-SET-ID NOT = WS-TO-NTWK-SET-ID-NUM     00065300
                   PERFORM 2510-UPDATE-TO-SETID-PEND THRU 2510-EXIT     00065500
                ELSE                                                    00065600
                   IF GAGW-GPN-NTWK-SET-ID = WS-TO-NTWK-SET-ID-NUM      00065700
                      ADD 1                          TO WS-SUCC-CNTR    00065800
13610                 MOVE DFHBLUE                   TO SECTA(WS-CNTR)  00065810
13610                                                   EFFDTA(WS-CNTR) 00065820
13610                                                   CRPCDA(WS-CNTR) 00065830
                   END-IF                                               00065900
                END-IF                                                  00066000
             WHEN +100                                                  00066100
                PERFORM 2520-INSERT-TO-SETID-PEND    THRU 2520-EXIT     00066200
             WHEN OTHER                                                 00066300
                MOVE -1                              TO SECTL(WS-CNTR)  00066400
                MOVE DFHBMUBF                        TO SECTA(WS-CNTR)  00066500
                STRING 'SELECT ERROR IN PARA 2500. SQLCODE = '          00066600
                       WS-SQLCODE DELIMITED BY SIZE                     00066700
                  INTO G1MSGO                                           00066800
                END-STRING                                              00066900
                PERFORM 9300-SEND-MAP-AND-DATA       THRU 9300-EXIT     00067000
                PERFORM 9999-RETURN-TRANSID          THRU 9999-EXIT     00067100
           END-EVALUATE.                                                00067200
                                                                        00067300
       2500-EXIT.                                                       00067400
           EXIT.                                                        00067500
                                                                        00067600
       2510-UPDATE-TO-SETID-PEND.                                       00067700
           MOVE '004'                TO GCPS-DB2-IO-FUNCTION-CODE.      00067800
           PERFORM 8001-GCPSDAS-DB2-PROCESS THRU 8001-EXIT.             00067900
                                                                        00068000
           MOVE SQLCODE TO WS-SQLCODE.                                  00068100
           EVALUATE SQLCODE                                             00068200
             WHEN 0                                                     00068300
                ADD 1                          TO WS-SUCC-CNTR          00068400
13610           MOVE DFHBLUE                   TO SECTA(WS-CNTR)        00068410
13610                                             EFFDTA(WS-CNTR)       00068411
13610                                             CRPCDA(WS-CNTR)       00068412
             WHEN OTHER                                                 00068500
                MOVE -1                        TO SECTL(WS-CNTR)        00068600
                MOVE DFHBMUBF                  TO SECTA(WS-CNTR)        00068700
                STRING 'UPDATE ERROR IN PARA 2510. SQLCODE = '          00068800
                       WS-SQLCODE DELIMITED BY SIZE                     00068900
                  INTO G1MSGO                                           00069000
                END-STRING                                              00069100
                PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT           00069200
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00069300
           END-EVALUATE.                                                00069400
                                                                        00069500
       2510-EXIT.                                                       00069600
           EXIT.                                                        00069700
                                                                        00069800
       2520-INSERT-TO-SETID-PEND.                                       00069900
           MOVE '005'                   TO GCPS-DB2-IO-FUNCTION-CODE.   00070100
           PERFORM 8001-GCPSDAS-DB2-PROCESS THRU 8001-EXIT.             00070200
                                                                        00070300
           MOVE SQLCODE TO WS-SQLCODE.                                  00070400
           EVALUATE SQLCODE                                             00070500
             WHEN +0                                                    00070600
                ADD 1                          TO WS-SUCC-CNTR          00070700
13610           MOVE DFHBLUE                   TO SECTA(WS-CNTR)        00070710
13610                                             EFFDTA(WS-CNTR)       00070720
13610                                             CRPCDA(WS-CNTR)       00070730
             WHEN OTHER                                                 00070800
                MOVE -1                        TO SECTL(WS-CNTR)        00070900
                MOVE DFHBMUBF                  TO SECTA(WS-CNTR)        00071000
                STRING 'INSERT ERROR IN PARA 2520. SQLCODE = '          00071100
                       WS-SQLCODE DELIMITED BY SIZE                     00071200
                  INTO G1MSGO                                           00071300
                END-STRING                                              00071400
                PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT           00071500
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00071600
           END-EVALUATE.                                                00071700
                                                                        00071800
       2520-EXIT.                                                       00071900
           EXIT.                                                        00072000
                                                                        00072100
       3000-PROCESS-PF10.                                               00072200
           IF EIBAID = DFHPF10                                          00072300
              MOVE '3' TO WS-PARA-SW                                    00072400
              PERFORM VARYING WS-CNTR FROM 1 BY 1                       00072500
                UNTIL WS-CNTR >= 15   OR WS-SPACE-FOUND                 00072600
                  PERFORM 2300-VALIDATE-SCRN-ROW-DATA THRU 2300-EXIT    00072700
                  PERFORM 2400-SCRN-ROW-BASE-CHECK    THRU 2400-EXIT    00072800
                  PERFORM 2500-SCRN-ROW-PEND-CHECK    THRU 2500-EXIT    00072900
              END-PERFORM                                               00073000
           END-IF.                                                      00073100
                                                                        00073200
       3000-EXIT.                                                       00073300
           EXIT.                                                        00073400
                                                                        00073500
       7001-PUT-DFHROUTE-CONT.                                          00073600
      *** PUT DFHROUTE CONTAINER                                        00073700
           MOVE CWABCENV            TO DYN-ENVIR-IND.                   00073800
           MOVE CWAPEARL            TO DYN-PEARL-IND.                   00073900
           MOVE 0                   TO DYN-ERROR-CODE.                  00074000
           MOVE 'GCMI'              TO DYN-TRANSACTION.                 00074100
           MOVE '<EYU9WRAM>'        TO DYN-ERROR-MESSAGE.               00074200
           MOVE WS-GCPSDAS          TO DYN-CALLED-PROGRAM.              00074300
                                                                        00074400
           IF  CWA-PROD-SYSTEM                                          00074500
               MOVE ' '             TO DYN-REL-MAINT-IND                00074600
           ELSE                                                         00074700
               MOVE CWATIND         TO DYN-REL-MAINT-IND                00074800
           END-IF.                                                      00074900
                                                                        00075000
           EXEC CICS PUT                                                00075100
                CONTAINER(GCPS-DFHROUTE-CONT)                           00075200
                CHANNEL(GCPS-IO-CHANNEL)                                00075300
                FROM(COPY-DYNROUTC)                                     00075400
                FLENGTH(LENGTH OF COPY-DYNROUTC)                        00075500
                RESP(WS-DFHRESP)                                        00075600
           END-EXEC.                                                    00075700
                                                                        00075800
           EVALUATE WS-DFHRESP                                          00075900
              WHEN 0                                                    00076000
                 CONTINUE                                               00076100
              WHEN OTHER                                                00076200
                 MOVE WS-GAGWSPGM          TO WS-ERR-MSG10-PGM          00076300
                 MOVE WS-DFHRESP           TO WS-ERR-MSG10-RESP-CODE    00076400
                 MOVE GCPS-DFHROUTE-CONT   TO WS-ERR-MSG10-CONT-NAME    00076500
                 MOVE WS-MESSAGE-TEXT-010  TO G1MSGO                    00076600
                 PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT          00076700
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00076800
           END-EVALUATE.                                                00076900
                                                                        00077000
       7001-EXIT.                                                       00077100
           EXIT.                                                        00077200
                                                                        00077300
       7002-PUT-PARM-CONT.                                              00077400
      *** PUT PARAMETER CONTAINER                                       00077500
           EXEC CICS PUT                                                00077600
                CONTAINER(GCPS-PARM-CONT)                               00077700
                CHANNEL(GCPS-IO-CHANNEL)                                00077800
                FROM(GCPS-DB2-IO-PARMS)                                 00077900
                FLENGTH(LENGTH OF GCPS-DB2-IO-PARMS)                    00078000
                RESP(WS-DFHRESP)                                        00078100
           END-EXEC.                                                    00078200
                                                                        00078300
           EVALUATE WS-DFHRESP                                          00078400
               WHEN 0                                                   00078500
                  CONTINUE                                              00078600
               WHEN OTHER                                               00078700
                  MOVE WS-GAGWSPGM         TO WS-ERR-MSG10-PGM          00078800
                  MOVE WS-DFHRESP          TO WS-ERR-MSG10-RESP-CODE    00078900
                  MOVE GCPS-PARM-CONT      TO WS-ERR-MSG10-CONT-NAME    00079000
                  MOVE WS-MESSAGE-TEXT-010 TO G1MSGO                    00079100
                  PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT         00079200
                  PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT         00079300
           END-EVALUATE.                                                00079400
                                                                        00079500
       7002-EXIT.                                                       00079600
           EXIT.                                                        00079700
                                                                        00079800
       7003-PUT-INPUT-CONT.                                             00079900
           PERFORM 7003A-MOVE-INPUT-CONT-DATA THRU 7003A-EXIT.          00080000
                                                                        00080100
      *** PUT INPUT CONTAINER                                           00080200
           EXEC CICS PUT                                                00080300
                CONTAINER(GCPS-DATA-CONT-IN)                            00080400
                CHANNEL(GCPS-IO-CHANNEL)                                00080500
                FROM(GAGW-IN-STORAGE)                                   00080600
                FLENGTH(LENGTH OF GAGW-IN-STORAGE)                      00080700
                RESP(WS-DFHRESP)                                        00080800
           END-EXEC.                                                    00080900
                                                                        00081000
           EVALUATE WS-DFHRESP                                          00081100
               WHEN 0                                                   00081200
                  CONTINUE                                              00081300
               WHEN OTHER                                               00081400
                  MOVE WS-GAGWSPGM         TO WS-ERR-MSG10-PGM          00081500
                  MOVE WS-DFHRESP          TO WS-ERR-MSG10-RESP-CODE    00081600
                  MOVE GCPS-PARM-CONT      TO WS-ERR-MSG10-CONT-NAME    00081700
                  MOVE WS-MESSAGE-TEXT-010 TO G1MSGO                    00081800
                  PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT         00081900
                  PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT         00082000
           END-EVALUATE.                                                00082100
                                                                        00082200
       7003-EXIT.                                                       00082300
           EXIT.                                                        00082400
                                                                        00082500
       7003A-MOVE-INPUT-CONT-DATA.                                      00082600
           MOVE EFFDTI(WS-CNTR)(1:2)    TO WS-EFFDT-MM.                 00082700
           MOVE EFFDTI(WS-CNTR)(4:2)    TO WS-EFFDT-DD.                 00082800
           MOVE EFFDTI(WS-CNTR)(7:4)    TO WS-EFFDT-CCYY.               00082900
           MOVE WS-GROUP                TO GAGW-IN-GRP-NBR.             00083000
           MOVE WS-SECTION              TO GAGW-IN-SECT-NBR.            00083100
           MOVE WS-AUD-USR              TO GAGW-AUD-USR-TEXT.           00083200
           MOVE LENGTH OF WS-AUD-USR    TO GAGW-AUD-USR-LEN.            00083300
           MOVE WS-REGION               TO GAGW-AUD-PROC-TEXT.          00083400
           MOVE LENGTH OF WS-REGION     TO GAGW-AUD-PROC-LEN.           00083500
           MOVE WS-EFFDT                TO GAGW-IN-REC-EFF-DT.          00083600
           MOVE CRPCDI(WS-CNTR)         TO GAGW-IN-CORP-ENT-CD.         00083700
           MOVE 'I'                     TO GAGW-IN-SRC-REC-STA-CD.      00083800
           MOVE FRLI                    TO GAGW-IN-FAM-RELSHP-LVL-CD.   00083900
                                                                        00084000
       7003A-EXIT.                                                      00084100
           EXIT.                                                        00084200
                                                                        00084300
       7004-GET-PARM-CONT.                                              00084400
      *** GET PARAMETER CONTAINER                                       00084500
           EXEC CICS GET                                                00084600
                CONTAINER(GCPS-PARM-CONT)                               00084700
                CHANNEL(GCPS-IO-CHANNEL)                                00084800
                INTO(GCPS-DB2-IO-PARMS)                                 00084900
                FLENGTH(LENGTH OF GCPS-DB2-IO-PARMS)                    00085000
                RESP(WS-DFHRESP)                                        00085100
           END-EXEC.                                                    00085200
                                                                        00085300
           EVALUATE WS-DFHRESP                                          00085400
               WHEN 0                                                   00085500
                 IF GCPS-DB2-IO-RET-RC NOT = 0                          00085600
                    MOVE WS-GCPSDAS                 TO WS-ERR-MSG12-PGM 00085700
                    MOVE GCPS-DB2-IO-RET-RC-MESSAGE TO WS-ERR-MSG12-TEXT00085800
                    MOVE WS-MESSAGE-TEXT-012        TO G1MSGO           00085900
                    PERFORM 9300-SEND-MAP-AND-DATA  THRU 9300-EXIT      00086000
                    PERFORM 9999-RETURN-TRANSID     THRU 9999-EXIT      00086100
                 ELSE                                                   00086200
      *** MOVE THE SQL CODE THAT YOU GOT FROM GCPSDAS                   00086300
                    MOVE GCPS-DB2-IO-RET-SQLCODE    TO SQLCODE          00086400
                 END-IF                                                 00086500
               WHEN OTHER                                               00086600
                  MOVE WS-GAGWSPGM         TO WS-ERR-MSG11-PGM          00086700
                  MOVE WS-DFHRESP          TO WS-ERR-MSG11-RESP-CODE    00086800
                  MOVE GCPS-PARM-CONT      TO WS-ERR-MSG11-CONT-NAME    00086900
                  MOVE WS-MESSAGE-TEXT-011 TO G1MSGO                    00087000
                 PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT          00087100
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00087200
           END-EVALUATE.                                                00087300
                                                                        00087400
       7004-EXIT.                                                       00087500
           EXIT.                                                        00087600
                                                                        00087700
       7005-GET-INPUT-CONT.                                             00087800
      *** GET INPUT CONTAINER                                           00087900
           EXEC CICS GET                                                00088000
                CONTAINER(GCPS-DATA-CONT-IN)                            00088100
                CHANNEL(GCPS-IO-CHANNEL)                                00088200
                INTO(GAGW-IN-STORAGE)                                   00088300
                FLENGTH(LENGTH OF GAGW-IN-STORAGE)                      00088400
                RESP(WS-DFHRESP)                                        00088500
           END-EXEC.                                                    00088600
                                                                        00088700
           EVALUATE WS-DFHRESP                                          00088800
               WHEN 0                                                   00088900
                 IF GCPS-DB2-IO-RET-RC NOT = 0                          00089000
                    MOVE WS-GAGWSPGM         TO WS-ERR-MSG11-PGM        00089100
                    MOVE WS-DFHRESP          TO WS-ERR-MSG11-RESP-CODE  00089200
                    MOVE GCPS-DATA-CONT-IN   TO WS-ERR-MSG11-CONT-NAME  00089300
                    MOVE WS-MESSAGE-TEXT-011 TO G1MSGO                  00089400
                    PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT       00089500
                    PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT       00089600
                 ELSE                                                   00089700
      *** MOVE THE SQL CODE THAT YOU GOT FROM GCPSDAS                   00089800
                    MOVE GCPS-DB2-IO-RET-SQLCODE    TO SQLCODE          00089900
                 END-IF                                                 00090000
               WHEN OTHER                                               00090100
                  MOVE WS-GAGWSPGM         TO WS-ERR-MSG11-PGM          00090200
                  MOVE WS-DFHRESP          TO WS-ERR-MSG11-RESP-CODE    00090300
                  MOVE GCPS-DATA-CONT-IN   TO WS-ERR-MSG11-CONT-NAME    00090400
                  MOVE WS-MESSAGE-TEXT-011 TO G1MSGO                    00090500
                 PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT          00090600
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00090700
           END-EVALUATE.                                                00090800
                                                                        00090900
       7005-EXIT.                                                       00091000
           EXIT.                                                        00091100
                                                                        00091200
       7006-GET-OUTPUT-CONT.                                            00091300
      *** GET OUTPUT CONTAINER                                          00091400
           EXEC CICS GET                                                00091500
                CONTAINER(GCPS-DATA-CONT-OUT)                           00091600
                CHANNEL(GCPS-IO-CHANNEL)                                00091700
                INTO(GAGW-OUT-STORAGE)                                  00091800
                FLENGTH(LENGTH OF GAGW-OUT-STORAGE)                     00091900
                RESP(WS-DFHRESP)                                        00092000
           END-EXEC.                                                    00092100
                                                                        00092200
           EVALUATE WS-DFHRESP                                          00092300
               WHEN 0                                                   00092400
                 IF GCPS-DB2-IO-RET-RC NOT = 0                          00092500
                    MOVE WS-GAGWSPGM         TO WS-ERR-MSG11-PGM        00092600
                    MOVE WS-DFHRESP          TO WS-ERR-MSG11-RESP-CODE  00092700
                    MOVE GCPS-DATA-CONT-OUT  TO WS-ERR-MSG11-CONT-NAME  00092800
                    MOVE WS-MESSAGE-TEXT-011 TO G1MSGO                  00092900
                    PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT       00093000
                    PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT       00093100
                 ELSE                                                   00093200
      *** MOVE THE SQL CODE THAT RETURNED FROM GCPSDAS                  00093300
                    MOVE GCPS-DB2-IO-RET-SQLCODE TO SQLCODE             00093400
                    IF  GAGW-OUT-REC-END-DT  NOT = (LOW-VALUES OR       00093410
                                                   SPACES)              00093411
                    AND GAGW-OUT-GRP-SECT-NTWK-SET-ID NOT = (ZEROES)    00093420
                       MOVE GAGW-OUT-REC-END-DT                         00093500
                         TO GAGW-IN-REC-END-DT                          00093600
                       MOVE GAGW-OUT-GRP-SECT-NTWK-SET-ID               00093601
                         TO GAGW-IN-GRP-SECT-NTWK-SET-ID                00093602
                    ELSE                                                00093630
                      IF GAGW-OUT-NTWK-SET-ID NOT = (ZEROES)            00093640
                         MOVE GAGW-OUT-NTWK-SET-ID                      00093650
                           TO GAGW-GPN-NTWK-SET-ID                      00093660
                      END-IF                                            00093900
                    END-IF                                              00093901
                 END-IF                                                 00093910
               WHEN OTHER                                               00094000
                  MOVE WS-GAGWSPGM         TO WS-ERR-MSG11-PGM          00094100
                  MOVE WS-DFHRESP          TO WS-ERR-MSG11-RESP-CODE    00094200
                  MOVE GCPS-DATA-CONT-OUT  TO WS-ERR-MSG11-CONT-NAME    00094300
                  MOVE WS-MESSAGE-TEXT-011 TO G1MSGO                    00094400
                 PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT          00094500
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00094600
           END-EVALUATE.                                                00094700
                                                                        00094800
       7006-EXIT.                                                       00094900
           EXIT.                                                        00095000
                                                                        00095100
       8001-GCPSDAS-DB2-PROCESS.                                        00095200
           MOVE WS-GAGWSPGM  TO GCPS-DB2-IO-CALLER-PGM.                 00095300
                                                                        00095400
           PERFORM 7001-PUT-DFHROUTE-CONT THRU 7001-EXIT.               00095500
           PERFORM 7002-PUT-PARM-CONT     THRU 7002-EXIT.               00095600
           PERFORM 7003-PUT-INPUT-CONT    THRU 7003-EXIT.               00095700
                                                                        00095800
           EXEC CICS LINK                                               00095900
              PROGRAM (WS-GCPSDAS)                                      00096000
              CHANNEL (GCPS-IO-CHANNEL)                                 00096100
              RESP    (WS-DFHRESP)                                      00096200
              RESP2   (WS-DFHRESP2)                                     00096300
           END-EXEC.                                                    00096400
                                                                        00096500
           EVALUATE WS-DFHRESP                                          00096600
               WHEN 0                                                   00096700
                  CONTINUE                                              00096800
               WHEN OTHER                                               00096900
                  MOVE WS-GAGWSPGM         TO WS-ERR-MSG13-PGM          00097000
                  MOVE WS-DFHRESP          TO WS-ERR-MSG13-RESP-CODE    00097100
                  MOVE WS-DFHRESP2         TO WS-ERR-MSG13-RESP2-CODE   00097200
                  MOVE WS-MESSAGE-TEXT-013 TO G1MSGO                    00097300
                  PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT         00097400
                  PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT         00097500
           END-EVALUATE.                                                00097600
                                                                        00097700
           PERFORM 7004-GET-PARM-CONT   THRU 7004-EXIT.                 00097800
           PERFORM 7005-GET-INPUT-CONT  THRU 7005-EXIT.                 00097900
                                                                        00098100
       8001-EXIT.                                                       00098200
           EXIT.                                                        00098300
                                                                        00098400
       9000-DISPLAY-SUCC-MSG.                                           00098500
           IF WS-CNTR > 15                                              00098600
              MOVE -1                     TO GRPNUML                    00098700
           ELSE                                                         00098800
              MOVE -1                     TO SECTL(WS-CNTR)             00098900
           END-IF.                                                      00099000
           MOVE SPACES                    TO G1MSGO.                    00099100
           PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT.               00099200
                                                                        00099300
           IF WS-SUCC-CNTR > 0                                          00099400
              MOVE -1                        TO GRPNUML                 00099500
              STRING 'THE ATB WAS SUCCESSFULL FOR '                     00099600
                     WS-SUCC-CNTR ' OUT OF '                            00099700
                     WS-TOT-CNTR  ' GROUPS.' DELIMITED BY SIZE          00099800
                INTO G1MSGO                                             00099900
              END-STRING                                                00100000
              PERFORM 9300-SEND-MAP-AND-DATA THRU 9300-EXIT             00100100
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00100200
           END-IF.                                                      00100300
                                                                        00100400
       9000-EXIT.                                                       00100500
           EXIT.                                                        00100600
                                                                        00100700
       9100-SEND-MAP-ONLY.                                              00100800
           EXEC CICS SEND                                               00100900
                MAP('GAGWI01')                                          00101000
                MAPSET('GAGWSET')                                       00101100
                MAPONLY                                                 00101200
                FREEKB                                                  00101300
                ERASE                                                   00101400
           END-EXEC.                                                    00101500
                                                                        00101600
       9100-EXIT.                                                       00101700
           EXIT.                                                        00101800
                                                                        00101900
       9200-SEND-DATA-ONLY.                                             00101910
           EXEC CICS SEND                                               00101994
                MAP   ('GAGWI01')                                       00101995
                MAPSET('GAGWSET')                                       00101996
                DATAONLY                                                00101997
                FREEKB                                                  00101998
           END-EXEC.                                                    00102001
                                                                        00102002
       9200-EXIT.                                                       00102003
           EXIT.                                                        00102004
                                                                        00102005
       9300-SEND-MAP-AND-DATA.                                          00102010
           EXEC CICS SEND                                               00102100
                MAP('GAGWI01')                                          00102200
                MAPSET('GAGWSET')                                       00102300
                CURSOR                                                  00102400
                ERASE                                                   00102410
                FREEKB                                                  00102500
           END-EXEC.                                                    00102700
                                                                        00102800
       9300-EXIT.                                                       00102900
           EXIT.                                                        00103000
                                                                        00103100
       9400-XCTL-TO-GCPSPGM.                                            00103200
           EXEC CICS XCTL                                               00103300
                PROGRAM('GCPSPGM')                                      00103400
           END-EXEC.                                                    00103500
                                                                        00103600
       9400-EXIT.                                                       00103700
           EXIT.                                                        00103800
                                                                        00103900
       9999-RETURN-TRANSID.                                             00104000
           EXEC CICS RETURN                                             00104100
                TRANSID('GAGW')                                         00104200
                COMMAREA(DFHCOMMAREA)                                   00104300
                LENGTH(LENGTH OF DFHCOMMAREA)                           00104400
           END-EXEC.                                                    00104500
                                                                        00104600
       9999-EXIT.                                                       00104700
           EXIT.                                                        00104800
