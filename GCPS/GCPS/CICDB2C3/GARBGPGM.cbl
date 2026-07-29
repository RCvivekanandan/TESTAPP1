       ID DIVISION.                                                     00000100
       PROGRAM-ID.   GARBGPGM.                                          00000200
       AUTHOR.       VENKATA SONTAM                                     00000300
       DATE-WRITTEN. 12/09/14.                                          00000400
      *                                                                 00000500
      ******************************************************************00000600
      *            M A I N T E N A N C E     L O G                     *00000700
      *----------------------------------------------------------------*00000800
      **-CHG-NUM-*  *-DATE-*  *-WHO-*  *-------DESCRIPTION-------------*00000900
      *  0000000    01/01/15  I331520  INITIAL VERSION.                *00001000
MK0109*  0000001    01/09/15  I333632  FOR FR333165, THE MESSAGE       *00001000
MK0109*                                'THE ATB WAS SUCCESSFULL FOR X  *00001000
MK0109*                                 OUT OF Y' MESSAGE IS OBSOLETE. *00001000
013512*  GCPS00250  01/12/15  I331520  FIX FOR DEFECT# 13512           *00001000
MK0112*                       I333632  CHANGED LINKAGE ELEMENTS AROUND.*00001000
MK0113*             01/13/15  I333632  RESTTING THE FLAGS IF AN INVALID*00001000
MK0113*                                PFKEY IS PRESSED. DON'T WANT THE*00001000
MK0113*                                FLAG RETAINED IN THIS CASE.     *00001000
013579*             01/20/15  I331520  FIX FOR DEFECT# 13579           *00001000
013603*             01/20/15  I331520  FIX FOR DEFECT# 13603           *00001000
013584*             01/20/15  I331520  NETWORK SET ID \
      *                                \
      *                                CORP ENTITY CODE                *00001000
013674*             01/23/15  I331520  FIX FOR DEFECT# 13674           *00001000
013683*             01/23/15  I307151  FIX FOR DEFECT# 13683           *00001000
      ******************************************************************00001100
      *                                                                *00001200
      ******************************************************************00001300
      *    GARBGPGM  - ATB: RANGE BY GROUP & SECTION NUMBERS           *00001400
      *                                                                *00001500
      *    TRANSID: GARB                                               *00001600
      *    MAPSET:  GARBSETC                                           *00001700
      *                                                                *00001900
      *    THE PROGRAM WILL ALLOW USER TO DO MASS INSERT ON PND TABLE  *00002010
      *    FOR A SINGLE NETWORK SET ID WITH A COMBINATION OF RANGE BY  *00002100
      *    GROUP, SECTION AND EFFECTIVE DATE                           *00002110
      *                                                                *00002111
      *    FUNCTION KEYS:                                              *00002120
      *        F3  - RETURN TO PREVIOUS MENU                           *00002130
      *        F9  - CLEAR SCREEN DATA                                 *00002140
      *        F10 - SAVE & EXECUTE                                    *00002150
      ******************************************************************00002800
                                                                        00002900
       ENVIRONMENT DIVISION.                                            00003000
       DATA DIVISION.                                                   00003100
      *                                                                 00003200
       WORKING-STORAGE SECTION.                                         00003300
      *** WORK FIELDS                                                   00003400
                                                                        00003400
       01 WS-DIAGNOSTICS.                                               00004600
           05 WS-BEGIN                PIC X(40)  VALUE                  00004700
           '*** STORAGE AREA FOR GARBGPGM STARTS ***'.                  00004800
           05 WS-TRAN-ID              PIC X(04)  VALUE 'GARB'.          00004900
           05 WS-GARBGPGM             PIC X(08)  VALUE 'GARBGPGM'.      00005000
           05 WS-GCPSDAS              PIC X(8)   VALUE 'GCPSDAS '.      00005010
           05 WS-DFHRESP              PIC S9(08) COMP VALUE +0.                 
           05 WS-DFHRESP2             PIC S9(08) COMP VALUE +0.                 
           05 WS-PARA-ID              PIC X(4)  VALUE SPACES.           00005100
           05 WS-ABEND-CODE           PIC X(04)  VALUE 'GARB'.          00005200
           05 GP-SS                   PIC S9(05) VALUE +0  COMP-3.      00005400
           05 WS-SQLCODE              PIC +++9 VALUE ZERO.              00003510
           05 WS-LOW-VALUES           PIC X(01) VALUE LOW-VALUES.       00003600
           05 WS-EIBTRNID             PIC X(04).                        00003700
              88 WS-VALID-TRAN-ID     VALUE 'GNAH' 'GAGW' 'GAGS' 'GARB'.00003800
              88 WS-TRAN-ID-GARB      VALUE 'GARB'.                     00003900
              88 WS-TRAN-ID-GNAH      VALUE 'GNAH'.                     00004000
           05 WS-FROM-NKSET-IDD       PIC ZZZZ9.                        00004010
           05 WS-FROM-NKSET-ID        PIC X(05).                        00004010
           05 WS-FROM-NKSET-ID-NUM REDEFINES                            00004011
              WS-FROM-NKSET-ID        PIC 9(05).                        00004012
           05 WS-TO-NKSET-IDD         PIC ZZZZ9.                        00004013
           05 WS-TO-NKSET-ID          PIC X(05).                        00004013
           05 WS-TO-NKSET-ID-NUM   REDEFINES                            00004014
              WS-TO-NKSET-ID          PIC 9(05).                        00004015
           05 WS-AUD-USR-TEXT              PIC X(7).                    00004015
           05 WS-RESP-CODE                 PIC S9(08) COMP.             00004015
           05 WS-UNDERSCORE-CNT            PIC 9(02) VALUE ZEROES.      00004015
           05 WS-REGION                    PIC X(04) VALUE SPACES.      00004015
           05 WS-GRP-SECT-NTWK-SET-ID      PIC S9(9) COMP.              00004015
           05 WS-CORP-ENT-CD               PIC X(03).                   00004015
           05 WS-FROM-NTWK-SET-ID          PIC S9(9) COMP.              00004015
           05 WS-TO-NTWK-SET-ID            PIC S9(9) COMP.              00004015
           05 WS-GRP-NBR-BV                PIC X(09).                   00004015
           05 WS-GRP-NBR-EV                PIC X(09).                   00004015
           05 WS-SECT-NBR-BV               PIC X(05).                   00004015
           05 WS-SECT-NBR-EV               PIC X(05).                   00004015
           05 WS-EFF-DT-BV.                                             00004015
              10 WS-EFF-DT-CCYY-BV         PIC 9(4).                            
              10 FILLER                    PIC X(1) VALUE '-'.                  
              10 WS-EFF-DT-MM-BV           PIC 9(2).                            
              10 FILLER                    PIC X(1) VALUE '-'.                  
              10 WS-EFF-DT-DD-BV           PIC 9(2).                            
           05 WS-EFF-DT-EV.                                             00004015
              10 WS-EFF-DT-CCYY-EV         PIC 9(4).                            
              10 FILLER                    PIC X(1) VALUE '-'.                  
              10 WS-EFF-DT-MM-EV           PIC 9(2).                            
              10 FILLER                    PIC X(1) VALUE '-'.                  
              10 WS-EFF-DT-DD-EV           PIC 9(2).                            
           05 WS-REC-EFF-DT-BV             PIC X(10).                   00004015
           05 WS-REC-EFF-DT-EV             PIC X(10).                   00004015
           05 WS-FAM-RELSHP-LVL-CD         PIC X(02).                   00004015
           05 WS-NTWK-LOAD-CSR-CNT         PIC 9(04) VALUE ZEROES.      00004015
           05 WS-SUCCESSFUL                PIC 9(04) VALUE ZEROES.      00004015
           05 WS-PEND-GRP-SECT-NTWK-SET-ID PIC S9(9) USAGE COMP.        00015200
           05 WS-PEND-REC-END-DT           PIC X(10) VALUE SPACES.      00015300
           05 WS-PEND-NTWK-SET-ID          PIC S9(9) USAGE COMP.        00015400
           05 WS-NTWK-LOAD-CSR1-EOF        PIC X(01) VALUE 'N'.         00004015
              88 WS-NTWK-LOAD-CSR1-EOF-YES           VALUE 'Y'.         00004015
              88 WS-NTWK-LOAD-CSR1-EOF-NO            VALUE 'N'.         00004015
           05 WS-NTWK-LOAD-CSR2-EOF        PIC X(01) VALUE 'N'.         00004015
              88 WS-NTWK-LOAD-CSR2-EOF-YES           VALUE 'Y'.         00004015
              88 WS-NTWK-LOAD-CSR2-EOF-NO            VALUE 'Y'.         00004015
013584     05 WS-NSB-FROM-CORP-ENT-CD       PIC X(03).                  00004015
013584     05 WS-NSB-TO-CORP-ENT-CD         PIC X(03).                  00004015
013674     05 WS-GRP-NUMBV-FND-SW            PIC X(01) VALUE 'Y'.       00004015
013674        88 WS-GNBV-FOUND                         VALUE 'Y'.       00004015
013674        88 WS-GNBV-NOT-FOUND                     VALUE 'N'.       00004015
013674     05 WS-GRP-NUMEV-FND-SW            PIC X(01) VALUE 'Y'.       00004015
013674        88 WS-GNEV-FOUND                         VALUE 'Y'.       00004015
013674        88 WS-GNEV-NOT-FOUND                     VALUE 'N'.       00004015
013674     05 WS-SEC-NUMBV-FND-SW            PIC X(01) VALUE 'Y'.       00004015
013674        88 WS-SNBV-FOUND                         VALUE 'Y'.       00004015
013674        88 WS-SNBV-NOT-FOUND                     VALUE 'N'.       00004015
013674     05 WS-SEC-NUMEV-FND-SW            PIC X(01) VALUE 'Y'.       00004015
013674        88 WS-SNEV-FOUND                         VALUE 'Y'.       00004015
013674        88 WS-SNEV-NOT-FOUND                     VALUE 'N'.       00004015
013674     05 WS-EFF-DTEBV-FND-SW            PIC X(01) VALUE 'Y'.       00004015
013674        88 WS-EDBV-FOUND                         VALUE 'Y'.       00004015
013674        88 WS-EDBV-NOT-FOUND                     VALUE 'N'.       00004015
013674     05 WS-EFF-DTEEV-FND-SW            PIC X(01) VALUE 'Y'.       00004015
013674        88 WS-EDEV-FOUND                         VALUE 'Y'.       00004015
013674        88 WS-EDEV-NOT-FOUND                     VALUE 'N'.       00004015
                                                                        00004015
      *** ERROR MESSAGES                                                00004200
       01 WS-MESSAGE-VALUES.                                            00004300
          05 WS-MESSAGE-TEXT-001 PIC X(79) VALUE                        00004400
             'THE GROUP NUMBER IS NOT VALID. PLEASE ENTER A VALID VALUE.00004500
      -      ''.                                                        00004600
          05 WS-MESSAGE-TEXT-002 PIC X(79) VALUE                        00004700
             'THE FROM NTWK SET ID IS NOT VALID. PLEASE ENTER A VALID VA00004800
      -      'LUE.'.                                                    00004900
          05 WS-MESSAGE-TEXT-003 PIC X(79) VALUE                        00005000
             'THE TO NTWK SET ID IS NOT VALID. PLEASE ENTER A VALID VALU00005100
      -      'E.'.                                                      00005200
          05 WS-MESSAGE-TEXT-004 PIC X(79) VALUE                        00005300
             'THE FR VALUE IS NOT VALID. PLEASE ENTER A VALID VALUE TO C00005400
      -      'ONTINUE.'.                                                        
          05 WS-MESSAGE-TEXT-005 PIC X(79) VALUE                        00005500
             'THE SECT NUMBER IS NOT VALID. PLEASE ENTER A VALID VALUE. 00005600
      -      ''.                                                        00005700
          05 WS-MESSAGE-TEXT-006 PIC X(79) VALUE                        00005800
             'THE ATB EFF DATE IS NOT VALID. PLEASE ENTER A VALID VALUE.00005900
      -      ''.                                                        00006000
          05 WS-MESSAGE-TEXT-007 PIC X(79) VALUE                        00006100
             'THE CORP ENTITY CODE VALUE ISN''T VALID. PLEASE ENTER A VA00006200
      -      'LID VALUE TO CONTINUE.'.                                  00006300
          05 WS-MESSAGE-TEXT-008 PIC X(79) VALUE                        00006400
             'THE ENTRY FOR ALL SECTIONS IS NOT A VALID OPTION. PLEASE E00006500
      -      'NTER A VALID VALUE.'.                                     00006600
          05 WS-MESSAGE-TEXT-009 PIC X(79) VALUE                        00006700
             '*** INVALID REQUEST. THE PF KEY USED HAS NO MEANING TO THI00006800
      -      'S PROGRAM ***'.                                           00006900
          05 WS-MESSAGE-TEXT-010 PIC X(79) VALUE                        00006400
             'PLEASE CORRECT THE VALUES HIGHLIGHTED IN RED TO CONTINUE. 00006500
      -      ''.                                                        00006600
          05 WS-MESSAGE-TEXT-011 PIC X(79) VALUE                        00006400
             'WARNING! YOU ARE ABOUT TO MAKE MASS CHANGES. HIT PF5 TO CO00006500
      -      'NTINUE.'.                                                 00006600
          05 WS-MESSAGE-TEXT-012 PIC X(79) VALUE                        00006400
             'WARNING! ARE YOU SURE? PF5 WILL IMPACT A RANGE OF GROUP/SE00006500
      -      'CTIONS.'.                                                 00006600
013584    05 WS-MESSAGE-TEXT-013 PIC X(79) VALUE                        00006400
013584       'BOTH \
013584-       'NTER A VALID VALUE.'.                                    00006600
013579    05 WS-MESSAGE-TEXT-014 PIC X(79) VALUE                        00006400
013579       'NO RECORD FOUND FOR THE ENTERED KEY VALUES. PLEASE ADJUST 00006500
013579-       'THE VALUES IN RED.'.                                     00006600
          05  ED-FREEFORM-ERROR-011.                                            
              10 FILLER                   PIC X(79) VALUE                       
MK0109        'THE ATB WAS SUCCESSFUL.'.                                        
MK0109*       'THE ATB WAS SUCCESSFULL FOR '.                                   
MK0109*       10 WS-CNT1                  PIC ZZZ9.                             
MK0109*       10 FILLER                   PIC X(08) VALUE                       
MK0109*       ' OUT OF '.                                                       
MK0109*       10 WS-CNT2                  PIC ZZZ9.                             
MK0109*       10 FILLER                   PIC X(08) VALUE                       
MK0109*       ' GROUPS.'.                                                       
           05 ED-FREEFORM-ERROR-009.                                            
              10 WS-ERR-MSG09-PGM          PIC X(08).                           
              10 FILLER                    PIC X(01) VALUE SPACE.               
              10 WS-ERR-MSG09-TEXT         PIC X(70).                           
           05 ED-FREEFORM-ERROR-008.                                            
              10 WS-ERR-MSG08-PGM          PIC X(08).                           
              10 WS-ERR-MSG08-TEXT         PIC X(18)                            
                 VALUE ' PUT CONT FAILED: '.                                    
              10 WS-ERR-MSG08-CONT-NAME    PIC X(16).                           
              10 FILLER                    PIC X(12)                            
                 VALUE ' RESP CODE: '.                                          
              10 WS-ERR-MSG08-RESP-CODE    PIC -9(08).                          
           05 ED-FREEFORM-ERROR-010.                                            
              10 WS-ERR-MSG10-PGM          PIC X(08).                           
              10 WS-ERR-MSG10-TEXT1        PIC X(32)                            
                 VALUE ' LINK TO GCPSDAS FAILED IN PARA '.                      
              10 WS-ERR-MSG10-PARA-NUMBER  PIC X(04).                           
              10 WS-ERR-MSG10-TEXT2        PIC X(11)                            
                 VALUE ' RESP CODE '.                                           
              10 WS-ERR-MSG10-RESP-CODE    PIC -9(08).                          
              10 FILLER                    PIC X(01) VALUE '|'.                 
              10 WS-ERR-MSG10-RESP2-CODE   PIC -9(08).                          
       01  WS-CHANNEL-AND-CONTAINERS.                                           
           05  GCPS-IO-CHANNEL      PIC X(16) VALUE 'GCPS_IO_CHANNEL'.          
           05  GCPS-DFHROUTE-CONT   PIC X(16) VALUE 'DFHROUTE'.                 
           05  GCPS-PARM-CONT       PIC X(16) VALUE 'PARM_CONT'.                
           05  GCPS-DATA-CONT-IN    PIC X(16) VALUE 'DATA_CONT_IN'.             
           05  GCPS-DATA-CONT-OUT   PIC X(16) VALUE 'DATA_CONT_OUT'.            
                                                                                
       01 GCPS-DB2-IO-PARMS.                                                    
          05 GCPS-DB2-IO-FUNCTION.                                              
             10 GCPS-DB2-IO-CALLER-PGM       PIC X(08) VALUE SPACES.            
             10 GCPS-DB2-IO-FUNCTION-CODE    PIC X(03) VALUE SPACES.            
          05 GCPS-DB2-IO-RET-SQLCODE         PIC S9(9) COMP-5 VALUE 0.          
          05 GCPS-DB2-IO-RET-RC              PIC S9(4) VALUE 0.                 
          05 GCPS-DB2-IO-RET-RC-MESSAGE      PIC X(80) VALUE SPACES.            
                                                                                
       01 WS-GARB-001-VARIABLES.                                                
           05 WS-FROM-NTWK-SET-ID-001       PIC S9(9) COMP.             00004015
           05 WS-TO-NTWK-SET-ID-001         PIC S9(9) COMP.             00004015
           05 WS-CORP-ENT-CD-001            PIC X(03).                  00004015
           05 WS-FAM-RELSHP-LVL-CD-001      PIC X(02) VALUE SPACES.             
           05 WS-GRP-NBR-BV-001             PIC X(10).                  00004015
           05 WS-GRP-NBR-EV-001             PIC X(10).                  00004015
           05 WS-SECT-NBR-BV-001            PIC X(10).                  00004015
           05 WS-SECT-NBR-EV-001            PIC X(10).                  00004015
           05 WS-REC-EFF-DT-BV-001          PIC X(10).                  00004015
           05 WS-REC-EFF-DT-EV-001          PIC X(10).                  00004015
           05 WS-AUD-PROC-001.                                                  
              49 WS-AUD-PROC-LEN-001           PIC S9(4) COMP.                  
              49 WS-AUD-PROC-TEXT-001          PIC X(30).                       
           05 WS-AUD-USR-001.                                                   
              49 WS-AUD-USR-LEN-001         PIC S9(4) COMP.                     
              49 WS-AUD-USR-TEXT-001        PIC X(50).                          
           05 WS-NTWK-LOAD-CSR-CNT-001      PIC 9(04) VALUE ZEROES.             
           05 WS-SUCCESSFUL-001             PIC 9(04) VALUE ZEROES.             
           05 WS-COUNT-01                   PIC S9(04) COMP.                    
           05 WS-COUNT-02                   PIC S9(04) COMP.                    
           05 WS-COUNT-03                   PIC S9(04) COMP.                    
013584     05 WS-NSB-FROM-CORP-ENT-CD-001   PIC X(03).                  00004015
013584     05 WS-NSB-TO-CORP-ENT-CD-001     PIC X(03).                  00004015
                                                                        00008600
13683 * GNTW002 - COPYBOOK FOR TRANSLATION VALUES                       00008400
13683   COPY GNTW002.                                                   00008500
                                                                        00008600
      ** DYNAMIC ROUTING COPYBOOK                                               
       01  COPY-DYNROUTC.                                                       
       COPY DYNROUTC.                                                           
      *                                                                 00007000
      *** MAP FIELD ATTRIBUTES                                          00007100
       COPY DFHBMSCA.                                                   00007200
                                                                        00007300
      *** ATTENTION KEYS                                                00007400
       COPY DFHAID.                                                     00007500
                                                                        00007600
      *** DATE                                                          00018500
013603 COPY MLDATE01.                                                   00018600
      *** GARB MAIN MENU                                                00007700
       COPY GARBSETC.                                                   00007800
                                                                        00007900
      *** SQL COMMUNICATION AREA                                        00008000
           EXEC SQL                                                     00008100
              INCLUDE SQLCA                                             00008200
           END-EXEC.                                                    00008300
      *** GRP-SECT-NETWORK SET TABLE                                    00008500
           EXEC SQL                                                     00008600
              INCLUDE GSNTWSET                                          00008700
           END-EXEC.                                                    00008800
                                                                        00008900
      *** GRP-SECT-NETWORK SET PEND TABLE                               00009000
           EXEC SQL                                                     00009100
              INCLUDE GSNWSETP                                          00009200
           END-EXEC.                                                    00009300
                                                                        00009400
      *** NETWORK SET TABLE                                             00009500
           EXEC SQL                                                     00009600
              INCLUDE NTWSET                                            00009700
           END-EXEC.                                                    00009800
                                                                        00008400
                                                                        00009900
       LINKAGE SECTION.                                                 00010000
MK0112 01 DFHCOMMAREA.                                                          
       03 WS-PF05-FIRST-ENTRY      PIC X(01).                           00010200
          88 WS-PF05-FIRST-ENTRY-YES         VALUE 'Y'.                 00010200
          88 WS-PF05-FIRST-ENTRY-NO          VALUE 'N'.                 00010200
MK0112 03 WS-COUNT-FLAG            PIC X(01).                           00010200
013512    88 WS-COUNT-GT5-YES                VALUE 'Y'.                 00010200
013512    88 WS-COUNT-GT5-NO                 VALUE 'N'.                 00010200
                                                                                
       COPY CWACOBOL.                                                   00010110
       PROCEDURE DIVISION.                                              00010300
                                                                        00010400
       0000-PROCESS-CONTROL.                                            00010500
                                                                        00010600
           PERFORM 1000-HOUSE-KEEPING    THRU 1000-EXIT.                00010600
           PERFORM 2000-PROCESS-INPUT    THRU 2000-EXIT.                00010700
           PERFORM 9000-DISPLAY-SUCC-MSG THRU 9000-EXIT                 00010700
           PERFORM 9999-RETURN-TRANSID   THRU 9999-EXIT.                00010800
                                                                        00010900
       0000-EXIT.                                                       00011000
           EXIT.                                                        00011100
                                                                        00011200
       1000-HOUSE-KEEPING.                                              00011300
                                                                        00011400
           EXEC CICS HANDLE CONDITION                                   00011400
                MAPFAIL(9300-XCTL-TO-GCPSPGM)                           00011500
           END-EXEC.                                                    00011600
                                                                        00011700
           EXEC CICS                                                    00011710
                ASSIGN USERID(WS-AUD-USR-TEXT)                          00011711
           END-EXEC.                                                    00011712
                                                                        00011713
           EXEC CICS                                                    00011720
                ADDRESS                                                 00011730
                CWA(ADDRESS OF CWACOBOL)                                00011740
                RESP  (WS-RESP-CODE)                                    00011740
           END-EXEC.                                                    00011760
                                                                        00011770
           IF CWA-PROD-SYSTEM                                           00011780
              MOVE 'PROD' TO WS-REGION                                  00011790
           ELSE                                                         00011791
              MOVE 'TEST' TO WS-REGION                                  00011792
           END-IF.                                                      00011793
                                                                        00011800
      *** VALIDATE TRANS. ID                                            00011800
           MOVE EIBTRNID TO WS-EIBTRNID.                                00011900
                                                                        00012000
           IF WS-VALID-TRAN-ID                                          00012100
              IF WS-TRAN-ID-GNAH                                        00012200
                 PERFORM 9100-SEND-MAP-ONLY  THRU 9100-EXIT             00012300
                 PERFORM 9999-RETURN-TRANSID THRU 9999-EXIT             00012300
              END-IF                                                    00012400
           ELSE                                                         00012800
              PERFORM 9300-XCTL-TO-GCPSPGM THRU 9300-EXIT               00012900
           END-IF.                                                      00013000
                                                                        00013100
      *** VALIDATE PFKEYS                                               00013200
           IF EIBAID = DFHENTER OR DFHCLEAR OR                          00013300
                       DFHPF3   OR DFHPF15  OR                          00013400
                       DFHPF5   OR DFHPF17  OR                          00013400
                       DFHPF9   OR DFHPF21  OR                          00013500
                       DFHPF10  OR DFHPF22                              00013600
              CONTINUE                                                  00013700
           ELSE                                                         00013800
              MOVE -1 TO NTKSTFML                                       00013900
      *       MOVE DFHBMUBF TO NTKSTFMA                                 00013900
              MOVE DFHBMUNF TO NTKSTTOA, COETCDOA, FAMRELOA, GRPNUMBA,  00013900
                       GRPNUMEA, SECNUMBA, SECNUMEA, EFFDATBA, EFFDATEA 00013900
              MOVE WS-MESSAGE-TEXT-009 TO ERRMSGO                       00014000
MK0113        MOVE SPACE TO WS-PF05-FIRST-ENTRY                         00017510
MK0113                      WS-COUNT-FLAG                               00017510
              PERFORM 9400-SEND-DATA-ONLY    THRU 9400-EXIT             00014100
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00014100
           END-IF.                                                      00014200
                                                                        00014300
      *** REQUEST MAIN MENU                                             00014400
           IF EIBAID = DFHPF3 OR DFHPF15                                00014500
              EXEC CICS                                                 00014600
                   XCTL PROGRAM('GNAHAPGM')                             00014700
              END-EXEC                                                  00014800
           END-IF.                                                      00014900
                                                                        00015000
      *** CLEAR SCREEN DATA                                             00015100
           IF EIBAID = DFHPF9                                           00015200
              PERFORM 9100-SEND-MAP-ONLY  THRU 9100-EXIT                00015300
              PERFORM 9999-RETURN-TRANSID THRU 9999-EXIT                00015300
           END-IF.                                                      00015400
                                                                        00015500
      *** CLEAR SCREEN                                                  00015600
           IF EIBAID = DFHCLEAR                                         00015700
              EXEC CICS                                                 00015800
                   SEND FROM(WS-LOW-VALUES)                             00015900
                   ERASE                                                00016000
              END-EXEC                                                  00016100
                                                                        00016200
              EXEC CICS                                                 00016300
                   RETURN                                               00016300
              END-EXEC                                                  00016300
           END-IF.                                                      00016400
                                                                        00016500
       1000-EXIT.                                                       00016600
           EXIT.                                                        00016700
                                                                        00016800
       2000-PROCESS-INPUT.                                              00016900
                                                                        00017000
      *** RECEIVE MAP DATA                                              00017100
                                                                        00017200
           EXEC CICS RECEIVE                                            00017200
                MAP('GARBI01')                                          00017210
                MAPSET('GARBSET')                                       00017300
           END-EXEC.                                                    00017400
                                                                        00017410
           IF EIBAID = DFHENTER OR DFHPF10 OR DFHPF5 OR DFHPF17         00017410
              PERFORM 2100-VALIDATE-FROM-SET-ID  THRU 2100-EXIT         00017502
              PERFORM 2200-VALIDATE-TO-SET-ID    THRU 2200-EXIT         00017501
013584        IF WS-NSB-FROM-CORP-ENT-CD = WS-NSB-TO-CORP-ENT-CD                
013584           CONTINUE                                                       
013584        ELSE                                                              
013584           MOVE -1 TO NTKSTFML                                    00017702
013584           MOVE DFHBMUBF TO NTKSTFMA                              00017703
013584           MOVE DFHBMUBF TO NTKSTTOA                              00017703
013584           MOVE WS-MESSAGE-TEXT-013 TO ERRMSGO                    00017704
013584           PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00017705
013584           PERFORM 9999-RETURN-TRANSID THRU 9999-EXIT             00017706
013584        END-IF                                                            
              PERFORM 2300-VALIDATE-CORP-ENTITY  THRU 2300-EXIT         00017501
              PERFORM 2400-VALIDATE-FAMREL-CD    THRU 2400-EXIT                 
013674        SET WS-GNBV-FOUND, WS-GNEV-FOUND TO TRUE                          
              PERFORM 2500-VALIDATE-GRP-NUM-BV   THRU 2500-EXIT         00017501
              PERFORM 2600-VALIDATE-GRP-NUM-EV   THRU 2600-EXIT         00017501
              IF SECNUMAI       = ('_' OR SPACES OR LOW-VALUES)         00017532
                 AND SECNUMBI = ('_____' OR SPACES OR LOW-VALUES)       00017532
                 AND SECNUMEI = ('_____' OR SPACES OR LOW-VALUES)       00017532
                 MOVE -1 TO SECNUMAL                                    00017535
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00017538
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00017539
              END-IF                                                    00017540
              IF SECNUMAI  = 'X' OR '_' OR SPACES OR LOW-VALUES         00017532
                 IF SECNUMAI = '_'                                      00017533
013674             SET WS-SNBV-FOUND , WS-SNEV-FOUND  TO TRUE                   
                   PERFORM 2700-VALIDATE-SEC-NUM-BV   THRU 2700-EXIT    00017501
                   PERFORM 2800-VALIDATE-SEC-NUM-EV   THRU 2800-EXIT    00017501
                 END-IF                                                 00017533
              ELSE                                                      00017534
                MOVE -1 TO SECNUMAL                                     00017535
                MOVE DFHBMUBF TO SECNUMAA                               00017536
                MOVE WS-MESSAGE-TEXT-008 TO ERRMSGO                     00017537
                PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT           00017538
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00017539
              END-IF                                                    00017540
013674        SET WS-EDBV-FOUND , WS-EDEV-FOUND TO TRUE                         
              PERFORM 2900-VALIDATE-EFF-DTE-BV THRU 2900-EXIT           00017501
              PERFORM 3000-VALIDATE-EFF-DTE-EV THRU 3000-EXIT           00017501
013674        IF (WS-GRP-NUMBV-FND-SW = 'N'                             00021509
013674                               AND WS-GRP-NUMEV-FND-SW = 'N') AND 00021509
013674           (WS-SEC-NUMBV-FND-SW = 'N'                             00021509
013674                               AND WS-SEC-NUMEV-FND-SW = 'N') AND 00021509
013674           (WS-EFF-DTEBV-FND-SW = 'N'                             00021509
013674                               AND WS-EFF-DTEEV-FND-SW = 'N')     00021509
013674           MOVE -1 TO GRPNUMBL                                    00021509
013674           MOVE DFHBMUBF TO GRPNUMBA,GRPNUMEA,                    00021509
013674                            SECNUMBA,SECNUMEA,                    00021509
013674                            EFFDATBA, EFFDATEA                    00021509
013674           MOVE WS-MESSAGE-TEXT-010 TO ERRMSGO                    00021509
013674           PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00021509
013674           PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00021509
013674        END-IF                                                    00021509
013674        IF (WS-GRP-NUMBV-FND-SW = 'N'                             00021509
013674                               AND WS-GRP-NUMEV-FND-SW = 'N') AND 00021509
013674           (WS-EFF-DTEBV-FND-SW = 'N'                             00021509
013674                               AND WS-EFF-DTEEV-FND-SW = 'N')     00021509
013674           MOVE -1 TO GRPNUMBL                                    00021509
013674           MOVE DFHBMUBF TO GRPNUMBA,GRPNUMEA,                    00021509
013674                            EFFDATBA, EFFDATEA                    00021509
013674           MOVE WS-MESSAGE-TEXT-010 TO ERRMSGO                    00021509
013674           PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00021509
013674           PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00021509
013674        END-IF                                                    00021509
013674        IF WS-GNBV-NOT-FOUND                                              
013674           MOVE -1 TO GRPNUMBL                                    00021507
013674           MOVE DFHBMUBF TO GRPNUMBA                              00021507
013674           MOVE WS-MESSAGE-TEXT-001 TO ERRMSGO                    00021508
013674           PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00021509
013674           PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00021509
013674        END-IF                                                    00021509
013674        IF WS-GNEV-NOT-FOUND                                              
013674           MOVE -1 TO GRPNUMEL                                    00021507
013674           MOVE DFHBMUBF TO GRPNUMEA                              00021507
013674           MOVE WS-MESSAGE-TEXT-001 TO ERRMSGO                    00021508
013674           PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00021509
013674           PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00021509
013674        END-IF                                                    00021509
013674        IF WS-SNBV-NOT-FOUND                                              
013674          MOVE -1 TO SECNUMBL                                             
013674          MOVE DFHBMUBF TO SECNUMBA                                       
013674          MOVE WS-MESSAGE-TEXT-005 TO ERRMSGO                             
013674          PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT                   
013674          PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT                   
013674        END-IF                                                            
013674        IF WS-SNEV-NOT-FOUND                                              
013674          MOVE -1 TO SECNUMEL                                             
013674          MOVE DFHBMUBF TO SECNUMEA                                       
013674          MOVE WS-MESSAGE-TEXT-005 TO ERRMSGO                             
013674          PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT                   
013674          PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT                   
013674        END-IF                                                            
013674        IF WS-EDBV-NOT-FOUND                                              
013674          MOVE -1 TO EFFDATBL                                     00021507
013674          MOVE DFHBMUBF TO EFFDATBA                               00021507
013674          MOVE WS-MESSAGE-TEXT-006 TO ERRMSGO                     00021508
013674          PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT           00021509
013674          PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00021509
013674        END-IF                                                    00021509
013674        IF WS-EDEV-NOT-FOUND                                              
013674          MOVE -1 TO EFFDATEL                                     00021507
013674          MOVE DFHBMUBF TO EFFDATEA                               00021507
013674          MOVE WS-MESSAGE-TEXT-006 TO ERRMSGO                     00021508
013674          PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT           00021509
013674          PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00021509
013674        END-IF                                                    00021509
           END-IF                                                       00017501
                                                                        00017500
           IF EIBAID = DFHPF10                                          00017510
013512        SET WS-COUNT-GT5-NO TO TRUE                               00017510
              IF SECNUMAI = '_'                                         00017510
                 PERFORM 3100-NTWK-CSR1-CNT        THRU 3100-EXIT       00017510
                 IF WS-COUNT-03 <= 5                                    00017510
                    PERFORM 3200-FETCH-NTWK-LOAD-CSR1 THRU 3200-EXIT    00017510
                 END-IF                                                 00017510
              END-IF                                                    00017510
              IF SECNUMAI = 'X'                                         00017510
                 PERFORM 3300-NTWK-CSR2-CNT        THRU 3300-EXIT       00017510
                 IF WS-COUNT-03 <= 5                                    00017510
                    PERFORM 3600-FETCH-NTWK-LOAD-CSR2 THRU 3600-EXIT    00017510
                 END-IF                                                 00017510
              END-IF                                                    00017510
           END-IF                                                       00017510
                                                                        00017510
013512*    IF EIBAID = DFHPF5                                           00017510
013512     IF (EIBAID = DFHPF5 AND WS-COUNT-GT5-YES)                    00017510
              IF WS-PF05-FIRST-ENTRY-YES                                00021709
                 MOVE -1                     TO GRPNUMBL                00021709
                 SET WS-PF05-FIRST-ENTRY-NO  TO TRUE                    00021509
                 MOVE WS-MESSAGE-TEXT-012 TO ERRMSGO                    00021711
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00021509
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00021509
              ELSE                                                      00021509
                 IF SECNUMAI = '_'                                      00017510
                    PERFORM 3200-FETCH-NTWK-LOAD-CSR1 THRU 3200-EXIT    00017510
                 END-IF                                                 00017510
                 IF SECNUMAI = 'X'                                      00017510
                    PERFORM 3600-FETCH-NTWK-LOAD-CSR2 THRU 3600-EXIT    00017510
                 END-IF                                                 00017510
013512           SET WS-COUNT-GT5-NO TO TRUE                            00017510
              END-IF                                                    00021509
           END-IF.                                                      00017510
                                                                        00017510
       2000-EXIT.                                                       00017511
           EXIT.                                                        00017512
                                                                        00017512
       2100-VALIDATE-FROM-SET-ID.                                       00021460
                                                                        00017410
      *** FROM SET ID NUMERIC CHECK                                     00017410
           INITIALIZE WS-UNDERSCORE-CNT.                                00017575
           IF NTKSTFMI NOT = ( '_____' AND SPACES AND LOW-VALUES)       00017576
              INSPECT NTKSTFMI REPLACING ALL '_' BY SPACES              00017577
              INSPECT FUNCTION REVERSE(NTKSTFMI)                        00017577
                      TALLYING WS-UNDERSCORE-CNT                        00017578
                      FOR LEADING SPACES                                00017579
                                                                        00017580
              ADD 1 TO WS-UNDERSCORE-CNT                                00017581
              MOVE '00000'  TO WS-FROM-NKSET-ID                         00017582
              MOVE NTKSTFMI TO WS-FROM-NKSET-ID(WS-UNDERSCORE-CNT:)     00017583
                                                                        00017590
              IF WS-FROM-NKSET-ID-NUM IS NUMERIC                        00017600
                 MOVE WS-FROM-NKSET-ID-NUM TO WS-FROM-NTWK-SET-ID       00017700
                                              WS-FROM-NKSET-IDD         00017700
              ELSE                                                      00017701
                 MOVE -1 TO NTKSTFML                                    00017702
                 MOVE DFHBMUBF TO NTKSTFMA                              00017703
                 MOVE WS-MESSAGE-TEXT-002 TO ERRMSGO                    00017704
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00017705
                 PERFORM 9999-RETURN-TRANSID THRU 9999-EXIT             00017706
              END-IF                                                    00017710
           ELSE                                                         00017800
              MOVE -1 TO NTKSTFML                                       00017900
              MOVE DFHBMUBF TO NTKSTFMA                                 00018000
              MOVE WS-MESSAGE-TEXT-002 TO ERRMSGO                       00018100
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00018200
              PERFORM 9999-RETURN-TRANSID THRU 9999-EXIT                00018300
           END-IF.                                                      00018400
                                                                        00021470
           MOVE '001'      TO GCPS-DB2-IO-FUNCTION-CODE                         
           PERFORM 5000-MOVE-001-VARIABLES  THRU 5000-EXIT                      
           PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                      
013584     PERFORM 6800-GET-GARB-STORAGE-DATA1 THRU 6800-EXIT                   
           MOVE GCPS-DB2-IO-RET-SQLCODE    TO SQLCODE                           
                                                                        00021499
           MOVE SQLCODE TO WS-SQLCODE.                                  00021500
                                                                        00021501
           EVALUATE SQLCODE                                             00021502
           WHEN +0                                                      00021503
013584*       CONTINUE                                                  00021504
013584      MOVE WS-NSB-FROM-CORP-ENT-CD-001 TO WS-NSB-FROM-CORP-ENT-CD 00021504
           WHEN +100                                                    00021505
              MOVE -1 TO NTKSTFML                                       00021507
              MOVE DFHBMUBF TO NTKSTFMA                                 00021507
              STRING 'THE VALUE '                                               
                     WS-FROM-NKSET-IDD                                          
                     ' FOR \
      -              'LID VALUE.' DELIMITED BY SIZE                             
                INTO ERRMSGO                                                    
              END-STRING                                                        
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           WHEN OTHER                                                   00021510
              MOVE -1 TO NTKSTFML                                       00021511
              STRING 'SELECT ERROR IN PARA 2100. SQLCODE = '            00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                        00021519
       2100-EXIT.                                                       00021520
           EXIT.                                                        00021521
                                                                        00021521
       2200-VALIDATE-TO-SET-ID.                                         00021410
                                                                        00021411
           INITIALIZE WS-UNDERSCORE-CNT.                                00017575
           IF NTKSTTOI NOT = ( '_____' AND SPACES AND LOW-VALUES)       00017576
              INSPECT NTKSTTOI REPLACING ALL '_' BY SPACES              00017577
              INSPECT FUNCTION REVERSE(NTKSTTOI)                        00017577
                      TALLYING WS-UNDERSCORE-CNT                        00017578
                      FOR LEADING SPACES                                00017579
                                                                        00017580
              ADD 1 TO WS-UNDERSCORE-CNT                                00017581
              MOVE '00000'  TO WS-TO-NKSET-ID                           00017582
              MOVE NTKSTTOI TO WS-TO-NKSET-ID(WS-UNDERSCORE-CNT:)       00017583
                                                                        00017590
              IF WS-TO-NKSET-ID-NUM IS NUMERIC                          00017600
                 MOVE WS-TO-NKSET-ID-NUM TO WS-TO-NTWK-SET-ID           00017700
                                            WS-TO-NKSET-IDD             00017700
              ELSE                                                      00017701
                 MOVE -1 TO NTKSTTOL                                    00017702
                 MOVE DFHBMUBF TO NTKSTTOA                              00017703
                 MOVE WS-MESSAGE-TEXT-003 TO ERRMSGO                    00017704
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00017705
                 PERFORM 9999-RETURN-TRANSID THRU 9999-EXIT             00017706
              END-IF                                                    00017710
           ELSE                                                         00017800
              MOVE -1 TO NTKSTTOL                                       00017900
              MOVE DFHBMUBF TO NTKSTTOA                                 00018000
              MOVE WS-MESSAGE-TEXT-003 TO ERRMSGO                       00018100
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00018200
              PERFORM 9999-RETURN-TRANSID THRU 9999-EXIT                00018300
           END-IF.                                                      00018400
                                                                        00021478
           MOVE '002'      TO GCPS-DB2-IO-FUNCTION-CODE                         
           PERFORM 5100-MOVE-001-VARIABLES  THRU 5100-EXIT                      
           PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                      
013584     PERFORM 6800-GET-GARB-STORAGE-DATA1 THRU 6800-EXIT                   
           MOVE GCPS-DB2-IO-RET-SQLCODE    TO SQLCODE                           
           MOVE SQLCODE TO WS-SQLCODE.                                  00021500
                                                                        00021501
           EVALUATE SQLCODE                                             00021502
           WHEN +0                                                      00021503
013584*       CONTINUE                                                  00021504
013584        MOVE WS-NSB-TO-CORP-ENT-CD-001 TO WS-NSB-TO-CORP-ENT-CD   00021504
           WHEN +100                                                    00021505
              MOVE -1 TO NTKSTTOL                                       00021507
              MOVE DFHBMUBF TO NTKSTTOA                                 00021507
              STRING 'THE VALUE '                                               
                     WS-TO-NKSET-IDD                                            
                     ' FOR \
      -              'D VALUE.' DELIMITED BY SIZE                               
                INTO ERRMSGO                                                    
              END-STRING                                                        
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           WHEN OTHER                                                   00021510
              MOVE -1 TO NTKSTFML                                       00021511
              STRING 'SELECT ERROR IN PARA 2200. SQLCODE = '            00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                        00021449
       2200-EXIT.                                                       00021449
           EXIT.                                                        00021450
                                                                        00021451
       2300-VALIDATE-CORP-ENTITY.                                       00017520
                                                                        00017410
      *** CORP ENTITY CODE                                              00017410
13683      MOVE COETCDOI TO TVD-VALIDATE-CORP-ENTITY.                           
13683                                                                           
13683      IF TVD-VALID-CORP-ENTITIES                                           
              MOVE COETCDOI  TO WS-CORP-ENT-CD                          00017800
           ELSE                                                         00017900
              MOVE -1       TO COETCDOL                                 00018000
              MOVE DFHBMUBF TO COETCDOA                                 00018000
              MOVE WS-MESSAGE-TEXT-007 TO ERRMSGO                       00018100
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00017538
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00017539
           END-IF                                                       00018300
                                                                        00021492
           MOVE '003'      TO GCPS-DB2-IO-FUNCTION-CODE                         
           PERFORM 5200-MOVE-001-VARIABLES  THRU 5200-EXIT                      
           PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                      
           MOVE GCPS-DB2-IO-RET-SQLCODE    TO SQLCODE                           
                                                                        00021499
           MOVE SQLCODE TO WS-SQLCODE.                                  00021500
                                                                        00021501
           EVALUATE SQLCODE                                             00021502
           WHEN +0                                                      00021503
              CONTINUE                                                  00021504
           WHEN +100                                                    00021505
013579*       MOVE -1 TO COETCDOL                                       00021507
              MOVE DFHBMUBF TO COETCDOA                                 00021507
013579*       MOVE WS-MESSAGE-TEXT-007 TO ERRMSGO                       00021508
013579        MOVE -1 TO NTKSTFML                                       00021507
013579        MOVE DFHBMUBF TO NTKSTFMA                                 00021507
013579        MOVE WS-MESSAGE-TEXT-014 TO ERRMSGO                       00021508
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           WHEN OTHER                                                   00021510
              MOVE -1 TO COETCDOL                                       00021511
              STRING 'SELECT ERROR IN PARA 2300. SQLCODE = '            00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                        00021449
       2300-EXIT.                                                       00021450
           EXIT.                                                        00021451
                                                                        00021451
       2400-VALIDATE-FAMREL-CD.                                         00017520
                                                                        00017410
      *** FRL                                                           00017410
           IF FAMRELOI NOT = ( '__' AND SPACES AND LOW-VALUES)          00017532
              MOVE FAMRELOI TO WS-FAM-RELSHP-LVL-CD                     00017533
           ELSE                                                         00017534
              MOVE -1 TO FAMRELOL                                       00017535
              MOVE DFHBMUBF TO FAMRELOA                                 00017536
              MOVE WS-MESSAGE-TEXT-004 TO ERRMSGO                       00017537
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00017538
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00017539
           END-IF                                                       00017540
                                                                        00021492
           MOVE '004'      TO GCPS-DB2-IO-FUNCTION-CODE                         
           PERFORM 5300-MOVE-001-VARIABLES  THRU 5300-EXIT                      
           PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                      
           MOVE GCPS-DB2-IO-RET-SQLCODE    TO SQLCODE                           
           MOVE SQLCODE TO WS-SQLCODE.                                  00021500
                                                                        00021501
           EVALUATE SQLCODE                                             00021502
           WHEN +0                                                      00021503
              CONTINUE                                                  00021504
           WHEN +100                                                    00021505
              MOVE -1 TO FAMRELOL                                       00021507
              MOVE DFHBMUBF TO FAMRELOA                                 00021507
              MOVE WS-MESSAGE-TEXT-004 TO ERRMSGO                       00021508
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           WHEN OTHER                                                   00021510
              MOVE -1 TO FAMRELOL                                       00021511
              STRING 'SELECT ERROR IN PARA 2400. SQLCODE = '            00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                        00021449
       2400-EXIT.                                                       00021450
           EXIT.                                                        00021451
                                                                        00021451
       2500-VALIDATE-GRP-NUM-BV.                                        00017520
                                                                        00017600
      *** GROUP NUMBER BEGIN VALUE                                      00017600
           INITIALIZE WS-UNDERSCORE-CNT.                                00017421
           IF GRPNUMBI NOT = ( '_________' AND SPACES AND LOW-VALUES)   00017430
              INSPECT GRPNUMBI REPLACING ALL '_' BY SPACES              00017577
              INSPECT FUNCTION REVERSE(GRPNUMBI)                        00017431
                      TALLYING WS-UNDERSCORE-CNT                        00017432
                      FOR LEADING SPACES                                00017433
                                                                        00017434
              ADD 1 TO WS-UNDERSCORE-CNT                                00017435
              MOVE '000000000' TO WS-GRP-NBR-BV                         00017436
              MOVE GRPNUMBI    TO WS-GRP-NBR-BV(WS-UNDERSCORE-CNT:)     00017437
              MOVE WS-GRP-NBR-BV TO GRPNUMBO                            00017440
           ELSE                                                         00017450
              MOVE -1 TO GRPNUMBL                                       00017460
              MOVE DFHBMUBF TO GRPNUMBA                                 00017470
              MOVE WS-MESSAGE-TEXT-001 TO ERRMSGO                       00017480
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00017490
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00017500
           END-IF                                                       00017501
                                                                        00021492
           MOVE '005'      TO GCPS-DB2-IO-FUNCTION-CODE                         
           PERFORM 5400-MOVE-001-VARIABLES  THRU 5400-EXIT                      
           PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                      
           MOVE GCPS-DB2-IO-RET-SQLCODE    TO SQLCODE                           
                                                                        00021499
           MOVE SQLCODE TO WS-SQLCODE.                                  00021500
                                                                        00021501
           EVALUATE SQLCODE                                             00021502
           WHEN +0                                                      00021503
              CONTINUE                                                  00021504
           WHEN +100                                                    00021505
013674        SET WS-GNBV-NOT-FOUND TO TRUE                             00021507
013674*       MOVE -1 TO GRPNUMBL                                       00021507
013674*       MOVE DFHBMUBF TO GRPNUMBA                                 00021507
013674*       MOVE WS-MESSAGE-TEXT-001 TO ERRMSGO                       00021508
013674*       PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
013674*       PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           WHEN OTHER                                                   00021510
              MOVE -1 TO GRPNUMBL                                       00021511
              STRING 'SELECT ERROR IN PARA 2500. SQLCODE = '            00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                        00017800
       2500-EXIT.                                                       00021200
           EXIT.                                                        00021300
                                                                        00017520
       2600-VALIDATE-GRP-NUM-EV.                                        00017520
                                                                        00017530
      *** GROUP NUMBER END   VALUE                                      00017600
           INITIALIZE WS-UNDERSCORE-CNT.                                00017421
           IF GRPNUMEI NOT = ( '_________' AND SPACES AND LOW-VALUES)   00017430
              INSPECT GRPNUMEI REPLACING ALL '_' BY SPACES              00017577
              INSPECT FUNCTION REVERSE(GRPNUMEI)                        00017431
                      TALLYING WS-UNDERSCORE-CNT                        00017432
                      FOR LEADING SPACES                                00017433
                                                                        00017434
              ADD 1 TO WS-UNDERSCORE-CNT                                00017435
              MOVE '000000000' TO WS-GRP-NBR-EV                         00017436
              MOVE GRPNUMEI    TO WS-GRP-NBR-EV(WS-UNDERSCORE-CNT:)     00017437
              MOVE WS-GRP-NBR-EV TO GRPNUMEO                            00017440
           ELSE                                                         00017450
              MOVE -1 TO GRPNUMEL                                       00017460
              MOVE DFHBMUBF TO GRPNUMEA                                 00017470
              MOVE WS-MESSAGE-TEXT-001 TO ERRMSGO                       00017480
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00017490
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00017500
           END-IF                                                       00017501
                                                                        00017501
           MOVE '006'      TO GCPS-DB2-IO-FUNCTION-CODE                         
           PERFORM 5500-MOVE-001-VARIABLES  THRU 5500-EXIT                      
           PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                      
           MOVE GCPS-DB2-IO-RET-SQLCODE     TO SQLCODE                          
                                                                        00021499
           MOVE SQLCODE TO WS-SQLCODE.                                  00021500
                                                                        00021501
           EVALUATE SQLCODE                                             00021502
           WHEN +0                                                      00021503
              CONTINUE                                                  00021504
           WHEN +100                                                    00021505
013674        SET WS-GNEV-NOT-FOUND TO TRUE                             00021507
013674*       MOVE -1 TO GRPNUMEL                                       00021507
013674*       MOVE DFHBMUBF TO GRPNUMEA                                 00021507
013674*       MOVE WS-MESSAGE-TEXT-001 TO ERRMSGO                       00021508
013674*       PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
013674*       PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           WHEN OTHER                                                   00021510
              MOVE -1 TO GRPNUMEL                                       00021511
              STRING 'SELECT ERROR IN PARA 2600. SQLCODE = '            00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                        00017800
       2600-EXIT.                                                       00021200
           EXIT.                                                        00021300
                                                                        00017520
       2700-VALIDATE-SEC-NUM-BV.                                        00017520
                                                                        00017530
      *** SECTION NUMBER BEGIN VALUE                                    00017600
           INITIALIZE WS-UNDERSCORE-CNT.                                00017421
           IF SECNUMBI NOT = ( '_____' AND SPACES AND LOW-VALUES)       00017430
              INSPECT SECNUMBI REPLACING ALL '_' BY SPACES              00017577
              INSPECT FUNCTION REVERSE(SECNUMBI)                        00017431
                      TALLYING WS-UNDERSCORE-CNT                        00017432
                      FOR LEADING SPACES                                00017433
                                                                        00017434
              ADD 1 TO WS-UNDERSCORE-CNT                                00017435
              MOVE '00000'     TO WS-SECT-NBR-BV                        00017436
              MOVE SECNUMBI    TO WS-SECT-NBR-BV(WS-UNDERSCORE-CNT:)    00017437
              MOVE WS-SECT-NBR-BV TO SECNUMBO                           00017440
           ELSE                                                         00017450
              MOVE -1 TO SECNUMBL                                       00017460
              MOVE DFHBMUBF TO SECNUMBA                                 00017470
              MOVE WS-MESSAGE-TEXT-005 TO ERRMSGO                       00017480
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00017490
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00017500
           END-IF                                                       00017501
                                                                        00017501
           MOVE '007'      TO GCPS-DB2-IO-FUNCTION-CODE                         
           PERFORM 5600-MOVE-001-VARIABLES  THRU 5600-EXIT                      
           PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                      
           MOVE GCPS-DB2-IO-RET-SQLCODE     TO SQLCODE                          
                                                                        00021499
           MOVE SQLCODE TO WS-SQLCODE.                                  00021500
                                                                        00021501
           EVALUATE SQLCODE                                             00021502
           WHEN +0                                                      00021503
              CONTINUE                                                  00021504
           WHEN +100                                                    00021505
013674        SET WS-SNBV-NOT-FOUND TO TRUE                             00021507
013674*       MOVE -1 TO SECNUMBL                                       00021507
013674*       MOVE DFHBMUBF TO SECNUMBA                                 00021507
013674*       MOVE WS-MESSAGE-TEXT-005 TO ERRMSGO                       00021508
013674*       PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
013674*       PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           WHEN OTHER                                                   00021510
              MOVE -1 TO SECNUMBL                                       00021511
              STRING 'SELECT ERROR IN PARA 2700. SQLCODE = '            00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                        00017800
       2700-EXIT.                                                       00021200
           EXIT.                                                        00021300
                                                                        00017520
       2800-VALIDATE-SEC-NUM-EV.                                        00017520
                                                                        00017530
      *** SECTION NUMBER END   VALUE                                    00017600
           INITIALIZE WS-UNDERSCORE-CNT.                                00017421
           IF SECNUMEI NOT = ( '_____' AND SPACES AND LOW-VALUES)       00017430
              INSPECT SECNUMEI REPLACING ALL '_' BY SPACES              00017577
              INSPECT FUNCTION REVERSE(SECNUMEI)                        00017431
                      TALLYING WS-UNDERSCORE-CNT                        00017432
                      FOR LEADING SPACES                                00017433
                                                                        00017434
              ADD 1 TO WS-UNDERSCORE-CNT                                00017435
              MOVE '00000'     TO WS-SECT-NBR-EV                        00017436
              MOVE SECNUMEI    TO WS-SECT-NBR-EV(WS-UNDERSCORE-CNT:)    00017437
              MOVE WS-SECT-NBR-EV TO SECNUMEO                           00017440
           ELSE                                                         00017450
              MOVE -1 TO SECNUMEL                                       00017460
              MOVE DFHBMUBF TO SECNUMEA                                 00017470
              MOVE WS-MESSAGE-TEXT-005 TO ERRMSGO                       00017480
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00017490
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00017500
           END-IF                                                       00017501
                                                                        00021492
           MOVE '008'      TO GCPS-DB2-IO-FUNCTION-CODE                         
           PERFORM 5700-MOVE-001-VARIABLES  THRU 5700-EXIT                      
           PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                      
           MOVE GCPS-DB2-IO-RET-SQLCODE     TO SQLCODE                          
                                                                        00021492
           MOVE SQLCODE TO WS-SQLCODE.                                  00021500
                                                                        00021501
           EVALUATE SQLCODE                                             00021502
           WHEN +0                                                      00021503
              CONTINUE                                                  00021504
           WHEN +100                                                    00021505
013674        SET WS-SNEV-NOT-FOUND TO TRUE                             00021507
013674*       MOVE -1 TO SECNUMEL                                       00021507
013674*       MOVE DFHBMUBF TO SECNUMEA                                 00021507
013674*       MOVE WS-MESSAGE-TEXT-005 TO ERRMSGO                       00021508
013674*       PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
013674*       PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           WHEN OTHER                                                   00021510
              MOVE -1 TO SECNUMEL                                       00021511
              STRING 'SELECT ERROR IN PARA 2800. SQLCODE = '            00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                        00017800
       2800-EXIT.                                                       00021200
           EXIT.                                                        00021300
       2900-VALIDATE-EFF-DTE-BV.                                        00017520
                                                                        00017530
      *** EFFECTIVE DATE BEGINI VALUE **                                00017600
           IF EFFDATBI(1:2) IS NUMERIC AND                              00021701
              EFFDATBI(3:1)= '/'       AND                              00021702
              EFFDATBI(4:2) IS NUMERIC AND                              00021703
              EFFDATBI(6:1)= '/'       AND                              00021704
              EFFDATBI(7:4) IS NUMERIC                                  00021705
                                                                        00021706
              MOVE EFFDATBI(1:2) TO WS-EFF-DT-MM-BV                     00021706
013603                              MLDATE-DATE1(1:2)                   00021706
              MOVE EFFDATBI(4:2) TO WS-EFF-DT-DD-BV                     00021706
013603                              MLDATE-DATE1(3:2)                   00021706
              MOVE EFFDATBI(7:4) TO WS-EFF-DT-CCYY-BV                   00021706
013603                              MLDATE-DATE1(5:4)                   00021706
              MOVE WS-EFF-DT-BV  TO WS-REC-EFF-DT-BV                    00021706
013603        PERFORM 2950-VALIDATE-EFF-DATE  THRU 2950-EXIT            00021706
013603        IF MLDATE-RETURN = '01'                                   00060300
013603          MOVE -1                        TO EFFDATBL              00060500
013603          MOVE DFHBMUBF                  TO EFFDATBA              00060600
013603          MOVE WS-MESSAGE-TEXT-006       TO ERRMSGO               00060700
013603          PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT           00060800
013603          PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00060900
013603        END-IF                                                    00061000
013674*    ELSE                                                         00021707
013674*       MOVE -1       TO EFFDATBL                                 00021709
013674*       MOVE DFHBMUBF TO EFFDATBA                                 00021710
013674*       MOVE WS-MESSAGE-TEXT-006 TO ERRMSGO                       00021711
013674*       PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021712
013674*       PERFORM 9999-RETURN-TRANSID THRU 9999-EXIT                00021713
           END-IF.                                                      00021714
                                                                        00021492
           IF SECNUMAI = 'X'                                            00021492
             MOVE '009'      TO GCPS-DB2-IO-FUNCTION-CODE                       
             PERFORM 5800-MOVE-001-VARIABLES  THRU 5800-EXIT                    
             PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                    
             MOVE GCPS-DB2-IO-RET-SQLCODE     TO SQLCODE                        
           ELSE                                                         00021499
             MOVE '010'      TO GCPS-DB2-IO-FUNCTION-CODE                       
             PERFORM 5900-MOVE-001-VARIABLES  THRU 5900-EXIT                    
             PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                    
             MOVE GCPS-DB2-IO-RET-SQLCODE     TO SQLCODE                        
           END-IF                                                       00021499
                                                                        00021499
           MOVE SQLCODE TO WS-SQLCODE.                                  00021500
                                                                        00021501
           EVALUATE SQLCODE                                             00021502
           WHEN +0                                                      00021503
              CONTINUE                                                  00021504
           WHEN +100                                                    00021505
013674     WHEN -180                                                    00021503
013674     WHEN -181                                                    00021503
013674        SET WS-EDBV-NOT-FOUND TO TRUE                             00021507
013674*       MOVE -1 TO EFFDATBL                                       00021507
013674*       MOVE DFHBMUBF TO EFFDATBA                                 00021507
013674*       MOVE WS-MESSAGE-TEXT-006 TO ERRMSGO                       00021508
013674*       PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
013674*       PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           WHEN OTHER                                                   00021510
              MOVE -1 TO EFFDATBL                                       00021511
              STRING 'SELECT ERROR IN PARA 2900. SQLCODE = '            00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                        00017800
       2900-EXIT.                                                       00021200
           EXIT.                                                        00021300
013603 2950-VALIDATE-EFF-DATE.                                          00058800
013603*** EFFECTIVE DATE VALIDATION                               *     00058900
013603     MOVE 'CNV'                TO MLDATE-FUNC                     00059000
013603     MOVE 'M'                  TO MLDATE-FORM1                    00059100
013603     MOVE 'Y'                  TO MLDATE-FORM2                    00059200
013603                                                                  00059600
013603     EXEC CICS LINK                                               00059700
013603          PROGRAM('MLDATEC')                                      00059800
013603          COMMAREA(MLDATE01)                                      00059900
013603          LENGTH(LENGTH OF MLDATE01)                              00060000
013603     END-EXEC.                                                    00060100
013603                                                                  00060200
013603 2950-EXIT.                                                       00061200
013603     EXIT.                                                        00061300
                                                                        00021522
       3000-VALIDATE-EFF-DTE-EV.                                        00017520
                                                                        00017530
      *** EFFECTIVE DATE BEGINI VALUE **                                00017600
           IF EFFDATEI(1:2) IS NUMERIC AND                              00021701
              EFFDATEI(3:1)= '/'       AND                              00021702
              EFFDATEI(4:2) IS NUMERIC AND                              00021703
              EFFDATEI(6:1)= '/'       AND                              00021704
              EFFDATEI(7:4) IS NUMERIC                                  00021705
                                                                        00021706
              MOVE EFFDATEI(1:2) TO WS-EFF-DT-MM-EV                     00021706
013603                              MLDATE-DATE1(1:2)                   00021706
              MOVE EFFDATEI(4:2) TO WS-EFF-DT-DD-EV                     00021706
013603                              MLDATE-DATE1(3:2)                   00021706
              MOVE EFFDATEI(7:4) TO WS-EFF-DT-CCYY-EV                   00021706
013603                              MLDATE-DATE1(5:4)                   00021706
              MOVE WS-EFF-DT-EV  TO WS-REC-EFF-DT-EV                    00021706
013603        PERFORM 2950-VALIDATE-EFF-DATE  THRU 2950-EXIT            00021706
013603        IF MLDATE-RETURN = '01'                                   00060300
013603          MOVE -1                        TO EFFDATEL              00060500
013603          MOVE DFHBMUBF                  TO EFFDATEA              00060600
013603          MOVE WS-MESSAGE-TEXT-006       TO ERRMSGO               00060700
013603          PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT           00060800
013603          PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00060900
013603        END-IF                                                    00061000
013674*    ELSE                                                         00021707
013674*       MOVE -1       TO EFFDATEL                                 00021709
013674*       MOVE DFHBMUBF TO EFFDATEA                                 00021710
013674*       MOVE WS-MESSAGE-TEXT-006 TO ERRMSGO                       00021711
013674*       PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021712
013674*       PERFORM 9999-RETURN-TRANSID THRU 9999-EXIT                00021713
           END-IF.                                                      00021714
                                                                        00021714
           IF WS-REC-EFF-DT-BV >  WS-REC-EFF-DT-EV                      00021714
              MOVE -1       TO EFFDATBL                                 00021709
              MOVE DFHBMUBF TO EFFDATBA                                 00021710
              MOVE WS-MESSAGE-TEXT-006 TO ERRMSGO                       00021711
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021712
              PERFORM 9999-RETURN-TRANSID THRU 9999-EXIT                00021713
           END-IF                                                       00021492
                                                                        00021492
           IF SECNUMAI = 'X'                                            00021492
             MOVE '011'      TO GCPS-DB2-IO-FUNCTION-CODE                       
             PERFORM 5910-MOVE-001-VARIABLES  THRU 5910-EXIT                    
             PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                    
             MOVE GCPS-DB2-IO-RET-SQLCODE     TO SQLCODE                        
           ELSE                                                         00021499
             MOVE '012'      TO GCPS-DB2-IO-FUNCTION-CODE                       
             PERFORM 5920-MOVE-001-VARIABLES  THRU 5920-EXIT                    
             PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                    
             MOVE GCPS-DB2-IO-RET-SQLCODE     TO SQLCODE                        
           END-IF                                                       00021499
                                                                        00021499
           MOVE SQLCODE TO WS-SQLCODE.                                  00021500
                                                                        00021501
           EVALUATE SQLCODE                                             00021502
           WHEN +0                                                      00021503
              CONTINUE                                                  00021503
           WHEN +100                                                    00021505
013674     WHEN -180                                                    00021503
013674     WHEN -181                                                    00021503
013674        SET WS-EDEV-NOT-FOUND TO TRUE                             00021507
013674*       MOVE -1       TO EFFDATEL                                 00021709
013674*       MOVE DFHBMUBF TO EFFDATEA                                 00021710
013674*       MOVE WS-MESSAGE-TEXT-006 TO ERRMSGO                       00021711
013674*       PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
013674*       PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           WHEN OTHER                                                   00021510
              MOVE -1 TO EFFDATEL                                       00021511
              STRING 'SELECT ERROR IN PARA 3000. SQLCODE = '            00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                        00017800
       3000-EXIT.                                                       00021200
           EXIT.                                                        00021300
       3100-NTWK-CSR1-CNT.                                              00021300
                                                                                
           MOVE '013'      TO GCPS-DB2-IO-FUNCTION-CODE                         
           PERFORM 5930-MOVE-001-VARIABLES  THRU 5930-EXIT                      
           PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                      
           PERFORM 6800-GET-GARB-STORAGE-DATA1 THRU 6800-EXIT                   
           MOVE GCPS-DB2-IO-RET-SQLCODE     TO SQLCODE                          
                                                                        00021500
           MOVE SQLCODE TO WS-SQLCODE                                   00021500
                                                                                
           EVALUATE SQLCODE                                             00021502
            WHEN +0                                                     00021503
            WHEN -811                                                   00021503
              ADD WS-COUNT-01 WS-COUNT-02 TO WS-COUNT-03                00021503
              IF WS-COUNT-03 > 5                                        00021503
                 MOVE -1                     TO GRPNUMBL                00021709
                 SET WS-PF05-FIRST-ENTRY-YES TO TRUE                    00017510
013512           SET WS-COUNT-GT5-YES        TO TRUE                    00017510
                 MOVE WS-MESSAGE-TEXT-011 TO ERRMSGO                    00021711
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00021509
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00021509
              END-IF                                                    00021503
            WHEN OTHER                                                  00021510
              STRING 'FETCH ERROR IN PARA 3100. SQLCODE = '             00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                                
       3100-EXIT.                                                               
           EXIT.                                                                
       3200-FETCH-NTWK-LOAD-CSR1.                                       00021300
                                                                                
           MOVE '014'      TO GCPS-DB2-IO-FUNCTION-CODE                         
           PERFORM 5930-MOVE-001-VARIABLES  THRU 5930-EXIT                      
           PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                      
           PERFORM 6800-GET-GARB-STORAGE-DATA1 THRU 6800-EXIT                   
           MOVE GCPS-DB2-IO-RET-SQLCODE     TO SQLCODE                          
                                                                        00021500
           MOVE SQLCODE TO WS-SQLCODE                                   00021500
                                                                                
           EVALUATE SQLCODE                                             00021502
            WHEN +0                                                     00021503
            CONTINUE                                                    00021503
            WHEN +100                                                   00021503
              IF WS-NTWK-LOAD-CSR-CNT-001 = 0                           00021504
                 MOVE -1       TO GRPNUMBL                              00021709
                 MOVE DFHBMUBF TO GRPNUMBA                              00021710
                 MOVE WS-MESSAGE-TEXT-010 TO ERRMSGO                    00021711
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00021509
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00021509
              END-IF                                                    00021504
            WHEN OTHER                                                  00021510
              STRING 'FETCH ERROR IN PARA 3200. SQLCODE = '             00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                                
       3200-EXIT.                                                               
           EXIT.                                                                
       3300-NTWK-CSR2-CNT.                                              00021300
           MOVE '015'      TO GCPS-DB2-IO-FUNCTION-CODE                         
           PERFORM 5940-MOVE-001-VARIABLES  THRU 5940-EXIT                      
           PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                      
           PERFORM 6800-GET-GARB-STORAGE-DATA1 THRU 6800-EXIT                   
           MOVE GCPS-DB2-IO-RET-SQLCODE     TO SQLCODE                          
                                                                        00021500
           MOVE SQLCODE TO WS-SQLCODE                                   00021500
                                                                                
           EVALUATE SQLCODE                                             00021502
            WHEN +0                                                     00021503
            WHEN -811                                                   00021503
              ADD WS-COUNT-01 WS-COUNT-02 TO WS-COUNT-03                00021503
              IF WS-COUNT-03 > 5                                        00021503
                 MOVE -1                     TO GRPNUMBL                00021709
                 SET WS-PF05-FIRST-ENTRY-YES TO TRUE                    00017510
013512           SET WS-COUNT-GT5-YES        TO TRUE                    00017510
                 MOVE WS-MESSAGE-TEXT-011 TO ERRMSGO                    00021711
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00021509
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT          00021509
              END-IF                                                    00021503
            WHEN OTHER                                                  00021510
              STRING 'FETCH ERROR IN PARA 3300. SQLCODE = '             00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                                
       3300-EXIT.                                                               
           EXIT.                                                                
       3600-FETCH-NTWK-LOAD-CSR2.                                       00021300
           MOVE '016'      TO GCPS-DB2-IO-FUNCTION-CODE                         
           PERFORM 5940-MOVE-001-VARIABLES  THRU 5940-EXIT                      
           PERFORM 6000-GET-DB2-DATA-001    THRU 6000-EXIT                      
           PERFORM 6800-GET-GARB-STORAGE-DATA1 THRU 6800-EXIT                   
           MOVE GCPS-DB2-IO-RET-SQLCODE     TO SQLCODE                          
           MOVE SQLCODE TO WS-SQLCODE                                   00021500
                                                                                
           EVALUATE SQLCODE                                             00021502
            WHEN +0                                                     00021503
            CONTINUE                                                    00021503
            WHEN +100                                                   00021503
              IF WS-NTWK-LOAD-CSR-CNT-001 = 0                           00021504
                 MOVE -1       TO GRPNUMBL                              00021709
                 MOVE DFHBMUBF TO GRPNUMBA                              00021710
                 MOVE WS-MESSAGE-TEXT-010 TO ERRMSGO                    00021711
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT          00021509
                PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT           00021509
              END-IF                                                    00021504
            WHEN OTHER                                                  00021510
              STRING 'FETCH ERROR IN PARA 3600. SQLCODE = '             00021512
                     WS-SQLCODE DELIMITED BY SIZE                       00021513
                INTO ERRMSGO                                            00021515
              END-STRING                                                00021516
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021509
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021509
           END-EVALUATE.                                                00021518
                                                                                
       3600-EXIT.                                                               
           EXIT.                                                                
       5000-MOVE-001-VARIABLES.                                                 
                                                                                
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-FROM-NTWK-SET-ID TO WS-FROM-NTWK-SET-ID-001.                 
                                                                                
       5000-EXIT.                                                               
           EXIT.                                                                
       5100-MOVE-001-VARIABLES.                                                 
                                                                                
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-TO-NTWK-SET-ID   TO WS-TO-NTWK-SET-ID-001.                   
                                                                                
       5100-EXIT.                                                               
           EXIT.                                                                
       5200-MOVE-001-VARIABLES.                                                 
                                                                                
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-FROM-NTWK-SET-ID TO WS-FROM-NTWK-SET-ID-001                  
           MOVE WS-CORP-ENT-CD      TO WS-CORP-ENT-CD-001.                      
                                                                                
       5200-EXIT.                                                               
           EXIT.                                                                
       5300-MOVE-001-VARIABLES.                                                 
                                                                                
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-FROM-NTWK-SET-ID   TO WS-FROM-NTWK-SET-ID-001                
           MOVE WS-CORP-ENT-CD        TO WS-CORP-ENT-CD-001                     
           MOVE WS-FAM-RELSHP-LVL-CD  TO WS-FAM-RELSHP-LVL-CD-001.              
                                                                                
       5300-EXIT.                                                               
           EXIT.                                                                
       5400-MOVE-001-VARIABLES.                                                 
                                                                                
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-FROM-NTWK-SET-ID   TO WS-FROM-NTWK-SET-ID-001                
           MOVE WS-CORP-ENT-CD        TO WS-CORP-ENT-CD-001                     
           MOVE WS-FAM-RELSHP-LVL-CD  TO WS-FAM-RELSHP-LVL-CD-001               
           MOVE WS-GRP-NBR-BV         TO WS-GRP-NBR-BV-001.                     
                                                                                
       5400-EXIT.                                                               
           EXIT.                                                                
       5500-MOVE-001-VARIABLES.                                                 
                                                                                
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-FROM-NTWK-SET-ID   TO WS-FROM-NTWK-SET-ID-001                
           MOVE WS-CORP-ENT-CD        TO WS-CORP-ENT-CD-001                     
           MOVE WS-FAM-RELSHP-LVL-CD  TO WS-FAM-RELSHP-LVL-CD-001               
           MOVE WS-GRP-NBR-EV         TO WS-GRP-NBR-EV-001.                     
                                                                                
       5500-EXIT.                                                               
           EXIT.                                                                
       5600-MOVE-001-VARIABLES.                                                 
                                                                                
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-FROM-NTWK-SET-ID   TO WS-FROM-NTWK-SET-ID-001                
           MOVE WS-CORP-ENT-CD        TO WS-CORP-ENT-CD-001                     
           MOVE WS-FAM-RELSHP-LVL-CD  TO WS-FAM-RELSHP-LVL-CD-001               
           MOVE WS-GRP-NBR-BV         TO WS-GRP-NBR-BV-001                      
           MOVE WS-GRP-NBR-EV         TO WS-GRP-NBR-EV-001                      
           MOVE WS-SECT-NBR-BV        TO WS-SECT-NBR-BV-001.                    
                                                                                
       5600-EXIT.                                                               
           EXIT.                                                                
       5700-MOVE-001-VARIABLES.                                                 
                                                                                
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-FROM-NTWK-SET-ID   TO WS-FROM-NTWK-SET-ID-001                
           MOVE WS-CORP-ENT-CD        TO WS-CORP-ENT-CD-001                     
           MOVE WS-FAM-RELSHP-LVL-CD  TO WS-FAM-RELSHP-LVL-CD-001               
           MOVE WS-GRP-NBR-BV         TO WS-GRP-NBR-BV-001                      
           MOVE WS-GRP-NBR-EV         TO WS-GRP-NBR-EV-001                      
           MOVE WS-SECT-NBR-EV        TO WS-SECT-NBR-EV-001.                    
                                                                                
       5700-EXIT.                                                               
           EXIT.                                                                
       5800-MOVE-001-VARIABLES.                                                 
                                                                                
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-FROM-NTWK-SET-ID   TO WS-FROM-NTWK-SET-ID-001                
           MOVE WS-CORP-ENT-CD        TO WS-CORP-ENT-CD-001                     
           MOVE WS-FAM-RELSHP-LVL-CD  TO WS-FAM-RELSHP-LVL-CD-001               
           MOVE WS-GRP-NBR-BV         TO WS-GRP-NBR-BV-001                      
           MOVE WS-GRP-NBR-EV         TO WS-GRP-NBR-EV-001                      
           MOVE WS-REC-EFF-DT-BV      TO WS-REC-EFF-DT-BV-001.                  
                                                                                
       5800-EXIT.                                                               
           EXIT.                                                                
       5900-MOVE-001-VARIABLES.                                                 
                                                                                
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-FROM-NTWK-SET-ID   TO WS-FROM-NTWK-SET-ID-001                
           MOVE WS-CORP-ENT-CD        TO WS-CORP-ENT-CD-001                     
           MOVE WS-FAM-RELSHP-LVL-CD  TO WS-FAM-RELSHP-LVL-CD-001               
           MOVE WS-GRP-NBR-BV         TO WS-GRP-NBR-BV-001                      
           MOVE WS-GRP-NBR-EV         TO WS-GRP-NBR-EV-001                      
           MOVE WS-SECT-NBR-BV        TO WS-SECT-NBR-BV-001                     
           MOVE WS-SECT-NBR-EV        TO WS-SECT-NBR-EV-001                     
           MOVE WS-REC-EFF-DT-BV      TO WS-REC-EFF-DT-BV-001.                  
                                                                                
       5900-EXIT.                                                               
           EXIT.                                                                
       5910-MOVE-001-VARIABLES.                                                 
                                                                                
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-FROM-NTWK-SET-ID   TO WS-FROM-NTWK-SET-ID-001                
           MOVE WS-CORP-ENT-CD        TO WS-CORP-ENT-CD-001                     
           MOVE WS-FAM-RELSHP-LVL-CD  TO WS-FAM-RELSHP-LVL-CD-001               
           MOVE WS-GRP-NBR-BV         TO WS-GRP-NBR-BV-001                      
           MOVE WS-GRP-NBR-EV         TO WS-GRP-NBR-EV-001                      
           MOVE WS-REC-EFF-DT-EV      TO WS-REC-EFF-DT-EV-001.                  
                                                                                
       5910-EXIT.                                                               
           EXIT.                                                                
       5920-MOVE-001-VARIABLES.                                                 
                                                                                
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-FROM-NTWK-SET-ID   TO WS-FROM-NTWK-SET-ID-001                
           MOVE WS-CORP-ENT-CD        TO WS-CORP-ENT-CD-001                     
           MOVE WS-FAM-RELSHP-LVL-CD  TO WS-FAM-RELSHP-LVL-CD-001               
           MOVE WS-GRP-NBR-BV         TO WS-GRP-NBR-BV-001                      
           MOVE WS-GRP-NBR-EV         TO WS-GRP-NBR-EV-001                      
           MOVE WS-SECT-NBR-BV        TO WS-SECT-NBR-BV-001                     
           MOVE WS-SECT-NBR-EV        TO WS-SECT-NBR-EV-001                     
           MOVE WS-REC-EFF-DT-EV      TO WS-REC-EFF-DT-EV-001.                  
                                                                                
       5920-EXIT.                                                               
           EXIT.                                                                
       5930-MOVE-001-VARIABLES.                                                 
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-FROM-NTWK-SET-ID   TO WS-FROM-NTWK-SET-ID-001                
           MOVE WS-TO-NTWK-SET-ID     TO WS-TO-NTWK-SET-ID-001                  
           MOVE WS-CORP-ENT-CD        TO WS-CORP-ENT-CD-001                     
           MOVE WS-FAM-RELSHP-LVL-CD  TO WS-FAM-RELSHP-LVL-CD-001               
           MOVE WS-GRP-NBR-BV         TO WS-GRP-NBR-BV-001                      
           MOVE WS-GRP-NBR-EV         TO WS-GRP-NBR-EV-001                      
           MOVE WS-SECT-NBR-BV        TO WS-SECT-NBR-BV-001                     
           MOVE WS-SECT-NBR-EV        TO WS-SECT-NBR-EV-001                     
           MOVE WS-REC-EFF-DT-BV      TO WS-REC-EFF-DT-BV-001                   
           MOVE WS-REC-EFF-DT-EV      TO WS-REC-EFF-DT-EV-001                   
           MOVE 4                     TO WS-AUD-PROC-LEN-001            00021510
           MOVE WS-REGION             TO WS-AUD-PROC-TEXT-001           00021510
           MOVE 7                     TO WS-AUD-USR-LEN-001             00021513
           MOVE WS-AUD-USR-TEXT       TO WS-AUD-USR-TEXT-001.           00021513
                                                                                
                                                                                
       5930-EXIT.                                                               
           EXIT.                                                                
       5940-MOVE-001-VARIABLES.                                                 
                                                                                
           INITIALIZE WS-GARB-001-VARIABLES                                     
           MOVE WS-FROM-NTWK-SET-ID   TO WS-FROM-NTWK-SET-ID-001                
           MOVE WS-TO-NTWK-SET-ID     TO WS-TO-NTWK-SET-ID-001                  
           MOVE WS-CORP-ENT-CD        TO WS-CORP-ENT-CD-001                     
           MOVE WS-FAM-RELSHP-LVL-CD  TO WS-FAM-RELSHP-LVL-CD-001               
           MOVE WS-GRP-NBR-BV         TO WS-GRP-NBR-BV-001                      
           MOVE WS-GRP-NBR-EV         TO WS-GRP-NBR-EV-001                      
           MOVE WS-REC-EFF-DT-BV      TO WS-REC-EFF-DT-BV-001                   
           MOVE WS-REC-EFF-DT-EV      TO WS-REC-EFF-DT-EV-001                   
           MOVE 4                     TO WS-AUD-PROC-LEN-001            00021510
           MOVE WS-REGION             TO WS-AUD-PROC-TEXT-001           00021510
           MOVE 7                     TO WS-AUD-USR-LEN-001             00021513
           MOVE WS-AUD-USR-TEXT       TO WS-AUD-USR-TEXT-001.           00021513
                                                                                
       5940-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6000-GET-DB2-DATA-001.                                                   
                                                                                
           MOVE '6000'      TO WS-PARA-ID                                       
           MOVE WS-GARBGPGM TO GCPS-DB2-IO-CALLER-PGM                           
                                                                                
           PERFORM 6100-PUT-DFHROUTE-CONT      THRU 6100-EXIT                   
           PERFORM 6200-PUT-PARM-CONT          THRU 6200-EXIT                   
           PERFORM 6400-PUT-GARB-STORAGE-DATA1 THRU 6400-EXIT                   
                                                                                
           EXEC CICS LINK                                                       
              PROGRAM (WS-GCPSDAS)                                              
              CHANNEL (GCPS-IO-CHANNEL)                                         
              RESP    (WS-DFHRESP)                                              
              RESP2   (WS-DFHRESP2)                                             
           END-EXEC                                                             
                                                                                
           EVALUATE WS-DFHRESP                                                  
              WHEN 0                                                            
                 CONTINUE                                                       
              WHEN OTHER                                                        
                 MOVE WS-PARA-ID             TO WS-ERR-MSG10-PARA-NUMBER        
                 MOVE WS-GARBGPGM            TO WS-ERR-MSG10-PGM                
                 MOVE WS-DFHRESP             TO WS-ERR-MSG10-RESP-CODE          
                 MOVE WS-DFHRESP2            TO WS-ERR-MSG10-RESP2-CODE         
                 MOVE ED-FREEFORM-ERROR-010  TO ERRMSGO                         
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT                  
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT                  
           END-EVALUATE                                                         
                                                                                
           PERFORM 6600-GET-PARM-CONT          THRU 6600-EXIT.                  
                                                                                
       6000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6100-PUT-DFHROUTE-CONT.                                                  
                                                                                
           MOVE '6100'  TO  WS-PARA-ID                                          
           MOVE CWABCENV            TO DYN-ENVIR-IND                            
           IF  CWA-PROD-SYSTEM                                                  
               MOVE ' '             TO DYN-REL-MAINT-IND                        
           ELSE                                                                 
               MOVE CWATIND         TO DYN-REL-MAINT-IND                        
           END-IF.                                                              
           MOVE 0                   TO DYN-ERROR-CODE                           
           MOVE '<EYU9WRAM>'        TO DYN-ERROR-MESSAGE                        
           MOVE WS-GCPSDAS          TO DYN-CALLED-PROGRAM                       
           MOVE CWAPEARL            TO DYN-PEARL-IND                            
           MOVE 'GCMI'              TO DYN-TRANSACTION                          
                                                                                
           EXEC CICS PUT                                                        
              CONTAINER (GCPS-DFHROUTE-CONT)                                    
              CHANNEL   (GCPS-IO-CHANNEL)                                       
              FROM      (COPY-DYNROUTC)                                         
              FLENGTH   (LENGTH OF COPY-DYNROUTC)                               
              RESP      (WS-DFHRESP)                                            
           END-EXEC.                                                            
                                                                                
           EVALUATE WS-DFHRESP                                                  
              WHEN 0                                                            
                 CONTINUE                                                       
              WHEN OTHER                                                        
                 MOVE WS-GARBGPGM           TO WS-ERR-MSG08-PGM                 
                 MOVE WS-DFHRESP            TO WS-ERR-MSG08-RESP-CODE           
                 MOVE GCPS-DFHROUTE-CONT    TO WS-ERR-MSG08-CONT-NAME           
                 MOVE ED-FREEFORM-ERROR-008 TO ERRMSGO                          
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT                  
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT                  
           END-EVALUATE.                                                        
                                                                                
       6100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6200-PUT-PARM-CONT.                                                      
                                                                                
           MOVE '6200'  TO  WS-PARA-ID.                                         
                                                                                
           EXEC CICS PUT                                                        
              CONTAINER (GCPS-PARM-CONT)                                        
              CHANNEL   (GCPS-IO-CHANNEL)                                       
              FROM      (GCPS-DB2-IO-PARMS)                                     
              FLENGTH   (LENGTH OF GCPS-DB2-IO-PARMS)                           
              RESP      (WS-DFHRESP)                                            
           END-EXEC.                                                            
                                                                                
           EVALUATE WS-DFHRESP                                                  
              WHEN 0                                                            
                 CONTINUE                                                       
              WHEN OTHER                                                        
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT                  
           END-EVALUATE.                                                        
                                                                                
       6200-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6400-PUT-GARB-STORAGE-DATA1.                                             
                                                                                
           MOVE '6400'  TO  WS-PARA-ID                                          
                                                                                
           EXEC CICS PUT                                                        
              CONTAINER (GCPS-DATA-CONT-IN)                                     
              CHANNEL   (GCPS-IO-CHANNEL)                                       
              FROM      (WS-GARB-001-VARIABLES)                                 
              FLENGTH   (LENGTH OF WS-GARB-001-VARIABLES)                       
              RESP      (WS-DFHRESP)                                            
           END-EXEC.                                                            
                                                                                
           EVALUATE WS-DFHRESP                                                  
              WHEN 0                                                            
                 CONTINUE                                                       
              WHEN OTHER                                                        
                 MOVE WS-GARBGPGM           TO WS-ERR-MSG08-PGM                 
                 MOVE WS-DFHRESP            TO WS-ERR-MSG08-RESP-CODE           
                 MOVE GCPS-DATA-CONT-IN     TO WS-ERR-MSG08-CONT-NAME           
                 MOVE ED-FREEFORM-ERROR-008 TO ERRMSGO                          
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT                  
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT                  
           END-EVALUATE.                                                        
                                                                                
       6400-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6600-GET-PARM-CONT.                                                      
                                                                                
           MOVE '6600'  TO  WS-PARA-ID                                          
                                                                                
           EXEC CICS GET                                                        
              CONTAINER (GCPS-PARM-CONT)                                        
              CHANNEL   (GCPS-IO-CHANNEL)                                       
              INTO      (GCPS-DB2-IO-PARMS)                                     
              FLENGTH   (LENGTH OF GCPS-DB2-IO-PARMS)                           
              RESP      (WS-DFHRESP)                                            
           END-EXEC.                                                            
                                                                                
           EVALUATE WS-DFHRESP                                                  
              WHEN 0                                                            
                 IF GCPS-DB2-IO-RET-RC NOT = 0                                  
                    MOVE WS-GCPSDAS            TO WS-ERR-MSG09-PGM              
                    MOVE GCPS-DB2-IO-RET-RC-MESSAGE                             
                      TO WS-ERR-MSG09-TEXT                                      
                    MOVE ED-FREEFORM-ERROR-009 TO ERRMSGO                       
                    PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT               
                    PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT               
                 ELSE                                                           
                    MOVE GCPS-DB2-IO-RET-SQLCODE TO SQLCODE                     
                 END-IF                                                         
              WHEN OTHER                                                        
                 MOVE WS-GARBGPGM           TO WS-ERR-MSG08-PGM                 
                 MOVE WS-DFHRESP            TO WS-ERR-MSG08-RESP-CODE           
                 MOVE GCPS-PARM-CONT        TO WS-ERR-MSG08-CONT-NAME           
                 MOVE ED-FREEFORM-ERROR-008 TO ERRMSGO                          
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT                  
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT                  
           END-EVALUATE.                                                        
                                                                                
       6600-EXIT.                                                               
           EXIT.                                                                
                                                                                
       6800-GET-GARB-STORAGE-DATA1.                                             
                                                                                
           MOVE '6800'  TO  WS-PARA-ID                                          
                                                                                
           EXEC CICS GET                                                        
              CONTAINER (GCPS-DATA-CONT-OUT)                                    
              CHANNEL   (GCPS-IO-CHANNEL)                                       
              INTO      (WS-GARB-001-VARIABLES)                                 
              FLENGTH   (LENGTH OF WS-GARB-001-VARIABLES)                       
              RESP      (WS-DFHRESP)                                            
           END-EXEC.                                                            
                                                                                
           EVALUATE WS-DFHRESP                                                  
              WHEN 0                                                            
                 CONTINUE                                                       
              WHEN OTHER                                                        
                 MOVE WS-GARBGPGM           TO WS-ERR-MSG08-PGM                 
                 MOVE WS-DFHRESP            TO WS-ERR-MSG08-RESP-CODE           
                 MOVE GCPS-DATA-CONT-OUT    TO WS-ERR-MSG08-CONT-NAME           
                 MOVE ED-FREEFORM-ERROR-008 TO ERRMSGO                          
                 PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT                  
                 PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT                  
           END-EVALUATE.                                                        
                                                                                
       6800-EXIT.                                                               
           EXIT.                                                                
       9100-SEND-MAP-ONLY.                                              00021530
                                                                        00021600
           EXEC CICS SEND                                               00021600
                MAP('GARBI01')                                          00021610
                MAPSET('GARBSET')                                       00021700
                ERASE                                                   00021800
                FREEKB                                                  00021900
                MAPONLY                                                 00022000
           END-EXEC.                                                    00022100
                                                                        00022400
       9100-EXIT.                                                       00022500
           EXIT.                                                        00022600
                                                                        00022700
       9200-SEND-MAP-AND-DATA.                                          00022800
                                                                        00022900
           EXEC CICS SEND                                               00022900
                MAP('GARBI01')                                          00022910
                MAPSET('GARBSET')                                       00023000
                CURSOR                                                  00023200
                FREEKB                                                  00023300
                WAIT                                                    00023300
                ERASE                                                   00023300
           END-EXEC.                                                    00023400
                                                                        00024200
       9200-EXIT.                                                       00024300
           EXIT.                                                        00024400
                                                                        00024500
       9300-XCTL-TO-GCPSPGM.                                            00024600
                                                                        00024700
           EXEC CICS XCTL                                               00024700
                PROGRAM('GCPSPGM')                                      00024710
           END-EXEC.                                                    00024800
                                                                        00024900
       9300-EXIT.                                                       00025000
           EXIT.                                                        00025100
       9400-SEND-DATA-ONLY.                                             00022800
                                                                        00022900
           EXEC CICS SEND                                               00022900
                MAP('GARBI01')                                          00022910
                MAPSET('GARBSET')                                       00023000
                FROM  (GARBI01O)                                        00023000
                DATAONLY                                                00023200
                CURSOR                                                  00023200
                WAIT                                                    00023200
                FREEKB                                                  00023300
           END-EXEC.                                                    00023400
                                                                        00024200
       9400-EXIT.                                                       00024300
           EXIT.                                                        00024400
                                                                        00025200
       9000-DISPLAY-SUCC-MSG.                                           00025300
                                                                        00021507
           MOVE -1       TO NTKSTFML                                    00021507
           MOVE SPACES   TO ERRMSGO                                     00021508
           PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT                00021509
                                                                        00026100
           IF WS-SUCCESSFUL-001 > 0                                     00021921
MK0109*       MOVE WS-SUCCESSFUL-001         TO WS-CNT1                 00021926
MK0109*       MOVE WS-NTWK-LOAD-CSR-CNT-001  TO WS-CNT2                 00021926
              MOVE ED-FREEFORM-ERROR-011     TO ERRMSGO                 00021926
              PERFORM 9200-SEND-MAP-AND-DATA THRU 9200-EXIT             00021928
              PERFORM 9999-RETURN-TRANSID    THRU 9999-EXIT             00021929
           END-IF.                                                      00021930
                                                                        00025500
       9000-EXIT.                                                       00025600
           EXIT.                                                        00025700
       9999-RETURN-TRANSID.                                             00025300
                                                                        00026100
           EXEC CICS RETURN                                             00026100
                TRANSID('GARB')                                         00026200
                COMMAREA(DFHCOMMAREA)                                   00026300
                LENGTH(LENGTH OF DFHCOMMAREA)                           00026400
           END-EXEC.                                                    00026500
                                                                        00025500
       9999-EXIT.                                                       00025600
           EXIT.                                                        00025700
