000100******************************************************************        
000200*                                                                *        
000210*  @@@@@@                                            @@@@@@      *        
000211*  @@@@@@                                            @@@@@@      *        
000212*  @@@@@@  THIS MEMBER MUST BE USED IN COMPILING     @@@@@@      *        
000213*  @@@@@@  THE BK VERSIONS OF GA1B,C,D,E ON TEST.    @@@@@@      *        
000214*  @@@@@@  PANLIB.                                   @@@@@@      *        
000215*  @@@@@@                                            @@@@@@      *        
000300*                        G A C D A C W A                         *        
000400*                                                                *        
000500*           ALL LEVEL ACCUMULATOR COMMON WORKAREAS               *        
000600*                                                                *        
000700*    COMMON WORKAREAS PASSED TO AND FROM:                        *        
000800*                                                                *        
000900*       1. GA1BPGM --                                            *        
001000*          GA1CPGM  |<---> GASEDIT1 (SCREEN EDIT PROGRAM)        *        
001100*          GA1DPGM  |                                            *        
001200*          GA1EPGM --                                            *        
001300*                                                                *        
001400*                                                                *        
001500*       2. GA1BPGM --                                            *        
001600*          GA1CPGM  |<---> GACDEPGM (CRITICAL DATA ELEMENT       *        
001700*          GA1DPGM  |                 PROCESSING PROGRAM)        *        
001800*          GA1EPGM --                                            *        
001900*                                                                *        
002000*       3. GA1BPGM   <---> GAS1UPD <---> GACDEPGM                *        
002100*                                                                *        
002200*       4. GA1CPGM   <---> GAS2UPD <---> GACDEPGM                *        
002300*                                                                *        
002400*       5. GA1DPGM   <---> GAS3UPD <---> GACDEPGM                *        
002500*                                                                *        
002600*       6. GA1EPGM   <---> GAS4UPD <---> GACDEPGM                *        
002700*                                                                *        
002800*                                                                *        
002900*                                        UPDATED:  5/06/87 JLA   *        
003000*  ADD FIELDS FOR GAS(1,2,3,4)UPD                  9/22/87 JLA   *        
003100*  ADD BLL POINTERS TO ACCOMODATE Q-SET RECORDS    9/23/87 JLA   *        
003110*  REMOVED WQ RECORDS POINTERS                     1/28/88 DES   *      07
003120*  DISCRIMINATE BETWEEN REC 'CDE' & OCCURS 'CDE'   2/02/88 DES   *      09
003130*  ADDED 1U & 2B STATUS COUNTERS                   2/16/88 DES   *      09
003200*  CHANGES IN POINTERS FIELDS TO HANDLE ALL        10/17/90 NE   *        
003200*  ACCUM PROGRAMS CONVERTED TO COBOL/2 AND                       *        
003200*  THE MAX RECORD SIZE IN THE WORKFILE, ISSR 11154               *        
003200*                                                                *        
003300******************************************************************        
003400                                                                          
003500     03  ACWA-COMMON-WORKAREAS.                                           
003600                                                                          
003700*-------- POINTERS PASSED FROM GA1B, GA1C, GA1D, OR GA1E                  
003800                                                                          
003900     05  ACWA-POINTERS.                                                   
004000                                                                          
004100         10  ACWA-WF-ALL-LEVEL-TAB-COMP     PIC S9(8)  COMP SYNC.         
004100         10  ACWA-WF-ALL-LEVEL-TAB-PNTR     REDEFINES                     
004200             ACWA-WF-ALL-LEVEL-TAB-COMP     POINTER.                      
004300                                                                          
004400         10  ACWA-COMM-KEY-COMP             PIC S9(8)  COMP SYNC.         
004400         10  ACWA-COMM-KEY-PNTR             REDEFINES                     
004400             ACWA-COMM-KEY-COMP             POINTER.                      
004300                                                                          
004400         10  ACWA-COPY-TAB-COMP             PIC S9(8)  COMP SYNC.         
004500         10  ACWA-COPY-TAB-PNTR             REDEFINES                     
004500             ACWA-COPY-TAB-COMP             POINTER.                      
004600                                                                          
004700         10  ACWA-WF-INTERNAL-TAB-COMP      PIC S9(8)  COMP SYNC.         
004800         10  ACWA-WF-INTERNAL-TAB-PNTR      REDEFINES                     
004800             ACWA-WF-INTERNAL-TAB-COMP      POINTER.                      
004900                                                                          
005000         10  ACWA-WF-GRP-SPEC-COMP          PIC S9(8)  COMP SYNC.         
005000         10  ACWA-WF-GRP-SPEC-PNTR          REDEFINES                     
005000             ACWA-WF-GRP-SPEC-COMP          POINTER.                      
005100                                                                          
005200         10  ACWA-WF-CONTRACT-COMP          PIC S9(8)  COMP SYNC.         
005300         10  ACWA-WF-CONTRACT-PNTR          REDEFINES                     
005300             ACWA-WF-CONTRACT-COMP          POINTER.                      
005400                                                                          
005500         10  ACWA-WF-BEN-PROV-COMP          PIC S9(8)  COMP SYNC.         
005500         10  ACWA-WF-BEN-PROV-PNTR          REDEFINES                     
005500             ACWA-WF-BEN-PROV-COMP          POINTER.                      
005600                                                                          
005700         10  ACWA-WF-CONTROL-RECORD-COMP    PIC S9(8)  COMP SYNC.         
005700         10  ACWA-WF-CONTROL-RECORD-PNTR    REDEFINES                     
005700             ACWA-WF-CONTROL-RECORD-COMP    POINTER.                      
005800                                                                          
005900         10  ACWA-PR-CONTRACT-COMP          PIC S9(8)  COMP SYNC.         
006000         10  ACWA-PR-CONTRACT-PNTR          REDEFINES                     
006000             ACWA-PR-CONTRACT-COMP          POINTER.                      
006100                                                                          
006200         10  ACWA-PR-GRP-SPEC-COMP          PIC S9(8)  COMP SYNC.         
006200         10  ACWA-PR-GRP-SPEC-PNTR          REDEFINES                     
006200             ACWA-PR-GRP-SPEC-COMP          POINTER.                      
006300                                                                          
006400         10  ACWA-PR-BEN-PROV-COMP          PIC S9(8)  COMP SYNC.         
006400         10  ACWA-PR-BEN-PROV-PNTR          REDEFINES                     
006400             ACWA-PR-BEN-PROV-COMP          POINTER.                      
006500                                                                          
006600         10  ACWA-PR-ALL-LEVEL-TAB-COMP     PIC S9(8)  COMP SYNC.         
006700         10  ACWA-PR-ALL-LEVEL-TAB-PNTR     REDEFINES                     
006700             ACWA-PR-ALL-LEVEL-TAB-COMP     POINTER.                      
006800                                                                          
007800                                                                          
007900         10  ACWA-MAPSET-COMP               PIC S9(8)  COMP SYNC.         
007900         10  ACWA-MAPSET-PNTR               REDEFINES                     
007900             ACWA-MAPSET-COMP               POINTER.                      
008000                                                                          
008100                                                                          
008200*-------- CRITICAL DATA ELEMENT WORK AREAS ----------------------*        
008300                                                                          
008400     05  ACWA-CRITICAL-DATA-ELE-WORK.                                     
008500                                                                          
008600         10  ACWA-CDE-REQUEST-CODE          PIC  X(4).                    
008700           88 ACWA-REQUEST-4500-CDE-PROTECT          VALUE '4500'.        
008800           88 ACWA-REQUEST-4600-CDE-STATUS           VALUE '4600'.        
008900           88 ACWA-REQUEST-4700-CNTL-RECORD          VALUE '4700'.      08
009000           88 ACWA-REQUEST-4800-CNTL-RECORD          VALUE '4800'.      08
009010           88 ACWA-REQUEST-4900-CNTL-RECORD          VALUE '4900'.      10
009100                                                                          
009200         10  ACWA-CDE-RETURN-CODE           PIC  X(2).                    
009300           88 ACWA-CDE-RETURN-CONTINUE               VALUE '00'.          
009400           88 ACWA-CDE-RETURN-DONT-SEND              VALUE '01'.          
009500           88 ACWA-CDE-RETURN-WITH-SEND              VALUE '02'.          
009600                                                                          
009700         10  ACWA-CDE-FIELD-CHANGE-IND      PIC  X(1).                    
009800           88 ACWA-CDE-FIELD-CHANGED                   VALUE 'Y'.         
009900           88 ACWA-CDE-FIELD-NOT-CHANGED               VALUE 'N'.         
009910                                                                          
009920         10  ACWA-CDE-REC-CHANGE-IND        PIC  X(1).                  09
009930           88 ACWA-CDE-REC-CHANGED                     VALUE 'Y'.       09
009940           88 ACWA-CDE-REC-NOT-CHANGED                 VALUE 'N'.       09
010000                                                                          
010100         10  ACWA-CDE-STATUS-CHANGE-IND     PIC  X(2).                    
010200           88 ACWA-CDE-STATUS-NOT-CHANGED              VALUE '  '.        
010300           88 ACWA-CDE-STATUS-CHANGED-TO-2             VALUE '2 '.        
010400           88 ACWA-CDE-STATUS-CHANGED-TO-1U            VALUE '1U'.        
010410                                                                          
010420         10  ACWA-CDE-1U-COUNT              PIC  S999  COMP-3.          13
010430         10  ACWA-CDE-2B-COUNT              PIC  S999  COMP-3.          13
010500                                                                          
010600         10  ACWA-CDE-RESET-WF-IND          PIC  X(1).                    
010700           88 ACWA-CDE-RESET-WF-STATUS                 VALUE 'Y'.         
010800                                                                          
010900         10  ACWA-CDE-INTERNAL-TAB-IND      PIC  X(1).                    
011000           88 ACWA-CDE-INTERNAL-TAB-ADD                VALUE 'A'.         
011100           88 ACWA-CDE-INTERNAL-TAB-DELETE             VALUE 'D'.         
011200                                                                          
011300                                                                          
011400*----------------------------------------------------------------*        
011500                                                                          
011600     05  FILLER.                                                          
011700         10  ACWA-ALT-WORKFILE-KEYS         PIC  X(63).                   
011800                                                                          
011900*        THIS IS PASSED ACCUM OCCURENCE INDEX (I.E. GAA-INDEX)            
012000         10  ACWA-INDEX-1                   PIC  S9(4) COMP.              
012100                                                                          
012200                                                                          
012300*-------- SCREEN EDIT WORKAREAS ---------------------------------*        
012400                                                                          
012500     05  ACWA-ERROR-SW                      PIC X(01).                    
012600         88  ACWA-SCREEN-HAS-NO-ERRORS             VALUE  'N'.            
012700         88  ACWA-SCREEN-HAS-ERRORS                VALUE  'Y'.            
012800                                                                          
012900     05  ACWA-SINGLE-TABULAR-WORK.                                        
013000         10  ACWA-STS-SLOT-NO               PIC  9(7).                    
013100          88 ACWA-STS-SLOT-NO-RANGE                VALUE 7000000          
013200                                                    THRU 7999999.         
013300                                                                          
013400     05  ACWA-DISPLAY-LEN-3                 PIC 999.                      
013500     05  ACWA-DISPLAY-LEN-3-X REDEFINES                                   
013600         ACWA-DISPLAY-LEN-3                 PIC XXX.                      
013700                                                                          
013800     05  ACWA-DISPLAY-LEN-5                 PIC 9(5).                     
013900     05  ACWA-DISPLAY-LEN-5-X REDEFINES                                   
014000         ACWA-DISPLAY-LEN-5                 PIC X(5).                     
014100                                                                          
014200     05  ACWA-DISPLAY-LEN-7                 PIC 9(7).                     
014300     05  ACWA-DISPLAY-LEN-7-X REDEFINES                                   
014400         ACWA-DISPLAY-LEN-7                 PIC X(7).                     
014500                                                                          
014600     05  ACWA-DISPLAY-LEN-9                 PIC 9(9).                     
014700     05  ACWA-DISPLAY-LEN-9-X REDEFINES                                   
014800         ACWA-DISPLAY-LEN-9                 PIC X(9).                     
014900     05  ACWA-DISPLAY-LEN-9-2 REDEFINES                                   
015000         ACWA-DISPLAY-LEN-9                 PIC 9(7)V99.                  
015100                                                                          
015200     05  ACWA-FIELD-CHG-CNT                 PIC 999  COMP-3.              
015300       88  ACWA-NO-CHANGE-FOUND                    VALUE 0.               
015400       88  ACWA-ONLY-1-CHANGED                     VALUE 1.               
015500       88  ACWA-ONLY-2-CHANGED                     VALUE 2.               
015600       88  ACWA-PROD-INTNL-TAB-CHG-ONLY            VALUE 100.             
015610       88  ACWA-PROD-INTERNAL-CHG                  VALUE 100              
015700                                                    THRU 200.             
015800       88  ACWA-INTERNAL-TAB-CHANGE-ONLY           VALUE 900.             
015900       88  ACWA-FIELD-CHG-FOUND                    VALUE 1                
016000                                                    THRU 900.             
016100                                                                          
016200*  *** NEGATIVE ONE AND DECIMAL AREA FOR VALUE LIMIT **                   
016300                                                                          
016400     05  ACWA-VALUE-LIMIT-9                 PIC S9(7)V99.                 
016500     05  ACWA-VALUE-LIMIT-7-2 REDEFINES                                   
016600         ACWA-VALUE-LIMIT-9.                                              
016700         10  ACWA-VALUE-LIMIT-7             PIC 9(7).                     
016800         10  ACWA-VALUE-LIMIT-2             PIC 99.                       
016900     05  ACWA-VALUE-LIMIT-9-9 REDEFINES                                   
017000         ACWA-VALUE-LIMIT-9                 PIC S9(9).                    
017100                                                                          
017200     05  ACWA-VAL-LIM-SCREEN                PIC X(10).                    
017300     05  ACWA-VAL-LIM-SCREEN-7-1-2 REDEFINES                              
017400         ACWA-VAL-LIM-SCREEN.                                             
017500         10  ACWA-VAL-LIM-SCREEN-7          PIC X(7).                     
017600         10  ACWA-VAL-LIM-SCREEN-1          PIC X.                        
017700         10  ACWA-VAL-LIM-SCREEN-2          PIC XX.                       
017800     05  ACWA-VAL-LIM-SCREEN-3-7 REDEFINES                                
017900         ACWA-VAL-LIM-SCREEN.                                             
018000         10  ACWA-VAL-LIM-SCREEN-NEG1-3     PIC X(3).                     
018100         10  ACWA-VAL-LIM-SCREEN-NEG2-7     PIC X(7).                     
018200     05  ACWA-VAL-LIM-SCREEN-7-3 REDEFINES                                
018300         ACWA-VAL-LIM-SCREEN.                                             
018400         10  ACWA-VAL-LIM-SCREEN-NEG2-7     PIC X(7).                     
018500         10  ACWA-VAL-LIM-SCREEN-NEG2-3     PIC X(3).                     
018600     05  ACWA-VAL-LIM-SCREEN-1-9 REDEFINES                                
018700         ACWA-VAL-LIM-SCREEN.                                             
018800         10  ACWA-VAL-LIM-SCREEN-0-1        PIC X(1).                     
018900         10  ACWA-VAL-LIM-SCREEN-2-10       PIC X(9).                     
019000                                                                          
019100     05  ACWA-DISPLAY-VALUE-LIMIT.                                        
019200         10  ACWA-DISPLAY-9                 PIC X(9).                     
019300         10  ACWA-DISPLAY-1                 PIC X.                        
019400                                                                          
019500     05  ACWA-EDIT-VALUE-LIMIT              PIC 9(7).99.                  
019600                                                                          
019700     05  ACWA-BNMXVALI-A                    PIC X(10).                    
019800     05  ACWA-BNMXVALI-N REDEFINES                                        
019900         ACWA-BNMXVALI-A                    PIC 9(10).                    
020000                                                                          
020100*-------- PASSED AREAS FOR GAS{1,2,3,4}UPD PROGRAMS -------------*        
020200                                                                          
020300     05  ACWA-UPD-PASSED-AREA.                                            
020400         10  ACWA-LVL2-B-SW                 PIC X.                        
020500         10  ACWA-LVL2-F-SW                 PIC X.                        
020600         10  ACWA-LVL2-G-SW                 PIC X.                        
020700         10  ACWA-INTR-TAB-PGM-ID           PIC X(8).                     
020800         10  FILLER                         PIC X(9).                     
020900                                                                          
021000     05  GCVI-COMMAREA-LEN                  PIC S9(4)  COMP SYNC.       11
021100                                                                          
021200*<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><        
021300*<><><><>                                                <><><><><        
021400*<><><><>  THIS HARDCODED COMMAREA IS NECESSARY UNTIL    <><><><><        
021500*<><><><>  COBOL-II IS USED ALLOWING IMBEDDED COPYBOOKS  <><><><><        
021600*<><><><>                                                <><><><><        
021700*<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><        
021800*===>   VALIDATION SUBSYSTEM COMMAREA <===*                               
021900**   COPY GCVINTRC.                                                       
022000     05  GCV-FIELD-EDIT-INTERFACE-PARMS.                                  
022100*****************************************************************         
022200*    THIS COPY MEMBER IS USED IN THE GENERIC CONTRACT SYSTEM.   *         
022300*                                                               *         
022400*    IT IS USED IN THE SUPERVISOR SUPPORT SECTION OF THE SYSTEM *         
022500*  TO PASS KEY INFORMATION FOR THE FIELD VALIDATION SUB-SYSTEM. *         
022600*                                                               *         
022700*****************************************************************         
022800**     ADDED   01/07/86 DES                                               
022900**                                                                        
023000**     CHANGED 03-13-86 BY MICHAEL D. DEVLIN                              
023100**     TO ADD GCVI-TABLE-NOT-LOADED VALUE TO THE RETURN-CODE              
023200**                                                                        
023300**                                                                        
023400       10  GCVI-FIELDS-KEY-ID               PIC X(06).                    
023500       10  GCVI-RETURN-CODE                 PIC XX.                       
023600         88  GCVI-VALUE-OK                 VALUE '00'.                    
023700         88  GCVI-VALUE-NOT-FOUND          VALUE '10'.                    
023800         88  GCVI-VALUE-NOT-LOADED         VALUE '20'.                    
023900       10  GCVI-VALUE                       PIC X(10).                    
024000       10  FILLER    REDEFINES  GCVI-VALUE.                               
024100         15  GCVI-VALUE-LEN-1               PIC X.                        
024200         15  FILLER                         PIC X(9).                     
024300       10  FILLER    REDEFINES  GCVI-VALUE.                               
024400         15  GCVI-VALUE-LEN-2               PIC XX.                       
024500         15  FILLER                         PIC X(8).                     
024600       10  FILLER    REDEFINES  GCVI-VALUE.                               
024700         15  GCVI-VALUE-LEN-3               PIC XXX.                      
024800         15  FILLER                         PIC X(7).                     
024900       10  FILLER    REDEFINES  GCVI-VALUE.                               
025000         15  GCVI-VALUE-LEN-5               PIC X(5).                     
025100         15  FILLER                         PIC X(5).                     
025200       10  FILLER    REDEFINES  GCVI-VALUE.                               
025300         15  GCVI-VALUE-LEN-6               PIC X(6).                     
025400         15  FILLER                         PIC X(4).                     
025500       10  FILLER    REDEFINES  GCVI-VALUE.                               
025600         15  GCVI-VALUE-LEN-9               PIC X(9).                     
025700         15  FILLER                         PIC X.                        
025800       10  GCVI-TABLE-SW                    PIC X(01).                    
025900                                                                          
026000*                                                                *        
026100*                                                                *        
026200*  END OF                G A C D A C W A                         *        
026300*                                                                *        
026400******************************************************************        
