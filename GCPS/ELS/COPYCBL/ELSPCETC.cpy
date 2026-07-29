000100******************************************************************        
000200*                                                                *        
000300*    COPYBOOK:   ELSPCETC                                        *        
000400*    DATE:       16-JAN-1987                                     *        
000500*    AUTHOR:     EDWARD G LISS                                   *        
000600*    FUNCTION:   PROVIDER CONTROL ELIMINATION TABLE.             *        
000700*                                                                *        
000800******************************************************************        
000900*                                                                *        
001000*                      MAINTENANCE HISTORY                       *        
001100*                                                                *        
001200*  MOD     DATE     BY  DRPT                ACTION               *        
001300* ----- ----------- --- ----- ---------------------------------- *        
001400* 01.00 16-JAN-1987 EGL       CREATED                            *        
001400* 02.00 14-MAR-1989 NAC       CHANGE VALUE FROM ZERO TO +7 WITHIN*        
      *                             PCT-LOB-LVL-TO-INDEX-DEF FOR LOB   *        
      *                             '06'.                              *        
001500*                                                                *        
001600******************************************************************        
001700                                                                          
001800 01  PROVIDER-CONTROL-TABLE-AREA.                                         
001900*                                                                         
002000*     THIS TABLE IS USED TO TRANSLATE PROVIDER CONTROL CODE TO            
002100*     LINE NUMBERS.                                                       
002200*                                                                         
002300     05  PCT-PC-TO-INDEX-DEF.                                             
002400         10  FILLER                 PICTURE XX         VALUE '0A'.        
002500         10  FILLER                 PICTURE S9(4) COMP VALUE +1.          
002600         10  FILLER                 PICTURE XX         VALUE '0B'.        
002700         10  FILLER                 PICTURE S9(4) COMP VALUE +2.          
002800         10  FILLER                 PICTURE XX         VALUE '0C'.        
002900         10  FILLER                 PICTURE S9(4) COMP VALUE +3.          
003000         10  FILLER                 PICTURE XX         VALUE '0D'.        
003100         10  FILLER                 PICTURE S9(4) COMP VALUE +4.          
003200         10  FILLER                 PICTURE XX         VALUE '0E'.        
003300         10  FILLER                 PICTURE S9(4) COMP VALUE +5.          
003400         10  FILLER                 PICTURE XX         VALUE '0F'.        
003500         10  FILLER                 PICTURE S9(4) COMP VALUE +6.          
003600         10  FILLER                 PICTURE XX         VALUE '0G'.        
003700         10  FILLER                 PICTURE S9(4) COMP VALUE +7.          
003800         10  FILLER                 PICTURE XX         VALUE '0H'.        
003900         10  FILLER                 PICTURE S9(4) COMP VALUE +8.          
004000         10  FILLER                 PICTURE XX         VALUE '0V'.        
004100         10  FILLER                 PICTURE S9(4) COMP VALUE +9.          
004200         10  FILLER                 PICTURE XX         VALUE '00'.        
004300         10  FILLER                 PICTURE S9(4) COMP VALUE +10.         
004400         10  FILLER                 PICTURE XX         VALUE '1A'.        
004500         10  FILLER                 PICTURE S9(4) COMP VALUE +11.         
004600         10  FILLER                 PICTURE XX         VALUE '1B'.        
004700         10  FILLER                 PICTURE S9(4) COMP VALUE +12.         
004800         10  FILLER                 PICTURE XX         VALUE '1C'.        
004900         10  FILLER                 PICTURE S9(4) COMP VALUE +13.         
005000         10  FILLER                 PICTURE XX         VALUE '1D'.        
005100         10  FILLER                 PICTURE S9(4) COMP VALUE +14.         
005200         10  FILLER                 PICTURE XX         VALUE '1E'.        
005300         10  FILLER                 PICTURE S9(4) COMP VALUE +15.         
005400         10  FILLER                 PICTURE XX         VALUE '1F'.        
005500         10  FILLER                 PICTURE S9(4) COMP VALUE +16.         
005600         10  FILLER                 PICTURE XX         VALUE '1G'.        
005700         10  FILLER                 PICTURE S9(4) COMP VALUE +17.         
005800         10  FILLER                 PICTURE XX         VALUE '1H'.        
005900         10  FILLER                 PICTURE S9(4) COMP VALUE +18.         
006000         10  FILLER                 PICTURE XX         VALUE '10'.        
006100         10  FILLER                 PICTURE S9(4) COMP VALUE +19.         
006200         10  FILLER                 PICTURE XX         VALUE '2A'.        
006300         10  FILLER                 PICTURE S9(4) COMP VALUE +20.         
006400         10  FILLER                 PICTURE XX         VALUE '2B'.        
006500         10  FILLER                 PICTURE S9(4) COMP VALUE +21.         
006600         10  FILLER                 PICTURE XX         VALUE '2C'.        
006700         10  FILLER                 PICTURE S9(4) COMP VALUE +22.         
006800         10  FILLER                 PICTURE XX         VALUE '2D'.        
006900         10  FILLER                 PICTURE S9(4) COMP VALUE +23.         
007000         10  FILLER                 PICTURE XX         VALUE '2E'.        
007100         10  FILLER                 PICTURE S9(4) COMP VALUE +24.         
007200         10  FILLER                 PICTURE XX         VALUE '2F'.        
007300         10  FILLER                 PICTURE S9(4) COMP VALUE +25.         
007400         10  FILLER                 PICTURE XX         VALUE '2G'.        
007500         10  FILLER                 PICTURE S9(4) COMP VALUE +26.         
007600         10  FILLER                 PICTURE XX         VALUE '2H'.        
007700         10  FILLER                 PICTURE S9(4) COMP VALUE +27.         
007800         10  FILLER                 PICTURE XX         VALUE '20'.        
007900         10  FILLER                 PICTURE S9(4) COMP VALUE +28.         
008000         10  FILLER                 PICTURE XX         VALUE '3V'.        
008100         10  FILLER                 PICTURE S9(4) COMP VALUE +29.         
008200         10  FILLER                 PICTURE XX         VALUE '4V'.        
008300         10  FILLER                 PICTURE S9(4) COMP VALUE +30.         
008400     05  PCT-PC-TO-INDEX-TABLE      REDEFINES  PCT-PC-TO-INDEX-DEF        
008500                                    OCCURS 30 TIMES                       
008600                                    ASCENDING KEY PCT-PC-CODE             
008700                                    INDEXED PCT-PC-TO-IDX.                
008800         10  PCT-PC-CODE            PICTURE XX.                           
008900         10  PCT-PC-INDEX           PICTURE S9(4) COMP.                   
009000*                                                                         
009100*     THIS TABLE IS USED TO TRANSLATE PROVIDER CONTROL CODE TO            
009200*     LINE NUMBERS.                                                       
009300*                                                                         
009400     05  PCT-LOB-LVL-TO-INDEX-DEF.                                        
009500         10  FILLER.                                                      
009600             15  FILLER             PICTURE XX         VALUE '00'.        
009700             15  FILLER             PICTURE S9(4) COMP VALUE +1.          
009800             15  FILLER             PICTURE S9(4) COMP VALUE +1.          
009900             15  FILLER             PICTURE S9(4) COMP VALUE +1.          
010000             15  FILLER             PICTURE S9(4) COMP VALUE +1.          
010100         10  FILLER.                                                      
010200             15  FILLER             PICTURE XX         VALUE '01'.        
010300             15  FILLER             PICTURE S9(4) COMP VALUE +2.          
010400             15  FILLER             PICTURE S9(4) COMP VALUE +2.          
010500             15  FILLER             PICTURE S9(4) COMP VALUE +2.          
010600             15  FILLER             PICTURE S9(4) COMP VALUE +2.          
010700         10  FILLER.                                                      
010800             15  FILLER             PICTURE XX         VALUE '02'.        
010900             15  FILLER             PICTURE S9(4) COMP VALUE +3.          
011000             15  FILLER             PICTURE S9(4) COMP VALUE +3.          
011100             15  FILLER             PICTURE S9(4) COMP VALUE +3.          
011200             15  FILLER             PICTURE S9(4) COMP VALUE +3.          
011300         10  FILLER.                                                      
011400             15  FILLER             PICTURE XX         VALUE '03'.        
011500             15  FILLER             PICTURE S9(4) COMP VALUE +4.          
011600             15  FILLER             PICTURE S9(4) COMP VALUE +4.          
011700             15  FILLER             PICTURE S9(4) COMP VALUE +4.          
011800             15  FILLER             PICTURE S9(4) COMP VALUE +4.          
011900         10  FILLER.                                                      
012000             15  FILLER             PICTURE XX         VALUE '04'.        
012100             15  FILLER             PICTURE S9(4) COMP VALUE +5.          
012200             15  FILLER             PICTURE S9(4) COMP VALUE +5.          
012300             15  FILLER             PICTURE S9(4) COMP VALUE +5.          
012400             15  FILLER             PICTURE S9(4) COMP VALUE +5.          
012500         10  FILLER.                                                      
012600             15  FILLER             PICTURE XX         VALUE '05'.        
012700             15  FILLER             PICTURE S9(4) COMP VALUE +6.          
012800             15  FILLER             PICTURE S9(4) COMP VALUE +6.          
012900             15  FILLER             PICTURE S9(4) COMP VALUE +6.          
013000             15  FILLER             PICTURE S9(4) COMP VALUE +6.          
013100         10  FILLER.                                                      
013200             15  FILLER             PICTURE XX         VALUE '06'.        
013300             15  FILLER             PICTURE S9(4) COMP VALUE +7.          
013400             15  FILLER             PICTURE S9(4) COMP VALUE +7.          
013500             15  FILLER             PICTURE S9(4) COMP VALUE +7.          
013600             15  FILLER             PICTURE S9(4) COMP VALUE +7.          
013700         10  FILLER.                                                      
013800             15  FILLER             PICTURE XX         VALUE '07'.        
013900             15  FILLER             PICTURE S9(4) COMP VALUE +8.          
014000             15  FILLER             PICTURE S9(4) COMP VALUE ZERO.        
014100             15  FILLER             PICTURE S9(4) COMP VALUE ZERO.        
014200             15  FILLER             PICTURE S9(4) COMP VALUE ZERO.        
014300         10  FILLER.                                                      
014400             15  FILLER             PICTURE XX         VALUE '08'.        
014500             15  FILLER             PICTURE S9(4) COMP VALUE +9.          
014600             15  FILLER             PICTURE S9(4) COMP VALUE ZERO.        
014700             15  FILLER             PICTURE S9(4) COMP VALUE ZERO.        
014800             15  FILLER             PICTURE S9(4) COMP VALUE ZERO.        
014900         10  FILLER.                                                      
015000             15  FILLER             PICTURE XX         VALUE '09'.        
015100             15  FILLER             PICTURE S9(4) COMP VALUE ZERO.        
015200             15  FILLER             PICTURE S9(4) COMP VALUE ZERO.        
015300             15  FILLER             PICTURE S9(4) COMP VALUE ZERO.        
015400             15  FILLER             PICTURE S9(4) COMP VALUE ZERO.        
015500*                                                                         
015600     05  PCT-LOB-LVL-TO-INDEX-TABLE REDEFINES                             
015700                                    PCT-LOB-LVL-TO-INDEX-DEF.     .       
015800         10  PCT-LOB-LVL-TABLE      OCCURS 10 TIMES                       
015900                                    ASCENDING KEY PCT-LOB-LVL-CODE        
016000                                    INDEXED BY PCT-LOB-IDX.               
016100             15  PCT-LOB-LVL-CODE   PICTURE XX.                           
016200             15  PCT-LOB-LVL-INDEX  OCCURS 4  TIMES                       
016300                                    INDEXED BY PCT-LOB-LVL-IDX            
016400                                    PICTURE S9(4) COMP.                   
016500                 88  PCT-LOB-LVL-NOT-USED              VALUE ZERO.        
016600*                                                                         
016700*   THE FOLLOWING AREA DESCRIBES FOR EACH TYPE OF CONTRACT WHICH          
016800*   PROVIDER CONTROLS SHOULD BE REJECTED DURING PROVIDER CONTROL          
016900*   PREPROCESSING.                                                        
017000*                                                                         
017100    05  PCT-ELIMINATION-DEF.                                              
017200        10  PCT-INST-BAS-VALUES.                                          
017300            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
017400            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
017500            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
017600            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
017700            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
017800            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
017900            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
018000            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
018100            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
018200            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
018300            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
018400            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
018500            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
018600            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
018700            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
018800            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
018900            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
019000            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
019100            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
019200            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
019300            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
019400            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
019500            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
019600            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
019700            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
019800            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
019900            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
020000            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
020100            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
020200            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
020300        10  PCT-INST-SUP-VALUES.                                          
020400            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
020500            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
020600            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
020700            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
020800            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
020900            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
021000            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
021100            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
021200            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
021300            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
021400            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
021500            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
021600            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
021700            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
021800            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
021900            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
022000            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
022100            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
022200            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
022300            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
022400            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
022500            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
022600            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
022700            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
022800            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
022900            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
023000            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
023100            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
023200            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
023300            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
023400        10  PCT-PROF-BAS-VALUES.                                          
023500            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
023600            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
023700            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
023800            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
023900            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
024000            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
024100            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
024200            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
024300            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
024400            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
024500            15  FILLER           PICTURE X(10) VALUE '1000000000'.        
024600            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
024700            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
024800            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
024900            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
025000            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
025100            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
025200            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
025300            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
025400            15  FILLER           PICTURE X(10) VALUE '1010000000'.        
025500            15  FILLER           PICTURE X(10) VALUE '1010000000'.        
025600            15  FILLER           PICTURE X(10) VALUE '1010000000'.        
025700            15  FILLER           PICTURE X(10) VALUE '1010000000'.        
025800            15  FILLER           PICTURE X(10) VALUE '1010000000'.        
025900            15  FILLER           PICTURE X(10) VALUE '1010000000'.        
026000            15  FILLER           PICTURE X(10) VALUE '1010000000'.        
026100            15  FILLER           PICTURE X(10) VALUE '1010000000'.        
026200            15  FILLER           PICTURE X(10) VALUE '1010000000'.        
026300            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
026400            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
026500        10  PCT-PROF-SUP-VALUES.                                          
026600            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
026700            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
026800            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
026900            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
027000            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
027100            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
027200            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
027300            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
027400            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
027500            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
027600            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
027700            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
027800            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
027900            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
028000            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
028100            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
028200            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
028300            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
028400            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
028500            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
028600            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
028700            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
028800            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
028900            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
029000            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
029100            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
029200            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
029300            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
029400            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
029500            15  FILLER           PICTURE X(10) VALUE ALL '0'.             
029600    05  PCT-ELIMINATION-TABLE       REDEFINES                             
029700                                    PCT-ELIMINATION-DEF.                  
029800        10  PCT-L-O-B-ITEMS         OCCURS 4 TIMES                        
029900                                    INDEXED BY PCT-LOB-ELIM-IDX.          
030000            15  PCT-PC              OCCURS 30 TIMES                       
030100                                    INDEXED BY PCT-PC-IDX.                
030200                20  PCT-PC-LVL      OCCURS 10 TIMES                       
030300                                    INDEXED BY PCT-PC-LVL-IDX             
030400                                    PICTURE X.                            
030500                    88  PCT-REJ               VALUE '1'.                  
030600                    88  PCT-SEL               VALUE '0'.                  
