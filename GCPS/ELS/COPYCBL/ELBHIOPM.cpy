      ******************************************************************        
      *                          ELBHIOPM                              *        
      *                                                                *        
      *    THIS IS THE ENGLISH LANGUAGE I/O PARM AREA FOR ELBIOPGM     *        
      *    WHICH IS A BATCH I/O MODULE USED TO ACCESS ALL ENGLISH      *        
      *    LANGUAGE FILES THROUGH THE USE OF COBLVSAM I/O MODULE.      *        
      *                                                                *        
      ******************************************************************        
      **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*        
      *                                                                *        
      *    XXXX    02/27/86  NAC  ORIGINAL MODULE                      *        
      *                                                                *        
      ******************************************************************        
           05  ENGLISH-LANGUAGE-BATCH-IOPARM.                                   
                                                                                
               10  ELBHIO-FILE-ID                   PIC XX.                     
                   88  RECORD-LIST-FILE                 VALUE 'RL'.             
                   88  DATA-ELEMENT-FILE                VALUE 'DE'.             
                   88  CODE-VALUE-FILE                  VALUE 'CV'.             
                   88  ENGLISH-NAME-FILE                VALUE 'EN'.             
                   88  COBOL-XREF-NAME-FILE             VALUE 'CN'.             
                                                                                
               10  ELBHIO-RETURN-CODE               PIC XX.                     
                   88  ELBHIO-GOOD-RETURN               VALUE '00'.             
                   88  ELBHIO-INVALID-FILE-ID           VALUE '01'.             
                   88  ELBHIO-INVALID-REQUEST-TYPE      VALUE '02'.             
                   88  ELBHIO-COBLVSAM-ERROR            VALUE '03'.             
                   88  ELBHIO-ENGLISH-NAME-REQD         VALUE '04'.             
                   88  ELBHIO-REQUEST-TYPE-DENIED       VALUE '05'.             
                   88  ELBHIO-ENGLISH-NAME-DUPL         VALUE '06'.             
                   88  ELBHIO-COBOL-XREF-DUPL           VALUE '07'.             
                                                                                
               10  ELBHIO-PARM-ONE.                                             
                   15  ELBHIO-RESERVED              PIC 9(8)   COMP.            
                   15  ELBHIO-RESERVED-X  REDEFINES  ELBHIO-RESERVED.           
      *                                                                         
      *    THE FIRST BYTE IS ASSUMED TO CONTAIN THE FUNCTION CODE               
      *    AND IS ALSO USED BY COBLVSAM TO STORE A RETURN CODE.                 
      *    REFER TO COBLVSAM LEVEL003 HANDOUT FOR FURTHER INFO.                 
      *                                                                         
                       20  ELBHIO-REQUEST-TYPE      PIC X.                      
                           88  ELBHIO-OPEN              VALUE 'O'.              
                           88  ELBHIO-CLOSE             VALUE 'C'.              
                           88  ELBHIO-T-CLOSE           VALUE 'T'.              
                           88  ELBHIO-SEQUENTIAL-GET    VALUE 'G'.              
                           88  ELBHIO-SEQUENTIAL-ADD    VALUE 'A'.              
                           88  ELBHIO-SEQUENTIAL-LOAD   VALUE 'L'.              
                           88  ELBHIO-READ              VALUE 'R'.              
                           88  ELBHIO-WRITE             VALUE 'W'.              
                           88  ELBHIO-UPDATE            VALUE 'U'.              
                           88  ELBHIO-DELETE            VALUE 'D'.              
                           88  ELBHIO-ERASE             VALUE 'E'.              
                           88  ELBHIO-INSERT            VALUE 'I'.              
                           88  ELBHIO-POINT             VALUE 'P'.              
                           88  ELBHIO-SET               VALUE 'S'.              
                                                                                
               10  ELBHIO-SET-VALUE                 PIC 9(8)   COMP.            
                                                                                
               10  ELBHIO-PARM-TWO.                                             
                   15  ELBHIO-RDW.                                              
      *                                                                         
      *    THE FIRST 4 BYTES OF THIS AREA MUST CONTAIN A STANDARD               
      *    RDW WHICH INDICATES THE LENGTH OF THE SEARCHKEY. THE                 
      *    LENGTH SPECIFIED MUST BE THE LENGTH OF THE SEARCHKEY PLUS            
      *    4.                                                                   
      *                                                                         
                       20  ELBHIO-RECORD-LENGTH     PIC 9(4)    COMP.           
      *                                                                         
      *    CONTAINS THE VSAM FEEDBACK CODE.                                     
      *                                                                         
                       20  ELBHIO-FEEDBACK          PIC 9(4)    COMP.           
                                                                                
                   15  ELBHIO-RECORD-AREA           PIC X(2000).                
                                                                                
      *  RECORD LIST AREA                                                       
                   15  ELBHIO-ELPRL  REDEFINES  ELBHIO-RECORD-AREA.             
                       20  ELBHIO-ELPRL-KEY         PIC X(08).                  
                       20  FILLER                   PIC X(53).                  
      *  DATA ELEMENT AREA                                                      
                   15  ELBHIO-ELPDE  REDEFINES  ELBHIO-RECORD-AREA.             
                       20  ELBHIO-ELPDE-KEY.                                    
                         25  ELBHIO-DE-RECORD-PREFIX PIC X(8).                  
                         25  ELBHIO-DE-ELEMENT-NBR PIC S999V99 COMP-3.          
                       20  ELBHIO-ELPDE-AREA.                                   
                         25  FILLER                PIC X(8).                    
                         25  ELBHIO-ELEMENT-NAME   PIC X(75).                   
                         25  FILLER                PIC X(38).                   
                         25  ELBHIO-COBOL-NAME     PIC X(30).                   
                         25  FILLER                PIC X(8).                    
                         25  ELBHIO-ELPDE-NBR-DESC-LINES                        
                                                   PIC S9(3)   COMP-3.          
                         25  ELBHIO-ELPDE-DESC-LINES                            
                                                   PIC X(79)                    
                                                   OCCURS 11 TIMES.             
      *  CODE VALUE AREA                                                        
                   15  ELBHIO-ELPCV  REDEFINES  ELBHIO-RECORD-AREA.             
                       20  ELBHIO-ELPCV-KEY.                                    
                         25  ELBHIO-CV-RECORD-PREFIX PIC X(8).                  
                         25  ELBHIO-CV-ELEMENT-NBR PIC S999V99 COMP-3.          
                         25  ELBHIO-CV-CODE-VALUE  PIC X(10).                   
                         25  ELBHIO-CV-CODE-DESC-SEQ PIC 99.                    
                       20  ELBHIO-ELPCV-AREA.                                   
                         25  FILLER                PIC X(51).                   
                         25  ELBHIO-ELPCV-NBR-DESC-LINES                        
                                                   PIC S9(3)   COMP-3.          
                         25  ELBHIO-ELPCV-DESC-LINES                            
                                                   PIC X(79)                    
                                                   OCCURS 12 TIMES.             
      *  ENGLISH NAME AREA                                                      
                   15  ELBHIO-ELPEN  REDEFINES  ELBHIO-RECORD-AREA.             
                       20  ELBHIO-ELPEN-KEY.                                    
                         25  ELBHIO-EN-RECORD-PREFIX PIC X(8).                  
                         25  ELBHIO-EN-ELEMENT-NAME  PIC X(75).                 
                         25  ELBHIO-EN-ELEMENT-NBR   PIC S9(3)V9(2)             
                                                          COMP-3.               
                         25  ELBHIO-EN-DELETE-FLAG   PIC X.                     
      *  COBOL XREF NAME AREA                                                   
                   15  ELBHIO-ELPCN  REDEFINES  ELBHIO-RECORD-AREA.             
                       20  ELBHIO-ELPCN-KEY.                                    
                         25  ELBHIO-CN-RECORD-PREFIX PIC X(8).                  
                         25  ELBHIO-CN-COBOL-NAME    PIC X(30).                 
                         25  ELBHIO-CN-ELEMENT-NBR   PIC S9(3)V9(2)             
                                                          COMP-3.               
                         25  ELBHIO-CN-DELETE-FLAG   PIC X.                     
                                                                                
