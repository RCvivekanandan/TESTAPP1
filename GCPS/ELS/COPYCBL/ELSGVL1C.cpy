       01  GVL1-OVERLAY-RECORD.                                                 
000100****************************************************************  00010000
000200*                                                              *  00020000
000300*    COPYBOOK:   ELSGVL1C                                      *  00030003
000400*    DATE:       07-SEP-1993                                   *  00040003
000500*    AUTHOR:     ANNE KEFFER-KING                              *  00050003
      *    FUNCTION:                                                 *          
      *                                                              *          
      *   THIS COPY MEMBER IS USED BY ELS AS AN OVERLAY FOR THE      *          
      *   ELUGVLSV FILE WHICH CONTAINS THE LIST OF PVDR NUMBERS      *          
      *   PVDR CONTROLS AND EFFECTIVE AND TERMINATE DATES AS         *          
      *   WELL AS PVDR NAMES                                         *          
      *   THERE IS ONE OVERLAY FOR EACH WORK FILE RECORD.            *          
      ****************************************************************          
                                                                                
001000****************************************************************  00100000
001100*                                                              *  00110000
001200*                      MAINTENANCE HISTORY                     *  00120000
001300*                                                              *  00130000
001400*  MOD     DATE     BY  DRPT                ACTION             *  00140000
001500* ----- ----------- --- ----- ----------------------------------  00150000
001100* 1.00   12-AUG-93  AKK       CREATED                          *  00110000
001100*                                                              *  00110000
      * 2.00   04-AUG-98  AKK       UPDATED DATES FOR YR 2000.                  
001000****************************************************************  00100000
      *                                                                         
             05  GVL1-PRVDR-DTL.                                                
                 10  GVL1-PRVDR-NM                       PIC X(33).             
                 10  GVL1-PRVDR-NBR                      PIC X(10).             
                 10  GVL1-PRVDR-SQ-NBR           COMP    PIC S9(04).            
                 10  GVL1-PRVDR-CNT              COMP-3  PIC S9(03).            
                 10  GVL1-PRVDR-TYPE-RQST                PIC  X(01).            
                     88  GVL1-INSTITUTIONAL              VALUE 'I'.             
                     88  GVL1-PROFESSIONAL               VALUE 'P'.             
             05    GVL1-ENTRY OCCURS 1 TO 100 TIMES                             
                                   DEPENDING ON GVL1-PRVDR-CNT                  
                                   INDEXED BY GVL1-INDEX                        
                                              GVL1-MAX-INDEX.                   
                 10  GVL1-PRVDR-EFF-DATE.                                       
                     15 GVL1-PRVDR-EFFDT-CC          PIC X(1).                  
                     15 GVL1-PRVDR-EFF-DT    COMP-3  PIC S9(05).                
                 10  GVL1-PRVDR-EFFDT-CEN REDEFINES                             
                        GVL1-PRVDR-EFF-DATE  COMP-3 PIC S9(07).                 
                 10  GVL1-PRVDR-TRMTN-DATE.                                     
                     15 GVL1-PRVDR-TRMTNDT-CC         PIC X(1).                 
                     15 GVL1-PRVDR-TRMTN-DT   COMP-3  PIC S9(05).               
                 10  GVL1-PRVDR-TRMTNDT-CEN REDEFINES                           
                        GVL1-PRVDR-TRMTN-DATE  COMP-3 PIC S9(07).               
                 10  GVL1-PVDR-CTL-1             PIC  X(02).                    
                 10  GVL1-PVDR-CTL-2             PIC  X(02).                    
