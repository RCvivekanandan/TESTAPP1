       01  GVL4-OVERLAY-RECORD.                                                 
000100****************************************************************  00010000
000200*                                                              *  00020000
000300*    COPYBOOK:   ELSGVL4C                                      *  00030003
000400*    DATE:       12-AUG-1993                                   *  00040003
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
001100* 2.00   03-AUG-98  AKK       EXPAND DATES FOR YR 2000         *  00110000
001000****************************************************************  00100000
             05  GVL4-PRVDR-DTL.                                                
                 10  GVL4-PRVDR-NM                       PIC X(33).             
                 10  GVL4-PRVDR-NBR                      PIC X(10).             
                 10  GVL4-PRVDR-SQ-NBR           COMP    PIC S9(04).            
                 10  GVL4-PRVDR-CNT              COMP-3  PIC S9(03).            
                 10  GVL4-PRVDR-TYPE-RQST                PIC  X(01).            
                     88  GVL4-INSTITUTIONAL              VALUE 'I'.             
                     88  GVL4-PROFESSIONAL               VALUE 'P'.             
             05    GVL4-ENTRY OCCURS 1 TO 100 TIMES                             
                                   DEPENDING ON GVL4-PRVDR-CNT                  
                                   INDEXED BY GVL4-INDEX                        
                                              GVL4-MAX-INDEX.                   
                 10  GVL4-PRVDR-EFF-DATE.                                       
                     15 GVL4-PRVDR-EFFDT-CC      PIC X(1).                      
                     15  GVL4-PRVDR-EFF-DT                                      
                                         COMP-3  PIC S9(05).                    
                 10  GVL4-PRVDR-EFFDT-CEN REDEFINES                             
                        GVL4-PRVDR-EFF-DATE COMP-3 PIC S9(07).                  
                 10  GVL4-PRVDR-TRMTN-DATE.                                     
                     15 GVL4-PRVDR-TRMTNDT-CC      PIC X(1).                    
                     15  GVL4-PVDR-TRMTN-DT                                     
                                         COMP-3  PIC S9(05).                    
                 10  GVL4-PRVDR-TRMTNDT-CEN REDEFINES                           
                        GVL4-PRVDR-TRMTN-DATE COMP-3 PIC S9(07).                
                 10  GVL4-PVDR-CTL-1             PIC  X(02).                    
                 10  GVL4-PVDR-CTL-2             PIC  X(02).                    
