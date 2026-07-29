000100******************************************************************        
000200*                                                                *        
000300*    COPYBOOK:   ELSCSGHC                                        *        
000400*    DATE:       26-JAN-1988                                     *        
000500*    AUTHOR:     RICHARD J. LUKETICH                             *        
000600*    FUNCTION:   CONTAINS THE HEADINGS FOR THE VARIOUS PROVISION *        
000700*                GROUPS WITHIN EACH SUBTOPIC.                    *        
000800*                                                                *        
000900******************************************************************        
001000*                                                                *        
001100*                      MAINTENANCE HISTORY                       *        
001200*                                                                *        
001300*  MOD     DATE     BY  DRPT                ACTION               *        
001400* ----- ----------- --- ----- ---------------------------------- *        
001500* 01.00 26-JAN-1988 RJL       CREATED                            *        
001000* 01.01 02-FEB-1988 REB       ELIMINATED UNNECESSARY HEADINGS    *        
001000* 01.02 12-FEB-1988 NAC       CHANGED DELIVERY TO OBSTETRICAL    *        
      *                             FOR OBS01.                         *        
001600******************************************************************        
001700 01  WS-PROVISION-GROUP-HEADINGS.                                         
001800     02 WS-NBR-PROVN-GRP-HDGS    PICTURE S9(04)  COMP                     
001900                                 VALUE +16.                               
002000     02 WS-PROVN-GRP-HDGS.                                                
002301        03 FILLER                PICTURE  X(35)                           
002302           VALUE 'IHS01HOME CARE:                    '.                   
002310        03 FILLER                PICTURE  X(35)                           
002400           VALUE 'IHS02ROOM AND BOARD:               '.                   
002420        03 FILLER                PICTURE  X(35)                           
002500           VALUE 'IHS03ANCILLARY CHARGES:            '.                   
002510        03 FILLER                PICTURE  X(35)                           
002600           VALUE 'IPS01TREATMENT AND DIAGNOSIS:      '.                   
002610        03 FILLER                PICTURE  X(35)                           
002700           VALUE 'IPS02VISITS:                       '.                   
002710        03 FILLER                PICTURE  X(35)                           
002800           VALUE 'IPS03OTHER:                        '.                   
002801        03 FILLER                PICTURE  X(35)                           
002810           VALUE 'OBS01OBSTETRICAL:                  '.                   
002811        03 FILLER                PICTURE  X(35)                           
002820           VALUE 'OBS02ABORTION:                     '.                   
002821        03 FILLER                PICTURE  X(35)                           
002830           VALUE 'OBS03STERILIZATION:                '.                   
002840        03 FILLER                PICTURE  X(35)                           
002900           VALUE 'OPS01OUTPATIENT SURGERY PHYSICIAN: '.                   
002901        03 FILLER                PICTURE  X(35)                           
002910           VALUE 'OPS02EMERGENCY SERVICES:           '.                   
002911        03 FILLER                PICTURE  X(35)                           
002920           VALUE 'OPS03EMERGENCY CARE PHYSICIAN:     '.                   
002921        03 FILLER                PICTURE  X(35)                           
002930           VALUE 'OPS04DIAGNOSTIC SERVICES:          '.                   
002931        03 FILLER                PICTURE  X(35)                           
002940           VALUE 'OPS05THERAPIES:                    '.                   
002941        03 FILLER                PICTURE  X(35)                           
002950           VALUE 'OPS06OTHER:                        '.                   
003000        03 FILLER                PICTURE  X(35)                           
003300           VALUE 'PSY01THERAPY:                      '.                   
003500                                                                          
003600     02 WS-PROVN-GRP-HDG-TABLE   REDEFINES WS-PROVN-GRP-HDGS.             
003800        03 WS-PROVN-GRP-HDG-TBL  OCCURS 16 TIMES                          
003820                                 ASCENDING KEY IS WS-PGHT-KEY             
003810                                 INDEXED BY WS-PGHT-IDX.                  
003900           04 WS-PGHT-KEY.                                                
004020              05 WS-PGHT-SUBTOPIC   PICTURE  X(03).                       
004220              05 WS-PGHT-PROVN-GRP  PICTURE  9(02).                       
004400           04 WS-PGHT-PROVN-GRP-HDG PICTURE  X(30).                       
