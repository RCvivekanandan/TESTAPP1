000100     05  GCV-FIELD-EDIT-COMMAREA.                                         
000200*****************************************************************         
000300**                    G2VCOMAC                                            
000310**   THIS COPY MEMBER IS USED IN THE GENERIC CONTRACT SYSTEM.             
000400**                                                                        
000500**   IT IS USED IN THE SUPERVISOR SUPPORT SECTION OF THE SYSTEM           
000600** TO PASS KEY INFORMATION FOR THE FIELD VALIDATION SUB-SYSTEM.           
000700**                                                                        
000710******************************************************************        
000720******** * * *  *  *        N O T E        *  *  * * * * *********        
000730*                                                                         
000740*   THIS COPYLIB MEMBER IS USED IN BOTH OS COBOL AND VS COBOL II          
000750* PROGRAMS. ANY MODIFICATIONS MUST BE MADE TO BOTH MEMBERS.               
000760* OS COBOL VERSION IS NAMED GCVCOMAC                                      
000770*                                                                         
000780********** * *  *  *  *   *    *    *   *  *  *  * * * * *********        
000790******************************************************************        
000800*****************************************************************         
000900******************************************************************        
001000*                                                                         
001100*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*               
001200*       *-*         U P D A T E   H I S T O R Y         *-*               
001300*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*               
001400*                                                                         
001500******************************************************************        
001600*  LOG#     DATE     WHO              DESCRIPTION                         
001700* ------  --------   ---  ----------------------------------------        
001800* CCONV   03/31/87   DES  CONVERTED TO VS COBOL 2 POINTER MODE            
001900*                                                                         
002000*         01/07/86   DES  ADDED                                           
002100******************************************************************        
002200       10  GCVI-FIELDS-KEY-ID         PIC X(06).                          
002300       10  FILLER                     PIC XX.                             
002400       10  GCVI-RECORD-USAGE-WORD-1   PIC S9(8)  COMP.                    
002500       10  GCVI-RECORD-USAGE-WORD-2   PIC S9(8)  COMP.                    
002600       10  GCVI-RECORD-USAGE-WORD-3   PIC S9(8)  COMP.                    
002700       10  GCVI-RECORD-POINTER-COMP   PIC S9(8)  COMP.                    
002710       10  GCVI-RECORD-POINTER        REDEFINES                           
002720           GCVI-RECORD-POINTER-COMP   POINTER.                            
002800       10  GCVI-FIELD-NAME            PIC X(50).                          
002900       10  FILLER                     PIC X(20).                          
