000100     05  ELO-OUTPUT-BLOCK.                                                
000101         10  ELO-PAGE-REC-AREA-ADDR                                       
000102                                 PIC S9(08) COMP.                         
000103         10  ELO-PAGE-COUNTER    PIC S9(04) COMP.                         
000104         10  ELO-LINE-COUNTER    PIC S9(04) COMP.                         
000110         10  ELO-HEADER-BLOCK.                                            
000200             15  ELO-NBR-HEADER-LINES                                     
000300                                 PIC S9(04) COMP.                         
000400             15  ELO-HEADER.                                              
000500                 20  ELO-HEADER-LINES                                     
000600                                 PIC X(79)                                
000700                                 OCCURS 3 TIMES                           
000800                                 INDEXED BY ELO-HDR-IX.                   
000900         10  ELO-DETAIL-BLOCK.                                            
001000             15  ELO-NBR-DETAIL-LINES                                     
001100                                 PIC S9(04) COMP.                         
001200             15  ELO-FUNCTION    PIC X.                                   
001300                 88  ELO-NEW-PAGE     VALUE 'P'.                          
001400                 88  ELO-END-OUTPUT   VALUE 'E'.                          
001500             15  ELO-DETAIL.                                              
001600                 20  ELO-DETAIL-LINE PIC X(79)                            
001700                                 OCCURS 19 TIMES                          
001800                                 INDEXED BY ELO-DET-IX.                   
