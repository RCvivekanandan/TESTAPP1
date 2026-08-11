 /*DSN=ONL14V.NMNA.BLUECHIP.INVADJ                                   */         
 DELETE                       ( ONL14V.NMNA.BLUECHIP.INVADJ )        -          
 PURGE                                                 -                        
 CLUSTER                                                                        
 DEFINE CLUSTER               (                                      -          
 NAME           ( ONL14V.NMNA.BLUECHIP.INVADJ )        -                        
 FILE           ( TSGVSAM )                            -                        
 OWNER          ($IAM)                                 -                        
 RECORDSIZE     ( 470 470 )                            -                        
 FREESPACE      ( 36 36 )                              -                        
 /*  CATLG CARD REMOVED/CONTINUATION REMOVED */                                 
 LISTCAT ENTRIES              ( ONL14V.NMNA.BLUECHIP.INVADJ ) ALL               
 /*                                                                 */          
