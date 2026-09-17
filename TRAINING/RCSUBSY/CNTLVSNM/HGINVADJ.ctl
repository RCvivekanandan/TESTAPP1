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
 KEYS           ( 20 0 )                               -                        
 SHAREOPTIONS   ( 2 3 )                                -                        
 STORCLAS       (ONLINE)                               -                        
 UNIQUE                                                -                        
 SPEED                                                 -                        
 ORDERED  )                                            -                        
 DATA                 (                                      -                  
 NAME           ( ONL14V.NMNA.BLUECHIP.INVADJ.DATA )   -                        
 CYLINDERS      ( 150 50)                              -                        
 CISZ           ( 4096 )                              )-                        
 INDEX                (                                      -                  
 NAME           ( ONL14V.NMNA.BLUECHIP.INVADJ.INDEX )  -                        
 CYLINDERS      ( 9 3 )                                -                        
 CISZ           ( 1024 )                              )                         
 /*  CATLG CARD REMOVED/CONTINUATION REMOVED */                                 
 LISTCAT ENTRIES              ( ONL14V.NMNA.BLUECHIP.INVADJ ) ALL               
 /*                                                                 */          
