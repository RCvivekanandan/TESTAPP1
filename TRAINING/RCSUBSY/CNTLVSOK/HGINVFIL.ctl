 /*DSN=ONL14V.OKNA.BLUECHIP.INVFILE                                 */          
 DELETE                       ( ONL14V.OKNA.BLUECHIP.INVFILE ) -                
 PURGE                                                 -                        
 CLUSTER                                                                        
 DEFINE CLUSTER               (                                      -          
 FILE           ( TSGVSAM )                            -                        
 OWNER          ($IAM)                                 -                        
 RECORDSIZE     ( 470 470 )                            -                        
 FREESPACE      ( 24 24 )                              -                        
 KEYS           ( 20 0 )                               -                        
 SHAREOPTIONS   ( 2 3 )                                -                        
 STORCLAS       (ONLINE)                               -                        
 UNIQUE                                                -                        
 SPEED                                                 -                        
 ORDERED                                              )-                        
 CISZ           ( 1024 )                              )                         
 /*  CATLG CARD REMOVED/CONTINUATION REMOVED */                                 
 LISTCAT ENTRIES         ( ONL14V.OKNA.BLUECHIP.INVFILE ) ALL                   
 /*                                                                 */          
