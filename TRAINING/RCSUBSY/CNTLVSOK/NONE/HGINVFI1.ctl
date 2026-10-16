 /*DSN=ONL14V.OKNA.BLUECHIP.INVFILE                                 */          
 DELETE                       ( ONL14V.OKNA.BLUECHIP.INVFILE ) -                
 PURGE                                                 -                        
 CLUSTER                                                                        
 DEFINE CLUSTER               (                                      -          
 NAME           ( ONL14V.OKNA.BLUECHIP.INVFILE ) -                              
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
 DATA                 (                                      -                  
 NAME        ( ONL14V.OKNA.BLUECHIP.INVFILE.DATA )      -                       
 CYLINDERS      ( 410 410 )                            -                        
 CISZ           ( 4096 )                              )-                        
 INDEX                (                                      -                  
 NAME      ( ONL14V.OKNA.BLUECHIP.INVFILE.INDEX ) -                             
 CYLINDERS      ( 15 2)                                -                        
 CISZ           ( 1024 )                              )                         
 /*  CATLG CARD REMOVED/CONTINUATION REMOVED */                                 
 LISTCAT ENTRIES         ( ONL14V.OKNA.BLUECHIP.INVFILE ) ALL                   
 /*                                                                 */          
