 /*DSN=ONL14V.NMNA.BLUECHIP.INVFILE                                 */          
 DELETE                       ( ONL14V.NMNA.BLUECHIP.INVFILE ) -                
 PURGE                                                 -                        
 CLUSTER                                                                        
 DEFINE CLUSTER               (                                      -          
 NAME           ( ONL14V.NMNA.BLUECHIP.INVFILE ) -                              
 FILE           ( TSGVSAM )                            -                        
 OWNER          ($IAM)                                 -                        
 CISZ           ( 1024 )                              )                         
 /*  CATLG CARD REMOVED/CONTINUATION REMOVED */                                 
 LISTCAT ENTRIES         ( ONL14V.NMNA.BLUECHIP.INVFILE ) ALL                   
 /*                                                                 */          
