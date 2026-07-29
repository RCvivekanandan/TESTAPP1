 /*DSN=HCGENV.ELS.ELP.CODES                                         */          
 DELETE                      ( HCGENV.ELS.ELP.CODES )                -          
 CLUSTER                                                -                       
 PURGE                                                                          
 DEFINE CLUSTER              (                                       -          
 NAME           ( HCGENV.ELS.ELP.CODES )                -                       
 KEYS           ( 23 0 )                                -                       
 CYLINDERS      ( 10 5 )                                -                       
 FREESPACE      ( 10 10 )                               -                       
 SHAREOPTIONS   ( 2 3 )                                 -                       
 RECORDSIZE     ( 473 1026 )                            -                       
 STORCLAS       ( ONLINE )                              -                       
 UNIQUE                                                )-                       
 DATA                 (                                       -                 
 NAME           ( HCGENV.ELS.ELP.CODES.DATA )           -                       
 CISZ           ( 4096 )                               )-                       
 INDEX               (                                       -                  
 NAME           ( HCGENV.ELS.ELP.CODES.INDEX )          -                       
 CISZ           ( 512 )                                )                        
 /*  CATLG CARD REMOVED/CONTINUATION REMOVED */                                 
 LISTCAT ENTRIES             ( HCGENV.ELS.ELP.CODES ) ALL                       
