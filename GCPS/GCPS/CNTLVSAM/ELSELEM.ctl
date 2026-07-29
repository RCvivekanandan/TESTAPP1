 /*DSN=HCGENV.ELS.ELP.ELEMENTS                                      */          
 DELETE                      ( HCGENV.ELS.ELP.ELEMENTS )             -          
 CLUSTER                                                -                       
 PURGE                                                                          
 DEFINE CLUSTER              (                                       -          
 NAME           ( HCGENV.ELS.ELP.ELEMENTS )             -                       
 KEYS           ( 11 0 )                                -                       
 CYLINDERS      ( 2 1 )                                 -                       
 FREESPACE      ( 10 10 )                               -                       
 SHAREOPTIONS   ( 2 3 )                                 -                       
 RECORDSIZE     ( 646 1043 )                            -                       
 STORCLAS       ( ONLINE )                              -                       
 UNIQUE                                                )-                       
 DATA                 (                                       -                 
 NAME           ( HCGENV.ELS.ELP.ELEMENTS.DATA )        -                       
 CISZ           ( 4096 )                               )-                       
 INDEX               (                                       -                  
 NAME           ( HCGENV.ELS.ELP.ELEMENTS.INDEX )       -                       
 CISZ           ( 512 )                                )                        
 /*  CATLG CARD REMOVED/CONTINUATION REMOVED */                                 
 LISTCAT ENTRIES             ( HCGENV.ELS.ELP.ELEMENTS ) ALL                    
