 /*DSN=HCGENV.ELS.ELP.ENGNAME                                       */          
 DELETE                      ( HCGENV.ELS.ELP.ENGNAME )              -          
 CLUSTER                                                -                       
 PURGE                                                                          
 DEFINE CLUSTER              (                                       -          
 NAME           ( HCGENV.ELS.ELP.ENGNAME )              -                       
 KEYS           ( 83 0 )                                -                       
 CYLINDERS      ( 4 1 )                                 -                       
 FREESPACE      ( 10 10 )                               -                       
 SHAREOPTIONS   ( 2 3 )                                 -                       
 RECORDSIZE     ( 87 87 )                               -                       
 STORCLAS       ( ONLINE )                              -                       
 UNIQUE                                                )-                       
 DATA                 (                                       -                 
 NAME           ( HCGENV.ELS.ELP.ENGNAME.DATA )         -                       
 CISZ           ( 4096 )                               )-                       
 INDEX              (                                       -                   
 NAME           ( HCGENV.ELS.ELP.ENGNAME.INDEX )        -                       
 CISZ           ( 512 )                                )                        
 /*  CATLG CARD REMOVED/CONTINUATION REMOVED */                                 
 LISTCAT ENTRIES             ( HCGENV.ELS.ELP.ENGNAME ) ALL                     
