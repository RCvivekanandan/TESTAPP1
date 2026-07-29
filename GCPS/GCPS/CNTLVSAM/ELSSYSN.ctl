 /*DSN=HCV.ELS.ELP.SYSNAME                                          */          
 DELETE                      ( HCV.ELS.ELP.SYSNAME )                 -          
 CLUSTER                                                -                       
 PURGE                                                                          
 DEFINE CLUSTER              (                                       -          
 NAME           ( HCV.ELS.ELP.SYSNAME )                 -                       
 KEYS           ( 38 0 )                                -                       
 CYLINDERS      ( 5 1 )                                 -                       
 FREESPACE      ( 10 10 )                               -                       
 SHAREOPTIONS   ( 2 3 )                                 -                       
 RECORDSIZE     ( 42 42 )                               -                       
 STORCLAS       ( ONLINE )                              -                       
 UNIQUE                                                )-                       
 DATA                 (                                       -                 
 NAME           ( HCV.ELS.ELP.SYSNAME.DATA )            -                       
 CISZ           ( 4096 )                               )-                       
 INDEX               (                                       -                  
 NAME           ( HCV.ELS.ELP.SYSNAME.INDEX )           -                       
 CISZ           ( 512 )                                )                        
 /*  CATLG CARD REMOVED/CONTINUATION REMOVED */                                 
 LISTCAT ENTRIES             ( HCV.ELS.ELP.SYSNAME ) ALL                        
