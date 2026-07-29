 /*DSN=HCV.ELS.ELP.RECLIST                                          */          
 DELETE                      ( HCV.ELS.ELP.RECLIST )                 -          
 CLUSTER                                                -                       
 PURGE                                                                          
 DEFINE CLUSTER              (                                       -          
 NAME           ( HCV.ELS.ELP.RECLIST )                 -                       
 KEYS           ( 8 0 )                                 -                       
 CYLINDERS      ( 1 1 )                                 -                       
 FREESPACE      ( 10 10 )                               -                       
 SHAREOPTIONS   ( 2 3 )                                 -                       
 RECORDSIZE     ( 61 61 )                               -                       
 STORCLAS       ( ONLINE )                              -                       
 UNIQUE                                                )-                       
 DATA                 (                                       -                 
 NAME           ( HCV.ELS.ELP.RECLIST.DATA )            -                       
 CISZ           ( 4096 )                               )-                       
 INDEX               (                                       -                  
 NAME           ( HCV.ELS.ELP.RECLIST.INDEX )           -                       
 CISZ           ( 512 )                                )                        
 /*  CATLG CARD REMOVED/CONTINUATION REMOVED */                                 
 LISTCAT ENTRIES             ( HCV.ELS.ELP.RECLIST ) ALL                        
