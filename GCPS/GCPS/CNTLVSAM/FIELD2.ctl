 /*DSN=HCGENV.GENERIC.GCFLDVL2                                      */          
 DELETE                       ( HCGENV.GENERIC.GCFLDVL2 )            -          
 CLUSTER                                                     -                  
 PURGE                                                                          
 DEFINE CLUSTER               (                                      -          
 NAME                 ( HCGENV.GENERIC.GCFLDVL2 )            -                  
 KEYS                 ( 6 0 )                                -                  
 CYLINDERS            ( 4 1 )                                -                  
 FREESPACE            ( 30 30 )                              -                  
 SHAREOPTIONS         ( 2 3 )                                -                  
 RECORDSIZE           ( 1000 8006 )                          -                  
 STORCLAS             ( ONLINE )                             -                  
 UNIQUE                                                     )-                  
 DATA                  (                                      -                 
 NAME                 ( HCGENV.GENERIC.GCFLDVL2.DATA )       -                  
 CISZ                 ( 8192 )                              )-                  
 INDEX                 (                                      -                 
 NAME                 ( HCGENV.GENERIC.GCFLDVL2.INDEX )      -                  
 CISZ                 ( 1042 )                              )                   
 /*  CATLG CARD REMOVED/CONTINUATION REMOVED */                                 
 /*                                                                 */          
