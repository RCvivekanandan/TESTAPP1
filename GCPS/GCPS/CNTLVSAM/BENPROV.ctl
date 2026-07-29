 /*DSN=HCGENV.EGENERIC.BENPROV                                       */         
 DELETE                   ( HCGENV.EGENERIC.BENPROV )                 -         
          CLUSTER                                                     -         
          PURGE                                                                 
 DEFINE   CLUSTER         (                                           -         
            NAME          ( HCGENV.EGENERIC.BENPROV )                 -         
            KEYS          ( 10 0 )                                    -         
            CYLINDERS     ( 1000 1000 )                               -         
            FREESPACE     ( 30 30 )                                   -         
            SHAREOPTIONS  ( 2 3 )                                     -         
            RECORDSIZE    ( 255 395 )                                 -         
            DATACLAS      ( COMPRES2 )                                -         
            STORCLAS      ( ONLINE )                                  -         
            UNIQUE                                                   )-         
          DATA            (                                           -         
            NAME          ( HCGENV.EGENERIC.BENPROV.DATA )            -         
            CISZ          ( 16384 )                                  )-         
 INDEX                    (                                           -         
            NAME          ( HCGENV.EGENERIC.BENPROV.INDEX )           -         
            CISZ          ( 1024 )                                   )          
 /*  CATLG CARD REMOVED/CONTINUATION REMOVED */                                 
