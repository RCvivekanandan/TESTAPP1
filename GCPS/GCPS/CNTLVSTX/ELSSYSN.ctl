 /*DSN=HCV.TXELS.ELP.SYSNAME                                        */  00130005
 DELETE                      ( HCV.TXELS.ELP.SYSNAME )               -  00190005
              CLUSTER                                                -  00200000
              PURGE                                                     00210000
 DEFINE CLUSTER              (                                       -  00230000
              NAME           ( HCV.TXELS.ELP.SYSNAME )               -  00241005
              KEYS           ( 38 0 )                                -  00242200
              CYLINDERS      ( 5 1 )                                 -  00242300
              FREESPACE      ( 10 10 )                               -  00242400
              SHAREOPTIONS   ( 2 3 )                                 -  00242500
              RECORDSIZE     ( 42 42 )                               -  00242600
              STORCLAS       ( OTXPOOL)                              -  00250009
              UNIQUE                                                )-  00340000
        DATA                 (                                       -  00350000
              NAME           ( HCV.TXELS.ELP.SYSNAME.DATA )          -  00350105
              CISZ           ( 4096 )                               )-  00360000
         INDEX               (                                       -  00370000
              NAME           ( HCV.TXELS.ELP.SYSNAME.INDEX )         -  00371005
              CISZ           ( 512 )                                )   00380007
 LISTCAT ENTRIES             ( HCV.TXELS.ELP.SYSNAME ) ALL              00400005
