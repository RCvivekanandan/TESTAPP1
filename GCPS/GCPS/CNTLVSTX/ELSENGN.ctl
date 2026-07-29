 /*DSN=HCV.TXELS.ELP.ENGNAME                                        */  00130005
 DELETE                      ( HCV.TXELS.ELP.ENGNAME )               -  00190005
              CLUSTER                                                -  00200000
              PURGE                                                     00210000
 DEFINE CLUSTER              (                                       -  00230000
              NAME           ( HCV.TXELS.ELP.ENGNAME )               -  00241005
              KEYS           ( 83 0 )                                -  00242201
              CYLINDERS      ( 4 1 )                                 -  00242301
              FREESPACE      ( 10 10 )                               -  00242400
              SHAREOPTIONS   ( 2 3 )                                 -  00242500
              RECORDSIZE     ( 87 87 )                               -  00242601
              STORCLAS       ( OTXPOOL)                              -  00250010
              UNIQUE                                                )-  00340000
        DATA                 (                                       -  00350000
              NAME           ( HCV.TXELS.ELP.ENGNAME.DATA )          -  00350105
              CISZ           ( 4096 )                               )-  00360000
          INDEX              (                                       -  00370000
              NAME           ( HCV.TXELS.ELP.ENGNAME.INDEX )         -  00371005
              CISZ           ( 512 )                                )   00380008
 LISTCAT ENTRIES             ( HCV.TXELS.ELP.ENGNAME ) ALL              00400005
