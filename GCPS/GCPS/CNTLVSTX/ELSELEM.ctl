 /*DSN=HCV.TXELS.ELP.ELEMENTS                                       */  00130005
 DELETE                      ( HCV.TXELS.ELP.ELEMENTS )              -  00190005
              CLUSTER                                                -  00200000
              PURGE                                                     00210000
 DEFINE CLUSTER              (                                       -  00230000
              NAME           ( HCV.TXELS.ELP.ELEMENTS )              -  00241005
              KEYS           ( 11 0 )                                -  00242202
              CYLINDERS      ( 2 1 )                                 -  00242301
              FREESPACE      ( 10 10 )                               -  00242400
              SHAREOPTIONS   ( 2 3 )                                 -  00242500
              RECORDSIZE     ( 646 1043 )                            -  00242601
              STORCLAS       ( OTXPOOL)                              -  00250010
              UNIQUE                                                )-  00340000
        DATA                 (                                       -  00350000
              NAME           ( HCV.TXELS.ELP.ELEMENTS.DATA )         -  00350105
              CISZ           ( 4096 )                               )-  00360000
         INDEX               (                                       -  00370000
              NAME           ( HCV.TXELS.ELP.ELEMENTS.INDEX )        -  00371005
              CISZ           ( 512 )                                )   00380008
 LISTCAT ENTRIES             ( HCV.TXELS.ELP.ELEMENTS ) ALL             00400005
