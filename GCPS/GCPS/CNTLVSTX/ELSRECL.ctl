 /*DSN=HCV.TXELS.ELP.RECLIST                                        */  00130007
 DELETE                      ( HCV.TXELS.ELP.RECLIST )               -  00190007
              CLUSTER                                                -  00200000
              PURGE                                                     00210000
 DEFINE CLUSTER              (                                       -  00230000
              NAME           ( HCV.TXELS.ELP.RECLIST )               -  00241007
              KEYS           ( 8 0 )                                 -  00242201
              CYLINDERS      ( 1 1 )                                 -  00242302
              FREESPACE      ( 10 10 )                               -  00242400
              SHAREOPTIONS   ( 2 3 )                                 -  00242500
              RECORDSIZE     ( 61 61 )                               -  00242601
              STORCLAS       ( OTXPOOL)                              -  00250012
              UNIQUE                                                )-  00340000
        DATA                 (                                       -  00350000
              NAME           ( HCV.TXELS.ELP.RECLIST.DATA )          -  00350107
              CISZ           ( 4096 )                               )-  00360000
         INDEX               (                                       -  00370000
              NAME           ( HCV.TXELS.ELP.RECLIST.INDEX )         -  00371007
              CISZ           ( 512 )                                )   00380010
 LISTCAT ENTRIES             ( HCV.TXELS.ELP.RECLIST ) ALL              00400007
