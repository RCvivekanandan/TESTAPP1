 /*DSN=HCV.TXELS.ELP.CODES                                          */  00130006
 DELETE                      ( HCV.TXELS.ELP.CODES )                 -  00190006
              CLUSTER                                                -  00200000
              PURGE                                                     00210000
 DEFINE CLUSTER              (                                       -  00230000
              NAME           ( HCV.TXELS.ELP.CODES )                 -  00241006
              KEYS           ( 23 0 )                                -  00242201
              CYLINDERS      ( 10 5 )                                -  00242301
              FREESPACE      ( 10 10 )                               -  00242400
              SHAREOPTIONS   ( 2 3 )                                 -  00242500
              RECORDSIZE     ( 473 1026 )                            -  00242601
              STORCLAS       ( OTXPOOL)                              -  00250016
              UNIQUE                                                )-  00340000
        DATA                 (                                       -  00350000
              NAME           ( HCV.TXELS.ELP.CODES.DATA )            -  00350106
              CISZ           ( 4096 )                               )-  00360000
         INDEX               (                                       -  00370000
              NAME           ( HCV.TXELS.ELP.CODES.INDEX )           -  00371006
              CISZ           ( 512 )                                )   00380010
