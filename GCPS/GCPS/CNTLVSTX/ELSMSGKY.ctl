 /*DSN=HCV.TXELS.ELP.MESSAGES                                       */  00000011
 DELETE                      ( HCV.TXELS.ELP.MESSAGES )              -  00000012
           CLUSTER                                                   -  00000013
           PURGE                                                        00000014
 DEFINE CLUSTER              (                                       -  00000015
                NAME         ( HCV.TXELS.ELP.MESSAGES )              -  00000016
                STORCLAS     ( OTXPOOL)                              -  00000017
                RECORDSIZE   ( 380 1100 )                            -  00000018
                CYLINDERS    ( 20 2 )                                -  00000019
                FREESPACE    ( 10 30 )                               -  00000020
                INDEXED                                              -  00000021
                KEYS         ( 5 0 )                                 -  00000022
                SHAREOPTIONS ( 2 3 )                                 -  00000023
                REUSE                                               )-  00000026
           DATA              (                                       -  00000027
                NAME         ( HCV.TXELS.ELP.MESSAGES.DATA )         -  00000028
                CISZ         ( 4096 )                               )-  00000029
           INDEX             (                                       -  00000030
                NAME         ( HCV.TXELS.ELP.MESSAGES.INDEX )        -  00000031
                CISZ         ( 1024 )                               )   00000032
    LISTCAT ENTRIES          ( HCV.TXELS.ELP.MESSAGES ) ALL             00000034
