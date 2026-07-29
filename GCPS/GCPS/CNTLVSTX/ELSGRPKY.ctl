 /*DSN=HCV.TXELS.ELP.GRPKEYS                                        */  00000601
 DELETE                      ( HCV.TXELS.ELP.GRPKEYS )               -  00000602
           CLUSTER                                                   -  00000603
           PURGE                                                        00000604
 DEFINE CLUSTER              (                                       -  00000605
                NAME         ( HCV.TXELS.ELP.GRPKEYS )               -  00000606
                STORCLAS     ( OTXPOOL)                              -  00000607
                RECORDSIZE   ( 161 161 )                             -  00000608
                CYLINDERS    ( 20 2 )                                -  00000609
                FREESPACE    ( 10 30 )                               -  00000610
                INDEXED                                              -  00000611
                KEYS         ( 56 0 )                                -  00000612
                SHAREOPTIONS ( 2 3 )                                 -  00000613
                REUSE                                               )-  00000616
           DATA              (                                       -  00000617
                NAME         ( HCV.TXELS.ELP.GRPKEYS.DATA )          -  00000618
                CISZ         ( 4096 )                               )-  00000619
           INDEX             (                                       -  00000620
                NAME         ( HCV.TXELS.ELP.GRPKEYS.INDEX )         -  00000621
                CISZ         ( 1024 )                               )   00000622
    LISTCAT ENTRIES          ( HCV.TXELS.ELP.GRPKEYS ) ALL              00000624
