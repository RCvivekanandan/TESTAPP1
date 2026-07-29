 DELETE                      ( OPERV.PSV.DIRNUM )                    -  00000012
              PURGE                                                  -  00000013
              CLUSTER                                                   00000014
 DEFINE  CLUSTER             (                                       -  00000015
              NAME           (OPERV.PSV.DIRNUM)                      -  00000016
              SHAREOPTIONS   ( 02 03 )                               -  00000018
              UNIQUE                                                 -  00000020
              SPEED                                                  -  00000020
              KEYS           ( 03 09 )                               -  00000026
              RECORDSIZE     ( 20 20 )                               -  00000027
              FREESPACE      ( 0 0 )                                 -  00000025
              STORCLAS       ( ONLINE )                             )-  00000021
         DATA                (                                       -  00000022
              NAME           (OPERV.PSV.DIRNUM.DATA)                 -  00000023
              CYLINDERS      ( 5 5 )                                 -  00000024
              CISZ           ( 8192 )                               )-  00000028
         INDEX               (                                       -  00000029
              NAME           (OPERV.PSV.DIRNUM.INDEX)                -  00000030
              CYLINDERS      ( 1 1 )                                 -  00000024
              CISZ           ( 1024 )                               )-  00000033
         CATALOG             ( OPERV )                                          
         LISTCAT ENTRIES     ( OPERV.PSV.DIRNUM ) ALL                           
                                                                                
 REPRO INFILE     (INFILE) OUTDATASET (OPERV.PSV.DIRNUM)                        
