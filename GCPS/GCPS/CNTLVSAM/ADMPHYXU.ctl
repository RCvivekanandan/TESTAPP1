  DELETE                     ( OPERV.PSV.ADMPHYXU )       -             00000100
               PURGE                                      -             00000200
               CLUSTER                                                  00000300
  DEFINE CLUSTER             (                            -             00000400
               NAME          ( OPERV.PSV.ADMPHYXU )       -             00000500
               STORCLAS      ( ONLINE )                   -             00000602
               SHAREOPTIONS  ( 2 3 )                      -             00000700
               CYLINDERS     ( 5 1 )                      -             00000800
               FREESPACE     ( 10 10 )                    -             00000900
               UNIQUE                                     -             00001000
               INDEXED                                    -             00001100
               SPEED                                     )-             00001200
         DATA                (                            -             00001300
               NAME          ( OPERV.PSV.ADMPHYXU.DATA ) -              00001400
               RECORDSIZE    ( 070 070 )                  -             00001500
               KEYS          ( 31 0 )                     -             00001600
               CISZ          ( 4096 )                    )-             00001700
         INDEX               (                            -             00001800
               NAME          ( OPERV.PSV.ADMPHYXU.INDEX ) -             00001900
               CISZ          ( 1024 )                    )-             00002000
         CATALOG             ( OPERV )                                  00002100
         LISTCAT ENTRIES     ( OPERV.PSV.ADMPHYXU ) ALL                 00002200
 REPRO    INFILE (INFILE)               -                               00002303
          ODS (OPERV.PSV.ADMPHYXU)                                      00002404
                                                                        00002500
