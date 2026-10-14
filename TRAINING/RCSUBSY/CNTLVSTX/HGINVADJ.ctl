 /*DSN=HCGENV.TXBCINV.INVADJ                                         */ 00010020
 DELETE                       ( HCGENV.TXBCINV.INVADJ )              -  00020020
               PURGE                                                 -  00030020
               CLUSTER                                                  00040020
 DEFINE CLUSTER               (                                      -  00050020
               NAME           ( HCGENV.TXBCINV.INVADJ )              -  00060020
               STORCLAS       ( OTXPOOL)                              - 00060123
               OWNER          ($IAM)                                 -  00061021
               RECORDSIZE     ( 470 470 )                            -  00070020
               FREESPACE      ( 36 36 )                              -  00080020
               KEYS           ( 20 0 )                               -  00090020
               SHAREOPTIONS   ( 2 3 )                                -  00100020
               UNIQUE                                                -  00110020
               SPEED                                                 -  00120020
               ORDERED  )                                            -  00130020
         DATA                 (                                      -  00140020
               NAME        ( HCGENV.TXBCINV.INVADJ.DATA )            -  00150020
               CYLINDERS      ( 60 40)                               -  00170020
               CISZ           ( 4096 )                              )-  00180020
         INDEX                (                                      -  00190020
               NAME       ( HCGENV.TXBCINV.INVADJ.INDEX )            -  00200020
               CYLINDERS      ( 9 3 )                                -  00220020
               CISZ           ( 1024 )                              )   00230020
 LISTCAT ENTRIES              ( HCGENV.TXBCINV.INVADJ ) ALL             00240020
 /*                                                                 */  00250020
