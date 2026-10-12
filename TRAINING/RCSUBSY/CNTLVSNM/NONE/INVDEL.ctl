 /*DSN=ONL14V.NMNA.INVFILE.COMPARE                                  */  00010003
 DELETE                       ( ONL14V.NMNA.INVFILE.COMPARE)         -  00020003
               PURGE                                                 -  00030003
               CLUSTER                                                  00040003
 DEFINE CLUSTER               (                                      -  00050003
               NAME           ( ONL14V.NMNA.INVFILE.COMPARE)         -  00060003
               FILE           ( TSGVSAM )                            -  00070003
               OWNER          ($IAM)                                 -  00071004
               RECORDSIZE     ( 470 470 )                           -   00080003
               FREESPACE      ( 36 36 )                              -  00090003
               KEYS           ( 20 0 )                               -  00100003
               SHAREOPTIONS   ( 2 3 )                                -  00110003
               STORCLAS       (ONLINE )                              -  00111005
               UNIQUE                                                -  00120003
               SPEED                                                 -  00130003
               ORDERED  )                                            -  00140003
         DATA                 (                                      -  00150003
               NAME   ( ONL14V.NMNA.INVFILE.COMPARE.DATA ) -            00160003
               CYLINDERS      ( 410 410)               -                00180003
               CISZ           ( 4096 )                              )-  00190003
         INDEX                (                                      -  00200003
               NAME   ( ONL14V.NMNA.INVFILE.COMPARE.INDEX ) -           00210003
               CYLINDERS      ( 15 2)                                -  00230003
               CISZ           ( 1024 )                              )   00240003
 LISTCAT ENTRIES           ( ONL14V.NMNA.INVFILE.COMPARE) ALL           00250003
 /*                                                                 */  00260003
