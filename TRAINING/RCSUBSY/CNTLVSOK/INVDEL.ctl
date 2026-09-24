 /*DSN=ONL14V.OKNA.INVFILE.COMPARE                                  */  00010012
 DELETE                       ( ONL14V.OKNA.INVFILE.COMPARE)         -  00020012
               PURGE                                                 -  00030012
               CLUSTER                                                  00040012
 DEFINE CLUSTER               (                                      -  00050012
               NAME           ( ONL14V.OKNA.INVFILE.COMPARE)         -  00060012
               FILE           ( TSGVSAM )                            -  00070012
               OWNER          ($IAM)                                 -  00071012
               RECORDSIZE     ( 470 470 )                           -   00072012
               FREESPACE      ( 36 36 )                              -  00073012
               KEYS           ( 20 0 )                               -  00074012
               SHAREOPTIONS   ( 2 3 )                                -  00075012
               STORCLAS       (ONLINE )                              -  00076012
               UNIQUE                                                -  00077012
               SPEED                                                 -  00078012
               ORDERED  )                                            -  00079012
         DATA                 (                                      -  00080012
               NAME   ( ONL14V.OKNA.INVFILE.COMPARE.DATA ) -            00090012
               CYLINDERS      ( 410 410)               -                00100012
               CISZ           ( 4096 )                              )-  00110012
         INDEX                (                                      -  00120012
               NAME   ( ONL14V.OKNA.INVFILE.COMPARE.INDEX ) -           00130012
               CYLINDERS      ( 15 2)                                -  00140012
               CISZ           ( 1024 )                              )   00150012
 LISTCAT ENTRIES           ( ONL14V.OKNA.INVFILE.COMPARE) ALL           00160012
 /*                                                                 */  00170012
