 /*DSN=ONL14V.TXNA.INVFILE.COMPARE                                   */ 00010008
 DELETE                       ( ONL14V.TXNA.INVFILE.COMPARE )        -  00020008
               PURGE                                                 -  00030008
               CLUSTER                                                  00040008
 DEFINE CLUSTER               (                                      -  00050008
               NAME           ( ONL14V.TXNA.INVFILE.COMPARE )        -  00060008
               FILE           ( TSGVSAM )                            -  00070008
               OWNER          ($IAM)                                 -  00071009
               RECORDSIZE     ( 470 470 )                            -  00080008
               FREESPACE      ( 36 36 )                              -  00090008
               STORCLAS       ( OTXPOOL )                            -  00091008
               KEYS           ( 20 0 )                               -  00092008
               SHAREOPTIONS   ( 2 3 )                                -  00093008
               UNIQUE                                                -  00094008
               SPEED                                                 -  00095008
               ORDERED  )                                            -  00096008
         DATA                 (                                      -  00097008
               NAME        ( ONL14V.TXNA.INVFILE.COMPARE.DATA ) -       00098008
               CYLINDERS      ( 410 410)                    -           00099008
               CISZ           ( 4096 )                              )-  00100008
         INDEX                (                                      -  00110008
               NAME       ( ONL14V.TXNA.INVFILE.COMPARE.INDEX ) -       00120008
               CYLINDERS      ( 15 2 )                               -  00130008
               CISZ           ( 1024 )                              )   00140008
 LISTCAT ENTRIES              ( ONL14V.TXNA.INVFILE.COMPARE ) ALL       00150008
 /*                                                                 */  00160008
