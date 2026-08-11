 /*DSN=HCGENV.TXBCINV.INVFILE                                       */  00010017
 DELETE                       ( HCGENV.TXBCINV.INVFILE ) -              00020017
               PURGE                                                 -  00030017
               CLUSTER                                                  00040017
               OWNER          ($IAM)                                 -  00061017
               RECORDSIZE     ( 470 470 )                            -  00070017
               FREESPACE      ( 36 36 )                              -  00080017
               STORCLAS       ( OTXPOOL )                            -  00081018
               KEYS           ( 20 0 )                               -  00090017
               SHAREOPTIONS   ( 2 3 )                                -  00100017
               UNIQUE                                                -  00110017
               SPEED                                                 -  00120017
               ORDERED                                              )-  00130017
         DATA                 (                                      -  00140017
         INDEX                (                                      -  00190017
 LISTCAT ENTRIES         ( HCGENV.TXBCINV.INVFILE ) ALL                 00240017
 /*                                                                 */  00250017
