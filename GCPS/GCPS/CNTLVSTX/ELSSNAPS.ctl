 /*DSN=HCV.TXELS.ELP.ELSSNAPS                                       */  00010007
 DELETE (HCV.TXELS.ELP.ELSSNAPS) CLUSTER                                00020007
 DEFINE -                                                               00030000
     CLUSTER ( -                                                        00040000
         NAME(HCV.TXELS.ELP.ELSSNAPS) -                                 00050007
         CYLINDERS(5 5) -                                               00060000
         STORCLAS       ( OTXPOOL)                              -       00070011
         NONINDEXED -                                                   00080000
         BUFFERSPACE(8192) -                                            00090000
         CONTROLINTERVALSIZE(4096) -                                    00100000
         RECORDSIZE(1000 4089) -                                        00110000
         SHAREOPTIONS(2 3) -                                            00120000
         UNIQUE -                                                       00130000
         ) -                                                            00140000
     DATA ( -                                                           00150000
         NAME(HCV.TXELS.ELP.ELSSNAPS.DATA) -                            00160007
          )                                                             00170009
 LISTCAT ENTRIES(HCV.TXELS.ELP.ELSSNAPS) ALL                            00190007
