  SORT  FIELDS=COPY                                                     00010000
  OUTFIL  FILES=01,                                                     00020000
  INCLUDE=(0003,03,CH,EQ,C'314')                                        00030001
  OUTFIL  FILES=02,                                                     00060000
  INCLUDE=(0003,03,CH,NE,C'314')                                        00070001
