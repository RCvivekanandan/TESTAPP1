   DELETE KIXP.HCUSRUPD.EZHPBASE.HELP01 PURGE                           00000100
   DELETE KIXP.HCUSRUPD.EZHPBASE.HELP02 PURGE                           00000200
   DELETE KIXP.HCUSRUPD.EZHPBASE.HELP03 PURGE                           00000300
   /* HELP01 */                                                         00000400
   DEFINE CLUSTER -                                                     00000500
      (NAME(KIXP.HCUSRUPD.EZHPBASE.HELP01) -                            00000600
      CISZ(20480) -                                                     00000700
      INDEXED -                                                         00000800
      KEYS (3 0) -                                                      00000900
      RECSZ (4086 4086) -                                               00001000
      REUSE -                                                           00001100
      FREESPACE(0 0) -                                                  00001200
      CYL(1 1) -                                                        00001300
      NOERASE -                                                         00001400
      SPEED -                                                           00001500
      SHR(2)) -                                                         00001600
      INDEX(NAME(KIXP.HCUSRUPD.EZHPBASE.HELP01.INDEX) -                 00001700
      CISZ(512)) -                                                      00001800
      DATA(NAME(KIXP.HCUSRUPD.EZHPBASE.HELP01.DATA))                    00001900
   /* HELP02 */                                                         00002000
   DEFINE CLUSTER -                                                     00002100
      (NAME(KIXP.HCUSRUPD.EZHPBASE.HELP02) -                            00002200
      CISZ(18432) -                                                     00002300
      INDEXED -                                                         00002400
      KEYS (24 0) -                                                     00002500
      RECSZ (1410 1410) -                                               00002600
      REUSE -                                                           00002700
      FREESPACE(20 10) -                                                00002800
      CYL(80 10) -                                                      00002900
      NOERASE -                                                         00003000
      SPEED -                                                           00003100
      SHR(2)) -                                                         00003200
      INDEX(NAME(KIXP.HCUSRUPD.EZHPBASE.HELP02.INDEX) -                 00003300
      CISZ(1024)) -                                                     00003400
      DATA(NAME(KIXP.HCUSRUPD.EZHPBASE.HELP02.DATA))                    00003500
   /* HELP03 */                                                         00003600
   DEFINE CLUSTER -                                                     00003700
      (NAME(KIXP.HCUSRUPD.EZHPBASE.HELP03) -                            00003800
      CISZ(18432) -                                                     00003900
      INDEXED -                                                         00004000
      KEYS (8 0) -                                                      00004100
      RECSZ (160 6160) -                                                00004200
      REUSE -                                                           00004300
      FREESPACE(00 10) -                                                00004400
      CYL(10 10) -                                                      00004500
      NOERASE -                                                         00004600
      SPEED -                                                           00004700
      SHR(2)) -                                                         00004800
      INDEX(NAME(KIXP.HCUSRUPD.EZHPBASE.HELP03.INDEX) -                 00004900
      CISZ(512)) -                                                      00005000
      DATA(NAME(KIXP.HCUSRUPD.EZHPBASE.HELP03.DATA))                    00005100
