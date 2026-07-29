 /* --------------------------------------*/                            00000100
 /*  DEFINE BACKUP EASYHELP  PROD FILE    */                            00000200
 /* --------------------------------------*/                            00000500
 DELETE  ( KIXP.HCSC.EZHPBASE.HELP01.BKUP ) -                           00000600
         CLUSTER -                                                      00000700
         PURGE                                                          00000800
 DEFINE CLUSTER -                                                       00000900
     (NAME(KIXP.HCSC.EZHPBASE.HELP01.BKUP) -                            00001000
     CISZ(20480) -                                                      00001100
     INDEXED -                                                          00001200
     KEYS (3 0) -                                                       00001300
     RECSZ (4086 4086) -                                                00001400
     REUSE -                                                            00001500
     FREESPACE(0 0) -                                                   00001600
     CYL(1 1) -                                                         00001700
     NOERASE -                                                          00001800
     SPEED -                                                            00001900
     SHR(2)) -                                                          00002000
     INDEX(NAME(KIXP.HCSC.EZHPBASE.HELP01.BKUP.INDEX) -                 00002200
     CISZ(512)) -                                                       00002300
     DATA(NAME(KIXP.HCSC.EZHPBASE.HELP01.BKUP.DATA))                    00002400
 /*                                       */                            00002420
 DELETE  ( KIXP.HCSC.EZHPBASE.HELP02.BKUP ) -                           00002500
         CLUSTER -                                                      00002600
         PURGE                                                          00002700
 DEFINE CLUSTER -                                                       00002800
    (NAME(KIXP.HCSC.EZHPBASE.HELP02.BKUP) -                             00002900
    CISZ(18432) -                                                       00003000
    INDEXED -                                                           00003100
    KEYS (24 0) -                                                       00003200
    RECSZ (1410 1410) -                                                 00003300
    REUSE -                                                             00003400
    FREESPACE(20 10) -                                                  00003500
    CYL(80 10) -                                                        00003600
    NOERASE -                                                           00003700
    SPEED -                                                             00003800
    SHR(2)) -                                                           00003900
    INDEX(NAME(KIXP.HCSC.EZHPBASE.HELP02.BKUP.INDEX) -                  00004000
    CISZ(1024)) -                                                       00004100
    DATA(NAME(KIXP.HCSC.EZHPBASE.HELP02.BKUP.DATA))                     00004200
 /*                                       */                            00004300
 DELETE  ( KIXP.HCSC.EZHPBASE.HELP03.BKUP ) -                           00004400
         CLUSTER -                                                      00004500
         PURGE                                                          00004600
 DEFINE CLUSTER -                                                       00004700
    (NAME(KIXP.HCSC.EZHPBASE.HELP03.BKUP) -                             00004800
    CISZ(18432) -                                                       00004900
    INDEXED -                                                           00005000
    KEYS (8 0) -                                                        00005100
    RECSZ (160 6160) -                                                  00005200
    REUSE -                                                             00005300
    FREESPACE(00 10) -                                                  00005400
    CYL(10 10) -                                                        00005500
    NOERASE -                                                           00005600
    SPEED -                                                             00005700
    SHR(2)) -                                                           00005800
    INDEX(NAME(KIXP.HCSC.EZHPBASE.HELP03.BKUP.INDEX) -                  00005900
    CISZ(512)) -                                                        00006000
    DATA(NAME(KIXP.HCSC.EZHPBASE.HELP03.BKUP.DATA))                     00006100
 /*                                       */                            00006110
