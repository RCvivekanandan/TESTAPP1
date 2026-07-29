000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSKEYSC                                        *00030000
000400*    DATE:       12-SEP-1986                                     *00040000
000500*    AUTHOR:     RICHARD J. LUKETICH                             *00050000
000600*    FUNCTION:   WORK AREA TO BUILD KEYS FOR ELS ACCESS.         *00060000
000700*                                                                *00070000
000800******************************************************************00080000
000900*                                                                *00090000
001000*                      MAINTENANCE HISTORY                       *00100000
001100*                                                                *00110000
001200*  MOD     DATE     BY  DRPT                ACTION               *00120000
001300* ----- ----------- --- ----- ---------------------------------- *00130000
001400* 01.00 12-SEP-1986 RJL       CREATED                            *00140000
001500* 01.01 12-SEP-1986 RJL       CORRECTED TYPOS.                   *00150000
001600* 01.02 03-OCT-1986 RJL       ADDED TEMPORARY STORAGE QUEUE      *00160000
001700*                             KEYS.  CORRECTED MISSING REDEFINI- *00170000
001800*                             TIONS.                             *00180000
001900* 01.03 03-OCT-1986 RJL       CORRECTED TYPOS.                   *00190000
002000* 01.04 30-OCT-1986 RJL       ADDED POINTER FOR TEMPORARY        *00200000
002100*                             STORAGE WORK.                      *00210000
002200*                                                                *00220000
002300* 01.05 25-JUL-1988 LET       ADDED KEY AREA TO REFERENCE THE TWO*00230000
002400*                             SPECIAL BENEFIT MESSAGE FILES,     *00240000
002500*                             GROUP KEY FILE AND MESSAGE FILE.   *00250000
002600*                                                                *00260000
002700* 01.06 18-MAR-1990 RKH       REMOVED KWA-GCDATES-KEY-CONTR      *00270000
002800*                             CHANGED KWA-GCDATES-KEY-GRPSPC  TO *00280000
002900*                                     KWA-GCDATES-KEY            *00290000
003000*                             ADDED:                             *00300000
003100*                                   KWA-GCDATES-L-O-B            *00310000
003200*                                   KWA-GCDATES-FILLER           *00320000
003300*                                   KWA-GCDATES-FILE-REF         *00330000
003400*                                                                *00340000
003500* 01.07 27-SEP-1991 RJL       EXPAND FAM-REL-LVL'S TO 2 BYTES.   *00350000
003600*                                                                *00360000
003610* 01.08 02-AUG-1993 BAK       ADD NEW GVL FILE TO KEYS           *00361007
003620*                                                                *00362001
003630* 01.09 12-AUG-1997 AKK       ADDED NEW PLAN CODE, EXPANDED      *00363004
003640*                             GROUP AND CONTRACT AND ADDED       *00364004
003650*                             SUPPORT FOR YEAR 2000.             *00365004
003660*                                                                *00366006
003670* 01.10 14-AUG-1997 AKK       ADDED NEW PACKAGE CODE.            *00367006
003700******************************************************************00370000
003800                                                                  00380000
003900 01  KWA-FILE-KEY-WORK-AREA.                                      00390000
004000     02 KWA-FILE-KEY             PICTURE  X(256).                 00400000
004100                                                                  00410000
004200     02 KWA-ELPRL-KEY            REDEFINES KWA-FILE-KEY.          00420000
004300        03 KWA-RL-RECORD-PREFIX  PICTURE  X(008).                 00430000
004400        03                       PICTURE  X(248).                 00440000
004500                                                                  00450000
004600     02 KWA-ELPCV-KEY            REDEFINES KWA-FILE-KEY.          00460000
004700        03 KWA-CV-RECORD-PREFIX  PICTURE  X(008).                 00470000
004800        03 KWA-CV-ELEMENT-NBR    PICTURE S9(003)V9(02)   COMP-3.  00480000
004900        03 KWA-CV-CODE-VALUE     PICTURE  X(010).                 00490000
005000        03 KWA-CV-CODE-DESC-SEQ  PICTURE  9(002).                 00500000
005100        03                       PICTURE  X(233).                 00510000
005200                                                                  00520000
005300     02 KWA-ELPDE-KEY            REDEFINES KWA-FILE-KEY.          00530000
005400        03 KWA-DE-RECORD-PREFIX  PICTURE  X(008).                 00540000
005500        03 KWA-DE-ELEMENT-NBR    PICTURE S9(003)V9(02)   COMP-3.  00550000
005600        03                       PICTURE  X(245).                 00560000
005700                                                                  00570000
005800     02 KWA-ELPEN-KEY            REDEFINES KWA-FILE-KEY.          00580000
005900        03 KWA-EN-RECORD-PREFIX  PICTURE  X(008).                 00590000
006000        03 KWA-EN-ELEMENT-NAME   PICTURE  X(075).                 00600000
006100        03                       PICTURE  X(173).                 00610000
006200                                                                  00620000
006300     02 KWA-ELPCN-KEY            REDEFINES KWA-FILE-KEY.          00630000
006400        03 KWA-CN-RECORD-PREFIX  PICTURE  X(008).                 00640000
006500        03 KWA-CN-COBOL-NAME     PICTURE  X(030).                 00650000
006600        03                       PICTURE  X(218).                 00660000
006700                                                                  00670000
006800     02 KWA-GCDATES-KEY          REDEFINES KWA-FILE-KEY.          00680000
006810        03 KWA-GCDATES-PLAN-CODE PICTURE  X(003).                 00681003
006900        03 KWA-GCDATES-GROUP-NUMBER.                              00690003
006910           05 KWA-GCDATES-GRP-NO-1-3 PICTURE X(003).              00691003
006920           05 KWA-GCDATES-GRP-NO     PICTURE X(006).              00692008
007000        03 KWA-GCDATES-SECTION-NUMBER.                            00700003
007010           05 KWA-GCDATES-SECT-NO-1  PICTURE X(001).              00701003
007020           05 KWA-GCDATES-SECT-NO    PICTURE X(004).              00702007
007030        03 KWA-GCDATES-PKG-CODE      PICTURE  X(003).             00703007
007100        03 KWA-GCDATES-L-O-B         PICTURE  X(001).             00710007
007200*---->  KWA-GCDATES-L-O-B  FOR GROUP SPECIFIC MUST BE LOW VALUES  00720000
007300        03 KWA-GCDATES-FILLER        PICTURE  X(010).             00730007
007400        03 KWA-GCDATES-FILE-REF      PICTURE  X(001).             00740007
007500        03                           PICTURE  X(224).             00750011
007600                                                                  00760000
007700     02 KWA-GCCONTR-KEY          REDEFINES KWA-FILE-KEY.          00770000
              03 KWA-GCT-PLAN-CODE         PICTURE X(003).              00771011
007800        03 KWA-GCT-GROUP-NUMBER.                                  00780003
007810           05 KWA-GCT-GRP-NO-1-3     PICTURE X(003).              00781003
007820           05 KWA-GCT-GRP-NO         PICTURE X(006).              00782003
007900        03 KWA-GCT-SECTION-NUMBER.                                00790003
007910           05 KWA-GCT-SECT-NO-1      PICTURE X(001).              00791003
007920           05 KWA-GCT-SECTN-NO       PICTURE X(004).              00792009
007930        03 KWA-GCT-PKG-CODE          PICTURE  X(003).             00793007
008000        03 KWA-GCT-L-O-B         PICTURE  X(001).                 00800000
008100        03 KWA-GCT-PROVDR-CONTROL                                 00810000
008200                                 PICTURE  X(002).                 00820000
008300        03 KWA-GCT-FAM-REL-LVL   PICTURE  X(002).                 00830000
008400        03 KWA-GCT-EFFECTIVE-DATE.                                00840003
008410            05 KWA-GCT-EFF-DT-CC  PICTURE  X.                     00841004
008420            05 KWA-GCT-EFF-DT    PICTURE  S9(005)  COMP-3.        00842003
008430        03 KWA-GCT-EFFECTIVE-DATE-CENTURY REDEFINES               00843003
008440           KWA-GCT-EFFECTIVE-DATE    PICTURE S9(07) COMP-3.       00844003
008500        03                       PICTURE  X(227).                 00850011
008600                                                                  00860000
008700     02 KWA-GCBENPRV-KEY         REDEFINES KWA-FILE-KEY.          00870000
008800        03 KWA-GCP-PROVN-ID.                                      00880000
008900           04 KWA-GCP-BEN-PR-ID  PICTURE  X(005).                 00890000
009000           04 KWA-GCP-BEN-PROVN-FORMAT-CODE                       00900000
009100                                 PICTURE  X(001).                 00910000
009200        03 KWA-GCP-PROVN-SLOT-NO PICTURE S9(007)         COMP-3.  00920000
009300        03                       PICTURE  X(246).                 00930000
009400                                                                  00940000
009500     02 KWA-GCGRPSPC-KEY         REDEFINES KWA-FILE-KEY.          00950000
009600        03 KWA-GCG-PLAN-CODE     PICTURE  X(003).                 00960011
009600        03 KWA-GCG-GROUP-NUMBER.                                  00960111
009601           05 KWA-GCG-GRP-NO-1-3 PICTURE  X(003).                 00960204
009602           05 KWA-GCG-GRP-NO     PICTURE  X(006).                 00960304
009603        03 KWA-GCG-SECTION-NUMBER.                                00960410
009604           05 KWA-GCG-SECTN-1    PICTURE  X(001).                 00960504
009605           05 KWA-GCG-SECTN      PICTURE  X(004).                 00960604
009606        03 KWA-GCG-PKG-CODE      PICTURE  X(003).                 00960707
009800        03 KWA-GCG-FAM-REL-LVL   PICTURE  X(002).                 00980000
009900        03 KWA-GCG-EFFECTIVE-DATE.                                00990004
009910           05 KWA-GCG-EFF-DT-CC   PICTURE X.                      00991008
009920           05 KWA-GCG-EFF-DT      PICTURE  S9(05) COMP-3.         00992004
009930        03 KWA-GCG-EFF-DATE-CENTURY REDEFINES                     00993004
009940           KWA-GCG-EFFECTIVE-DATE PICTURE S9(07) COMP-3.          00994008
010000        03                       PICTURE  X(230).                 01000012
010100                                                                  01010000
010200     02 KWA-GCPRVTB2-KEY         REDEFINES KWA-FILE-KEY.          01020001
010300        03 KWA-GVL-PRVDR-ID      PICTURE  X(006).                 01030001
010400        03 KWA-GVL-PRVDR-SLOT-NO PICTURE S9(007)         COMP-3.  01040001
010410        03 KWA-GVL-PRVDR-NBR     PICTURE  X(010).                 01041001
010420        03 KWA-GVL-PRVDR-SEQ-NBR PICTURE S9(004)         COMP.    01042002
010500        03                       PICTURE  X(234).                 01050002
010510                                                                  01051001
010520     02 KWA-GCTABULR-KEY         REDEFINES KWA-FILE-KEY.          01052001
010530        03 KWA-PROVISION-ID      PICTURE  X(006).                 01053001
010540        03 KWA-PROVISION-SLOT-NO PICTURE S9(007)         COMP-3.  01054001
010550        03                       PICTURE  X(246).                 01055001
010600                                                                  01060000
010700     02 KWA-TMPSTG-KEY           REDEFINES KWA-FILE-KEY.          01070000
010800        03 KWA-TMPSTG-PTR        POINTER.                         01080000
010900        03 KWA-TMPSTG-ITEM       PICTURE S9(04)          COMP.    01090000
011000        03 KWA-TMPSTG-QUEUE-ID.                                   01100000
011100           04 KWA-TMPSTG-PFX     PICTURE  X(04).                  01110000
011200           04 KWA-TMPSTG-SFX     PICTURE  X(04).                  01120000
011300        03                       PICTURE  X(242).                 01130000
011400                                                                  01140000
011500     02 KWA-ELPGRPKY-KEY         REDEFINES KWA-FILE-KEY.          01150000
011600        03 KWA-ELPGRPKY          PICTURE  X(56).                  01160000
011700        03                       PICTURE  X(200).                 01170000
011800                                                                  01180000
011900     02 KWA-ELPMSGKY-KEY         REDEFINES KWA-FILE-KEY.          01190000
012000        03 KWA-ELPMSGKY          PICTURE  X(05).                  01200000
012100        03                       PICTURE  X(251).                 01210000
