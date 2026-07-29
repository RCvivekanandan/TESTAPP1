000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSCFDBC                                     *00050000
000600*    Member Title:  Confidence Factor Data Block                 *00060000
000700*    Date Created:  05-Oct-1992                                  *00070000
000800*    Author:        Richard J. Luketich                          *00080000
000900*                                                                *00090000
001000*    Function:                                                   *00100000
001100*       This member provides the layout of the area in which     *00110000
001200*       confidence factors are returned to calling programs.     *00120000
001300*                                                                *00130000
001400*       Typical uses of this program are:                        *00140000
001500*                                                                *00150000
001600*       In the WORKING-STORAGE section of a program that calls a *00160000
001700*       subroutine that calculates one or more confidence        *00170000
001800*       factors based on other parameters.                       *00180000
001900*                                                                *00190000
002000*       In the LINKAGE section of a subroutine that calculates   *00200000
002100*       one or more confidence factors based on other            *00210000
002200*       parameters.                                              *00220000
002300*                                                                *00230000
002400******************************************************************00240000
002500*                                                                *00250000
002600*                       Maintenance History                      *00260000
002700*                                                                *00270000
002800*  Mod     Date      By               Action/Reason              *00280000
002900* ----- ----------- ---- --------------------------------------- *00290000
003000* 01.00 05-Oct-1992 RJL1 Created                                 *00300000
003100*                                                                *00310000
003000* 01.01 21-aug-2000 akk  Added support for #IPGS tabular         *00311001
003100*                                                                *00312001
003200******************************************************************00320000
003300                                                                  00330000
003400 01  CFDB-CNFDNC-FCTR-DATA-BLCK.                                  00340000
003500                                                                  00350000
003600     02 CFDB-CF                             PICTURE  X(80).       00360000
003700                                                                  00370000
003800     02 CFDB-CF-CLDR                        REDEFINES CFDB-CF.    00380000
003900                                                                  00390000
004000        03 CFDB-CF-CLDR-WGHTD-LST-MTCH      COMP-1.               00400000
004100        03 CFDB-CF-CLDR-UNWGHTD-LST-MTCH    COMP-1.               00410000
004200        03                                  PICTURE  X(72).       00420000
004300                                                                  00430000
004400     02 CFDB-CF-IBGR                        REDEFINES CFDB-CF.    00440000
004500        03 CFDB-CF-IBGR-INSTTNL             COMP-1.               00450000
004600        03 CFDB-CF-IBGR-PRFSNL              COMP-1.               00460000
004700        03 CFDB-CF-IBGR-IP-ONLY             COMP-1.               00470000
004800        03 CFDB-CF-IBGR-IP-BOTH             COMP-1.               00480000
004900        03 CFDB-CF-IBGR-OP-ONLY             COMP-1.               00490000
005000        03 CFDB-CF-IBGR-OP-BOTH             COMP-1.               00500000
005100        03 CFDB-CF-IBGR-OV                  COMP-1.               00510000
005200        03 CFDB-CF-IBGR-WGHTD-LST-MTCH      COMP-1.               00520000
005300        03 CFDB-CF-IBGR-UNWGHTD-LST-MTCH    COMP-1.               00530000
005400        03                                  PICTURE  X(44).       00540000
005500                                                                  00550000
005600     02 CFDB-CF-IDGD                        REDEFINES CFDB-CF.    00560000
005700        03 CFDB-CF-IDGD-OV                  COMP-1.               00570000
005800        03 CFDB-CF-IDGD-WGHTD-LST-MTCH      COMP-1.               00580000
005900        03 CFDB-CF-IDGD-UNWGHTD-LST-MTCH    COMP-1.               00590000
006000        03                                  PICTURE  X(68).       00600000
006100                                                                  00610000
006200     02 CFDB-CF-IPGN                        REDEFINES CFDB-CF.    00620000
006300        03 CFDB-CF-IPGN-OV                  COMP-1.               00630000
006400        03 CFDB-CF-IPGN-WGHTD-LST-MTCH      COMP-1.               00640000
006500        03 CFDB-CF-IPGN-UNWGHTD-LST-MTCH    COMP-1.               00650000
006600        03                                  PICTURE  X(68).       00660000
006700                                                                  00670000
006800     02 CFDB-CF-IPGP                        REDEFINES CFDB-CF.    00680000
006900        03 CFDB-CF-IPGP-INSTTNL             COMP-1.               00690000
007000        03 CFDB-CF-IPGP-PRFSNL              COMP-1.               00700000
007100        03 CFDB-CF-IPGP-OV                  COMP-1.               00710000
007200        03 CFDB-CF-IPGP-WGHTD-LST-MTCH      COMP-1.               00720000
007300        03 CFDB-CF-IPGP-UNWGHTD-LST-MTCH    COMP-1.               00730000
007400        03                                  PICTURE  X(60).       00740000
007500                                                                  00750000
007600     02 CFDB-CF-IPGT                        REDEFINES CFDB-CF.    00760000
007700        03 CFDB-CF-IPGT-INSTTNL             COMP-1.               00770000
007800        03 CFDB-CF-IPGT-PRFSNL              COMP-1.               00780000
007900        03 CFDB-CF-IPGT-PLAN                COMP-1.               00790000
008000        03 CFDB-CF-IPGT-NON-PLAN            COMP-1.               00800000
008100        03 CFDB-CF-IPGT-OV                  COMP-1.               00810000
008200        03 CFDB-CF-IPGT-WGHTD-LST-MTCH      COMP-1.               00820000
008300        03 CFDB-CF-IPGT-UNWGHTD-LST-MTCH    COMP-1.               00830000
008400        03                                  PICTURE  X(52).       00840000
008500                                                                  00850000
007600     02 CFDB-CF-IPGS                        REDEFINES CFDB-CF.    00860001
007800        03 CFDB-CF-IPGS-PRFSNL              COMP-1.               00880001
007900        03 CFDB-CF-IPGS-PLAN                COMP-1.               00890001
008000        03 CFDB-CF-IPGS-NON-PLAN            COMP-1.               00900001
008100        03 CFDB-CF-IPGS-OV                  COMP-1.               00910001
008200        03 CFDB-CF-IPGS-WGHTD-LST-MTCH      COMP-1.               00920001
008300        03 CFDB-CF-IPGS-UNWGHTD-LST-MTCH    COMP-1.               00930001
008400        03                                  PICTURE  X(56).       00940002
008500                                                                  00950001
