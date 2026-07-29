000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSBPVLC                                     *00050000
000600*    Member Title:  Benefit Provision Match List                 *00060000
000700*    Date Created:  07-Aug-1992                                  *00070000
000800*    Author:        Richard J. Luketich                          *00080000
000900*                                                                *00090000
001000*    Function:                                                   *00100000
001100*       This member is used to provide a list of benefit         *00110000
001200*       provisions.                                              *00120000
001300*                                                                *00130000
001400*       Typical uses for this overlay are to map a list of       *00140000
001500*       benefit provisions to be matched against against a GCPS  *00150000
001600*       tabular or other list containing benefit provisions.     *00160000
001700*       This is useful in processes involving confidence factor  *00170000
001800*       development, for which this member was originally        *00180000
001900*       developed.                                               *00190000
002000*                                                                *00200000
002100*       When used with confidence factor development processes,  *00210000
002200*       the value of the count of the number of occurrences at   *00220000
002300*       beginning of the member indicates the type of matching   *00230000
002400*       being performed. A zero value indicates that general     *00240000
002500*       confidence factor computations are to take place, a      *00250000
002600*       value of one indicates that a specific benefit provision *00260000
002700*       is to be matched and a value greater than zero indicates *00270000
002800*       a list match. If a single value or list match is to be   *00280000
002900*       performed, general confidence factor calculations are    *00290000
003000*       not performed.                                           *00300000
003100*                                                                *00310000
003200******************************************************************00320000
003300*                                                                 00330000
003400*                                                                *00340000
003500*                       Maintenance History                      *00350000
003600*                                                                *00360000
003700*  Mod     Date      By               Action/Reason              *00370000
003800* ----- ----------- ---- --------------------------------------- *00380000
003900* 01.00 07-Aug-1992 RJL1 Created.                                *00390000
004000*                                                                *00400000
004100******************************************************************00410000
004200                                                                  00420000
004300 01  BPVL-BNFT-PRVSN-TBL.                                         00430000
004400     02 BPVL-NBR-ENTRS           PICTURE S9(04) COMP.             00440000
004500        88 BPVL-MAX-NBR-ENTRS    VALUE +1000.                     00450000
004600     02 BPVL-TBL.                                                 00460000
004700        03 BPVL-ENTRY            OCCURS 1 TO 1000 TIMES           00470000
004800                                 DEPENDING ON BPVL-NBR-ENTRS      00480000
004900                                 INDEXED BY BPVL-IDX              00490000
005000                                            BPVL-MAX-IDX.         00500000
005100           04 BPVL-BNFT-PRVSN.                                    00510000
005200              05 BPVL-BNFT-PRVSN-ID         PICTURE  X(05).       00520000
005300              05 BPVL-BNFT-PRVSN-FMT        PICTURE  X(01).       00530000
005400                 88 BPVL-INSTTTNL           VALUE 'A', 'B', 'W'.  00540000
005500                 88 BPVL-PRFSNL             VALUE 'C', 'D', 'E'.  00550000
005600                                                                  00560000
005700                                                                  00570000
