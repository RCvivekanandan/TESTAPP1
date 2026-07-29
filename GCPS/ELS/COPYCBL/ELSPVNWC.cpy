000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSPVNWC                                     *00050000
000600*    Member Title:  Weighted Provider Match List                 *00060000
000700*    Date Created:  07-Aug-1992                                  *00070000
000800*    Author:        Richard J. Luketich                          *00080000
000900*                                                                *00090000
001000*    Function:                                                   *00100000
001100*       This member is used to provide a list of provider        *00110000
001200*       numbers with weights.                                    *00120000
001300*                                                                *00130000
001400*       Typical uses for this overlay are to map a list of       *00140000
001500*       provider numbers with weights to be matched against a    *00150000
001600*       GCPS tabular or other list containing provider numbers.  *00160000
001700*       This is useful in processes involving confidence factor  *00170000
001800*       development, for which this member was originally        *00180000
001900*       developed. The weights are used to indicate the relative *00190000
002000*       importance of each entry in the table.                   *00200000
002100*                                                                *00210000
002200*       When used with confidence factor development processes,  *00220000
002300*       the value of the count of the number of occurrences at   *00230000
002400*       beginning of the member indicates the type of matching   *00240000
002500*       being performed. A zero value indicates that general     *00250000
002600*       confidence factor computations are to take place, a      *00260000
002700*       value of one indicates that a specific provider number   *00270000
002800*       is to be matched and a value greater than zero indicates *00280000
002900*       a list match. If a single value or list match is to be   *00290000
003000*       performed, general confidence factor calculations are    *00300000
003100*       not performed.                                           *00310000
003200*                                                                *00320000
003300*       NOTE: When computing general confidence factors or       *00330000
003400*       performing specific provider number matching, the        *00340000
003500*       processing is identical to that used with the unweighted *00350000
003600*       list (ELSPVNLC).                                         *00360000
003700*                                                                *00370000
003800******************************************************************00380000
003900*                                                                *00390000
004000*                       Maintenance History                      *00400000
004100*                                                                *00410000
004200*  Mod     Date      By               Action/Reason              *00420000
004300* ----- ----------- ---- --------------------------------------- *00430000
004400* 01.00 07-Aug-1992 RJL1 Created.                                *00440000
004500*                                                                *00450000
004600******************************************************************00460000
004700                                                                  00470000
004800 01  PVNW-PRVDR-NBR-TBL.                                          00480000
004900     02 PVNW-NBR-ENTRS           PICTURE S9(04) COMP.             00490000
005000        88 PVNW-MAX-NGR-ENTRS    VALUE +1000.                     00500000
005100     02 PVNW-TBL.                                                 00510000
005200        03 PVNW-ENTRY            OCCURS 1 TO 1000 TIMES           00520000
005300                                 DEPENDING ON PVNW-NBR-ENTRS      00530000
005400                                 INDEXED BY PVNW-IDX              00540000
005500                                            PVNW-MAX-IDX.         00550000
005600           04 PVNW-PRVDR-NBR     PICTURE  X(10).                  00560000
005700           04 PVNW-WGHT          PICTURE S9(04) COMP.             00570000
005800                                                                  00580000
