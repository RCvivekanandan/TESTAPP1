000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSPRCWC                                     *00050000
000600*    Member Title:  Weighted Procedure Code Match List           *00060000
000700*    Date Created:  07-Aug-1992                                  *00070000
000800*    Author:        Richard J. Luketich                          *00080000
000900*                                                                *00090000
001000*    Function:                                                   *00100000
001100*       This member is used to provide a list of procedure codes *00110000
001200*       with weights.                                            *00120000
001300*                                                                *00130000
001400*       Typical uses for this overlay are to map a list of       *00140000
001500*       procedure codes with weights to be matched against       *00150000
001600*       against a GCPS tabular or other list containing          *00160000
001700*       procedure codes. This is useful in processes involving   *00170000
001800*       confidence factor development, for which this member was *00180000
001900*       originally developed.                                    *00190000
002000*                                                                *00200000
002100*       When used with confidence factor development processes,  *00210000
002200*       the value of the count of the number of occurrences at   *00220000
002300*       beginning of the member indicates the type of matching   *00230000
002400*       being performed. A zero value indicates that general     *00240000
002500*       confidence factor computations are to take place, a      *00250000
002600*       value of one indicates that a specific procedure code is *00260000
002700*       to be matched and a value greater than zero indicates a  *00270000
002800*       list match. If a single value or list match is to be     *00280000
002900*       performed, general confidence factor calculations are    *00290000
003000*       not performed.                                           *00300000
003100*                                                                *00310000
003200*       NOTE: When computing general confidence factors or       *00320000
003300*       performing specific procedure code matching, the         *00330000
003400*       processing is identical to that used with the unweighted *00340000
003500*       list (ELSPRCLC).                                         *00350000
003600*                                                                *00360000
003700******************************************************************00370000
003800*                                                                *00380000
003900*                       Maintenance History                      *00390000
004000*                                                                *00400000
004100*  Mod     Date      By               Action/Reason              *00410000
004200* ----- ----------- ---- --------------------------------------- *00420000
004300* 01.00 07-Aug-1992 RJL1 Created.                                *00430000
004400*                                                                *00440000
004300* 01.01 07-may-2003 akk  expanded procedure to 7 char from       *00440100
004400*                              6.                                *00440200
004500******************************************************************00450000
004600                                                                  00460000
004700 01  PRCW-DX-TBL.                                                 00470000
004800     02 PRCW-NBR-ENTRS           PICTURE S9(04) COMP.             00480000
004900        88 PRCW-MAX-NBR-ENTRS    VALUE +1000.                     00490000
005000     02 PRCW-TBL.                                                 00500000
005100        03 PRCW-ENTRY            OCCURS 1 TO 1000 TIMES           00510000
005200                                 DEPENDING ON PRCW-NBR-ENTRS      00520000
005300                                 INDEXED BY PRCW-IDX              00530000
005400                                            PRCW-MAX-IDX.         00540000
005500           04 PRCW-PRCDR         PICTURE  X(07).                  00550000
005600           04 PRCW-WGHT          PICTURE S9(04) COMP.             00560000
005700                                                                  00570000
