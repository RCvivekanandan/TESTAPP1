000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSPRCLC                                     *00050000
000600*    Member Title:  Procedure Code Match List                    *00060000
000700*    Date Created:  07-Aug-1992                                  *00070000
000800*    Author:        Richard J. Luketich                          *00080000
000900*                                                                *00090000
001000*    Function:                                                   *00100000
001100*       This member is used to provide a list of procedure       *00110000
001200*       codes.                                                   *00120000
001300*                                                                *00130000
001400*       Typical uses for this overlay are to map a list of       *00140000
001500*       procedure codes to be matched against against a GCPS     *00150000
001600*       tabular or other list containing procedure codes. This   *00160000
001700*       is useful in processes involving confidence factor       *00170000
001800*       development, for which this member was originally        *00180000
001900*       developed.                                               *00190000
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
003200******************************************************************00320000
003300*                                                                *00330000
003400*                       Maintenance History                      *00340000
003500*                                                                *00350000
003600*  Mod     Date      By               Action/Reason              *00360000
003700* ----- ----------- ---- --------------------------------------- *00370000
003800* 01.00 07-Aug-1992 RJL1 Created.                                *00380000
003900*                                                                *00390000
003800* 02.00 07-may-2003 akk  changed procedure to 7 chanracters      *00390100
003900*                           from 6.                              *00390200
004000******************************************************************00400000
004100                                                                  00410000
004200 01  PRCL-DX-TBL.                                                 00420000
004300     02 PRCL-NBR-ENTRS           PICTURE S9(04) COMP.             00430000
004400        88 PRCL-MAX-NBR-ENTRS    VALUE +1000.                     00440000
004500     02 PRCL-TBL.                                                 00450000
004600        03 PRCL-ENTRY            OCCURS 1 TO 1000 TIMES           00460000
004700                                 DEPENDING ON PRCL-NBR-ENTRS      00470000
004800                                 INDEXED BY PRCL-IDX              00480000
004900                                            PRCL-MAX-IDX.         00490000
005000           04 PRCL-PRCDR         PICTURE  X(07).                  00500000
005100                                                                  00510000
