000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSPVTLC                                     *00050000
000600*    Member Title:  Provider Type Match List                     *00060000
000700*    Date Created:  07-Aug-1992                                  *00070000
000800*    Author:        Richard J. Luketich                          *00080000
000900*                                                                *00090000
001000*    Function:                                                   *00100000
001100*       This member is used to provide a list of provider types. *00110000
001200*                                                                *00120000
001300*       Typical uses for this overlay are to map a list of       *00130000
001400*       provider types to be matched against against a GCPS      *00140000
001500*       tabular or other list containing provider types. This is *00150000
001600*       useful in processes involving confidence factor          *00160000
001700*       development, for which this member was originally        *00170000
001800*       developed.                                               *00180000
001900*                                                                *00190000
002000*       When used with confidence factor development processes,  *00200000
002100*       the value of the count of the number of occurrences at   *00210000
002200*       beginning of the member indicates the type of matching   *00220000
002300*       being performed. A zero value indicates that general     *00230000
002400*       confidence factor computations are to take place, a      *00240000
002500*       value of one indicates that a specific provider type is  *00250000
002600*       to be matched and a value greater than zero indicates a  *00260000
002700*       list match. If a single value or list match is to be     *00270000
002800*       performed, general confidence factor calculations are    *00280000
002900*       not performed.                                           *00290000
003000*                                                                *00300000
003100******************************************************************00310000
003200*                                                                *00320000
003300*                       Maintenance History                      *00330000
003400*                                                                *00340000
003500*  Mod     Date      By               Action/Reason              *00350000
003600* ----- ----------- ---- --------------------------------------- *00360000
003700* 01.00 07-Aug-1992 RJL1 Created.                                *00370000
003800*                                                                *00380000
003900******************************************************************00390000
004000                                                                  00400000
004100 01  PVTL-PRVDR-TYP-TBL.                                          00410000
004200     02 PVTL-NBR-ENTRS           PICTURE S9(04) COMP.             00420000
004300        88 PVTL-MAX-NGR-ENTRS    VALUE +1000.                     00430000
004400     02 PVTL-TBL.                                                 00440000
004500        03 PVTL-ENTRY            OCCURS 1 TO 1000 TIMES           00450000
004600                                 DEPENDING ON PVTL-NBR-ENTRS      00460000
004700                                 INDEXED BY PVTL-IDX              00470000
004800                                            PVTL-MAX-IDX.         00480000
004900           04 PVTL-PRVDR-TYP     PICTURE  X(02).                  00490000
005000                                                                  00500000
