000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSDXSLC                                     *00050000
000600*    Member Title:  Diagnosis Code Match List                    *00060000
000700*    Date Created:  07-Aug-1992                                  *00070000
000800*    Author:        Richard J. Luketich                          *00080000
000900*                                                                *00090000
001000*    Function:                                                   *00100000
001100*       This member is used to provide a list of diagnosis       *00110000
001200*       codes.                                                   *00120000
001300*                                                                *00130000
001400*       Typical uses for this overlay are to map a list of       *00140000
001500*       diagnosis codes to be matched against  against a GCPS    *00150000
001600*       tabular or other list containing diagnosis codes. This   *00160000
001700*       is useful in processes involving confidence factor       *00170000
001800*       development, for which this member was originally        *00180000
001900*       developed.                                               *00190000
002000*                                                                *00200000
002100******************************************************************00210000
002200*                                                                *00220000
002300*                       Maintenance History                      *00230000
002400*                                                                *00240000
002500*  Mod     Date      By               Action/Reason              *00250000
002600* ----- ----------- ---- --------------------------------------- *00260000
002700* 01.00 07-Aug-1992 RJL1 Created.                                *00270000
002800*                                                                *00280000
002900******************************************************************00290000
003000                                                                  00300000
003100 01  DXSL-DX-TBL.                                                 00310000
003200     02 DXSL-NBR-ENTRS           PICTURE S9(04) COMP.             00320000
003300        88 DXSL-MAX-NBR-ENTRS    VALUE +1000.                     00330000
003400     02 DXSL-TBL.                                                 00340000
003500        03 DXSL-ENTRY            OCCURS 1 TO 1000 TIMES           00350000
003600                                 DEPENDING ON DXSL-NBR-ENTRS      00360000
003700                                 INDEXED BY DXSL-IDX              00370000
003800                                            DXSL-MAX-IDX.         00380000
003900           04 DXSL-DX            PICTURE  X(06).                  00390000
