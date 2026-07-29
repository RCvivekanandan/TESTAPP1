000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSPVTSC                                     *00050001
000600*    Member Title:  Provider Spec Match List                     *00060001
000700*    Date Created:  25-Aug-2000                                  *00070001
000800*    Author:        Anne Keffer King                             *00080001
000900*                                                                *00090000
001000*    Function:                                                   *00100000
001100*       This member is used to provide a list of provider spec.  *00110001
001200*                                                                *00120000
001300*       Typical uses for this overlay are to map a list of       *00130000
001400*       provider spec  to be matched against against a GCPS      *00140001
001500*       tabular or other list containing provider spec . This is *00150001
001600*       useful in processes involving confidence factor          *00160000
001700*       development, for which this member was originally        *00170000
001800*       developed.                                               *00180000
001900*                                                                *00190000
002000*       When used with confidence factor development processes,  *00200000
002100*       the value of the count of the number of occurrences at   *00210000
002200*       beginning of the member indicates the type of matching   *00220000
002300*       being performed. A zero value indicates that general     *00230000
002400*       confidence factor computations are to take place, a      *00240000
002500*       value of one indicates that a specific provider spec is  *00250001
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
003700* 01.00 25-Aug-2000 AKK  Created and cloned from elspvtlc.  Is   *00370001
003800*                        used for provider specialties.          *00380001
003900******************************************************************00390000
004000                                                                  00400000
004100 01  PVSL-PRVDR-SPC-TBL.                                          00410001
004200     02 PVSL-NBR-ENTRS           PICTURE S9(04) COMP.             00420001
004300        88 PVSL-MAX-NGR-ENTRS    VALUE +1000.                     00430001
004400     02 PVSL-TBL.                                                 00440001
004500        03 PVSL-ENTRY            OCCURS 1 TO 1000 TIMES           00450001
004600                                 DEPENDING ON PVSL-NBR-ENTRS      00460001
004700                                 INDEXED BY PVSL-IDX              00470001
004800                                            PVSL-MAX-IDX.         00480001
004900           04 PVSL-PRVDR-SPC     PICTURE  X(03).                  00490001
005000                                                                  00500000
