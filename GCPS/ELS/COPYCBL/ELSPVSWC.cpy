000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSPVTsC                                     *00050001
000600*    Member Title:  Weighted Provider Spec Match List            *00060001
000700*    Date Created:  25-Aug-2000                                  *00070001
000800*    Author:        anne keffer king                             *00080001
000900*                                                                *00090000
001000*    Function:                                                   *00100000
001100*       This member is used to provide a list of provider spec   *00110001
001200*       with weights.                                            *00120000
001300*                                                                *00130000
001400*       Typical uses for this overlay are to map a list of       *00140000
001500*       provider spec  with weights to be matched against        *00150001
001600*       against a GCPS tabular or other list containing provider *00160000
001700*       spec.  This is useful in processes involving confidence  *00170001
001800*       factor development, for which this member was originally *00180000
001900*       developed.                                               *00190000
002000*                                                                *00200000
002100*       When used with confidence factor development processes,  *00210000
002200*       the value of the count of the number of occurrences at   *00220000
002300*       beginning of the member indicates the type of matching   *00230000
002400*       being performed. A zero value indicates that general     *00240000
002500*       confidence factor computations are to take place, a      *00250000
002600*       value of one indicates that a specific provider spec is  *00260001
002700*       to be matched and a value greater than zero indicates a  *00270000
002800*       list match. If a single value or list match is to be     *00280000
002900*       performed, general confidence factor calculations are    *00290000
003000*       not performed.                                           *00300000
003100*                                                                *00310000
003200*       NOTE: When computing general confidence factors or       *00320000
003300*       performing specific provider spec matching, the          *00330001
003400*       processing is identical to that used with the unweighted *00340000
003500*       list (ELSPVSLC).                                         *00350001
003600*                                                                *00360000
003700******************************************************************00370000
003800*                                                                *00380000
003900*                       Maintenance History                      *00390000
004000*                                                                *00400000
004100*  Mod     Date      By               Action/Reason              *00410000
004200* ----- ----------- ---- --------------------------------------- *00420000
004300* 01.00 25-Aug-2000 akk  Created and cloned from elspvtwc.       *00430001
004400*                        this copy member handles specialties.   *00440001
004500******************************************************************00450000
004600                                                                  00460000
004700 01  PVSW-PRVDR-SPC-TBL.                                          00470001
004800     02 PVSW-NBR-ENTRS           PICTURE S9(04) COMP.             00480001
004900        88 PVSW-MAX-NGR-ENTRS    VALUE +1000.                     00490001
005000     02 PVSW-TBL.                                                 00500001
005100        03 PVSW-ENTRY            OCCURS 1 TO 1000 TIMES           00510001
005200                                 DEPENDING ON PVSW-NBR-ENTRS      00520001
005300                                 INDEXED BY PVSW-IDX              00530001
005400                                            PVSW-MAX-IDX.         00540001
005500           04 PVSW-PRVDR-SPC     PICTURE  X(03).                  00550002
005600           04 PVSW-WGHT          PICTURE S9(04) COMP.             00560001
005700                                                                  00570000
