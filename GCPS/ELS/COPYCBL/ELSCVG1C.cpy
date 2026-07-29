000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSCVG1C                                     *00050000
000600*    Date Created:  08-Oct-1992                                  *00060000
000700*    Author:        Richard J. Luketich                          *00070000
000800*                                                                *00080000
000900*    Function:                                                   *00090000
001000*       This copy member contains constants used in the English  *00100000
001100*       Contract Inquiry and related systems. The constants in   *00110000
001200*       this member are used in the computation of confidence    *00120000
001300*       factors for accumulator tabulars and their attached      *00130000
001400*       internal tabulars.                                       *00140000
001500*                                                                *00150000
001600*       The values for the number of diagnosis and procedure     *00160000
001700*       codes are fairly stable and should be checked every six  *00170000
001800*       to twelve months. The number of providers is more likely *00180000
001900*       to change and should be checked at least once every      *00190000
002000*       three months. Note, however, that the relevant           *00200000
002100*       documentation implies that the exact numbers are not     *00210000
002200*       likely to have a significantly deleterious effect on any *00220000
002300*       of the computations currently performed.                 *00230000
002400*                                                                *00240000
002500******************************************************************00250000
002600*                                                                *00260000
002700*                       Maintenance History                      *00270000
002800*                                                                *00280000
002900*  Mod     Date      By               Action/Reason              *00290000
003000* ----- ----------- ---- --------------------------------------- *00300000
003100* 01.00 08-Oct-1992 RJL1 Created.                                *00310000
003200*                                                                *00320000
003300******************************************************************00330000
003400                                                                  00340000
003500 01  CVG1-CNSTNTS-GRP-1.                                          00350000
003600     02 CVG1-NBR-CPT-PRCDR-CDS   PICTURE S9(08) COMP              00360000
003700                                 VALUE +10807.                    00370000
003800     02 CVG1-NBR-HCPCS-PRCDR-CDS PICTURE S9(08) COMP              00380000
003900                                 VALUE +4197.                     00390000
004000     02 CVG1-NBR-ICD9-DX-CDS     PICTURE S9(08) COMP              00400000
004100                                 VALUE +13310.                    00410000
004200     02 CVG1-NBR-PRVDRS          PICTURE S9(08) COMP              00420000
004300                                 VALUE +344010.                   00430000
004400                                                                  00440000
