000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSSRTPC                                        *00030000
000400*    DATE:       25-SEP-1986                                     *00040000
000500*    AUTHOR:     RICHARD J. LUKETICH                             *00050000
000600*    FUNCTION:   CONTROL BLOCKS FOR EACH SUBROUTINE THAT         *00060000
000700*                REQUIRES PARAMETERS OTHER THAN THE STANDARD     *00070000
000800*                CIA OR IOP AREAS                                *00080000
000900*                                                                *00090000
001000******************************************************************00100000
001100*                                                                *00110000
001200*                      MAINTENANCE HISTORY                       *00120000
001300*                                                                *00130000
001400*  MOD     DATE     BY  DRPT                ACTION               *00140000
001500* ----- ----------- --- ----- ---------------------------------- *00150000
001600* 01.00 25-SEP-1986 RJL       CREATED                            *00160000
001700* 01.01 30-SEP-1986 AMJ       ADDED ONE MORE 88-LEVEL FOR        *00170000
001800*                             ELGCACUM PER DISCUSSION LAST WEEK  *00180000
001900* 01.02 28-OCT-1986 AMJ       ADDED SUPPORT FOR INTERNAL TABULAR *00190000
002000*                             PROGRAMS PER INSTRUCTIONS          *00200000
002100* 01.03 06-MAR-1987 NAC       ADDED SUPPORT FOR INTERNAL TABULAR *00210000
002200*                             PROGRAMS- ACCUM TYPE, INTERNAL     *00220000
002300*                             DESCRIPTOR VALUE.                  *00230000
002400*                             NOTE: UPON REQUEST INTERNAL DES-   *00240000
002500*                             CRIPTOR WILL NOT BE USED AT THIS   *00250000
002600*                             TIME BUT IT'S USE IS ANTICIPATED.  *00260000
002700*                                                                *00270000
002800* 01.04 22-JUN-1987 LET       ADDED PARAMETERS FOR NEW PROCESSING*00280000
002900*                             OF THE ACCUMULATORS AT ALL LEVELS. *00290000
003000*                                                                *00300000
003100* 01.05 28-JUL-1987 AKK       ADDED PARMS FOR PROCESSING G-TAB-  *00310000
003200*                             ULARS (ID, SLOT-NO AND PROGRAM     *00320000
003300*                             NAME).                             *00330000
003400*                                                                *00340000
003500* 01.06 13-AUG-1987 REB       ADDED PARM NEEDED FOR THE GENERATOR*00350000
003600*                             (ELGCBRI).                         *00360000
003700*                                                                *00370000
003800* 01.07 17-AUG-1987 LET       REDESIGNED THE COPYBOOK.           *00380000
003900*                                                                *00381000
004000* 01.08 31-AUG-1987 LET       ADDED AN 88 LEVEL FOR ACCUMS AT    *00382000
004100*                             THE GROUP SPECIFIC & CONTRACT LEVEL*00383000
004200*                                                                *00383100
004300* 01.09 16-SEP-1987 LET       ADDED SWITCH TO LET THE GRP. SPEC. *00384000
004400*                             & CONTR. LEVEL KNOW THAT ACCUMS    *00385000
004500*                             EXIST AT THE BEN PROV LEVEL.       *00386000
004600*                                                                *00387000
004700* 01.10 25-SEP-1987 LET       ADDED SWITCH SO THAT A BENEFIT     *00388000
004800*                             PROVISION TOPIC CAN INDICATE WHERE *00389000
004900*                             IT'S CURRENTLY PROCESSING AT (I-IP,*00389100
005000*                             I-OP, P-IP, OR P-OP).              *00389200
005100*                                                                *00389300
005200* 01.11 08-OCT-1987 LET       CHANGED DATA NAME FROM             *00389400
005300*                             SRP-BP-PROV-CLASS-SERV-PTR  TO     *00389500
005400*                             SRP-BP-PYMT-LEVEL-REC-PTR.         *00389600
005500*                                                                *00389700
005600* 01.12 09-NOV-1987 LET       ADDED A PARAMENTER NAMED           *00389800
005700*                             SRP-ACCUM-INT-PERCENT AS REQUESTED *00389900
005800*                             BY REB.                            *00390000
005810*                                                                *00390100
005820* 01.13 03-JAN-1992 RJL       ADDED 'BOTH' PROVIDER CLASS.       *00390200
005900*                             MADE COSMETIC CHANGES FOR          *00390300
005910*                             READABILITY/COMPACTNESS.           *00390400
005920*                                                                *00390500
005820* 01.14 21-AUG-2000 AKK       ADDED PROVIDER SPECIALTY PARMS     *00390601
005900*                             IN SUPPORT OF #IPGS TABULAR      *  00390701
005920*                                                                *00390901
006000******************************************************************00391000
006100                                                                  00400000
006200 01  SRP-SUBROUTINE-PARAMETERS.                                   00410000
006300     02 SRP-ELSACCUM-PARMS.                                       00420000
006400        03  SRP-ACCUM-PROVIDER-CLASS                              00430000
006500                                 PICTURE  X(01).                  00440000
006600            88 SRP-ACCUM-PROV-CLASS-INST  VALUE 'I'.              00450000
006700            88 SRP-ACCUM-PROV-CLASS-PROF  VALUE 'P'.              00460000
006800            88 SRP-ACCUM-PROV-CLASS-SUPP  VALUE 'S'.              00470000
006810            88 SRP-ACCUM-PROV-CLASS-BOTH  VALUE 'B'.              00471000
006400        03  SRP-ACCUM-PROVIDER-SPEC                               00472001
006500                                 PICTURE  X(01).                  00473001
006600            88 SRP-ACCUM-PROV-SPEC-INST  VALUE 'I'.               00474001
006700            88 SRP-ACCUM-PROV-SPEC-PROF  VALUE 'P'.               00475001
006800            88 SRP-ACCUM-PROV-SPEC-SUPP  VALUE 'S'.               00476001
006810            88 SRP-ACCUM-PROV-SPEC-BOTH  VALUE 'B'.               00477001
006900        03  SRP-ACCUM-LEVEL-TYPE PICTURE  X(02).                  00480000
007000            88  SRP-BEN-PROV-ACCUM        VALUE 'BP'.             00490000
007100            88  SRP-COST-CONT-ACCUM       VALUE 'CC'.             00500000
007200            88  SRP-GROUP-CONTR-ACCUM     VALUE 'GC'.             00501000
007300            88  SRP-TOPIC-ACCUM           VALUE 'TP'.             00510000
007400        03  SRP-ACCUM-FOUND-Y-OR-N                                00520000
007500                                 PICTURE  X(01).                  00530000
007600            88  SRP-NO-ACCUMS-FOUND       VALUE 'N'.              00540000
007700            88  SRP-YES-ACCUMS-FOUND      VALUE 'Y'.              00550000
007800        03  SRP-ACCUM-NOT-APPLICABLE                              00560000
007900                                 PICTURE  X(01).                  00570000
008000            88  SRP-INST-NOT-APPLICABLE   VALUE 'I'.              00580000
008100            88  SRP-PROF-NOT-APPLICABLE   VALUE 'P'.              00590000
008200     02 SRP-TABULAR-PARMS.                                        00591000
008300        03  SRP-ACCUM-TABULAR-KEY.                                00600000
008400            05  SRP-ACCUM-TAB-ID PICTURE  X(06).                  00610000
008500            05  SRP-ACCUM-SLOT-NBR                                00620000
008600                                 PICTURE S9(07) COMP-3.           00630000
008700        03 SRP-INTERNAL-TAB REDEFINES SRP-ACCUM-TABULAR-KEY.      00640000
008800           05  SRP-INTERNAL-TAB-ID                                00650000
008900                                 PICTURE  X(06).                  00660000
009000           05  SRP-INTERNAL-TAB-SLOT-NBR                          00670000
009100                                 PICTURE S9(07) COMP-3.           00680000
009200        03 SRP-GTABULAR REDEFINES SRP-ACCUM-TABULAR-KEY.          00690000
009300           05  SRP-TABULAR-ID    PICTURE X(06).                   00700000
009400           05  SRP-TABULAR-SLOT-NO                                00710000
009500                                 PICTURE S9(07) COMP-3.           00720000
009600     02 SRP-INTERNAL-TAB-PARMS.                                   00730000
009700        03 SRP-ACCUM-RECORD-PREFIX                                00740000
009800                                 PICTURE  X(08).                  00750000
009900        03 SRP-INTERNAL-DESCRIPTOR                                00760000
010000                                 PICTURE  X(09).                  00770000
010100     02 SRP-CCP-PARMS.                                            00780000
010200        03  SRP-COST-CONT-TYPE   PICTURE  X(02).                  00790000
010300        03  SRP-CCP-COMB-BENE-REDUCT-IND                          00800000
010400                                 PICTURE  X(02).                  00810000
010500        03  SRP-CCP-NAME         PICTURE  X(45).                  00820000
010600     02 SRP-ACCUM-GC-BP.                                          00830000
010700        03  SRP-ACCUM-GC-BP-SW   PICTURE  X(02).                  00840000
010800            88  ACCUM-GC-BP-NONE          VALUE '  '.             00850000
010900            88  ACCUM-GC-HAS-BP           VALUE 'BP'.             00860000
011000            88  ACCUM-BP-HAS-GC           VALUE 'GC'.             00870000
011100     02 SRP-BEN-PROVN-TOPIC-PARMS.                                00880000
011200        03  SRP-BP-PYMT-LEVEL-REC-PTR                             00881000
011300                                 POINTER  VALUE NULL.             00882000
011400        03  SRP-BP-PROV-CLASS-SERV-SW                             00890000
011500                                 PICTURE  X(03).                  00891000
011600            88  BEN-PROV-INST-IP          VALUE 'IIP'.            00900000
011700            88  BEN-PROV-INST-OP          VALUE 'IOP'.            00910000
011800            88  BEN-PROV-PROF-IP          VALUE 'PIP'.            00920000
011900            88  BEN-PROV-PROF-OP          VALUE 'POP'.            00930000
012000     02 SRP-ACCUM-INT-PERCENT    PICTURE S9(03) COMP-3.           00940000
