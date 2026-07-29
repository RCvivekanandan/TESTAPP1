000100******************************************************************00010003
000200*                                                                *00020003
000300*    COPYBOOK:   ELSPRKYC                                        *00030003
000400*    DATE:       25-APR-1989                                     *00040003
000500*    AUTHOR:     EDWARD G LISS                                   *00050003
000600*    FUNCTION:   ELS ABEND PROCESSING GROUP AND CONTRACT         *00060003
000700*                KEY HOLD TABLE.                                 *00070003
000800*                                                                *00080003
000900******************************************************************00090003
001000*                                                                *00100003
001100*                      MAINTENANCE HISTORY                       *00110003
001200*                                                                *00120003
001300*  MOD     DATE     BY  DRPT                ACTION               *00130003
001400* ----- ----------- --- ----- ---------------------------------- *00140003
001500* 01.00 25-APR-1989 EGL       CREATED                            *00150003
001510* 01.01 27-SEP-1991 RJL       EXPANDED FAM-REL-LVL'S TO 2 BYTES. *00151003
001600*                                                                *00160003
001510* 01.02 11-AUG-1997 AKK       EXPANDED GROUP/SECTION AND DATES.  *00161004
001600*                             TO ACCOMODATE YEAR 2000.           *00161104
001600*                                                                *00162004
001700******************************************************************00170003
001800                                                                  00180003
001900 01  KYA-KEY-HOLD-AREA.                                           00190003
002000     05  KYA-GROUP-SPEC-KEY.                                      00200003
002100         10  KYA-GSK-PLAN-CODE     PICTURE X(3).                  00210006
002100         10  KYA-GSK-GRP.                                         00210111
                   15 KYA-GSK-GRP-1-3    PICTURE X(3).                  00211011
                   15 KYA-GSK-GROUP      PICTURE X(6).                  00212011
002200         10  KYA-GSK-SECTION.                                     00220005
                   15 KYA-GSK-SECT-1     PICTURE X(1).                  00221005
                   15 KYA-GSK-SECT       PICTURE X(4).                  00222005
002100         10  KYA-GSK-PKG-CODE      PICTURE X(3).                  00223010
002300         10  KYA-GSK-FAM-REL-LVL   PICTURE X(2).                  00230003
002400         10  KYA-GSK-EFF-DATE.                                    00240011
002500             15  KYA-GSK-EFF-DT-CC PICTURE X.                     00250007
002500             15  KYA-GSK-EFF-DT    PICTURE S9(5) COMP-3.          00251011
002400         10  KYA-GSK-EFF-DT-CENTURY REDEFINES                     00252007
                   KYA-GSK-EFF-DATE      PICTURE S9(7) COMP-3.          00253011
002600         10  KYA-GSK-STATUS-SW     PICTURE X.                     00260003
002700             88  KYA-GSK-PRINT           VALUE 'Y'.               00270003
002800     05  KYA-CONTRACT-TYPE         PICTURE S9(4)  COMP.           00280003
002900         88  KYA-CONTRACT-IB             VALUE 1.                 00290003
003000         88  KYA-CONTRACT-IS             VALUE 2.                 00300003
003100         88  KYA-CONTRACT-PB             VALUE 3.                 00310003
003200         88  KYA-CONTRACT-PS             VALUE 4.                 00320003
003300     05  KYA-CONTRACT-KEYS       OCCURS 4 TIMES                   00330003
003400                                 INDEXED BY KYA-IDX.              00340003
003500         10  KYA-CK-STATUS-SW      PICTURE X.                     00350003
003600             88  KYA-CK-PRINT            VALUE 'Y'.               00360003
003700         10  KYA-CK-PLAN-CODE      PICTURE X(3).                  00370006
003700         10  KYA-CK-GROUP.                                        00371006
                   15 KYA-CK-GRP-1-3     PICTURE X(3).                  00372006
                   15 KYA-CK-GRP         PICTURE X(6).                  00373006
003800         10  KYA-CK-SECTION.                                      00380006
003800             15 KYA-CK-SECT-1      PICTURE X(1).                  00381006
003800             15 KYA-CK-SECT        PICTURE X(4).                  00382006
003900         10  KYA-CK-PKG-CODE       PICTURE X(03).                 00390011
003900         10  KYA-CK-L-O-B          PICTURE X.                     00391011
004000         10  KYA-CK-PROVDR-CONTROL PICTURE X(2).                  00400003
004100         10  KYA-CK-FAM-REL-LVL    PICTURE X(2).                  00410003
004200         10  KYA-CK-EFF-DT.                                       00420011
004300             15  KYA-CK-EFF-DT-CC  PICTURE X.                     00430012
004300             15  KYA-CK-EFF-DATE   PICTURE S9(5) COMP-3.          00440007
               10  KYA-CK-EFF-DT-CENTURY REDEFINES                      00450007
                   KYA-CK-EFF-DT         PICTURE S9(7) COMP-3.          00460011
