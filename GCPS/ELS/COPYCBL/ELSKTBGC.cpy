000100******************************************************************00010002
000200*                                                                *00020002
000300*    COPYBOOK:   ELSKTBGC                                        *00030002
000400*    DATE:       16-SEP-1986                                     *00040002
000500*    AUTHOR:     RICHARD J. LUKETICH                              00050002
000600*    FUNCTION:   GROUP SPECIFIC KEYS FOR GIVE GROUP/SECTION      *00060002
000700*                                                                *00070002
000800******************************************************************00080002
000900*                                                                *00090002
001000*                      MAINTENANCE HISTORY                       *00100002
001100*                                                                *00110002
001200*  MOD     DATE     BY  DRPT                ACTION               *00120002
001300* ----- ----------- --- ----- ---------------------------------- *00130002
001400* 01.00 16-SEP-1986 RJL       CREATED                            *00140002
001500* 01.01 29-SEP-1986 EGL       CORRECTED TYPO                     *00150002
001600* 01.02 28-OCT-1986 RJL       ADD TERMINATION DATE.              *00160002
001700* 01.03 05-NOV-1986 RJL       ADD LOB CONTRACT LEVEL INDICATOR.  *00170002
001800* 01.04 17-NOV-1986 NAC       CORRECTED TYPO                     *00180002
001900* 01.05 17-NOV-1986 RJL       ADDED TABLE FULL CONDITION.        *00190002
002000* 01.06 15-JAN-1987 RJL       ADDED CONTRACT PROVIDER CONTROL    *00200002
002100*                             LEVELS INDICATORS                  *00210002
002300* 01.07 20-JAN-1988 LET       ADDED PARTICIPATING PROVIDER       *00230002
002400*                             OPTION (03 LEVEL) AS REQUESTED BY  *00240002
002500*                             EGL.                               *00250002
002700* 01.08 14-OCT-1988 LET       ADDED NEW 88 LEVEL FOR KTG-IND     *00270002
002800*                             AS REQUESTED BY JPB.               *00280002
002810* 01.09 27-SEP-1991 RJL       EXPANDED FAM-REL-LVL'S TO 2 BYTES. *00281002
002900*                             ADDED MCN/POS PARTICIPATION        *00290002
002910*                             INDICATORS FOR NOTICES SCREEN.     *00291002
002810* 01.10 04-JAN-1993 AKK       ADDED RPO PARTICIPATION IND        *00291103
      *                             FOR NOTICES SCREEN.                *00291203
002810* 01.11 03-FEB-1994 AKK       ADDED PRODUCT TYPE IND             *00291304
002810* 01.12 20-FEB-1995 AKK       ADDED CPO INDICATOR                *00291405
002920*                                                                *00292002
002810* 01.13 23-MAY-1995 AKK       ADDED BLUE SCRIPT INDICATOR        *00293012
002920*                                                                *00294012
002810* 01.14 12-SEP-1995 AKK       ADDING SWITCH TO ACCOMODATE        *00295007
002920*                             ALLIANCE.  THIS WILL BE            *00296007
002920*                             POPULATED FROM MEMBERSHIP.         *00296107
      *                             NORMALLY ALL THESE ITEMS            00296208
      *                             ARE FROM GROUP SPECIFIC.            00296308
002920*                                                                *00297007
002810* 01.15 22-FEB-1996 AKK       ADDED COMMUNITY BLUE AND           *00298012
      *                             PREFERRED ANCILARY NETWORK.         00298112
002920*                                                                *00299012
002810* 01.16 07-AUG-1997 AKK       ADDED CHANGES DUE TO YEAR          *00299113
      *                             2000.                               00299213
002920*                                                                *00299313
003000******************************************************************00300002
003100                                                                  00310002
003200 01  KTG-GCGRPSPC-KEY-TABLE.                                      00320002
003300     02 KTG-NBR-KEYS             PICTURE S9(04)          COMP.    00330002
003400        88 KTG-TBL-FULL          VALUE +100.                      00340002
003500     02 KTG-KEY-TBL              OCCURS 1 TO 100 TIMES            00350002
003600                                 DEPENDING ON KTG-NBR-KEYS        00360002
003700                                 INDEXED BY KTG-IDX.              00370002
003800        03 KTG-PLAN-CODE         PICTURE  X(03).                  00380016
003800        03 KTG-PKG-CODE          PICTURE  X(03).                  00381016
003800        03 KTG-FAM-REL-LVL       PICTURE  X(02).                  00390015
              03 KTG-EFFECTIVE-DATE.                                    00401013
                 05 KTG-EFF-DT-CC      PICTURE X.                       00402013
                 05 KTG-EFF-DT         PICTURE S9(05) COMP-3.           00403013
              03 KTG-EFF-DT-CENTURY REDEFINES KTG-EFFECTIVE-DATE        00404013
                                       PICTURE S9(07) COMP-3.           00405014
              03 KTG-TERMINATION-DATE.                                  00406013
                 05 KTG-TERMN-DT-CC  PICTURE X.                         00407013
                 05 KTG-TERMN-DT     PICTURE S9(05)    COMP-3.          00408013
              03 KTG-TERM-DT-CENTURY REDEFINES KTG-TERMINATION-DATE     00409013
                                       PICTURE S9(07) COMP-3.           00409114
004100        03 KTG-L-O-B-CONTRACT-LEVEL-IND                           00410002
004200                                 PICTURE  X(02).                  00420002
004300        03 KTG-PARTICIPAT-PROV-OPTION                             00430002
004400                                 PICTURE  X(02).                  00440002
004420        03 KTG-POS-PARTICP-IND   PICTURE  X(02).                  00442002
004420        03 KTG-RPO-PARTICP-IND   PICTURE  X(02).                  00443003
004420        03 KTG-CPO-PARTICP-IND   PICTURE  X(02).                  00443105
004420        03 KTG-CBL-PARTICP-IND   PICTURE  X(02).                  00443212
004420        03 KTG-PAN-PARTICP-IND   PICTURE  X(02).                  00443312
004440        03 KTG-NEW-POS-IND       PICTURE  X(02).                  00444002
004440        03 KTG-PRODUCT-TYPE      PICTURE  X(09).                  00445004
004440        03 KTG-BLUE-SCRIPT       PICTURE  X(02).                  00446006
004500        03 KTG-PC-LVL-IND-LST.                                    00450002
004600           04 KTG-PC-LVL-INST-BAS-IND                             00460002
004700                                 PICTURE  X(02).                  00470002
004800           04 KTG-PC-LVL-INST-SUP-IND                             00480002
004900                                 PICTURE  X(02).                  00490002
005000           04 KTG-PC-LVL-PROF-BAS-IND                             00500002
005100                                 PICTURE  X(02).                  00510002
005200           04 KTG-PC-LVL-PROF-SUP-IND                             00520002
005300                                 PICTURE  X(02).                  00530002
005400        03 KTG-PC-LVL-IND-TBL    REDEFINES KTG-PC-LVL-IND-LST.    00540002
005500           04 KTG-PC-LVL-IND     PICTURE  X(02)                   00550002
005600                                 OCCURS 4 TIMES                   00560002
005700                                 INDEXED BY KTG-CLV-IDX.          00570002
005800                                                                  00580002
005900        03 KTG-IND               PICTURE  X(01).                  00590002
006000           88 KTG-SEL            VALUE 'S'.                       00600002
006100           88 KTG-REJ            VALUE 'R' 'X'.                   00610002
006200           88 KTG-EXC            VALUE 'X'.                       00620002
 04440     02 KTG-ALLIANCE-IND      PICTURE  X(02).                     00630011
