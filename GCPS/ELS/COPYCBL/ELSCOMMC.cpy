000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSCOMMC                                        *00030000
000400*    DATE:       22-SEP-1986                                     *00040000
000500*    AUTHOR:     RICHARD J. LUKETICH                             *00050000
000600*    FUNCTION:   CICS COMMUNICATION AREA.                        *00060000
000700*                                                                *00070000
000800*    NOTES:      DATA PASSED IN THIS COMMUNICATION AREA IS NOT   *00080000
000900*                USED DIRECLY.  THE ONLY FIELD PASSED IS THE     *00090000
001000*                POINTER TO THE COMMON INTERFACE AREA (CIA),     *00100000
001100*                WHICH IS IN A DIFFERENT LOCATION FROM ONE       *00110000
001200*                INVOCATION OF THE TRANSACTION TO THE NEXT.  THE *00120000
001300*                ONLY PURPUSE SERVED BY HAVING THE COMMUNICATION *00130000
001400*                AREA AT ALL IS TO INDICATE TO THE TRANSACTION   *00140000
001500*                MAINLINE (ELELIQML) THAT IT IS PROCESSING A     *00150000
001600*                SUBSEQUENT INVOCATION (EIBCALEN > 0) AS OPPOSED *00160000
001700*                TO AN INITIAL INVOCATION (EIBCALEN = 0).        *00170000
001800*                                                                *00180000
001900*                THE CIA POINTER FIELD IN THE COMMUNICATION      *00190000
002000*                AREA, THEN, WILL ALWAYS BE SET TO \
002100*                TO EXITING THE TRANSACTION, AND INITIALIZED TO  *00210000
002200*                THE ADDRESS OF THE CIA UPON RESTARTING THE      *00220000
002300*                TRANSACTION.  SEE DOCUMENTATION FOR ELELIQML    *00230000
002400*                FOR FURTHER DETAIL.                             *00240000
002500*                                                                *00250000
002600******************************************************************00260000
002700*                                                                *00270000
002800*                      MAINTENANCE HISTORY                       *00280000
002900*                                                                *00290000
003000*  MOD     DATE     BY  DRPT                ACTION               *00300000
003100* ----- ----------- --- ----- ---------------------------------- *00310000
003200* 01.00 22-SEP-1986 RJL       CREATED                            *00320000
003210* 01.01 22-SEP-1986 RJL       DELETED 01 LEVEL DUE TO NEW VS     *00321001
003300*                             COBOL II COPY CLAUSE PROCESSING.   *00330001
003310*                                                                *00331001
003400******************************************************************00340000
003500                                                                  00350000
003700     02 ECA-CIA-PTR              POINTER.                         00370000
