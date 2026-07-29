000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSTWAC                                         *00030000
000400*    DATE:       22-SEP-1986                                     *00040000
000500*    AUTHOR:     RICHARD J. LUKETICH                             *00050000
000600*    FUNCTION:   TRANSACTION WORK AREA                           *00060000
000700*                                                                *00070000
000810*    NOTES:      THE TRANSACTION WORK AREA IS USED TO STORE THE  *00081000
000820*                ADDRESS OF THE CICS COMMUNICATION AREA IN       *00082000
000830*                EFFECT AT THE TIME OF THE CURRENT TRANSACTION   *00083000
000840*                EXECUTION.  THIS VALUE IS USED BY THE ABEND     *00084000
000850*                HANDLER (ELUABEND) TO LOCATE THE COMMUNICATION  *00085000
000860*                AREA, COMMON INTERFACE AREA AND ANY OTHER AREAS *00086000
000870*                NEEDED TO DIAGNOSE AND REPORT THE ABEND.        *00087000
000900*                                                                *00090000
001000******************************************************************00100000
001100*                                                                *00110000
001200*                      MAINTENANCE HISTORY                       *00120000
001300*                                                                *00130000
001400*  MOD     DATE     BY  DRPT                ACTION               *00140000
001500* ----- ----------- --- ----- ---------------------------------- *00150000
001600* 01.00 22-SEP-1986 RJL       CREATED                            *00160000
001610* 01.01 08-DEC-1986 RJL       ADDED FIELDS FOR PAGE DISPLAY USE. *00161001
001620* 01.02 16-DEC-1986 RJL       ADDED FIELDS FOR PAGE DISPLAY USE. *00162002
001700*                                                                *00170000
001800******************************************************************00180000
001900                                                                  00190000
002000 01  TWA-TRANSACTION-WORK-AREA.                                   00200000
002100     02 TWA-ELSCOMM-PTR          POINTER.                         00210000
002200     02 TWA-COMMAREA-LEN         PICTURE S9(04)          COMP.    00220001
002300     02 TWA-RETURN-PGM-ID        PICTURE  X(08).                  00230001
002400     02 TWA-RETURN-TXN-ID        PICTURE  X(04).                  00240002
