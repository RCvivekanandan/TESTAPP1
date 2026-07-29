000100******************************************************************00010001
000200*                                                                *00020001
000300*    COPYBOOK:   ELSPRPAC                                        *00030001
000400*    DATE:       10-JUN-1987                                     *00040001
000500*    AUTHOR:     EDWARD G. LISS                                  *00050001
000600*    FUNCTION:   PRINTER PARAMETER AREA.                         *00060001
000700*                                                                *00070001
000800*                PASSES INFORMATION TO THE 'BACKGROUND' PRINT    *00080001
000900*                PRINT FUNCTION.                                 *00090001
001000*                                                                *00100001
001100******************************************************************00110001
001200*                                                                *00120001
001300*                      MAINTENANCE HISTORY                       *00130001
001400*                                                                *00140001
001500*  MOD     DATE     BY  DRPT                ACTION               *00150001
001600* ----- ----------- --- ----- ---------------------------------- *00160001
001700* 01.00 10-JUN-1987 EGL       CREATED                            *00170001
001800* 01.01 24-FEB-1988 EGL       ADD APPLID FIELD                   *00180001
001900*                                                                *00190001
002000******************************************************************00200001
002100                                                                  00210001
002200 01  PPB-PRINT-PARAMETER-BLOCK.                                   00220001
002300     05  PPB-TRAN-DATE           PICTURE S9(5)    COMP-3.         00230001
002400     05  PPB-TRAN-TIME           PICTURE S9(6)    COMP-3.         00240001
002500     05  PPB-TSQ-NAME            PICTURE X(8).                    00250001
002600     05  PPB-OWNER-APPLID        PICTURE X(8).                          01
002700     05  PPB-OWNER-SYSID         PICTURE X(4).                    00260001
002800     05  PPB-TERM-ID             PICTURE X(4).                    00270001
002900     05  PPB-PRINTER-ID          PICTURE X(4).                    00280001
