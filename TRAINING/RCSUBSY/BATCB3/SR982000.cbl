000100**     LAST MAINTENANCE TIME: 12.59.46  DATE: 06/04/87            00000100
000500 IDENTIFICATION DIVISION.                                         00000500
000600 PROGRAM-ID.    SR982000.                                         00000600
001000 DATE-COMPILED.                                                   00001000
001100                                                                  00001100
006400 ENVIRONMENT DIVISION.                                            00006400
006500 CONFIGURATION SECTION.                                           00006500
006600 SOURCE-COMPUTER.  IBM-3831.                                      00006600
006700 OBJECT-COMPUTER.  IBM-3831.                                      00006700
006800 INPUT-OUTPUT SECTION.                                            00006800
006900/                                                                 00006900
007000 FILE-CONTROL.                                                    00007000
013900 01  WS200-FLAGS.                                                 00013900
014000     05  WS200-ACTIVE-FLAG       PIC X     VALUE 'N'.             00014000
014100         88  WS200-ACT-ON                  VALUE 'Y'.             00014100
014200         88  WS200-ACT-OFF                 VALUE 'N'.             00014200
044800 900-EXIT.                                                        00044800
044900     EXIT.                                                        00044900
