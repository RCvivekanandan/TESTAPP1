000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    CN860001.                                         00000200
000500 DATE-WRITTEN.  12-23-13.                                         00000500
000600 DATE-COMPILED.                                                   00000600
003800****************************************************************  00002500
003900/                                                                 00002600
004000                                                                  00002700
004100 ENVIRONMENT DIVISION.                                            00002800
004200 CONFIGURATION SECTION.                                           00002900
004300 SOURCE-COMPUTER.  AMDAHL-5890.                                   00003000
004400 OBJECT-COMPUTER.  AMDAHL-5890.                                   00003100
004500 INPUT-OUTPUT SECTION.                                            00003200
004600                                                                  00003300
005200 DATA DIVISION.                                                   00003900
005300 FILE SECTION.                                                    00004000
                                                                        00004110
005500 FD  RCSUBSYTORY-FILE                                               00004200
005600     RECORDING MODE IS F                                          00004300
005700     LABEL RECORDS ARE STANDARD                                   00004400
005800     BLOCK CONTAINS 0 RECORDS                                     00004500
006000     DATA RECORD IS RCSUBSYTORY-REC.                                00004600
071100 900-EXIT.                                                        00029600
071200     EXIT.                                                        00029700
                                                                        00029710
