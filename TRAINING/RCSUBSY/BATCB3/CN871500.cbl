00001 *      LAST MAINTENANCE TIME: 16.30.17  DATE: 01/11/88            11/06/01
00002 *      LAST MAINTENANCE TIME: 15.30.42  DATE: 11/28/87            CN871500
00003  IDENTIFICATION DIVISION.                                            LV001
00004                                                                   CN871500
00005  PROGRAM-ID.    CN871500.                                         CN871500
00008  DATE-WRITTEN.  OCTOBER 1993.                                     CN871500
00009  DATE-COMPILED.                                                   CN871500
00010                                                                   CN871500
00116  WORKING-STORAGE SECTION.                                         CN871500
00117                                                                   CN871500
00118  01  PAR9708-RECORD.                                              CN871500
00119  COPY INV9708.                                                    CN871500
00120 /                                                                 CN871500
00121  01  WS300-CONSTANT-VALUES.                                       CN871500
00122      05  WS300-ONE               PIC S9(1)       COMP-3           CN871500
00123                                                  VALUE +1.        CN871500
00124      05  WS300-TWO               PIC S9(1)       COMP-3           CN871500
00125                                                  VALUE +2.        CN871500
00126      05  WS300-YES               PIC X(01)       VALUE 'Y'.       CN871500
00127      05  WS300-NO                PIC X(01)       VALUE 'N'.       CN871500
00128      05  WS300-CHAR-ONE          PIC X(01)       VALUE '1'.       CN871500
00129                                                                   CN871500
00130      05  WS300-CLEAR-RECORD      COMP-3.                          CN871500
00131          10  FILLER              PIC S9(07)      VALUE ZEROES.    CN871500
00132          10  FILLER              PIC S9(05)      VALUE ZEROES.    CN871500
00133          10  FILLER              PIC S9(05)      VALUE ZEROES.    CN871500
00755  5000-OUTPUT-EXIT.                                                CN871500
00756      EXIT.                                                        CN871500
