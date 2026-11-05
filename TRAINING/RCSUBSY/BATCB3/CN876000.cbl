00001 *      LAST MAINTENANCE TIME:  8.54.42  DATE: 12/11/87            11/06/01
00002  IDENTIFICATION DIVISION.                                         CN876000
00003  PROGRAM-ID.    CN876000.                                            LV002
00006  DATE-WRITTEN.  FEBRUARY, 1982.                                   CN876000
00007  DATE-COMPILED.                                                   CN876000
00143                                                                   CN876000
00144  0000-MAINLINE.                                                   CN876000
00145                                                                   CN876000
00146      PERFORM 1000-INITIALIZE                                      CN876000
00147         THRU 1000-EXIT.                                           CN876000
00148      PERFORM 2000-PROCESS-RECORD                                  CN876000
00149         THRU 2000-EXIT                                            CN876000
00150         UNTIL WS200-EOF.                                          CN876000
00151      PERFORM 3000-END-PROCESSING                                  CN876000
00152         THRU 3000-EXIT.                                           CN876000
00153                                                                   CN876000
00154      STOP RUN.                                                    CN876000
00155                                                                   CN876000
00156  0000-EXIT.                                                       CN876000
00157      EXIT.                                                        CN876000
00365  9100-EXIT.                                                       CN876000
00366      EXIT.                                                        CN876000
