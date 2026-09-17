00001 *      LAST MAINTENANCE TIME: 14.46.44  DATE: 12/13/88            11/06/01
00002 *      LAST MAINTENANCE TIME: 15.32.08  DATE: 11/28/87            CN872000
00003 *      LAST MAINTENANCE TIME: 11.03.12  DATE: 05/12/87               LV001
00004  IDENTIFICATION DIVISION.                                         CN872000
00005  PROGRAM-ID.     CN872000.                                        CN872000
00008  DATE-WRITTEN.   OCTOBER 1993.                                    CN872000
00009  DATE-COMPILED.                                                   CN872000
00010                                                                   CN872000
00139  0000-MAINLINE.                                                   CN872000
00140                                                                   CN872000
00141      PERFORM 0100-INITIALIZE                                      CN872000
00142         THRU 0100-EXIT.                                           CN872000
00143      PERFORM 1000-PROCESS-REC                                     CN872000
00144         THRU 1000-EXIT                                            CN872000
00145         UNTIL WS200-EOF.                                          CN872000
00146      PERFORM 2000-EOJ-PROCESS                                     CN872000
00147         THRU 2000-EXIT.                                           CN872000
00148      STOP RUN.                                                    CN872000
00149  0000-EXIT.                                                       CN872000
00150      EXIT.                                                        CN872000
00279  9020-EXIT.                                                       CN872000
00280      EXIT.                                                        CN872000
