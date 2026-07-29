00001  IDENTIFICATION DIVISION.                                         12/09/02
00002 *THIS IS A COBOL/2 PROGRAM                                        GC0071  
00003  PROGRAM-ID.    GC0071.                                              LV002
00004  AUTHOR.        DIANE UMPOROWICZ.                                 GC0071  
00005  DATE-WRITTEN.  AUGUST 1996.                                      GC0071  
00006  DATE-COMPILED.                                                   GC0071  
00007 ******************************************************************GC0071  
00008 *    THE PURPOSE OF THIS PROGRAM IS TO READ THE CDE PEND CONTROL *GC0071  
00009 *    FILE WHICH DETERMINES IF THE PROCS READING THE CDE PEND FILE*GC0071  
00010 *    WILL BE RAN.                                                *GC0071  
00011 ******************************************************************GC0071  
00012 **              **MODIFICATIONS:**                              **GC0071  
00013 *                                                                *GC0071  
00014 * ISSR    DATE     ANALYST  COMMENT                              *GC0071  
00015 * -----  --------    ---    ------------------------------------ *GC0071  
00016 *        08/29/96    DAU    ORIGINAL MODULE                      *GC0071  
00017 *15057   09/11/97    AB   ADDED CODE TO SUPPORT THE YEAR         *GC0071  
00018 *                           2000 AND THE EXPANSION OF THE        *GC0071  
00019 *                           CONTRACT KEY TO SUPPORT THE TX       *GC0071  
00020 *                           MERGER.                              *GC0071  
00021 *                                                                *GC0071  
00022 ******************************************************************GC0071  
00023                                                                   GC0071  
00024  ENVIRONMENT DIVISION.                                            GC0071  
00025  CONFIGURATION SECTION.                                           GC0071  
00026  SOURCE-COMPUTER.  IBM-370.                                       GC0071  
00027  OBJECT-COMPUTER.  IBM-370.                                       GC0071  
00028                                                                   GC0071  
00029  INPUT-OUTPUT SECTION.                                            GC0071  
00030  FILE-CONTROL.                                                    GC0071  
00031                                                                   GC0071  
00032      SELECT CHKPEND-FILE     ASSIGN TO UT-S-GC0071A.              GC0071  
00033                                                                   GC0071  
00034  DATA DIVISION.                                                   GC0071  
00035  FILE SECTION.                                                    GC0071  
00036                                                                   GC0071  
00037  FD  CHKPEND-FILE                                                 GC0071  
00038      BLOCK CONTAINS 0 RECORDS                                     GC0071  
00039      LABEL RECORDS ARE STANDARD                                   GC0071  
00040      RECORDING MODE IS F.                                         GC0071  
00041                                                                   GC0071  
00042  01  CHKPEND-REC-IN.                                              GC0071  
00043      05 CHKPEND-FLAG                PIC X.                        GC0071  
00044      05 CHKPEND-FILLER              PIC X(79).                    GC0071  
00045                                                                   GC0071  
00046 /                                                                 GC0071  
00047  WORKING-STORAGE SECTION.                                         GC0071  
00048  01  FILLER                         PIC X(31) VALUE               GC0071  
00049                               'WORKING STORAGE PGM GC0077     '.  GC0071  
00050                                                                   GC0071  
00051  01  WS-MISC.                                                     GC0071  
00052      05  EOF-FLAG                  PIC X(1) VALUE 'N'.            GC0071  
00053      05  WS-RETURN-CODE            PIC 9(4) VALUE 0000.           GC0071  
00054                                                                   GC0071  
00055  01  FILLER                         PIC X(25) VALUE               GC0071  
00056                                'WORKING STORAGE ENDS HERE'.       GC0071  
00057  LINKAGE SECTION.                                                 GC0071  
00058 /                                                                 GC0071  
00059  PROCEDURE DIVISION.                                              GC0071  
00060 /*****************************************************************GC0071  
00061 ******************************************************************GC0071  
00062  0000-MAINLINE.                                                   GC0071  
00063                                                                   GC0071  
00064       PERFORM 1000-PERFORM-HOUSEKEEPING THRU 1000-EXIT.           GC0071  
00065       PERFORM 2000-PROCESS-RUN-FILE THRU 2000-EXIT                GC0071  
00066         UNTIL EOF-FLAG = 'Y'.                                     GC0071  
00067       PERFORM 3000-PERFORM-EOJ-ROUTINE THRU 3000-EXIT.            GC0071  
00068       STOP RUN.                                                   GC0071  
00069                                                                   GC0071  
00070  0000-EXIT.                                                       GC0071  
00071      EXIT.                                                        GC0071  
00072 /*****************************************************************GC0071  
00073 ******************************************************************GC0071  
00074  1000-PERFORM-HOUSEKEEPING.                                       GC0071  
00075                                                                   GC0071  
00076      PERFORM 1100-OPEN-FILE THRU 1100-EXIT.                       GC0071  
00077      PERFORM 1200-READ-FILE THRU 1200-EXIT.                       GC0071  
00078                                                                   GC0071  
00079  1000-EXIT.                                                       GC0071  
00080      EXIT.                                                        GC0071  
00081 /*****************************************************************GC0071  
00082 ******************************************************************GC0071  
00083  1100-OPEN-FILE.                                                  GC0071  
00084                                                                   GC0071  
00085      OPEN INPUT CHKPEND-FILE.                                     GC0071  
00086                                                                   GC0071  
00087  1100-EXIT.                                                       GC0071  
00088      EXIT.                                                        GC0071  
00089 /*****************************************************************GC0071  
00090 ******************************************************************GC0071  
00091  1200-READ-FILE.                                                  GC0071  
00092                                                                   GC0071  
00093      READ CHKPEND-FILE                                            GC0071  
00094          AT END MOVE 'Y' TO EOF-FLAG.                             GC0071  
00095                                                                   GC0071  
00096  1200-EXIT.                                                       GC0071  
00097      EXIT.                                                        GC0071  
00098 /**************************************************************   GC0071  
00099 ***************************************************************   GC0071  
00100  2000-PROCESS-RUN-FILE.                                           GC0071  
00101                                                                   GC0071  
00102      IF CHKPEND-FLAG = 'Y'                                        GC0071  
00103          MOVE 4 TO WS-RETURN-CODE.                                GC0071  
00104      PERFORM 1200-READ-FILE THRU 1200-EXIT.                       GC0071  
00105                                                                   GC0071  
00106  2000-EXIT.                                                       GC0071  
00107      EXIT.                                                        GC0071  
00108 /**************************************************************   GC0071  
00109 ***************************************************************   GC0071  
00110  3000-PERFORM-EOJ-ROUTINE.                                        GC0071  
00111                                                                   GC0071  
00112      CLOSE CHKPEND-FILE.                                          GC0071  
00113      MOVE WS-RETURN-CODE TO RETURN-CODE.                          GC0071  
00114                                                                   GC0071  
00115  3000-EXIT.                                                       GC0071  
00116      EXIT.                                                        GC0071  
00117 /**************************************************************   GC0071  
