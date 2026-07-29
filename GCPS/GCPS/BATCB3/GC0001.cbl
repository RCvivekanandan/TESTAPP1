00001  IDENTIFICATION DIVISION.                                         12/09/02
00002  PROGRAM-ID.    GC0001.                                           GC0001  
00003  AUTHOR.        NABIL ELBAZ.                                         LV002
00004  INSTALLATION.  HCSC.                                             GC0001  
00005  DATE-WRITTEN.  JUNE 1988.                                        GC0001  
00006  DATE-COMPILED.                                                   GC0001  
00007 ****************************************************************  GC0001  
00008 *                                                                 GC0001  
00009 *    THIS PROGRAM WILL DO THE FOLLOWING FUNCTIONS:                GC0001  
00010 *         1)  READ THE EMPTY VSAM CDE PENDING FILE AND ADD ONE    GC0001  
00011 *             LOW-VALUE RECORD. (TSGVSAM1)                        GC0001  
00012 *         2)  READ THE EMPTY VSAM CDE SELECT FILE AND ADD ONE     GC0001  
00013 *             BLANK RECORD. (TSGVSAM2)                            GC0001  
00014 ****************************************************************  GC0001  
00015 ****************************************************************  GC0001  
00016 *     DATE      PGMER     MAINTENANCE REQUIRED                 *  GC0001  
00017 * -------------------------------------------------------------*  GC0001  
00018 *   6-17-88      NE   ORGINAL PROGRAM BUILD                    *  GC0001  
00019 *                                                              *  GC0001  
00020 *   9-17-91      TPM  INCREASED THE PND-REC-KEY FROM 40 TO 41  *  GC0001  
00021 *                     AND DECREASED THE PND-REC-DATA FROM      *  GC0001  
00022 *  #D12009            5960 TO 5959 TO ACCOMODATE FOR THE       *  GC0001  
00023 *                     EXPANSION OF THE FAMILY-RELATION FIELD   *  GC0001  
00024 *                                                              *  GC0001  
00025 * D12009  9-25-91  FRY  MODIFIED SEL-RECORD FROM:              *  GC0001  
00026 *                              05  SEL-REC-KEY     PIC X(62).  *  GC0001  
00027 *                              05  SEL-REC-DATA    PIC X(18).  *  GC0001  
00028 *                                             TO:              *  GC0001  
00029 *                              05  SEL-REC-KEY     PIC X(63).  *  GC0001  
00030 *                              05  SEL-REC-DATA    PIC X(17).  *  GC0001  
00031 *                                                              *  GC0001  
00032 *         1/5/95   EMS/GDM  CHANGED TO COBOL II.               *  GC0001  
00033 *                                                              *  GC0001  
00034 *  14726/ 11/15/97  DAU  ADDED CODE TO SUPPORT THE YEAR 2000   *  GC0001  
00035 *  15057                 THE EXPANSION OF THE GROUP SPECIFIC   *  GC0001  
00036 *                        AND CONTRACT KEY TO SUPPORT THE TEXAS *  GC0001  
00037 *                        MERGER.                               *  GC0001  
00038 *                                                              *  GC0001  
00039 *                                                              *  GC0001  
00040 ****************************************************************  GC0001  
00041                                                                   GC0001  
00042  ENVIRONMENT DIVISION.                                            GC0001  
00043  CONFIGURATION SECTION.                                           GC0001  
00044  SOURCE-COMPUTER. IBM-370.                                        GC0001  
00045  OBJECT-COMPUTER. IBM-370.                                        GC0001  
00046  INPUT-OUTPUT SECTION.                                            GC0001  
00047  FILE-CONTROL.                                                    GC0001  
00048  DATA DIVISION.                                                   GC0001  
00049  FILE SECTION.                                                    GC0001  
00050  WORKING-STORAGE SECTION.                                         GC0001  
00051                                                                   GC0001  
00052  77  FILLER                      PIC X(25)   VALUE                GC0001  
00053                                  'GC0001 WORKING STORAGE'.        GC0001  
00054  77  WS-CONT-REC                 PIC S999  COMP-3 VALUE ZEROS.    GC0001  
00055  77  PG-COUNC                    PIC S999  COMP-3 VALUE ZEROS.    GC0001  
00056                                                                   GC0001  
00057                                                                   GC0001  
00058  01  WS-DATE-YMD                 PIC X(8).                        GC0001  
00059  01  WS-FILLER REDEFINES WS-DATE-YMD.                             GC0001  
00060      05  WS-Y            PIC XXXX.                                GC0001  
00061      05  WS-M            PIC XX.                                  GC0001  
00062      05  WS-D            PIC XX.                                  GC0001  
00063  01  FILLER                      PIC X(45) VALUE                  GC0001  
00064      'THE FOLLOWING IS THE REASON FOR THE ABEND****'.             GC0001  
00065  01  DUMP-MSG1                   PIC X(32)  VALUE SPACES.         GC0001  
00066  01  ABEND-CODE                  PIC 9(4)    COMP.                GC0001  
00067                                                                   GC0001  
00068 *                                                                 GC0001  
00069 *************************************************************     GC0001  
00070 *     PARMS FOR TSGVSAM1 -  CDEPEND FILE                          GC0001  
00071 *************************************************************     GC0001  
00072                                                                   GC0001  
00073  01  PARM-SET.                                                    GC0001  
00074      05  SET-RDW.                                                 GC0001  
00075          10  SET-REC-LENG        PIC 9(4)    VALUE ZEROS COMP.    GC0001  
00076          10  SET-FEEDBACK        PIC 9(4)    VALUE ZEROS COMP.    GC0001  
00077      05  SET-VALUE               PIC 9(8)    VALUE ZEROS COMP.    GC0001  
00078                                                                   GC0001  
00079  01  PARM-ONE.                                                    GC0001  
00080      05  RESERVED-FLDS-1         PIC 9(8)  VALUE ZEROS COMP.      GC0001  
00081      05  RESERVED-X-1 REDEFINES RESERVED-FLDS-1.                  GC0001  
00082          10  REQUEST-TYPE-1      PIC X.                           GC0001  
00083          10  FILLER              PIC X(3).                        GC0001  
00084                                                                   GC0001  
00085  01   PARM-ONEA.                                                  GC0001  
00086       03  ONEA-RDW.                                               GC0001  
00087           10  ONEA-REC-LENG       PIC 9(4)  VALUE ZEROS COMP.     GC0001  
00088           10  ONEA-FEEDBACK       PIC 9(4)  VALUE ZEROS COMP.     GC0001  
00089 *     03  ONEA-REC-AREA           PIC X(8213).                    GC0001  
00090 *     03  PND-RECORD  REDEFINES  ONEA-REC-AREA.                   GC0001  
00091 *         05  PND-REC-KEY         PIC X(57).                      GC0001  
00092 *         05  PND-REC-DATA        PIC X(8156).                    GC0001  
00093       03  ONEA-REC-AREA           PIC X(100).                     GC0001  
00094                                                                   GC0001  
00095 *                                                                 GC0001  
00096 *************************************************************     GC0001  
00097 *     PARMS FOR TSGVSAM2 - CDESLECT FILE                          GC0001  
00098 *************************************************************     GC0001  
00099                                                                   GC0001  
00100                                                                   GC0001  
00101  01  PARM-TWO.                                                    GC0001  
00102      05  RESERVED-FLDS-2         PIC 9(8)  VALUE ZEROS COMP.      GC0001  
00103      05  RESERVED-X-2 REDEFINES RESERVED-FLDS-2.                  GC0001  
00104          10  REQUEST-TYPE-2      PIC X.                           GC0001  
00105          10  FILLER              PIC X(3).                        GC0001  
00106                                                                   GC0001  
00107  01   PARM-TWOA.                                                  GC0001  
00108       03  TWOA-RDW.                                               GC0001  
00109           10  TWOA-REC-LENG       PIC 9(4)  VALUE ZEROS COMP.     GC0001  
00110           10  TWOA-FEEDBACK       PIC 9(4)  VALUE ZEROS COMP.     GC0001  
00111       03  TWOA-REC-AREA.                                          GC0001  
00112       COPY GCDESELC.                                              GC0001  
00113 /                                                                 GC0001  
00114  01  TIME-AREA.                                                   GC0001  
00115      05  TIME-HH                 PIC XX.                          GC0001  
00116      05  TIME-MM                 PIC XX.                          GC0001  
00117      05  TIME-SS                 PIC XX.                          GC0001  
00118 /                                                                 GC0001  
00119                                                                   GC0001  
00120  01  WS-LINE-MESSAGES.                                            GC0001  
00121      05  WS-CDEPEND-MSG             PIC X(63) VALUE               GC0001  
00122      '*** ONE RECORD HAS BEEN ADDED TO THE CDE PENDING FILE **    GC0001  
00123 -    '*  '.                                                       GC0001  
00124      05  WS-CDESLECT-MSG            PIC X(63) VALUE               GC0001  
00125      '*** ONE RECORD HAS BEEN ADDED TO THE CDE SELECT FILE ***    GC0001  
00126 -    '   '.                                                       GC0001  
00127                                                                   GC0001  
00128  01  MSG-LINE.                                                    GC0001  
00129      03  FILLER         PIC X(10)  VALUE SPACES.                  GC0001  
00130      03  T-TRL-MSG      PIC X(63)  VALUE SPACES.                  GC0001  
00131      03  FILLER         PIC X(59)  VALUE SPACES.                  GC0001  
00132                                                                   GC0001  
00133  01  PRINT-DOUBLE-SPACE.                                          GC0001  
00134      05  FILLER               PIC X(133)  VALUE '0'.              GC0001  
00135                                                                   GC0001  
00136  01  RPT-IND    VALUE '0000'  PIC X(4).                           GC0001  
00137                                                                   GC0001  
00138  01  PRINT-LINE-CC.                                               GC0001  
00139      05  CONTROL-CHARACTERS         PIC X.                        GC0001  
00140      05  PRINT-LINE-132             PIC X(132).                   GC0001  
00141                                                                   GC0001  
00142                                                                   GC0001  
00143                                                                   GC0001  
00144  LINKAGE SECTION.                                                 GC0001  
00145 /                                                                 GC0001  
00146  PROCEDURE DIVISION.                                              GC0001  
00147                                                                   GC0001  
00148  0000-MAINLINE.                                                   GC0001  
00149      PERFORM 0005-OPEN-FILES THRU 0005-EXIT.                      GC0001  
00150                                                                   GC0001  
00151      PERFORM 0010-ADD-INITIAL THRU 0010-EXIT.                     GC0001  
00152                                                                   GC0001  
00153      PERFORM 0030-CLOSE-FILES THRU 0030-EXIT.                     GC0001  
00154      DISPLAY WS-CDEPEND-MSG.                                      GC0001  
00155      DISPLAY WS-CDESLECT-MSG.                                     GC0001  
00156                                                                   GC0001  
00157      GOBACK.                                                      GC0001  
00158  0000-EXIT.                                                       GC0001  
00159      EXIT.                                                        GC0001  
00160 /                                                                 GC0001  
00161  0005-OPEN-FILES.                                                 GC0001  
00162                                                                   GC0001  
00163      MOVE 'O'                    TO REQUEST-TYPE-1.               GC0001  
00164      CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC0001  
00165      IF  REQUEST-TYPE-1 NOT EQUAL 'O'                             GC0001  
00166          MOVE ONEA-FEEDBACK        TO ABEND-CODE                  GC0001  
00167          MOVE 'BAD CALL WITH O TO TSGVSAM1' TO DUMP-MSG1          GC0001  
00168          PERFORM  0999-ERROR-RTN.                                 GC0001  
00169                                                                   GC0001  
00170      MOVE 'O'                    TO REQUEST-TYPE-2.               GC0001  
00171      CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC0001  
00172      IF  REQUEST-TYPE-2 NOT EQUAL 'O'                             GC0001  
00173          MOVE TWOA-FEEDBACK  TO ABEND-CODE                        GC0001  
00174          MOVE 'BAD CALL WITH O TO TSGVSAM2 ' TO DUMP-MSG1         GC0001  
00175          PERFORM  0999-ERROR-RTN.                                 GC0001  
00176                                                                   GC0001  
00177                                                                   GC0001  
00178  0005-EXIT.                                                       GC0001  
00179        EXIT.                                                      GC0001  
00180 /                                                                 GC0001  
00181  0030-CLOSE-FILES.                                                GC0001  
00182      MOVE 'C'                    TO REQUEST-TYPE-1.               GC0001  
00183      CALL 'TSGVSAM1' USING PARM-ONE PARM-ONEA.                    GC0001  
00184      IF  REQUEST-TYPE-1 NOT EQUAL 'C'                             GC0001  
00185          MOVE ONEA-FEEDBACK        TO ABEND-CODE                  GC0001  
00186          MOVE 'BAD CALL WITH C TO TSGVSAM1' TO DUMP-MSG1          GC0001  
00187          PERFORM  0999-ERROR-RTN.                                 GC0001  
00188                                                                   GC0001  
00189      MOVE 'C'                    TO REQUEST-TYPE-2.               GC0001  
00190      CALL 'TSGVSAM2' USING PARM-TWO PARM-TWOA.                    GC0001  
00191      IF  REQUEST-TYPE-2 NOT EQUAL 'C'                             GC0001  
00192          MOVE TWOA-FEEDBACK  TO ABEND-CODE                        GC0001  
00193          MOVE 'BAD CALL WITH C TO TSGVSAM2 ' TO DUMP-MSG1         GC0001  
00194          PERFORM  0999-ERROR-RTN.                                 GC0001  
00195                                                                   GC0001  
00196  0030-EXIT.                                                       GC0001  
00197      EXIT.                                                        GC0001  
00198 /                                                                 GC0001  
00199  0010-ADD-INITIAL.                                                GC0001  
00200 ***  ADD LOW VALUE RECORD TO THE CDE PENDING FILE ***             GC0001  
00201      MOVE 'A' TO REQUEST-TYPE-1.                                  GC0001  
00202 *    MOVE LOW-VALUES   TO PND-REC-KEY.                            GC0001  
00203 *    MOVE LOW-VALUES   TO PND-REC-DATA.                           GC0001  
00204 *    MOVE +8217        TO ONEA-REC-LENG.                          GC0001  
00205      MOVE LOW-VALUES   TO ONEA-REC-AREA.                          GC0001  
00206      MOVE +104         TO ONEA-REC-LENG.                          GC0001  
00207                                                                   GC0001  
00208      CALL 'TSGVSAM1' USING PARM-ONE   PARM-ONEA.                  GC0001  
00209      IF  REQUEST-TYPE-1 NOT EQUAL 'A'                             GC0001  
00210          MOVE ONEA-FEEDBACK    TO ABEND-CODE                      GC0001  
00211          MOVE 'BAD CALL WITH A TO TSGVSAM1' TO DUMP-MSG1          GC0001  
00212          PERFORM  0999-ERROR-RTN.                                 GC0001  
00213                                                                   GC0001  
00214 ***  ADD SPACES    RECORD TO THE CDE SELECT  FILE ***             GC0001  
00215      MOVE 'A' TO REQUEST-TYPE-2.                                  GC0001  
00216      MOVE SPACES       TO TWOA-REC-AREA.                          GC0001  
00217      MOVE +84          TO TWOA-REC-LENG.                          GC0001  
00218                                                                   GC0001  
00219      CALL 'TSGVSAM2' USING PARM-TWO   PARM-TWOA.                  GC0001  
00220      IF  REQUEST-TYPE-2 NOT EQUAL 'A'                             GC0001  
00221          MOVE TWOA-FEEDBACK    TO ABEND-CODE                      GC0001  
00222          MOVE 'BAD CALL WITH A TO TSGVSAM2' TO DUMP-MSG1          GC0001  
00223          PERFORM  0999-ERROR-RTN.                                 GC0001  
00224  0010-EXIT.                                                       GC0001  
00225       EXIT.                                                       GC0001  
00226 /                                                                 GC0001  
00227  0999-ERROR-RTN.                                                  GC0001  
00228                                                                   GC0001  
00229      CALL 'TSGEND' USING ABEND-CODE.                              GC0001  
00230                                                                   GC0001  
00231  0999-EXIT.     EXIT.                                             GC0001  
