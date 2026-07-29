00001  IDENTIFICATION DIVISION.                                         12/09/02
00002  PROGRAM-ID.    GC024030.                                         GC024030
00003 *THIS IS A COBOL/2 PROGRAM                                           LV002
00004  AUTHOR.        ED WITKUS.                                        GC024030
00005  INSTALLATION.  HCSC.                                             GC024030
00006  DATE-WRITTEN.  MAY, 1986.                                        GC024030
00007  DATE-COMPILED.                                                   GC024030
00008 ******************************************************************GC024030
00009 *                                                                 GC024030
00010 *   THIS PROGRAM PERFORMS THE UPDATES TO THE DISCREPANCY FILE.    GC024030
00011 *   AS OF 05/12/86 IT IS CALLED BY GC024010 AND GC024020 AND      GC024030
00012 *   GC024025.                                                     GC024030
00013 *   IS NOT TO BE EXECUTED AS A STAND ALONE PROGRAM.               GC024030
00014 *                                                                 GC024030
00015 *    TSGVSAM2 IS THE ONLINE DISCREPANCY FILE. (I-O)               GC024030
00016 *                                                                 GC024030
00017 ******************************************************************GC024030
00018 ******************************************************************GC024030
00019 ******************************************************************GC024030
00020 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC024030
00021 *       *-*         U P D A T E   H I S T O R Y         *-*      *GC024030
00022 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GC024030
00023 *                                                                *GC024030
00024 * CHG NUM    DATE    BY   *---------DESCRIPTION------------------*GC024030
00025 * ______   ________  ___  _______________________________________*GC024030
00026 *  D116    09/09/87  FRY    UPDATE OPERATOR-ID FIELD.            *GC024030
00027 *                                                                *GC024030
00028 *  D12009  09/19/91  TPM    UPDATE OPERATOR-ID FIELD.            *GC024030
00029 *                     EXPANSION OF THE FAMILY-RELATION FIELD.    *GC024030
00030 *                     CHANGED THE RECORD LENGTH FROM 32 TO 33    *GC024030
00031 *                     WHEN CALLING THE TSGVSAM ROUTINE.          *GC024030
00032 *                                                                *GC024030
00033 *          01/18/95  GDM    CONVERT TO COBOL II                  *GC024030
00034 *                                                                 GC024030
00035 * 14726/15057                                                    *GC024030
00036 *         10/22/97 AB   ADDED CODE         TO SUPPORT THE YEAR   *GC024030
00037 *                       2000 AND THE EXPANSION OF THE GROUP      *GC024030
00038 *                       SPECIFIC AND CONTRACT KEY TO SUPPORT THE *GC024030
00039 *                       TEXAS MERGER.                            *GC024030
00040 *                                                                *GC024030
00041 * 14726/                                                         *GC024030
00042 * 15057   01/16/98 GSP  MOVED MLDATE-JUL1 TO DF2-ERRDT-CEN       *GC024030
00043 *                       RATHER THAN MLDATE-DATE1.                *GC024030
00044 *                                                                *GC024030
00045 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GC024030
00046 *                                                                *GC024030
00047 ******************************************************************GC024030
00048 ******************************************************************GC024030
00049  ENVIRONMENT DIVISION.                                            GC024030
00050  CONFIGURATION SECTION.                                           GC024030
00051  SOURCE-COMPUTER. IBM-370.                                        GC024030
00052  OBJECT-COMPUTER. IBM-370.                                        GC024030
00053  INPUT-OUTPUT SECTION.                                            GC024030
00054  FILE-CONTROL.                                                    GC024030
00055  DATA DIVISION.                                                   GC024030
00056  FILE SECTION.                                                    GC024030
00057 /                                                                 GC024030
00058  WORKING-STORAGE SECTION.                                         GC024030
00059  01  FILLER                    PIC X(32) VALUE                    GC024030
00060      'GC024030 WORKING STORAGE'.                                  GC024030
00061  01  VSAM-WS-ERR-MSG           PIC X(32) VALUE SPACES.            GC024030
00062                                                                   GC024030
00063  01  ABEND-CODE                PIC 9(4)  COMP.                    GC024030
00064                                                                   GC024030
00065  01  DATE-WORK-AREA.                                              GC024030
00066      05  JUL-DATE            PIC 9(7).                            GC024030
00067                                                                   GC024030
00068      COPY MLDATE01.                                               GC024030
00069                                                                   GC024030
00070  01  DATE-CHECK.                                                  GC024030
00071      05  DATE-CHK            PIC 9(5).                            GC024030
00072      05  D-CHK REDEFINES DATE-CHK.                                GC024030
00073          10  D-CHK-YY        PIC 99.                              GC024030
00074          10  D-CHK-DD        PIC 999.                             GC024030
00075                                                                   GC024030
00076  01  DATE-WORK-AREA.                                              GC024030
00077      05  JUL-DATE.                                                GC024030
00078          10  JUL-YY          PIC 99.                              GC024030
00079          10  JUL-DD          PIC 999.                             GC024030
00080      05  JUL-DTE REDEFINES JUL-DATE PIC 9(5).                     GC024030
00081                                                                   GC024030
00082      COPY HSCDATES.                                               GC024030
00083 /                                                                 GC024030
00084  01  PARM-SET.                                                    GC024030
00085      05  SET-RDW.                                                 GC024030
00086          10  SET-REC-LENG    PIC 9(4)    VALUE ZEROS     COMP.    GC024030
00087          10  SET-FEEDBACK    PIC 9(4)    VALUE ZEROS     COMP.    GC024030
00088      05  SET-VALUE           PIC 9(8)                    COMP.    GC024030
00089                                                                   GC024030
00090  01  PARM-ONE.                                                    GC024030
00091      05  RESERVED-FLDS-1     PIC 9(8)    VALUE ZEROS     COMP.    GC024030
00092      05  RESERVED-X-1 REDEFINES RESERVED-FLDS-1.                  GC024030
00093          10  REQUEST-TYPE-1  PIC X.                               GC024030
00094          10  FILLER          PIC X(3).                            GC024030
00095                                                                   GC024030
00096  01  PARM-ONEA.                                                   GC024030
00097      02  ONEA-RDW.                                                GC024030
00098          05  ONEA-REC-LENG   PIC 9(4)    VALUE ZEROS     COMP.    GC024030
00099          05  ONEA-FEEDBACK   PIC 9(4)    VALUE ZEROS     COMP.    GC024030
00100      02  ONEA-REC-AREA.                                           GC024030
00101          COPY GCDFILE2.                                           GC024030
00102 /                                                                 GC024030
00103  LINKAGE SECTION.                                                 GC024030
00104                                                                   GC024030
00105  01  GC024030-CALL-AREA.                                          GC024030
00106      05  GC024030-IND          PIC X.                             GC024030
00107          88  OPEN-FILE                VALUE 'O'.                  GC024030
00108          88  CLOSE-FILE               VALUE 'C'.                  GC024030
00109          88  BAD-EDIT                 VALUE 'E'.                  GC024030
00110          88  GOOD-EDIT                VALUE 'G'.                  GC024030
00111      COPY GCDFILEC.                                               GC024030
00112 /                                                                 GC024030
00113  PROCEDURE DIVISION USING GC024030-CALL-AREA.                     GC024030
00114                                                                   GC024030
00115  0000-MAINLINE.                                                   GC024030
00116                                                                   GC024030
00117      IF  GC024030-IND =  'O'                                      GC024030
00118          PERFORM 9000-OPEN-FILES THRU 9000-EXIT                   GC024030
00119          GOBACK.                                                  GC024030
00120                                                                   GC024030
00121      IF  GC024030-IND = 'C'                                       GC024030
00122          PERFORM 9010-CLOSE-FILES THRU 9010-EXIT                  GC024030
00123          GOBACK.                                                  GC024030
00124                                                                   GC024030
00125      PERFORM 0100-READ-DFILE THRU 0100-EXIT.                      GC024030
00126                                                                   GC024030
00127      IF REQUEST-TYPE-1 = 'R'                                      GC024030
00128          IF GC024030-IND = 'E'                                    GC024030
00129              PERFORM 1000-ERROR-UPDATE THRU 1000-EXIT             GC024030
00130          ELSE                                                     GC024030
00131              PERFORM 2000-NO-ERROR-UPDATE THRU 2000-EXIT          GC024030
00132      ELSE                                                         GC024030
00133          IF GC024030-IND = 'E'                                    GC024030
00134              PERFORM 3000-ADD-NEW-REC THRU 3000-EXIT.             GC024030
00135                                                                   GC024030
00136      GOBACK.                                                      GC024030
00137                                                                   GC024030
00138  0000-EXIT.                                                       GC024030
00139      EXIT.                                                        GC024030
00140 /                                                                 GC024030
00141  0100-READ-DFILE.                                                 GC024030
00142      MOVE DF-RECORD-KEY TO DF2-RECORD-KEY.                        GC024030
00143      MOVE 40 TO ONEA-REC-LENG.                                    GC024030
00144      MOVE 'R' TO REQUEST-TYPE-1.                                  GC024030
00145      CALL 'TSGVSAM2' USING PARM-ONE PARM-ONEA.                    GC024030
00146  0100-EXIT.                                                       GC024030
00147      EXIT.                                                        GC024030
00148 /                                                                 GC024030
00149  1000-ERROR-UPDATE.                                               GC024030
00150      MOVE 'TDY'                  TO MLDATE-FUNC.                  GC024030
00151      MOVE 'J'                    TO MLDATE-FORM1.                 GC024030
00152      CALL 'MLDATE' USING MLDATE01.                                GC024030
00153      MOVE MLDATE-JUL1            TO DF2-ERRDT-CEN.                GC024030
00154                                                                   GC024030
00155 *    CALL 'TCDTES' USING HSCDATES.                                GC024030
00156 *    MOVE JYR                TO JUL-YY.                           GC024030
00157 *    MOVE JDA                TO JUL-DD.                           GC024030
00158 *    MOVE JUL-DTE TO DF2-ERROR-DATE.                              GC024030
00159      MOVE 'O1' TO DF2-ACTION-FLAG1.                               GC024030
00160      MOVE DF-OPERATOR-ID   TO  DF2-OPERATOR-ID.                   GC024030
00161      MOVE DF-ERROR-COUNT   TO  DF2-ERROR-COUNT.                   GC024030
00162      MOVE DF2-ERROR-COUNT  TO  DF2-ERROR-COUNT.                   GC024030
00163      MOVE DF-ERROR-ENTRIES TO  DF2-ERROR-ENTRIES.                 GC024030
00164      MOVE 'U' TO REQUEST-TYPE-1.                                  GC024030
00165      COMPUTE ONEA-REC-LENG = (DF2-ERROR-COUNT * 3) + 61.          GC024030
00166      CALL 'TSGVSAM2' USING PARM-ONE PARM-ONEA.                    GC024030
00167      IF REQUEST-TYPE-1 NOT = 'U'                                  GC024030
00168          MOVE '1000-ERROR-UPDATE BAD UPDATE' TO VSAM-WS-ERR-MSG   GC024030
00169          MOVE ONEA-FEEDBACK TO ABEND-CODE                         GC024030
00170          GO TO 9999-ERROR-RTN.                                    GC024030
00171  1000-EXIT.                                                       GC024030
00172      EXIT.                                                        GC024030
00173                                                                   GC024030
00174 /                                                                 GC024030
00175  2000-NO-ERROR-UPDATE.                                            GC024030
00176      MOVE 'TDY'                  TO MLDATE-FUNC.                  GC024030
00177      MOVE 'J'                    TO MLDATE-FORM1.                 GC024030
00178      CALL 'MLDATE' USING MLDATE01.                                GC024030
00179      MOVE MLDATE-JUL1            TO DF2-ERRDT-CEN.                GC024030
00180 *    CALL 'TCDTES' USING HSCDATES.                                GC024030
00181 *    MOVE JYR                TO JUL-YY.                           GC024030
00182 *    MOVE JDA                TO JUL-DD.                           GC024030
00183 *    MOVE JUL-DTE            TO DF2-ERROR-DATE.                   GC024030
00184      MOVE 'D2'               TO DF2-ACTION-FLAG1.                 GC024030
00185      MOVE 'U' TO REQUEST-TYPE-1.                                  GC024030
00186      COMPUTE ONEA-REC-LENG = (DF2-ERROR-COUNT * 3) + 61.          GC024030
00187      CALL 'TSGVSAM2' USING PARM-ONE PARM-ONEA.                    GC024030
00188      IF REQUEST-TYPE-1 NOT = 'U'                                  GC024030
00189          MOVE '2000-ERROR-UPDATE BAD UPDATE' TO VSAM-WS-ERR-MSG   GC024030
00190          MOVE ONEA-FEEDBACK TO ABEND-CODE                         GC024030
00191          GO TO 9999-ERROR-RTN.                                    GC024030
00192  2000-EXIT.                                                       GC024030
00193      EXIT.                                                        GC024030
00194 /                                                                 GC024030
00195  3000-ADD-NEW-REC.                                                GC024030
00196      MOVE 'TDY'                  TO MLDATE-FUNC.                  GC024030
00197      MOVE 'J'                    TO MLDATE-FORM1.                 GC024030
00198      CALL 'MLDATE' USING MLDATE01.                                GC024030
00199      MOVE MLDATE-JUL1            TO DF2-ERRDT-CEN.                GC024030
00200 *    CALL 'TCDTES' USING HSCDATES.                                GC024030
00201 *    MOVE JYR                TO JUL-YY.                           GC024030
00202 *    MOVE JDA                TO JUL-DD.                           GC024030
00203 *    MOVE JUL-DTE            TO DF2-ERROR-DATE.                   GC024030
00204      MOVE 'A1'               TO DF2-ACTION-FLAG1.                 GC024030
00205      MOVE DF-OPERATOR-ID     TO DF2-OPERATOR-ID.                  GC024030
00206      MOVE DF-ERROR-COUNT     TO DF2-ERROR-COUNT.                  GC024030
00207      MOVE DF2-ERROR-COUNT    TO DF2-ERROR-COUNT.                  GC024030
00208      MOVE DF-ERROR-ENTRIES   TO DF2-ERROR-ENTRIES.                GC024030
00209      COMPUTE ONEA-REC-LENG = (DF2-ERROR-COUNT * 3) + 61.          GC024030
00210      MOVE 'W' TO REQUEST-TYPE-1.                                  GC024030
00211      CALL 'TSGVSAM2' USING PARM-ONE PARM-ONEA.                    GC024030
00212      IF REQUEST-TYPE-1 NOT = 'W'                                  GC024030
00213          MOVE '3000-ERROR-UPDATE BAD WRITE ' TO VSAM-WS-ERR-MSG   GC024030
00214          MOVE ONEA-FEEDBACK TO ABEND-CODE                         GC024030
00215          GO TO 9999-ERROR-RTN.                                    GC024030
00216  3000-EXIT.                                                       GC024030
00217      EXIT.                                                        GC024030
00218 /                                                                 GC024030
00219  9000-OPEN-FILES.                                                 GC024030
00220      MOVE 'S'                TO REQUEST-TYPE-1.                   GC024030
00221      MOVE 8                  TO SET-REC-LENG.                     GC024030
00222      MOVE 3                  TO SET-VALUE.                        GC024030
00223      CALL 'TSGVSAM2' USING PARM-ONE PARM-SET.                     GC024030
00224      IF  REQUEST-TYPE-1 NOT = 'S'                                 GC024030
00225          MOVE 'GC024030--BAD START        ' TO VSAM-WS-ERR-MSG    GC024030
00226          MOVE SET-FEEDBACK   TO ABEND-CODE                        GC024030
00227          GO TO 9999-ERROR-RTN.                                    GC024030
00228                                                                   GC024030
00229      MOVE 'O' TO REQUEST-TYPE-1.                                  GC024030
00230      CALL 'TSGVSAM2' USING PARM-ONE PARM-ONEA.                    GC024030
00231      IF  REQUEST-TYPE-1 NOT = 'O'                                 GC024030
00232          MOVE 'GC024030--BAD OPEN         ' TO VSAM-WS-ERR-MSG    GC024030
00233          MOVE ONEA-FEEDBACK TO ABEND-CODE                         GC024030
00234          GO TO 9999-ERROR-RTN.                                    GC024030
00235                                                                   GC024030
00236  9000-EXIT.                                                       GC024030
00237      EXIT.                                                        GC024030
00238 /                                                                 GC024030
00239  9010-CLOSE-FILES.                                                GC024030
00240      MOVE 'C' TO REQUEST-TYPE-1.                                  GC024030
00241      CALL 'TSGVSAM2' USING PARM-ONE PARM-ONEA.                    GC024030
00242      IF  REQUEST-TYPE-1 NOT = 'C'                                 GC024030
00243          MOVE 'GC024030--BAD CLOSE       ' TO VSAM-WS-ERR-MSG     GC024030
00244          MOVE ONEA-FEEDBACK  TO ABEND-CODE                        GC024030
00245          GO TO 9999-ERROR-RTN.                                    GC024030
00246                                                                   GC024030
00247  9010-EXIT.                                                       GC024030
00248      EXIT.                                                        GC024030
00249 /                                                                 GC024030
00250  9999-ERROR-RTN.                                                  GC024030
00251                                                                   GC024030
00252      CALL 'TSGEND' USING ABEND-CODE.                              GC024030
00253                                                                   GC024030
00254  9999-EXIT.                                                       GC024030
00255      EXIT.                                                        GC024030
