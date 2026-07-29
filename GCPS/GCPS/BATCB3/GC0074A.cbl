00001  IDENTIFICATION DIVISION.                                         00010000
00002 *THIS IS A COBOL/2 PROGRAM                                        00020000
00003  PROGRAM-ID.    GC0074A.                                          00030000
00004  AUTHOR.        ANNE KING.                                        00040000
00005  DATE-WRITTEN.  JUL 2001.                                         00050000
00006  DATE-COMPILED.                                                   00060000
00007 ******************************************************************00070000
00008 ***                                                            ***00080000
00009 ***  GENERIC CONTRACT PROCESSING SYSTEM (GCPS)                 ***00090000
00010 ***  CDRS TABULAR SLOT ASSIGNMENT                              ***00100000
00011 ***                                                            ***00110000
00012 ***   INPUT FILE:   OPER.GCPS.GC0064C.CDRS.TAB.RLSE.UPDA       ***00120000
00013 ***  OUTPUT FILE:   OPER.GCPS.GC0074B.CDRS.SLOTUPDA            ***00130000
00014 ***                                                            ***00140000
00015 ***  PURPOSE:                                                  ***00150000
00016 ***      CDRS  SLOT ASSIGNMENT USING HASHING PROGRAM.          ***00160000
00017 ***                                                            ***00170000
00018 ******************************************************************00180000
00019 ***              U P D A T E   H I S T O R Y                   ***00190000
00020 ******************************************************************00200000
00021 ***  CHG-NUM    DATE    WHO          DESCRIPTION               ***00210000
00022 ***                                                            ***00220000
00023 ***  D0358    07/24/01  AKK     CLONED FROM GC0070A TO SUPPORT ***00230000
00024 ***                             #CDRS.                         ***00240000
00025 ***                                                            ***00250000
00026 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *00260000
00027 *                                                                *00270000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00271001
00028 ******************************************************************00280000
00029  ENVIRONMENT DIVISION.                                            00290000
00030  CONFIGURATION SECTION.                                           00300000
00031  SOURCE-COMPUTER.  IBM-370.                                       00310000
00032  OBJECT-COMPUTER.  IBM-370.                                       00320000
00033                                                                   00330000
00034  INPUT-OUTPUT SECTION.                                            00340000
00035  FILE-CONTROL.                                                    00350000
00036                                                                   00360000
00037      SELECT ACCUM-SLOT-UNASSIGN-FILE ASSIGN TO GC0074A.           00370000
00038      SELECT ACCUM-SLOT-ASSIGN-FILE   ASSIGN TO GC0074B.           00380000
00039                                                                   00390000
00040  DATA DIVISION.                                                   00400000
00041  FILE SECTION.                                                    00410000
00042                                                                   00420000
00043  FD  ACCUM-SLOT-UNASSIGN-FILE                                     00430000
00044      BLOCK CONTAINS 0 RECORDS                                     00440000
00045      LABEL RECORDS ARE STANDARD                                   00450000
00046      RECORDING MODE IS V.                                         00460000
00047  01  INPUT-WRK-RECORD.                                            00470000
00048      COPY GCWRKDCC.                                               00480000
00049      05  INPUT-WRK-TABULAR-RECORD.                                00490000
00050          10  INPUT-WRK-TAB-REC-KEY.                               00500000
00051              15  INPUT-WRK-TAB-REC-ID     PIC X(6).               00510000
00052              15  INPUT-WRK-TAB-REC-SLOT   PIC S9(7) COMP-3.       00520000
00053          10  FILLER                       PIC X(27).              00530000
00054          10  INPUT-WRK-COMPARE.                                   00540000
00055              15  INPUT-WRK-TAB-REC-CNT    PIC S9(5) COMP-3.       00550000
      *DM9441 09/04/09 JJS  CHANGED        FOR FILE CONVERSION          00551001
00056              15  INPUT-WRK-ENTRIES        PIC X(31330).           00560001
00057 /                                                                 00570000
00058  01  CDRS-SLOT-UNASSIGN-REC.                                      00580000
00059      05  FILLER                        PIC X(100).                00590000
00060      COPY GCTCDRSC.                                               00600000
00061 /                                                                 00610000
00062 /                                                                 00620000
00063  FD  ACCUM-SLOT-ASSIGN-FILE                                       00630000
00064      BLOCK CONTAINS 0 RECORDS                                     00640000
00065      LABEL RECORDS ARE STANDARD                                   00650000
00066      RECORDING MODE IS V.                                         00660000
00067                                                                   00670000
      *DM9441 09/04/09 JJS  CHANGED        FOR FILE CONVERSION          00671002
00068  01  OUTPUT-WRK-RECORD                 PIC X(31470).              00680002
00069 /                                                                 00690000
00070  01  CDRS-SLOT-ASSIGN-REC.                                        00700000
00071      05  FILLER                        PIC X(100).                00710000
00072      COPY GCTCDRS2.                                               00720000
00073 /                                                                 00730000
00074  WORKING-STORAGE SECTION.                                         00740000
00075 ******************************************************************00750000
00076 ***        W O  R K I N G    S T O R A G E    A R E A          ***00760000
00077 ******************************************************************00770000
00078                                                                   00780000
00079  01  FILLER                            PIC X(36) VALUE            00790000
00080                           'GC0074A WORKING STORAGE STARTS HERE'.  00800000
00081                                                                   00810000
00082  01  WORK-AREAS.                                                  00820000
00083      05  FLAGS.                                                   00830000
00084          10  EOF-FLAG            PIC X(01)  VALUE 'N'.            00840000
00085      05  HASH-PROGRAM            PIC X(17)  VALUE 'GHS1BAT'.      00850000
00086                                                                   00860000
00087 **** GHS1BAT LINKAGE AREA                                         00870000
00088                                                                   00880000
00089  01  WS-GHS1BAT-CALL-AREA.                                        00890000
00090    03  WS-GHS1BAT-PROCESS-IND    PIC X.                           00900000
00091        88  CALL-FOR-OPEN                    VALUE 'O'.            00910000
00092        88  CALL-FOR-CLOSE                   VALUE 'C'.            00920000
00093        88  CALL-FOR-PROCESS                 VALUE 'P'.            00930000
00094                                                                   00940000
00095  01  ACCUM-SLOT-AREA.                                             00950000
00096      05  HASH-RETURN-CODE        PIC X(02).                       00960000
00097      05  ASUR-REC-AREA.                                           00970000
00098          10 ASUR-TAB-ID          PIC X(0006).                     00980000
00099          10 ASUR-SLOT            PIC S9(07) COMP-3.               00990000
      *DM9441 09/04/09 JJS  CHANGED FILLER FOR FILE CONVERSION          00991001
00100          10 FILLER               PIC X(31360).                    01000001
00101                                                                   01010000
00102                                                                   01020000
00103 *01  LENGTHS.                                                     01030000
00104 *    COPY GCCDRLEN SUPPRESS.                                      01040000
00105                                                                   01050000
00106  01  FILLER                         PIC X(25) VALUE               01060000
00107                                'WORKING STORAGE ENDS HERE'.       01070000
00108 /                                                                 01080000
00109  PROCEDURE DIVISION.                                              01090000
00110                                                                   01100000
00111 ******************************************************************01110000
00112 **                                                                01120000
00113 **               P R O C E S S    C O N T R O L                   01130000
00114 **                                                                01140000
00115 ******************************************************************01150000
00116  0000-MAINLINE.                                                   01160000
00117                                                                   01170000
00118      PERFORM 1000-OPEN-THE-FILES THRU 1000-EXIT.                  01180000
00119                                                                   01190000
00120      IF EOF-FLAG = 'Y'                                            01200000
00121         DISPLAY 'GC0074A--NO INPUT RECORDS RECEIVED'              01210000
00122      ELSE                                                         01220000
00123         PERFORM 2000-PROCESS-WORK-FILE THRU 2000-EXIT             01230000
00124           UNTIL EOF-FLAG = 'Y'.                                   01240000
00125                                                                   01250000
00126      PERFORM 3000-CLOSE-THE-FILES THRU 3000-EXIT.                 01260000
00127      STOP RUN.                                                    01270000
00128                                                                   01280000
00129  0000-EXIT.                                                       01290000
00130      EXIT.                                                        01300000
00131 /*****************************************************************01310000
00132 ***                                                            ***01320000
00133  1000-OPEN-THE-FILES.                                             01330000
00134                                                                   01340000
00135      OPEN INPUT  ACCUM-SLOT-UNASSIGN-FILE                         01350000
00136           OUTPUT ACCUM-SLOT-ASSIGN-FILE.                          01360000
00137                                                                   01370000
00138      SET CALL-FOR-OPEN TO TRUE.                                   01380000
00139      CALL HASH-PROGRAM USING WS-GHS1BAT-CALL-AREA.                01390000
00140                                                                   01400000
00141 **--- READ THE FIRST RECORD.                                      01410000
00142 **                                                                01420000
00143      PERFORM 4000-READ-INPUT-FILE THRU 4000-EXIT.                 01430000
00144                                                                   01440000
00145  1000-EXIT.  EXIT.                                                01450000
00146 /*****************************************************************01460000
00147  2000-PROCESS-WORK-FILE.                                          01470000
00148                                                                   01480000
00149      IF  INPUT-WRK-RECORD = LOW-VALUES                            01490000
00150          GO TO 2000-EXIT.                                         01500000
00151                                                                   01510000
00152                                                                   01520000
00153      EVALUATE  INPUT-WRK-TAB-REC-ID                               01530000
00154        WHEN  '#CDRS '                                             01540000
00155          PERFORM 2100-PROCESS-CDRS-TABULAR THRU 2100-EXIT         01550000
00156      END-EVALUATE.                                                01560000
00157                                                                   01570000
00158      PERFORM 4000-READ-INPUT-FILE THRU 4000-EXIT.                 01580000
00159                                                                   01590000
00160                                                                   01600000
00161  2000-EXIT.  EXIT.                                                01610000
00162 /                                                                 01620000
00163  2100-PROCESS-CDRS-TABULAR.                                       01630000
00164                                                                   01640000
00165 *********************************************************         01650000
00166 ***** IF SLOT IS LESS THAN 9,000,000                *****         01660000
00167 ***** BYPASS THIS RECORD.                           *****         01670000
00168 *********************************************************         01680000
00169                                                                   01690000
00170      IF GTE-PROVISION-SLOT-NO NOT > +8999999                      01700000
00171          GO TO 2100-EXIT.                                         01710000
00172                                                                   01720000
00173 *********************************************************         01730000
00174 ***** CALL TABULAR HASHING PROGRAM                  *****         01740000
00175 ***** TO RETREIVE EXISTING OR NEW SLOT NUMBER.      *****         01750000
00176 *********************************************************         01760000
00177                                                                   01770000
00178      MOVE LOW-VALUES               TO ASUR-REC-AREA.              01780000
00179      MOVE INPUT-WRK-TABULAR-RECORD TO ASUR-REC-AREA.              01790000
00180      SET CALL-FOR-PROCESS          TO TRUE.                       01800000
00181      CALL HASH-PROGRAM             USING WS-GHS1BAT-CALL-AREA     01810000
00182                                          ACCUM-SLOT-AREA.         01820000
00183                                                                   01830000
00184 *********************************************************         01840000
00185 ***** MARK RECORD AS 'EXISTING' OR 'NEW'.           *****         01850000
00186 *********************************************************         01860000
00187                                                                   01870000
00188      IF HASH-RETURN-CODE = '00'                                   01880000
00189         MOVE 'E'                    TO WRK-SIGNAL-BATCH-INTERNAL  01890000
00190      ELSE                                                         01900000
00191         IF HASH-RETURN-CODE = '01'                                01910000
00192            MOVE 'N'                 TO WRK-SIGNAL-BATCH-INTERNAL  01920000
00193         ELSE                                                      01930000
00194            GO TO 2100-EXIT.                                       01940000
00195                                                                   01950000
00196                                                                   01960000
00197 *********************************************************         01970000
00198 ***** MOVE IN TABULAR SLOT AND WRITE RECORD TO      *****         01980000
00199 ***** THE SEQUENTIAL FILE.                          *****         01990000
00200 *********************************************************         02000000
00201                                                                   02010000
00202      MOVE LOW-VALUES                TO OUTPUT-WRK-RECORD.         02020000
00203      MOVE INPUT-WRK-TAB-REC-CNT     TO GTE2-ENTRY-COUNT.          02030000
00204      MOVE CDRS-SLOT-UNASSIGN-REC    TO OUTPUT-WRK-RECORD.         02040000
00205      MOVE ASUR-SLOT                 TO GTE2-PROVISION-SLOT-NO.    02050000
00206      WRITE CDRS-SLOT-ASSIGN-REC.                                  02060000
00207                                                                   02070000
00208  2100-EXIT.                                                       02080000
00209      EXIT.                                                        02090000
00210 /*****************************************************************02100000
00211  3000-CLOSE-THE-FILES.                                            02110000
00212                                                                   02120000
00213      CLOSE ACCUM-SLOT-UNASSIGN-FILE                               02130000
00214            ACCUM-SLOT-ASSIGN-FILE.                                02140000
00215                                                                   02150000
00216      SET CALL-FOR-CLOSE TO TRUE.                                  02160000
00217      CALL HASH-PROGRAM  USING WS-GHS1BAT-CALL-AREA.               02170000
00218                                                                   02180000
00219  3000-EXIT.  EXIT.                                                02190000
00220 /*****************************************************************02200000
00221  4000-READ-INPUT-FILE.                                            02210000
00222                                                                   02220000
00223      READ ACCUM-SLOT-UNASSIGN-FILE AT END MOVE                    02230000
00224        'Y' TO EOF-FLAG.                                           02240000
00225                                                                   02250000
00226  4000-EXIT.  EXIT.                                                02260000
