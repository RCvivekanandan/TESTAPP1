00001  IDENTIFICATION DIVISION.                                         08/20/03
00002                                                                   GBIGPGM 
00003  PROGRAM-ID.         GBIGPGM.                                        LV001
00004                                                                   GBIGPGM 
00005  AUTHOR.             JUNE PON.                                    GBIGPGM 
00006                                                                   GBIGPGM 
00007  DATE-WRITTEN.       05-APR-2001.                                 GBIGPGM 
00008                                                                   GBIGPGM 
00009                                                                   GBIGPGM 
00010      SKIP3                                                        GBIGPGM 
00011  TITLE 'BENEFIT HIGHLIGHTS - SUBSCRIBER INQUIRY MODULE    '.      GBIGPGM 
00012  ENVIRONMENT DIVISION.                                            GBIGPGM 
00013                                                                   GBIGPGM 
00014  CONFIGURATION SECTION.                                           GBIGPGM 
00015  SOURCE-COMPUTER.    IBM-3033.                                    GBIGPGM 
00016  OBJECT-COMPUTER.    IBM-3033.                                    GBIGPGM 
00017      EJECT                                                        GBIGPGM 
00018 ******************************************************************GBIGPGM 
00019 *                                                                *GBIGPGM 
00020 *    PROGRAM:    GBIGPGM                                         *GBIGPGM 
00021 *    DATE:       05-APR-2001                                     *GBIGPGM 
00022 *    AUTHOR:     JUNE PON                                        *GBIGPGM 
00023 *    FUNCTION:                                                   *GBIGPGM 
00024 *      THIS MODULE ACCESSES MEMBERSHIP FILES TO RETURN THE       *GBIGPGM 
00025 *      SECTION NUMBER FOR A GROUP/SUBSCRIBER ENTERED IN THE      *GBIGPGM 
00026 *      BENEFITS HIGHTLIGHTS INQUIRY MENU (GHILPGM).              *GBIGPGM 
00027 *                                                                *GBIGPGM 
00028 *                                                                *GBIGPGM 
00029 *                                                                *GBIGPGM 
00030 *                                                                *GBIGPGM 
00031 *                                                                *GBIGPGM 
00032 *                                                                *GBIGPGM 
00033 *    NOTES:      NONE                                            *GBIGPGM 
00034 *                                                                *GBIGPGM 
00035 ******************************************************************GBIGPGM 
00036 *                                                                *GBIGPGM 
00037 *                      MAINTENANCE HISTORY                       *GBIGPGM 
00038 *                                                                *GBIGPGM 
00039 * MOD      DATE     BY  DRPT                ACTION               *GBIGPGM 
00040 * ----- ----------- --- ----- ---------------------------------- *GBIGPGM 
00041 * 01.00 05-APR-2001  JP       CREATED                            *GBIGPGM 
00042 *                                                                *GBIGPGM 
00043 * 01.01 05-JUN-2001  GSP      CHANGED ALL REFERENCES OF GBIA TO  *GBIGPGM 
00044 *                             GHIL (TRANSACTION WAS RENAMED).    *GBIGPGM 
00045 *                                                                *GBIGPGM 
00046 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GBIGPGM 
00047 *                                                                *GBIGPGM 
00048 *                                                                *GBIGPGM 
00049 ******************************************************************GBIGPGM 
00050      EJECT                                                        GBIGPGM 
00051  DATA DIVISION.                                                   GBIGPGM 
00052  WORKING-STORAGE SECTION.                                         GBIGPGM 
00053  01  FILLER                     PICTURE X(32)                     GBIGPGM 
00054           VALUE '******* WS STARTS HERE *******'.                 GBIGPGM 
00055                                                                   GBIGPGM 
00056                                                                   GBIGPGM 
00057  01  WS-SMFALTX-AREA-1.                                           GBIGPGM 
00058      02  WS-SMFALTX-KEY.                                          GBIGPGM 
00059          05 WS-SMFALTX-GRP-SUB.                                   GBIGPGM 
00060            10 WS-SMFALTX-GROUP.                                   GBIGPGM 
00061               15  WS-SMFALTX-GRP-1      PIC X(01)  VALUE ZEROES.  GBIGPGM 
00062               15  WS-SMFALTX-GRP-2      PIC X(02)  VALUE ZEROES.  GBIGPGM 
00063               15  WS-SMFALTX-GRP-6      PIC X(06)  VALUE SPACES.  GBIGPGM 
00064            10 WS-SMFALTX-SUBSCRIBER.                              GBIGPGM 
00065               15  WS-SMFALTX-SUB-7       PIC X(07)  VALUE ZEROES. GBIGPGM 
00066               15  WS-SMFALTX-SUB-NUM     PIC X(09)  VALUE SPACES. GBIGPGM 
00067          05 WS-SMFALTX-CANCEL-DT     PIC 9(06).                   GBIGPGM 
00068          05 WS-SMFALTX-SECTION       PIC X(05).                   GBIGPGM 
00069      02  WS-SMFALTX-RESP1             PIC S9(8) COMP VALUE +0.    GBIGPGM 
00070                                                                   GBIGPGM 
00071  01  WS-DISPLAY-AREA.                                             GBIGPGM 
00072      05  WS-DISPLAY-SUB.                                          GBIGPGM 
00073          10  WS-DISP-SUB-7                PIC X(07).              GBIGPGM 
00074          10  WS-DISP-SUB-9                PIC X(09).              GBIGPGM 
00075      05  WS-DISPLAY-GROUP.                                        GBIGPGM 
00076          10  WS-DISP-GRP-3                PIC X(03).              GBIGPGM 
00077          10  WS-DISP-GRP-6                PIC X(06).              GBIGPGM 
00078                                                                   GBIGPGM 
00079                                                                   GBIGPGM 
00080  01  WS-G2COMKEC-AREA.                                            GBIGPGM 
00081      05 WS-GIC-GROUP-NUM.                                         GBIGPGM 
00082         10 WS-GIC-GROUP-NO-1-3              PIC X(3).             GBIGPGM 
00083         10 WS-GIC-GRP-NO                    PIC X(6).             GBIGPGM 
00084      05 WS-SERV-DT-CEN              COMP-3  PIC S9(7).            GBIGPGM 
00085      05 WS-GI-SUBSCRIBER                    PIC X(09).            GBIGPGM 
00086                                                                   GBIGPGM 
00087  01  WS-WORK-FIELDS.                                              GBIGPGM 
00088      05  WS-HEX-00                 PIC X.                         GBIGPGM 
00089      05  WS-COMM-KEY-LEN           PIC S9(4) COMP   VALUE +150.   GBIGPGM 
00090      05  WS-ABEND-CODE             PIC X(04) VALUE SPACES.        GBIGPGM 
00091      05  WS-SSNFILE-RESP1             PIC S9(8) COMP VALUE +0.    GBIGPGM 
00092      05  WS-PRIMARY-SSN-DISPLAY.                                  GBIGPGM 
00093          10  WS-PRIM-SSN-3            PIC X(03).                  GBIGPGM 
00094          10  WS-PRIM-SSN-9            PIC X(09).                  GBIGPGM 
00095      05  WS-GROUP-COMPARE.                                        GBIGPGM 
00096          10  WS-GROUP-COMPARE-3       PIC X(03) VALUE ZEROES.     GBIGPGM 
00097          10  WS-GROUP-COMPARE-6       PIC X(06).                  GBIGPGM 
00098      05  WS-COMP-DATE-NUM          PIC 9(06)    VALUE ZEROES.     GBIGPGM 
00099                                                                   GBIGPGM 
00100  01  WS-SWITCHES.                                                 GBIGPGM 
00101      05  WS-SEARCH-SMFALTX-SW              PIC X(01) VALUE 'Y'.   GBIGPGM 
00102          88  CONT-SMFALTX-SEARCH                     VALUE 'Y'.   GBIGPGM 
00103          88  STOP-SMFALTX-SEARCH                     VALUE 'N'.   GBIGPGM 
00104      05  WS-SMFALTX-REC-FOUND              PIC X(01) VALUE 'N'.   GBIGPGM 
00105          88  SMFALTX-REC-FOUND                       VALUE 'Y'.   GBIGPGM 
00106          88  SMFALTX-REC-NOT-FD                      VALUE 'N'.   GBIGPGM 
00107      05  WS-REC-IS-CURRENT-SW              PIC X(01) VALUE 'N'.   GBIGPGM 
00108          88  REC-IS-CURRENT                          VALUE 'Y'.   GBIGPGM 
00109          88  REC-IS-NOT-CURRENT                      VALUE 'N'.   GBIGPGM 
00110      05  WS-SSN-MATCH-FD-SW                PIC X(01) VALUE 'N'.   GBIGPGM 
00111          88  SSN-MATCH-FOUND                         VALUE 'Y'.   GBIGPGM 
00112          88  NO-SSN-MATCH-FOUND                      VALUE 'N'.   GBIGPGM 
00113                                                                   GBIGPGM 
00114  01  WS-DATE-FIELDS.                                              GBIGPGM 
00115      05  WS-CURRENT-DATE-JUL      PIC 9(07) VALUE ZEROES.         GBIGPGM 
00116      05  WS-SUB-CANCEL-DT-JUL      PIC S9(07) COMP-3.             GBIGPGM 
00117      05  WS-CONV-YYMMDD           PIC 9(06).                      GBIGPGM 
00118      05  WS-CSDTCONV-OPERATION       PIC X(01) VALUE SPACES.      GBIGPGM 
00119      05  DATE-YMD-6                  PIC X(06) VALUE SPACES.      GBIGPGM 
00120      05  DATE-YMD-3-COMP1            PIC X(03) VALUE SPACES.      GBIGPGM 
00121      05  DATE-YMD-3-COMP2            PIC X(03) VALUE SPACES.      GBIGPGM 
00122      05  DATE-YMD-3                  PIC X(03) VALUE SPACES.      GBIGPGM 
00123      05  DATE-YMD-IN.                                             GBIGPGM 
00124          10  DATE-YMD-XXX            PIC X(03) VALUE SPACES.      GBIGPGM 
00125          10  DATE-YMD                PIC X(03) VALUE SPACES.      GBIGPGM 
00126      05  WS-CSDTCONV-UNPACKED-DT.                                 GBIGPGM 
00127          10 WS-CSDTCONV-UNPACKED-DT-2     PIC X(02) VALUE SPACES. GBIGPGM 
00128          10 WS-CSDTCONV-UNPACKED-DT-6.                            GBIGPGM 
00129             15 WS-CSDTCONV-UNPACKED-DT-YY PIC X(02) VALUE SPACES. GBIGPGM 
00130             15 WS-CSDTCONV-UNPACKED-DT-MM PIC X(02) VALUE SPACES. GBIGPGM 
00131             15 WS-CSDTCONV-UNPACKED-DT-DD PIC X(02) VALUE SPACES. GBIGPGM 
00132      05  WS-CONV-8-GREG.                                          GBIGPGM 
00133          10  WS-CONV-2-GREG         PIC X(02) VALUE SPACES.       GBIGPGM 
00134          10  WS-CONV-6-GREG.                                      GBIGPGM 
00135              15  WS-CONV-YY-GREG    PIC X(02) VALUE SPACES.       GBIGPGM 
00136              15  WS-CONV-MM-GREG    PIC X(02) VALUE SPACES.       GBIGPGM 
00137              15  WS-CONV-DD-GREG    PIC X(02) VALUE SPACES.       GBIGPGM 
00138                                                                   GBIGPGM 
00139  01  WS-SSN-FILE-AREA-1.                                          GBIGPGM 
00140      05  WS-SSN-RECORD-KEY.                                       GBIGPGM 
00141          07  WS-SSN.                                              GBIGPGM 
00142              09  WS-SSN-1-3               PIC X(03).              GBIGPGM 
00143              09  WS-SSN-4-12              PIC X(09).              GBIGPGM 
00144          07  WS-SSN-CANCEL-DATE-COMP          PIC X(03).          GBIGPGM 
00145          07  WS-SSN-GROUP-NO                  PIC X(06).          GBIGPGM 
00146          07  WS-SSN-SUB-NO                    PIC X(12).          GBIGPGM 
00147      05  WS-SSN-LAST-NAME                     PIC X(15).          GBIGPGM 
00148      05  WS-SSN-FIRST-NAME                    PIC X(09).          GBIGPGM 
00149      05  WS-SSN-MIDDLE-INIT                   PIC X(01).          GBIGPGM 
00150      05  WS-SSN-SEX                           PIC X(01).          GBIGPGM 
00151      05  WS-SSN-BIRTH-DATE                    PIC X(03).          GBIGPGM 
00152      05  WS-SSN-ORIG-EFF-DATE                 PIC X(03).          GBIGPGM 
00153      05  WS-SSN-SUB-DEP-IND                   PIC X(01).          GBIGPGM 
00154      05  FILLER                             PIC X(03).            GBIGPGM 
00155      05  WS-SSN-MILLENNIUM-FLAG               PIC X(01).          GBIGPGM 
00156                                                                   GBIGPGM 
00157  COPY MLDATE01.                                                   GBIGPGM 
00158                                                                   GBIGPGM 
00159                                                                   GBIGPGM 
00160  01  WS-SMFALTX-AREA-2.                                           GBIGPGM 
00161  COPY SMFAIXRC.                                                   GBIGPGM 
00162                                                                   GBIGPGM 
00163                                                                   GBIGPGM 
00164  01  WS-SSN-FILE-AREA-2.                                          GBIGPGM 
00165  COPY MSRL5222.                                                   GBIGPGM 
00166                                                                   GBIGPGM 
00167  COPY DFHAID.                                                     GBIGPGM 
00168  COPY GBIGSETC.                                                   GBIGPGM 
00169                                                                   GBIGPGM 
00170  LINKAGE SECTION.                                                 GBIGPGM 
00171                                                                   GBIGPGM 
00172  01  DFHCOMMAREA.                                                 GBIGPGM 
00173      02  GHIL-COMMAREA-IN.                                        GBIGPGM 
00174      COPY  G2COMKEC.                                              GBIGPGM 
00175          10   GI-REDF-AREA REDEFINES GI-FILLER.                   GBIGPGM 
00176               15  GI-SUBSCRIBER     PIC X(09).                    GBIGPGM 
00177               15  GI-LAST-NAME      PIC X(15).                    GBIGPGM 
00178               15  GI-FIRST-NAME     PIC X(09).                    GBIGPGM 
00179               15  GI-REDF-FILLER    PIC X(03).                    GBIGPGM 
00180                                                                   GBIGPGM 
00181  01  GI-COMMAREA2-RECORD.                                         GBIGPGM 
00182      COPY  G2COMKE2.                                              GBIGPGM 
00183      10   GI2-REDF-AREA REDEFINES GI2-FILLER.                     GBIGPGM 
00184           15  GI2-SUBSCRIBER     PIC X(09).                       GBIGPGM 
00185           15  GI2-LAST-NAME      PIC X(15).                       GBIGPGM 
00186           15  GI2-FIRST-NAME     PIC X(09).                       GBIGPGM 
00187           15  GI2-REDF-FILLER    PIC X(03).                       GBIGPGM 
00188                                                                   GBIGPGM 
00189                                                                   GBIGPGM 
00190  PROCEDURE DIVISION.                                              GBIGPGM 
00191 ************************************************************      GBIGPGM 
00192 *                                                          *      GBIGPGM 
00193 *                    PROCEDURE DIVISION                    *      GBIGPGM 
00194 *                                                          *      GBIGPGM 
00195 ************************************************************      GBIGPGM 
00196  0000-MAINLINE.                                                   GBIGPGM 
00197                                                                   GBIGPGM 
00198      MOVE  LOW-VALUES  TO  WS-HEX-00.                             GBIGPGM 
00199                                                                   GBIGPGM 
00200      EXEC CICS GETMAIN  SET (ADDRESS OF GI-COMMAREA2-RECORD)      GBIGPGM 
00201                         INITIMG(WS-HEX-00)                        GBIGPGM 
00202                         LENGTH (WS-COMM-KEY-LEN)                  GBIGPGM 
00203                         END-EXEC.                                 GBIGPGM 
00204                                                                   GBIGPGM 
00205      IF EIBTRNID      =  'GHIL'                                   GBIGPGM 
00206         PERFORM 1000-INITIAL-PROCESSING THRU 1000-EXIT            GBIGPGM 
00207         PERFORM 9100-RETURN                                       GBIGPGM 
00208      END-IF                                                       GBIGPGM 
00209                                                                   GBIGPGM 
00210      PERFORM 9200-RECEIVE-SCREEN THRU 9200-EXIT                   GBIGPGM 
00211                                                                   GBIGPGM 
00212      IF EIBTRNID      =  'GBIG'                                   GBIGPGM 
00213         EVALUATE EIBAID                                           GBIGPGM 
00214           WHEN DFHPF3                                             GBIGPGM 
00215             PERFORM  9400-XCTL-TO-GHILPGM THRU 9400-EXIT          GBIGPGM 
00216           WHEN DFHPF15                                            GBIGPGM 
00217             PERFORM  9400-XCTL-TO-GHILPGM THRU 9400-EXIT          GBIGPGM 
00218           WHEN DFHPF9                                             GBIGPGM 
00219             IF (IGSECTL  >  0)  AND (IGTRDDL = 0)                 GBIGPGM 
00220                PERFORM  5000-XCTL-TO-GBIBPGM THRU 5000-EXIT       GBIGPGM 
00221             ELSE                                                  GBIGPGM 
00222                MOVE SPACES TO IGERLN2O                            GBIGPGM 
00223                MOVE 'PF9 INVALID WHEN SUBSCRIBER NOT FOUND'       GBIGPGM 
00224                    TO IGERLN2O                                    GBIGPGM 
00225             END-IF                                                GBIGPGM 
00226           WHEN DFHENTER                                           GBIGPGM 
00227             PERFORM 2000-PROCESS-INPUT THRU 2000-EXIT             GBIGPGM 
00228           WHEN OTHER                                              GBIGPGM 
00229             MOVE SPACES TO IGERLN2O                               GBIGPGM 
00230             MOVE 'INVALID PF KEY ' TO IGERLN2O                    GBIGPGM 
00231         END-EVALUATE                                              GBIGPGM 
00232                                                                   GBIGPGM 
00233      END-IF.                                                      GBIGPGM 
00234                                                                   GBIGPGM 
00235                                                                   GBIGPGM 
00236      PERFORM 9500-SEND-DATAONLY   THRU  9500-EXIT                 GBIGPGM 
00237      PERFORM 9100-RETURN                                          GBIGPGM 
00238                                                                   GBIGPGM 
00239                                                                   GBIGPGM 
00240                                                                   GBIGPGM 
00241      GOBACK.                                                      GBIGPGM 
00242                                                                   GBIGPGM 
00243  0000-EXIT.                                                       GBIGPGM 
00244      EXIT.                                                        GBIGPGM 
00245                                                                   GBIGPGM 
00246 ************************************************************      GBIGPGM 
00247 *                                                          *      GBIGPGM 
00248 *                                                          *      GBIGPGM 
00249 *                                                          *      GBIGPGM 
00250 ************************************************************      GBIGPGM 
00251  1000-INITIAL-PROCESSING.                                         GBIGPGM 
00252                                                                   GBIGPGM 
00253      IF GI-RETURN-CODE = 'SO'                                     GBIGPGM 
00254         PERFORM 3000-PROCESS-SSN-ONLY THRU 3000-EXIT              GBIGPGM 
00255      END-IF.                                                      GBIGPGM 
00256                                                                   GBIGPGM 
00257                                                                   GBIGPGM 
00258      IF GI-RETURN-CODE = 'GS'                                     GBIGPGM 
00259         MOVE GIC-GROUP-NUM     TO  WS-GIC-GROUP-NUM               GBIGPGM 
00260                                    IGGRPXO                        GBIGPGM 
00261         MOVE GIGT-SLOT-NUMBER  TO  WS-SERV-DT-CEN                 GBIGPGM 
00262                                    IGSVDTO                        GBIGPGM 
00263         MOVE GI-SUBSCRIBER     TO  WS-GI-SUBSCRIBER               GBIGPGM 
00264                                    IGSUBNO                        GBIGPGM 
00265         PERFORM 4000-PROCESS-GRP-SUB  THRU 4000-EXIT              GBIGPGM 
00266      END-IF.                                                      GBIGPGM 
00267                                                                   GBIGPGM 
00268  1000-EXIT.                                                       GBIGPGM 
00269      EXIT.                                                        GBIGPGM 
00270                                                                   GBIGPGM 
00271                                                                   GBIGPGM 
00272 ************************************************************      GBIGPGM 
00273 *                                                          *      GBIGPGM 
00274 *                                                          *      GBIGPGM 
00275 *                                                          *      GBIGPGM 
00276 ************************************************************      GBIGPGM 
00277  2000-PROCESS-INPUT.                                              GBIGPGM 
00278                                                                   GBIGPGM 
00279                                                                   GBIGPGM 
00280      MOVE IGSUBNO   TO  WS-GI-SUBSCRIBER                          GBIGPGM 
00281      MOVE IGSVDTO   TO  WS-SERV-DT-CEN                            GBIGPGM 
00282      MOVE IGGRPXO   TO  WS-GIC-GROUP-NUM                          GBIGPGM 
00283                                                                   GBIGPGM 
00284                                                                   GBIGPGM 
00285      PERFORM 4000-PROCESS-GRP-SUB  THRU 4000-EXIT.                GBIGPGM 
00286                                                                   GBIGPGM 
00287                                                                   GBIGPGM 
00288  2000-EXIT.                                                       GBIGPGM 
00289      EXIT.                                                        GBIGPGM 
00290                                                                   GBIGPGM 
00291 ************************************************************      GBIGPGM 
00292 *                                                          *      GBIGPGM 
00293 *                                                          *      GBIGPGM 
00294 *                                                          *      GBIGPGM 
00295 ************************************************************      GBIGPGM 
00296  3000-PROCESS-SSN-ONLY.                                           GBIGPGM 
00297                                                                   GBIGPGM 
00298      MOVE ZEROES        TO WS-SSN-1-3                             GBIGPGM 
00299      MOVE GI-SUBSCRIBER TO WS-SSN-4-12                            GBIGPGM 
00300                                                                   GBIGPGM 
00301      PERFORM 6200-START-BROWSE-SSNFILE THRU 6200-EXIT             GBIGPGM 
00302                                                                   GBIGPGM 
00303      IF WS-SSNFILE-RESP1 = DFHRESP(NOTFND)                        GBIGPGM 
00304           MOVE  'SUBSCRIBER NUMBER NOT FOUND' TO IGERMSGO         GBIGPGM 
00305           MOVE GI-SUBSCRIBER TO IGSUBNO                           GBIGPGM 
00306           PERFORM 9300-SEND-MAP THRU 9300-EXIT                    GBIGPGM 
00307           GO TO 3000-EXIT                                         GBIGPGM 
00308      ELSE                                                         GBIGPGM 
00309         PERFORM 6300-READ-SSNFILE THRU 6300-EXIT                  GBIGPGM 
00310      END-IF.                                                      GBIGPGM 
00311                                                                   GBIGPGM 
00312                                                                   GBIGPGM 
00313      EVALUATE  WS-SSNFILE-RESP1                                   GBIGPGM 
00314         WHEN DFHRESP(NORMAL)                                      GBIGPGM 
00315           PERFORM 3100-DISPLAY-SSN-INFO THRU 3100-EXIT            GBIGPGM 
00316         WHEN OTHER                                                GBIGPGM 
00317           MOVE  'ERROR READING SSNFILE FILE'  TO IGERMSGO         GBIGPGM 
00318           MOVE GI-SUBSCRIBER TO IGSUBNO                           GBIGPGM 
00319           PERFORM 9300-SEND-MAP THRU 9300-EXIT                    GBIGPGM 
00320           GO TO 3000-EXIT                                         GBIGPGM 
00321      END-EVALUATE.                                                GBIGPGM 
00322                                                                   GBIGPGM 
00323                                                                   GBIGPGM 
00324                                                                   GBIGPGM 
00325                                                                   GBIGPGM 
00326  3000-EXIT.                                                       GBIGPGM 
00327      EXIT.                                                        GBIGPGM 
00328                                                                   GBIGPGM 
00329                                                                   GBIGPGM 
00330 ************************************************************      GBIGPGM 
00331 *                                                          *      GBIGPGM 
00332 *                                                          *      GBIGPGM 
00333 *                                                          *      GBIGPGM 
00334 ************************************************************      GBIGPGM 
00335  3100-DISPLAY-SSN-INFO.                                           GBIGPGM 
00336                                                                   GBIGPGM 
00337         MOVE 5222-SSN-4-12 TO IGSUBNO                             GBIGPGM 
00338         MOVE 5222-GROUP-NO TO IGGRPNO                             GBIGPGM 
00339         MOVE 5222-SUB-NO   TO WS-PRIMARY-SSN-DISPLAY              GBIGPGM 
00340         MOVE WS-PRIM-SSN-9 TO IGSUBPO                             GBIGPGM 
00341         MOVE 5222-LAST-NAME TO IGLNAMO                            GBIGPGM 
00342         MOVE 5222-FIRST-NAME TO IGFNAMO                           GBIGPGM 
00343 *       MOVE 5222-ORIG-EFF-DATE TO DATE-YMD-3                     GBIGPGM 
00344 *       PERFORM 8100-CONVERT-CANCEL-DT-3-TO-8  THRU 8100-EXIT     GBIGPGM 
00345 *       MOVE WS-CONV-YY-GREG TO IGEFYYO                           GBIGPGM 
00346 *       MOVE WS-CONV-MM-GREG TO IGEFMMO                           GBIGPGM 
00347 *       MOVE WS-CONV-DD-GREG TO IGEFDDO                           GBIGPGM 
00348                                                                   GBIGPGM 
00349 ****==> 5222-CANCEL-DATE-COMP = LOW-VALUES INDICATES ACTIVE       GBIGPGM 
00350 ****==> MEMBERSHIP                                                GBIGPGM 
00351                                                                   GBIGPGM 
00352      IF 5222-CANCEL-DATE-COMP = LOW-VALUES                        GBIGPGM 
00353         PERFORM 3200-SEARCH-SMFALTX-SO   THRU 3200-EXIT           GBIGPGM 
00354      ELSE                                                         GBIGPGM 
00355         PERFORM 3150-CONV-SSN-CANCEL-DT  THRU 3150-EXIT           GBIGPGM 
00356         MOVE  'NO CURRENT SSN REC FOUND  '  TO IGERMSGO           GBIGPGM 
00357      END-IF.                                                      GBIGPGM 
00358                                                                   GBIGPGM 
00359         PERFORM 9300-SEND-MAP THRU 9300-EXIT.                     GBIGPGM 
00360                                                                   GBIGPGM 
00361  3100-EXIT.                                                       GBIGPGM 
00362      EXIT.                                                        GBIGPGM 
00363                                                                   GBIGPGM 
00364  3150-CONV-SSN-CANCEL-DT.                                         GBIGPGM 
00365                                                                   GBIGPGM 
00366          IF 5222-CANCEL-DATE-COMP    NOT NUMERIC                  GBIGPGM 
00367             MOVE 5222-CANCEL-DATE-COMP TO DATE-YMD-3-COMP1        GBIGPGM 
00368             PERFORM 8300-CONVERT-CANCEL-DT-3-TO-3  THRU 8300-EXIT GBIGPGM 
00369             MOVE DATE-YMD-3-COMP2 TO DATE-YMD-3                   GBIGPGM 
00370             PERFORM 8100-CONVERT-CANCEL-DT-3-TO-8 THRU 8100-EXIT  GBIGPGM 
00371             IF MLDATE-RETURN  = '00'                              GBIGPGM 
00372                MOVE MLDATE-JUL2  TO WS-SUB-CANCEL-DT-JUL          GBIGPGM 
00373                                     GICB2-SLOT-NUMBER             GBIGPGM 
00374             ELSE                                                  GBIGPGM 
00375                MOVE 'IG31' TO WS-ABEND-CODE                       GBIGPGM 
00376                PERFORM 9900-ABEND  THRU 9900-EXIT                 GBIGPGM 
00377             END-IF                                                GBIGPGM 
00378          ELSE                                                     GBIGPGM 
00379               MOVE 5222-CANCEL-DATE-COMP TO WS-COMP-DATE-NUM      GBIGPGM 
00380               PERFORM 8200-CONVERT-CANCEL-DT-NUM  THRU 8200-EXIT  GBIGPGM 
00381               IF MLDATE-RETURN  = '00'                            GBIGPGM 
00382                  MOVE MLDATE-JUL2  TO WS-SUB-CANCEL-DT-JUL        GBIGPGM 
00383                                       GICB2-SLOT-NUMBER           GBIGPGM 
00384               ELSE                                                GBIGPGM 
00385                  MOVE 'IG01' TO WS-ABEND-CODE                     GBIGPGM 
00386                  PERFORM 9900-ABEND   THRU 9900-EXIT              GBIGPGM 
00387               END-IF                                              GBIGPGM 
00388          END-IF.                                                  GBIGPGM 
00389                                                                   GBIGPGM 
00390 *       MOVE WS-CONV-YY-GREG TO IGTRYYO.                          GBIGPGM 
00391 *       MOVE WS-CONV-MM-GREG TO IGTRMMO.                          GBIGPGM 
00392 *       MOVE WS-CONV-DD-GREG TO IGTRDDO.                          GBIGPGM 
00393                                                                   GBIGPGM 
00394                                                                   GBIGPGM 
00395  3150-EXIT.                                                       GBIGPGM 
00396      EXIT.                                                        GBIGPGM 
00397                                                                   GBIGPGM 
00398                                                                   GBIGPGM 
00399  3200-SEARCH-SMFALTX-SO.                                          GBIGPGM 
00400                                                                   GBIGPGM 
00401         MOVE 5222-GROUP-NO     TO   WS-SMFALTX-GRP-6              GBIGPGM 
00402         MOVE 5222-SUB-NO       TO   WS-PRIMARY-SSN-DISPLAY        GBIGPGM 
00403         MOVE WS-PRIM-SSN-9     TO   WS-SMFALTX-SUB-NUM            GBIGPGM 
00404         MOVE ZEROES            TO   WS-SMFALTX-SUB-7              GBIGPGM 
00405                                                                   GBIGPGM 
00406         PERFORM 3500-READ-SMFALTX-SO THRU 3500-EXIT               GBIGPGM 
00407                                                                   GBIGPGM 
00408         IF CONT-SMFALTX-SEARCH                                    GBIGPGM 
00409            INITIALIZE WS-SMFALTX-AREA-1                           GBIGPGM 
00410            MOVE '9' TO WS-SMFALTX-GRP-1                           GBIGPGM 
00411            MOVE ZEROES TO WS-SMFALTX-GRP-2                        GBIGPGM 
00412            MOVE 5222-GROUP-NO     TO   WS-SMFALTX-GRP-6           GBIGPGM 
00413            MOVE 5222-SUB-NO       TO   WS-PRIMARY-SSN-DISPLAY     GBIGPGM 
00414            MOVE WS-PRIM-SSN-9     TO   WS-SMFALTX-SUB-NUM         GBIGPGM 
00415            MOVE ZEROES            TO   WS-SMFALTX-SUB-7           GBIGPGM 
00416            PERFORM 3500-READ-SMFALTX-SO THRU 3500-EXIT            GBIGPGM 
00417         END-IF.                                                   GBIGPGM 
00418                                                                   GBIGPGM 
00419         IF CONT-SMFALTX-SEARCH                                    GBIGPGM 
00420            MOVE 'P3200 NO SMF MATCH TO SSN REC ' TO IGERMSGO      GBIGPGM 
00421         ELSE                                                      GBIGPGM 
00422            MOVE 'P3200 SUCCESSFUL SMF MATCH !! ' TO IGERMSGO      GBIGPGM 
00423         END-IF.                                                   GBIGPGM 
00424                                                                   GBIGPGM 
00425         PERFORM 9300-SEND-MAP THRU 9300-EXIT.                     GBIGPGM 
00426                                                                   GBIGPGM 
00427  3200-EXIT.                                                       GBIGPGM 
00428      EXIT.                                                        GBIGPGM 
00429                                                                   GBIGPGM 
00430  3500-READ-SMFALTX-SO.                                            GBIGPGM 
00431                                                                   GBIGPGM 
00432         PERFORM 6000-START-BROWSE-SMFALTX THRU 6000-EXIT          GBIGPGM 
00433                                                                   GBIGPGM 
00434      IF WS-SMFALTX-RESP1 = DFHRESP(NOTFND)                        GBIGPGM 
00435           MOVE  'REC NOT FOUND ON SMFALTX   ' TO IGERMSGO         GBIGPGM 
00436           GO TO 3500-EXIT                                         GBIGPGM 
00437      ELSE                                                         GBIGPGM 
00438         PERFORM 6100-READ-SMFALTX THRU 6100-EXIT                  GBIGPGM 
00439      END-IF.                                                      GBIGPGM 
00440                                                                   GBIGPGM 
00441      EVALUATE  WS-SMFALTX-RESP1                                   GBIGPGM 
00442         WHEN DFHRESP(NORMAL)                                      GBIGPGM 
00443           PERFORM 3600-GET-SMFATLX-SECT    THRU 3600-EXIT         GBIGPGM 
00444         WHEN OTHER                                                GBIGPGM 
00445           MOVE  'P3200 READ SMF ERROR      '  TO IGERMSGO         GBIGPGM 
00446           MOVE GI-SUBSCRIBER TO IGSUBNO                           GBIGPGM 
00447           PERFORM 9300-SEND-MAP THRU 9300-EXIT                    GBIGPGM 
00448           GO TO 3500-EXIT                                         GBIGPGM 
00449      END-EVALUATE.                                                GBIGPGM 
00450                                                                   GBIGPGM 
00451  3500-EXIT.                                                       GBIGPGM 
00452      EXIT.                                                        GBIGPGM 
00453                                                                   GBIGPGM 
00454                                                                   GBIGPGM 
00455  3600-GET-SMFATLX-SECT.                                           GBIGPGM 
00456                                                                   GBIGPGM 
00457       MOVE SMFALTX-KEY-SUBSCRIBER TO WS-SMFALTX-SUBSCRIBER        GBIGPGM 
00458       MOVE SMFALTX-KEY-GROUP TO WS-GROUP-COMPARE                  GBIGPGM 
00459       MOVE 5222-SUB-NO TO WS-PRIMARY-SSN-DISPLAY                  GBIGPGM 
00460       IF (WS-GROUP-COMPARE-6 = 5222-GROUP-NO  )   AND             GBIGPGM 
00461          (WS-SMFALTX-SUB-NUM  = WS-PRIM-SSN-9)  AND               GBIGPGM 
00462          (SMFALTX-CANCEL-COMP-DATE = 0)                           GBIGPGM 
00463          MOVE SMFALTX-KEY-SECTION TO IGSECTO                      GBIGPGM 
00464          MOVE 'N' TO WS-SEARCH-SMFALTX-SW                         GBIGPGM 
00465       END-IF.                                                     GBIGPGM 
00466                                                                   GBIGPGM 
00467                                                                   GBIGPGM 
00468  3600-EXIT.                                                       GBIGPGM 
00469      EXIT.                                                        GBIGPGM 
00470                                                                   GBIGPGM 
00471 ************************************************************      GBIGPGM 
00472 *                                                          *      GBIGPGM 
00473 *                                                          *      GBIGPGM 
00474 *                                                          *      GBIGPGM 
00475 ************************************************************      GBIGPGM 
00476  4000-PROCESS-GRP-SUB.                                            GBIGPGM 
00477                                                                   GBIGPGM 
00478      MOVE LOW-VALUES TO  IGLNAMO                                  GBIGPGM 
00479                          IGFNAMO                                  GBIGPGM 
00480                          IGSUBPO                                  GBIGPGM 
00481                          IGGRPNO                                  GBIGPGM 
00482                          IGSECTO                                  GBIGPGM 
00483                          IGEFMMO                                  GBIGPGM 
00484                          IGEFDDO                                  GBIGPGM 
00485                          IGEFYYO                                  GBIGPGM 
00486                          IGTRMMO                                  GBIGPGM 
00487                          IGTRDDO                                  GBIGPGM 
00488                          IGTRYYO                                  GBIGPGM 
00489                          IGERMSGO                                 GBIGPGM 
00490                          IGERLN2O                                 GBIGPGM 
00491                          IGTRYYO                                  GBIGPGM 
00492                          IGTRMMO                                  GBIGPGM 
00493                          IGTRDDO                                  GBIGPGM 
00494                                                                   GBIGPGM 
00495                                                                   GBIGPGM 
00496      MOVE +0             TO     IGSECTL                           GBIGPGM 
00497                                 IGTRDDL                           GBIGPGM 
00498      MOVE WS-SERV-DT-CEN    TO IGSVDTO                            GBIGPGM 
00499      MOVE 'N' TO WS-SMFALTX-REC-FOUND                             GBIGPGM 
00500      MOVE LOW-VALUES        TO   WS-SMFALTX-AREA-1                GBIGPGM 
00501      MOVE WS-GIC-GROUP-NUM  TO   WS-SMFALTX-GROUP                 GBIGPGM 
00502      MOVE ZEROES            TO   WS-SMFALTX-SUB-7                 GBIGPGM 
00503      MOVE WS-GI-SUBSCRIBER  TO   WS-SMFALTX-SUB-NUM               GBIGPGM 
00504                                                                   GBIGPGM 
00505      PERFORM 6000-START-BROWSE-SMFALTX THRU 6000-EXIT             GBIGPGM 
00506                                                                   GBIGPGM 
00507      IF WS-SMFALTX-RESP1 = DFHRESP(NORMAL)                        GBIGPGM 
00508         PERFORM 6100-READ-SMFALTX THRU 6100-EXIT                  GBIGPGM 
00509      END-IF                                                       GBIGPGM 
00510                                                                   GBIGPGM 
00511      IF (WS-SMFALTX-GRP-6   =  WS-GIC-GRP-NO)      AND            GBIGPGM 
00512         (WS-SMFALTX-SUB-NUM =   WS-GI-SUBSCRIBER)                 GBIGPGM 
00513         MOVE 'Y' TO WS-SMFALTX-REC-FOUND                          GBIGPGM 
00514      ELSE                                                         GBIGPGM 
00515         PERFORM 6400-END-BROWSE-SMFALTX THRU 6400-EXIT            GBIGPGM 
00516         MOVE LOW-VALUES        TO   WS-SMFALTX-AREA-1             GBIGPGM 
00517         MOVE WS-GIC-GROUP-NUM  TO   WS-SMFALTX-GROUP              GBIGPGM 
00518         MOVE ZEROES            TO   WS-SMFALTX-SUB-7              GBIGPGM 
00519         MOVE WS-GI-SUBSCRIBER  TO   WS-SMFALTX-SUB-NUM            GBIGPGM 
00520         MOVE '9' TO WS-SMFALTX-GRP-1                              GBIGPGM 
00521         PERFORM 6000-START-BROWSE-SMFALTX THRU 6000-EXIT          GBIGPGM 
00522         PERFORM 6100-READ-SMFALTX THRU 6100-EXIT                  GBIGPGM 
00523      END-IF                                                       GBIGPGM 
00524                                                                   GBIGPGM 
00525      IF (WS-SMFALTX-GRP-6  =   WS-GIC-GRP-NO)      AND            GBIGPGM 
00526         (WS-SMFALTX-SUB-NUM =   WS-GI-SUBSCRIBER)                 GBIGPGM 
00527         MOVE 'Y' TO WS-SMFALTX-REC-FOUND                          GBIGPGM 
00528      END-IF                                                       GBIGPGM 
00529                                                                   GBIGPGM 
00530      IF SMFALTX-REC-FOUND                                         GBIGPGM 
00531           PERFORM 4200-DISPLAY-ALT-SECT THRU 4200-EXIT            GBIGPGM 
00532      ELSE                                                         GBIGPGM 
00533           MOVE  'SUBSCRIBER NUMBER NOT FOUND' TO IGERMSGO         GBIGPGM 
00534             MOVE                                                  GBIGPGM 
00535       'CORRECT GROUP/SUB & HIT ENTER OR PF3 FOR PREV MENU   '     GBIGPGM 
00536                   TO IGERLN2O                                     GBIGPGM 
00537           MOVE WS-GI-SUBSCRIBER TO IGSUBNO                        GBIGPGM 
00538           MOVE WS-GIC-GROUP-NUM TO IGGRPXO                        GBIGPGM 
00539      END-IF.                                                      GBIGPGM 
00540                                                                   GBIGPGM 
00541      PERFORM 9300-SEND-MAP THRU 9300-EXIT.                        GBIGPGM 
00542                                                                   GBIGPGM 
00543                                                                   GBIGPGM 
00544                                                                   GBIGPGM 
00545  4000-EXIT.                                                       GBIGPGM 
00546      EXIT.                                                        GBIGPGM 
00547                                                                   GBIGPGM 
00548 ************************************************************      GBIGPGM 
00549 *                                                          *      GBIGPGM 
00550 *     GET SECTION NUMBER FROM ALT INDEX RECORD             *      GBIGPGM 
00551 *                                                          *      GBIGPGM 
00552 ************************************************************      GBIGPGM 
00553 *4100-GET-SECTION.                                                GBIGPGM 
00554 *                                                                 GBIGPGM 
00555 *     MOVE SMFALTX-KEY-SUBSCRIBER TO WS-SMFALTX-SUBSCRIBER        GBIGPGM 
00556 *                                                                 GBIGPGM 
00557 *     IF (SMFALTX-KEY-GROUP = IGGRPXO      )   AND                GBIGPGM 
00558 *        (WS-SMFALTX-SUB-NUM  = IGSUBNO      )                    GBIGPGM 
00559 *                                                                 GBIGPGM 
00560 *             MOVE ZEROES            TO  GIC2-PLAN-CODE           GBIGPGM 
00561 *             MOVE SMFALTX-KEY-GROUP TO  GIC2-GROUP-NUM           GBIGPGM 
00562 *             MOVE WS-SMFALTX-SUB-NUM     TO GI2-SUBSCRIBER       GBIGPGM 
00563 *             MOVE SMFALTX-KEY-SECTION TO GIC2-SECTION-NUM        GBIGPGM 
00564 *                                                                 GBIGPGM 
00565 *        IF SMFALTX-CANCEL-COMP-DATE NOT NUMERIC                  GBIGPGM 
00566 *           PERFORM 8000-CONVERT-CANCEL-DT-6-TO-3 THRU 8000-EXIT  GBIGPGM 
00567 *           PERFORM 8100-CONVERT-CANCEL-DT-3-TO-8 THRU 8100-EXIT  GBIGPGM 
00568 *           IF MLDATE-RETURN  = '00'                              GBIGPGM 
00569 *              MOVE MLDATE-JUL2  TO WS-SUB-CANCEL-DT-JUL          GBIGPGM 
00570 *                                   GICB2-SLOT-NUMBER             GBIGPGM 
00571 *           ELSE                                                  GBIGPGM 
00572 *              MOVE 'IG01' TO WS-ABEND-CODE                       GBIGPGM 
00573 *              PERFORM 9900-ABEND  THRU 9900-EXIT                 GBIGPGM 
00574 *           END-IF                                                GBIGPGM 
00575 *        ELSE                                                     GBIGPGM 
00576 *          IF SMFALTX-CANCEL-COMP-DATE = 0                        GBIGPGM 
00577 *             MOVE +9999999     TO WS-SUB-CANCEL-DT-JUL           GBIGPGM 
00578 *                                  GICB2-SLOT-NUMBER              GBIGPGM 
00579 *          ELSE                                                   GBIGPGM 
00580 *             MOVE SMFALTX-CANCEL-COMP-DATE TO WS-COMP-DATE-NUM   GBIGPGM 
00581 *             PERFORM 8200-CONVERT-CANCEL-DT-NUM  THRU 8200-EXIT  GBIGPGM 
00582 *             IF MLDATE-RETURN  = '00'                            GBIGPGM 
00583 *                MOVE MLDATE-JUL2  TO WS-SUB-CANCEL-DT-JUL        GBIGPGM 
00584 *                                     GICB2-SLOT-NUMBER           GBIGPGM 
00585 *             ELSE                                                GBIGPGM 
00586 *                MOVE 'IG01' TO WS-ABEND-CODE                     GBIGPGM 
00587 *                PERFORM 9900-ABEND  THRU 9900-EXIT               GBIGPGM 
00588 *             END-IF                                              GBIGPGM 
00589 *          END-IF                                                 GBIGPGM 
00590 *        END-IF                                                   GBIGPGM 
00591 *     ELSE                                                        GBIGPGM 
00592 *         MOVE ZEROES            TO  GIC2-PLAN-CODE               GBIGPGM 
00593 *         MOVE SMFALTX-KEY-GROUP TO  GIC2-GROUP-NUM               GBIGPGM 
00594 *         MOVE WS-SMFALTX-SUB-NUM     TO GI2-SUBSCRIBER           GBIGPGM 
00595 *     END-IF.                                                     GBIGPGM 
00596 *                                                                 GBIGPGM 
00597 *                                                                 GBIGPGM 
00598 *4100-EXIT.                                                       GBIGPGM 
00599 *    EXIT.                                                        GBIGPGM 
00600                                                                   GBIGPGM 
00601                                                                   GBIGPGM 
00602 ************************************************************      GBIGPGM 
00603 *                                                          *      GBIGPGM 
00604 *  DISPLAY SECTION NUMBER FROM ALT INDEX RECORD            *      GBIGPGM 
00605 *                                                          *      GBIGPGM 
00606 ************************************************************      GBIGPGM 
00607  4200-DISPLAY-ALT-SECT.                                           GBIGPGM 
00608                                                                   GBIGPGM 
00609       MOVE 'N' TO WS-REC-IS-CURRENT-SW                            GBIGPGM 
00610                                                                   GBIGPGM 
00611       IF (WS-SMFALTX-GRP-6  = WS-GIC-GRP-NO     )  AND            GBIGPGM 
00612          (WS-SMFALTX-SUB-NUM    = WS-GI-SUBSCRIBER      ) AND     GBIGPGM 
00613           SMFALTX-CANCEL-COMP-DATE NUMERIC                        GBIGPGM 
00614                                                                   GBIGPGM 
00615          IF (SMFALTX-CANCEL-COMP-DATE = ZEROES)                   GBIGPGM 
00616                                                                   GBIGPGM 
00617             PERFORM 4300-DISPLAY-SSN-INFO THRU 4300-EXIT          GBIGPGM 
00618             MOVE SMFALTX-KEY-SUBSCRIBER TO WS-DISPLAY-SUB         GBIGPGM 
00619             MOVE WS-DISP-SUB-9          TO IGSUBNO                GBIGPGM 
00620             MOVE ZEROES       TO  WS-SMFALTX-GRP-1                GBIGPGM 
00621                                   WS-SMFALTX-GRP-2                GBIGPGM 
00622             MOVE WS-SMFALTX-GROUP       TO IGGRPXO                GBIGPGM 
00623             MOVE SMFALTX-KEY-SECTION    TO IGSECTO                GBIGPGM 
00624 *           MOVE '00'                   TO IGTRYYO                GBIGPGM 
00625 *                                          IGTRMMO                GBIGPGM 
00626 *                                          IGTRDDO                GBIGPGM 
00627             MOVE 'SUBSCRIBER NUMBER FOUND !       ' TO IGERMSGO   GBIGPGM 
00628             MOVE 'HIT PF9 TO CONTINUE  OR  PF3 FOR PREV MENU'     GBIGPGM 
00629                   TO IGERLN2O                                     GBIGPGM 
00630             MOVE 'Y' TO WS-REC-IS-CURRENT-SW                      GBIGPGM 
00631          END-IF                                                   GBIGPGM 
00632       END-IF                                                      GBIGPGM 
00633                                                                   GBIGPGM 
00634       IF REC-IS-CURRENT                                           GBIGPGM 
00635          GO TO 4200-EXIT                                          GBIGPGM 
00636       ELSE                                                        GBIGPGM 
00637          IF SMFALTX-CANCEL-COMP-DATE NUMERIC                      GBIGPGM 
00638             MOVE SMFALTX-CANCEL-COMP-DATE TO WS-COMP-DATE-NUM     GBIGPGM 
00639             PERFORM 8200-CONVERT-CANCEL-DT-NUM  THRU 8200-EXIT    GBIGPGM 
00640          END-IF                                                   GBIGPGM 
00641          IF SMFALTX-CANCEL-COMP-DATE NOT NUMERIC                  GBIGPGM 
00642             PERFORM 8000-CONVERT-CANCEL-DT-6-TO-3 THRU 8000-EXIT  GBIGPGM 
00643             PERFORM 8100-CONVERT-CANCEL-DT-3-TO-8 THRU 8100-EXIT  GBIGPGM 
00644          END-IF                                                   GBIGPGM 
00645                                                                   GBIGPGM 
00646             PERFORM 4300-DISPLAY-SSN-INFO THRU 4300-EXIT          GBIGPGM 
00647             MOVE SMFALTX-KEY-SUBSCRIBER TO WS-DISPLAY-SUB         GBIGPGM 
00648             MOVE WS-DISP-SUB-9          TO IGSUBNO                GBIGPGM 
00649             MOVE ZEROES       TO  WS-SMFALTX-GRP-1                GBIGPGM 
00650                                   WS-SMFALTX-GRP-2                GBIGPGM 
00651             MOVE WS-SMFALTX-GROUP       TO IGGRPXO                GBIGPGM 
00652             MOVE SMFALTX-KEY-SECTION    TO IGSECTO                GBIGPGM 
00653          MOVE WS-CONV-YY-GREG TO IGTRYYO                          GBIGPGM 
00654          MOVE WS-CONV-MM-GREG TO IGTRMMO                          GBIGPGM 
00655          MOVE WS-CONV-DD-GREG TO IGTRDDO                          GBIGPGM 
00656          MOVE 'SUBSCRIBER IS CANCELLED FOR THIS GROUP/SECT'       GBIGPGM 
00657                 TO IGERMSGO                                       GBIGPGM 
00658          MOVE                                                     GBIGPGM 
00659       'CORRECT GROUP/SUB & HIT ENTER  OR  PF3 FOR PREV MENU'      GBIGPGM 
00660                   TO IGERLN2O                                     GBIGPGM 
00661       END-IF.                                                     GBIGPGM 
00662                                                                   GBIGPGM 
00663                                                                   GBIGPGM 
00664                                                                   GBIGPGM 
00665  4200-EXIT.                                                       GBIGPGM 
00666      EXIT.                                                        GBIGPGM 
00667                                                                   GBIGPGM 
00668                                                                   GBIGPGM 
00669 ************************************************************      GBIGPGM 
00670 *                                                          *      GBIGPGM 
00671 *                                                          *      GBIGPGM 
00672 *                                                          *      GBIGPGM 
00673 ************************************************************      GBIGPGM 
00674  4300-DISPLAY-SSN-INFO.                                           GBIGPGM 
00675                                                                   GBIGPGM 
00676      MOVE ZEROES        TO WS-SSN-1-3                             GBIGPGM 
00677      MOVE WS-GI-SUBSCRIBER TO WS-SSN-4-12                         GBIGPGM 
00678                                                                   GBIGPGM 
00679      PERFORM 6200-START-BROWSE-SSNFILE THRU 6200-EXIT             GBIGPGM 
00680                                                                   GBIGPGM 
00681      IF WS-SSNFILE-RESP1 = DFHRESP(NOTFND)                        GBIGPGM 
00682           MOVE  'SUBSCRIBER NUMBER NOT FOUND' TO IGERMSGO         GBIGPGM 
00683           MOVE WS-GI-SUBSCRIBER   TO IGSUBNO                      GBIGPGM 
00684           GO TO 4300-EXIT                                         GBIGPGM 
00685      END-IF                                                       GBIGPGM 
00686                                                                   GBIGPGM 
00687      PERFORM 6300-READ-SSNFILE THRU 6300-EXIT                     GBIGPGM 
00688                                                                   GBIGPGM 
00689      MOVE 'N' TO WS-SSN-MATCH-FD-SW                               GBIGPGM 
00690                                                                   GBIGPGM 
00691      PERFORM 4400-LOAD-SSN-FIELDS THRU 4400-EXIT UNTIL            GBIGPGM 
00692              SSN-MATCH-FOUND OR                                   GBIGPGM 
00693              (5222-SSN-4-12 NOT = WS-GI-SUBSCRIBER).              GBIGPGM 
00694                                                                   GBIGPGM 
00695      IF NO-SSN-MATCH-FOUND                                        GBIGPGM 
00696           MOVE  'SUB NUMBER NOT FOUND ON SSN FILE' TO IGERMSGO    GBIGPGM 
00697           MOVE WS-GI-SUBSCRIBER   TO IGSUBNO                      GBIGPGM 
00698      END-IF.                                                      GBIGPGM 
00699                                                                   GBIGPGM 
00700                                                                   GBIGPGM 
00701  4300-EXIT.                                                       GBIGPGM 
00702      EXIT.                                                        GBIGPGM 
00703                                                                   GBIGPGM 
00704                                                                   GBIGPGM 
00705  4400-LOAD-SSN-FIELDS.                                            GBIGPGM 
00706                                                                   GBIGPGM 
00707      EVALUATE  WS-SSNFILE-RESP1                                   GBIGPGM 
00708         WHEN DFHRESP(NORMAL)                                      GBIGPGM 
00709              CONTINUE                                             GBIGPGM 
00710         WHEN OTHER                                                GBIGPGM 
00711           MOVE  'ERROR READING SSNFILE FILE'  TO IGERMSGO         GBIGPGM 
00712           MOVE WS-GI-SUBSCRIBER TO IGSUBNO                        GBIGPGM 
00713           GO TO 4400-EXIT                                         GBIGPGM 
00714      END-EVALUATE.                                                GBIGPGM 
00715                                                                   GBIGPGM 
00716      IF  5222-SSN-4-12 NOT = WS-GI-SUBSCRIBER                     GBIGPGM 
00717          GO TO 4400-EXIT                                          GBIGPGM 
00718      END-IF.                                                      GBIGPGM 
00719                                                                   GBIGPGM 
00720       IF (5222-GROUP-NO  = WS-GIC-GRP-NO )  AND                   GBIGPGM 
00721          (5222-SSN-4-12  =     WS-GI-SUBSCRIBER )                 GBIGPGM 
00722                                                                   GBIGPGM 
00723         MOVE 'Y' TO WS-SSN-MATCH-FD-SW                            GBIGPGM 
00724         MOVE 5222-GROUP-NO TO IGGRPNO                             GBIGPGM 
00725         MOVE 5222-SUB-NO   TO WS-PRIMARY-SSN-DISPLAY              GBIGPGM 
00726         MOVE WS-PRIM-SSN-9 TO IGSUBPO                             GBIGPGM 
00727         MOVE 5222-LAST-NAME TO IGLNAMO                            GBIGPGM 
00728         MOVE 5222-FIRST-NAME TO IGFNAMO                           GBIGPGM 
00729 *       MOVE 5222-ORIG-EFF-DATE TO DATE-YMD-3                     GBIGPGM 
00730 *       PERFORM 8100-CONVERT-CANCEL-DT-3-TO-8  THRU 8100-EXIT     GBIGPGM 
00731 *       MOVE WS-CONV-YY-GREG TO IGEFYYO                           GBIGPGM 
00732 *       MOVE WS-CONV-MM-GREG TO IGEFMMO                           GBIGPGM 
00733 *       MOVE WS-CONV-DD-GREG TO IGEFDDO                           GBIGPGM 
00734                                                                   GBIGPGM 
00735       END-IF.                                                     GBIGPGM 
00736                                                                   GBIGPGM 
00737      PERFORM 6300-READ-SSNFILE THRU 6300-EXIT.                    GBIGPGM 
00738                                                                   GBIGPGM 
00739  4400-EXIT.                                                       GBIGPGM 
00740      EXIT.                                                        GBIGPGM 
00741                                                                   GBIGPGM 
00742                                                                   GBIGPGM 
00743                                                                   GBIGPGM 
00744                                                                   GBIGPGM 
00745  5000-XCTL-TO-GBIBPGM.                                            GBIGPGM 
00746                                                                   GBIGPGM 
00747      IF IGSECTL > ZERO                                            GBIGPGM 
00748         CONTINUE                                                  GBIGPGM 
00749      ELSE                                                         GBIGPGM 
00750         MOVE                                                      GBIGPGM 
00751      'GROUP/SECTION NOT FOUND. RE-ENTER SUB/GRP OR PF3 FOR MENU'  GBIGPGM 
00752         TO IGERLN2O                                               GBIGPGM 
00753      END-IF.                                                      GBIGPGM 
00754                                                                   GBIGPGM 
00755      MOVE IGSUBPO          TO GI2-SUBSCRIBER                      GBIGPGM 
00756                                                                   GBIGPGM 
00757      MOVE ZEROES           TO  GIC2-PLAN-CODE                     GBIGPGM 
00758                                GIC2-GROUP-NO-1-3                  GBIGPGM 
00759      MOVE IGGRPNO          TO GIC2-GRP-NO                         GBIGPGM 
00760      MOVE IGSECTO          TO GIC2-SECTION-NUM                    GBIGPGM 
00761      MOVE 'GBIG'           TO GI2-MULT-PATH-ID                    GBIGPGM 
00762      MOVE 'GS'             TO GI2-RETURN-CODE                     GBIGPGM 
00763 *                                                                 GBIGPGM 
00764 **** MOVE DATE OF SERVICE TO SLOT NUMBER FIELD                    GBIGPGM 
00765      MOVE IGSVDTO              TO GIGT2-SLOT-NUMBER               GBIGPGM 
00766                                                                   GBIGPGM 
00767                                                                   GBIGPGM 
00768                                                                   GBIGPGM 
00769      EXEC CICS XCTL  PROGRAM('GBIBPGM')                           GBIGPGM 
00770                      COMMAREA(GI-COMMAREA2-RECORD)                GBIGPGM 
00771                      LENGTH  (WS-COMM-KEY-LEN)                    GBIGPGM 
00772                      END-EXEC.                                    GBIGPGM 
00773                                                                   GBIGPGM 
00774                                                                   GBIGPGM 
00775  5000-EXIT.                                                       GBIGPGM 
00776      EXIT.                                                        GBIGPGM 
00777                                                                   GBIGPGM 
00778                                                                   GBIGPGM 
00779                                                                   GBIGPGM 
00780 ************************************************************      GBIGPGM 
00781 *                                                          *      GBIGPGM 
00782 *                                                          *      GBIGPGM 
00783 *                                                          *      GBIGPGM 
00784 ************************************************************      GBIGPGM 
00785  6000-START-BROWSE-SMFALTX.                                       GBIGPGM 
00786                                                                   GBIGPGM 
00787      EXEC CICS STARTBR GTEQ                                       GBIGPGM 
00788                DATASET ('SMFALTX ')                               GBIGPGM 
00789                RIDFLD  (WS-SMFALTX-GRP-SUB)                       GBIGPGM 
00790                RESP    (WS-SMFALTX-RESP1)                         GBIGPGM 
00791                END-EXEC.                                          GBIGPGM 
00792                                                                   GBIGPGM 
00793  6000-EXIT.                                                       GBIGPGM 
00794      EXIT.                                                        GBIGPGM 
00795                                                                   GBIGPGM 
00796 ************************************************************      GBIGPGM 
00797 *                                                          *      GBIGPGM 
00798 *     READ SUBSCRIBER ALTERNATE INDEX FILE                 *      GBIGPGM 
00799 *                                                          *      GBIGPGM 
00800 ************************************************************      GBIGPGM 
00801  6100-READ-SMFALTX.                                               GBIGPGM 
00802                                                                   GBIGPGM 
00803      EXEC CICS READNEXT                                           GBIGPGM 
00804                DATASET ('SMFALTX ')                               GBIGPGM 
00805                INTO    (SMFALTX-KEY)                              GBIGPGM 
00806                RIDFLD  (WS-SMFALTX-KEY)                           GBIGPGM 
00807                RESP    (WS-SMFALTX-RESP1)                         GBIGPGM 
00808                END-EXEC.                                          GBIGPGM 
00809                                                                   GBIGPGM 
00810                                                                   GBIGPGM 
00811  6100-EXIT.                                                       GBIGPGM 
00812      EXIT.                                                        GBIGPGM 
00813                                                                   GBIGPGM 
00814 ************************************************************      GBIGPGM 
00815 *                                                          *      GBIGPGM 
00816 *                                                          *      GBIGPGM 
00817 *                                                          *      GBIGPGM 
00818 ************************************************************      GBIGPGM 
00819  6200-START-BROWSE-SSNFILE.                                       GBIGPGM 
00820                                                                   GBIGPGM 
00821      EXEC CICS STARTBR GTEQ                                       GBIGPGM 
00822                DATASET ('SSNIFILE')                               GBIGPGM 
00823                RIDFLD  (WS-SSN-RECORD-KEY)                        GBIGPGM 
00824                RESP    (WS-SSNFILE-RESP1)                         GBIGPGM 
00825                END-EXEC.                                          GBIGPGM 
00826                                                                   GBIGPGM 
00827  6200-EXIT.                                                       GBIGPGM 
00828      EXIT.                                                        GBIGPGM 
00829                                                                   GBIGPGM 
00830 ************************************************************      GBIGPGM 
00831 *                                                          *      GBIGPGM 
00832 *     READ SUBSCRIBER SOCIAL SECURITY NUMBER FILE          *      GBIGPGM 
00833 *                                                          *      GBIGPGM 
00834 ************************************************************      GBIGPGM 
00835  6300-READ-SSNFILE.                                               GBIGPGM 
00836                                                                   GBIGPGM 
00837      EXEC CICS READNEXT                                           GBIGPGM 
00838                DATASET ('SSNIFILE')                               GBIGPGM 
00839                INTO    (WS-SSN-FILE-AREA-2)                       GBIGPGM 
00840                RIDFLD  (WS-SSN-RECORD-KEY)                        GBIGPGM 
00841                RESP    (WS-SSNFILE-RESP1)                         GBIGPGM 
00842                END-EXEC.                                          GBIGPGM 
00843                                                                   GBIGPGM 
00844                                                                   GBIGPGM 
00845  6300-EXIT.                                                       GBIGPGM 
00846      EXIT.                                                        GBIGPGM 
00847                                                                   GBIGPGM 
00848                                                                   GBIGPGM 
00849 ************************************************************      GBIGPGM 
00850 *                                                          *      GBIGPGM 
00851 *                                                          *      GBIGPGM 
00852 *                                                          *      GBIGPGM 
00853 ************************************************************      GBIGPGM 
00854  6400-END-BROWSE-SMFALTX.                                         GBIGPGM 
00855                                                                   GBIGPGM 
00856      EXEC CICS ENDBR                                              GBIGPGM 
00857                DATASET ('SMFALTX ')                               GBIGPGM 
00858                RESP    (WS-SMFALTX-RESP1)                         GBIGPGM 
00859                END-EXEC.                                          GBIGPGM 
00860                                                                   GBIGPGM 
00861  6400-EXIT.                                                       GBIGPGM 
00862      EXIT.                                                        GBIGPGM 
00863                                                                   GBIGPGM 
00864                                                                   GBIGPGM 
00865 ************************************************************      GBIGPGM 
00866 *                                                          *      GBIGPGM 
00867 *                                                          *      GBIGPGM 
00868 ************************************************************      GBIGPGM 
00869  8000-CONVERT-CANCEL-DT-6-TO-3.                                   GBIGPGM 
00870                                                                   GBIGPGM 
00871      MOVE SMFALTX-CANCEL-COMP-DATE TO DATE-YMD-6                  GBIGPGM 
00872      MOVE '6' TO WS-CSDTCONV-OPERATION.                           GBIGPGM 
00873      CALL 'CSDTCONV' USING DATE-YMD-6                             GBIGPGM 
00874                      DATE-YMD-3                                   GBIGPGM 
00875                      WS-CSDTCONV-OPERATION.                       GBIGPGM 
00876                                                                   GBIGPGM 
00877                                                                   GBIGPGM 
00878  8000-EXIT.                                                       GBIGPGM 
00879      EXIT.                                                        GBIGPGM 
00880                                                                   GBIGPGM 
00881                                                                   GBIGPGM 
00882                                                                   GBIGPGM 
00883 ************************************************************      GBIGPGM 
00884 *                                                          *      GBIGPGM 
00885 *                                                          *      GBIGPGM 
00886 ************************************************************      GBIGPGM 
00887  8100-CONVERT-CANCEL-DT-3-TO-8.                                   GBIGPGM 
00888                                                                   GBIGPGM 
00889      MOVE '2' TO WS-CSDTCONV-OPERATION.                           GBIGPGM 
00890      CALL 'CSDTCONV' USING DATE-YMD-3                             GBIGPGM 
00891                      WS-CSDTCONV-UNPACKED-DT                      GBIGPGM 
00892                      WS-CSDTCONV-OPERATION.                       GBIGPGM 
00893                                                                   GBIGPGM 
00894      MOVE WS-CSDTCONV-UNPACKED-DT TO WS-CONV-8-GREG.              GBIGPGM 
00895      PERFORM 8500-CONV-8-GREG-TO-JULIAN   THRU 8500-EXIT.         GBIGPGM 
00896                                                                   GBIGPGM 
00897  8100-EXIT.                                                       GBIGPGM 
00898      EXIT.                                                        GBIGPGM 
00899                                                                   GBIGPGM 
00900                                                                   GBIGPGM 
00901 ************************************************************      GBIGPGM 
00902 *                                                          *      GBIGPGM 
00903 *                                                          *      GBIGPGM 
00904 ************************************************************      GBIGPGM 
00905                                                                   GBIGPGM 
00906  8200-CONVERT-CANCEL-DT-NUM.                                      GBIGPGM 
00907                                                                   GBIGPGM 
00908       COMPUTE WS-CONV-YYMMDD =                                    GBIGPGM 
00909            +999999 - WS-COMP-DATE-NUM                             GBIGPGM 
00910       MOVE WS-CONV-YYMMDD TO WS-CONV-6-GREG                       GBIGPGM 
00911                                                                   GBIGPGM 
00912       IF WS-CONV-YY-GREG > '70'                                   GBIGPGM 
00913          MOVE '19' TO  WS-CONV-2-GREG                             GBIGPGM 
00914       ELSE                                                        GBIGPGM 
00915          MOVE '20' TO  WS-CONV-2-GREG                             GBIGPGM 
00916       END-IF.                                                     GBIGPGM 
00917                                                                   GBIGPGM 
00918      PERFORM 8500-CONV-8-GREG-TO-JULIAN   THRU 8500-EXIT.         GBIGPGM 
00919                                                                   GBIGPGM 
00920                                                                   GBIGPGM 
00921                                                                   GBIGPGM 
00922  8200-EXIT.                                                       GBIGPGM 
00923      EXIT.                                                        GBIGPGM 
00924                                                                   GBIGPGM 
00925 ************************************************************      GBIGPGM 
00926 *                                                          *      GBIGPGM 
00927 *                                                          *      GBIGPGM 
00928 ************************************************************      GBIGPGM 
00929  8300-CONVERT-CANCEL-DT-3-TO-3.                                   GBIGPGM 
00930                                                                   GBIGPGM 
00931      MOVE '4' TO WS-CSDTCONV-OPERATION.                           GBIGPGM 
00932      CALL 'CSDTCONV' USING DATE-YMD-3-COMP1                       GBIGPGM 
00933                      DATE-YMD-3-COMP2                             GBIGPGM 
00934                      WS-CSDTCONV-OPERATION.                       GBIGPGM 
00935                                                                   GBIGPGM 
00936                                                                   GBIGPGM 
00937  8300-EXIT.                                                       GBIGPGM 
00938      EXIT.                                                        GBIGPGM 
00939                                                                   GBIGPGM 
00940                                                                   GBIGPGM 
00941                                                                   GBIGPGM 
00942 ************************************************************      GBIGPGM 
00943 *                                                          *      GBIGPGM 
00944 *                                                          *      GBIGPGM 
00945 ************************************************************      GBIGPGM 
00946  8500-CONV-8-GREG-TO-JULIAN.                                      GBIGPGM 
00947                                                                   GBIGPGM 
00948      MOVE 'CNV'  TO  MLDATE-FUNC.                                 GBIGPGM 
00949      MOVE 'Y'    TO  MLDATE-FORM1.                                GBIGPGM 
00950      MOVE WS-CONV-8-GREG  TO  MLDATE-DATE1.                       GBIGPGM 
00951      MOVE 'J'    TO  MLDATE-FORM2.                                GBIGPGM 
00952      EXEC CICS LINK PROGRAM ('MLDATEC')                           GBIGPGM 
00953                     COMMAREA (MLDATE01)                           GBIGPGM 
00954                     END-EXEC.                                     GBIGPGM 
00955                                                                   GBIGPGM 
00956  8500-EXIT.                                                       GBIGPGM 
00957      EXIT.                                                        GBIGPGM 
00958                                                                   GBIGPGM 
00959                                                                   GBIGPGM 
00960 ************************************************************      GBIGPGM 
00961 *                                                          *      GBIGPGM 
00962 *    CICS RETURN                                           *      GBIGPGM 
00963 *                                                          *      GBIGPGM 
00964 ************************************************************      GBIGPGM 
00965  9100-RETURN.                                                     GBIGPGM 
00966                                                                   GBIGPGM 
00967      EXEC CICS RETURN END-EXEC.                                   GBIGPGM 
00968                                                                   GBIGPGM 
00969  9100-EXIT.                                                       GBIGPGM 
00970      EXIT.                                                        GBIGPGM 
00971                                                                   GBIGPGM 
00972 ***************************************************************   GBIGPGM 
00973 *                                                             *   GBIGPGM 
00974 *           RECEIVE THE CURRENT SCREEN                        *   GBIGPGM 
00975 *                                                             *   GBIGPGM 
00976 ***************************************************************   GBIGPGM 
00977  9200-RECEIVE-SCREEN.                                             GBIGPGM 
00978                                                                   GBIGPGM 
00979      EXEC CICS RECEIVE   MAP   ('GBIGI01')                        GBIGPGM 
00980                          MAPSET('GBIGSET')                        GBIGPGM 
00981                          INTO  (GBIGI01I)                         GBIGPGM 
00982                          END-EXEC.                                GBIGPGM 
00983                                                                   GBIGPGM 
00984  9200-EXIT.                                                       GBIGPGM 
00985      EXIT.                                                        GBIGPGM 
00986                                                                   GBIGPGM 
00987                                                                   GBIGPGM 
00988 ***************************************************************   GBIGPGM 
00989 *                                                             *   GBIGPGM 
00990 *           SEND MAP                                          *   GBIGPGM 
00991 *                                                             *   GBIGPGM 
00992 ***************************************************************   GBIGPGM 
00993  9300-SEND-MAP.                                                   GBIGPGM 
00994                                                                   GBIGPGM 
00995      MOVE -1 TO IGSUBNL.                                          GBIGPGM 
00996                                                                   GBIGPGM 
00997                                                                   GBIGPGM 
00998      EXEC CICS SEND      MAP   ('GBIGI01')                        GBIGPGM 
00999                          MAPSET('GBIGSET')                        GBIGPGM 
01000                          ERASE                                    GBIGPGM 
01001                          FROM (GBIGI01O)                          GBIGPGM 
01002                          CURSOR                                   GBIGPGM 
01003                          END-EXEC.                                GBIGPGM 
01004                                                                   GBIGPGM 
01005  9300-EXIT.                                                       GBIGPGM 
01006      EXIT.                                                        GBIGPGM 
01007                                                                   GBIGPGM 
01008                                                                   GBIGPGM 
01009  9400-XCTL-TO-GHILPGM.                                            GBIGPGM 
01010                                                                   GBIGPGM 
01011      MOVE IGGRPXO  TO  GIC2-GROUP-NUM                             GBIGPGM 
01012      MOVE IGSUBNO  TO  GI2-SUBSCRIBER                             GBIGPGM 
01013      MOVE 'GS'     TO  GI2-RETURN-CODE                            GBIGPGM 
01014                                                                   GBIGPGM 
01015            EXEC CICS XCTL  PROGRAM('GHILPGM')                     GBIGPGM 
01016                            COMMAREA(GI-COMMAREA2-RECORD)          GBIGPGM 
01017                            LENGTH  (WS-COMM-KEY-LEN)              GBIGPGM 
01018                            END-EXEC.                              GBIGPGM 
01019                                                                   GBIGPGM 
01020  9400-EXIT.                                                       GBIGPGM 
01021      EXIT.                                                        GBIGPGM 
01022                                                                   GBIGPGM 
01023  9500-SEND-DATAONLY.                                              GBIGPGM 
01024                                                                   GBIGPGM 
01025      MOVE -1 TO IGSUBNL                                           GBIGPGM 
01026                                                                   GBIGPGM 
01027      EXEC CICS  SEND MAP   ('GBIGI01')                            GBIGPGM 
01028                      MAPSET('GBIGSET')                            GBIGPGM 
01029                      FREEKB                                       GBIGPGM 
01030                      DATAONLY                                     GBIGPGM 
01031                      CURSOR                                       GBIGPGM 
01032                      END-EXEC.                                    GBIGPGM 
01033                                                                   GBIGPGM 
01034  9500-EXIT.                                                       GBIGPGM 
01035      EXIT.                                                        GBIGPGM 
01036                                                                   GBIGPGM 
01037 /***************************************************************  GBIGPGM 
01038 *                                                              *  GBIGPGM 
01039 * 9900  ABEND THE TASK                                         *  GBIGPGM 
01040 *                                                              *  GBIGPGM 
01041 ****************************************************************  GBIGPGM 
01042  9900-ABEND.                                                      GBIGPGM 
01043                                                                   GBIGPGM 
01044      EXEC CICS  ABEND                                             GBIGPGM 
01045                 ABCODE(WS-ABEND-CODE)                             GBIGPGM 
01046      END-EXEC.                                                    GBIGPGM 
01047                                                                   GBIGPGM 
01048                                                                   GBIGPGM 
01049  9900-EXIT.                                                       GBIGPGM 
01050      EXIT.                                                        GBIGPGM 
01051                                                                   GBIGPGM 
01052                                                                   GBIGPGM 
01053 ************************************************************      GBIGPGM 
01054 *                                                          *      GBIGPGM 
01055 *                                                          *      GBIGPGM 
01056 *                                                          *      GBIGPGM 
01057 ************************************************************      GBIGPGM 
01058                                                                   GBIGPGM 
