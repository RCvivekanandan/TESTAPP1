00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUABEND
00003  PROGRAM-ID.         ELUABEND.                                       LV001
00004                                                                   ELUABEND
00005  AUTHOR.             EDWARD G LISS                                ELUABEND
00006                                                                   ELUABEND
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUABEND
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELUABEND
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUABEND
00010                      233 N. MICHIGAN AVE                          ELUABEND
00011                      CHICAGO, ILLINOIS 60601                      ELUABEND
00012                                                                   ELUABEND
00013  DATE-WRITTEN.       19-MAY-1989.                                 ELUABEND
00014                                                                   ELUABEND
00015  DATE-COMPILED.                                                   ELUABEND
00016                                                                   ELUABEND
00017  SECURITY.           COPYRIGHT 1989,                              ELUABEND
00018                      HEALTH CARE SERVICE CORPORATION              ELUABEND
00019      SKIP3                                                        ELUABEND
00020  ENVIRONMENT DIVISION.                                            ELUABEND
00021                                                                   ELUABEND
00022  CONFIGURATION SECTION.                                           ELUABEND
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELUABEND
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELUABEND
00025  TITLE 'ELS ABEND HANDLER PROGRAM'.                               ELUABEND
00026 ******************************************************************ELUABEND
00027 *                                                                *ELUABEND
00028 *                      MAINTENANCE HISTORY                       *ELUABEND
00029 *                                                                *ELUABEND
00030 *  MOD     DATE     BY  DRPT                ACTION               *ELUABEND
00031 * ----- ----------- --- ----- ---------------------------------- *ELUABEND
00032 * 01.00 19-MAY-1989 EGL       CREATED NEW VERSION TO WRITE ALL   *ELUABEND
00033 *                             USER AREAS TO A SNAP SHOT FILES    *ELUABEND
00034 *                             FOR FURTHER ANALYSIS.              *ELUABEND
00035 * 01.01 03-JAN-1990 EGL       INITIALIZED WORKING STORAGE TABLE  *ELUABEND
00036 *                             SIZE TO ZERO TO SOLVE PROBLEM WITH *ELUABEND
00037 *                             RECORDS NOT BEING DUMPED.          *ELUABEND
00038 * 01.02 16-MAR-1990 EGL       CORRECTE PROGRAM TO SUPPRESS THE   *ELUABEND
00039 *                             WRITING OF AN AREA GREATER THAN    *ELUABEND
00040 *                             65K.                               *ELUABEND
00041 *                                                                *ELUABEND
00042 * 01.03 01-OCT-1997 AKK       ADD SUPPORT FOR YR 2000 AND TEXAS  *ELUABEND
00043 *                             MERGER.                            *ELUABEND
00044 *                                                                *ELUABEND
00045 ******************************************************************ELUABEND
00046                                                                   ELUABEND
00047  DATA DIVISION.                                                   ELUABEND
00048  WORKING-STORAGE SECTION.                                         ELUABEND
00049  01  WS-LITERALS.                                                 ELUABEND
00050    05  WS-ONE                      PIC S9(4) COMP VALUE 1.        ELUABEND
00051    05  WS-KEY-INFO-1.                                             ELUABEND
00052      10  FILLER                    PIC X(7)     VALUE 'GROUP: '.  ELUABEND
00053      10  WS-HDR1-GROUP-NO          PIC X(6)     VALUE ALL '?'.    ELUABEND
00054      10  FILLER                    PIC X(10) VALUE ' SECTION: '.  ELUABEND
00055      10  WS-HDR1-SECT-NO           PIC X(5)     VALUE ALL '?'.    ELUABEND
00056      10  FILLER                    PIC X        VALUE SPACES.     ELUABEND
00057      10  WS-HDR1-FAM-REL           PIC X(22)    VALUE ALL '?'.    ELUABEND
00058      10  FILLER                    PIC X(07)    VALUE ' FROM: '.  ELUABEND
00059      10  WS-HDR1-FROM-DATE         PIC 99/99/99 VALUE ZERO.       ELUABEND
00060      10  FILLER                    PIC X(05)    VALUE ' TO: '.    ELUABEND
00061      10  WS-HDR1-TO-DATE           PIC 99/99/99 VALUE ZERO.       ELUABEND
00062    05  WS-MSG1-1                   PIC X(40) VALUE                ELUABEND
00063          'SSD TECH SUPPORT HAS BEEN NOTIFIED THAT '.              ELUABEND
00064    05  WS-MSG1-2                   PIC X(20) VALUE                ELUABEND
00065          ' ABEND HAS OCCURRED.'.                                  ELUABEND
00066    05  WS-MSG2-1.                                                 ELUABEND
00067      10  FILLER                    PIC X(40) VALUE                ELUABEND
00068               'AN ELIQ APPLICATION ERROR HAS OCCURRED '.          ELUABEND
00069      10  FILLER                    PIC X(22) VALUE                ELUABEND
00070               'WHILE PROCESSING THE '.                            ELUABEND
00071    05  WS-MSG2-2                   PICTURE X(7) VALUE ' TOPIC.'.  ELUABEND
00072    05  WS-MSG3-1                   PIC X(36) VALUE                ELUABEND
00073                'PLEASE NOTIFY THE HELP DESK AT X6675'.            ELUABEND
00074    05  WS-MSG3-2                   PIC X(36) VALUE                ELUABEND
00075                'AND GIVE THE FOLLOWING ABEND CODE - '.            ELUABEND
00076    05  WS-MSG4-1                   PIC X(35) VALUE                ELUABEND
00077                'PRESS <ENTER> TO CONTINUE WITH ELIQ'.             ELUABEND
00078    05  WS-MSG5-1                   PIC X(26) VALUE                ELUABEND
00079                'PRESS <CLEAR> TO END ELIQ'.                       ELUABEND
00080    05  WS-MSG6-1                   PIC X(5)  VALUE 'FILE '.       ELUABEND
00081    05  WS-MSG6-2                   PIC X(11) VALUE ' IS CLOSED.'. ELUABEND
00082    05  WS-MSG6-3                   PIC X(26) VALUE                ELUABEND
00083                ' HAD A CRITICAL I/O ERROR.'.                      ELUABEND
00084    05  WS-MSG7-1.                                                 ELUABEND
00085        10  FILLER                  PIC X(22) VALUE                ELUABEND
00086                'AN UNDOCUMENTED ABEND '.                          ELUABEND
00087        10  WS-MSG7-ABCODE          PIC X(4).                      ELUABEND
00088        10  FILLER                  PIC X(14) VALUE                ELUABEND
00089                ' HAS OCCURRED.'.                                  ELUABEND
00090    05  WS-MSG8-1.                                                 ELUABEND
00091        10  FILLER                  PIC X(4)  VALUE '*** '.        ELUABEND
00092        10  FILLER                  PIC X(39) VALUE                ELUABEND
00093                'UNABLE TO PRODUCE A COMPLETE SNAPSHOT'.           ELUABEND
00094        10  FILLER                  PIC X(16) VALUE                ELUABEND
00095                'FOR THIS ABEND.'.                                 ELUABEND
00096        10  FILLER                  PIC X(4)  VALUE '*** '.        ELUABEND
00097 /                                                                 ELUABEND
00098  01  WS-MISC-STUFF.                                               ELUABEND
00099      05  WS-TWA-LEN                PIC S9(4) COMP.                ELUABEND
00100      05  WS-NEXT-TEXT-LINE         PIC S9(4) COMP.                ELUABEND
00101      05  WS-TWA-PTR                POINTER.                       ELUABEND
00102      05  WS-ABCODE                 PIC X(4).                      ELUABEND
00103      05  WS-ABCODE-RED   REDEFINES WS-ABCODE.                     ELUABEND
00104          10  WS-ABCODE-1-2         PIC XX.                        ELUABEND
00105              88  WS-ABCODE-ELS         VALUE 'EL'.                ELUABEND
00106          10  WS-ABCODE-3-4         PIC XX.                        ELUABEND
00107      05  WS-RECOVERY-ABCODE        PIC X(4).                      ELUABEND
00108      05  WS-RECORD-PTR             POINTER.                       ELUABEND
00109      05  WS-RECORD-LEN             PIC S9(8) COMP.                ELUABEND
00110      05  WS-REMAIN-LEN             PIC S9(8) COMP.                ELUABEND
00111      05  WS-DUMP-POS               PIC S9(8) COMP.                ELUABEND
00112      05  WS-MAX-LEN                PIC S9(8) COMP.                ELUABEND
00113      05  WS-MOVE-LEN               PIC S9(8) COMP.                ELUABEND
00114      05  WS-RBA                    PIC S9(8) COMP.                ELUABEND
00115      05  WS-PAD-CHAR               PIC X     VALUE ' '.           ELUABEND
00116      05  WS-ABEND-QUEUE-ID         PIC X(8)  VALUE 'ELSABEND'.    ELUABEND
00117      05  WS-SNAPSHOT-FILE-ID       PIC X(8)  VALUE 'ELSSNAPS'.    ELUABEND
00118      05  WS-DATE-EDIT              PIC 99/99/99.                  ELUABEND
00119      05  WS-SUB                    PIC S9(4) COMP.                ELUABEND
00120      05  WS-CHECK-SUB              PIC S9(4) COMP.                ELUABEND
00121      05  WS-SMA-SUB                PIC S9(4) COMP.                ELUABEND
00122      05  WS-MOVE-SUB               PIC S9(4) COMP.                ELUABEND
00123      05  WS-FILE-FULL-SW           PIC X     VALUE 'N'.           ELUABEND
00124          88  WS-FILE-NOT-FULL                VALUE 'N'.           ELUABEND
00125          88  WS-FILE-FULL                    VALUE 'Y'.           ELUABEND
00126      05  WS-TOPIC-TITLE            PIC X(80) VALUE SPACES.        ELUABEND
00127      05  WS-TERMINATE-SW           PIC X     VALUE 'N'.           ELUABEND
00128          88  WS-TERMINATE-PGM                VALUE 'Y'.           ELUABEND
00129      05  WS-TERMINATION-MSG        PIC X(80) VALUE SPACES.        ELUABEND
00130      05  WS-TSQ-COUNT              PIC S9(4) COMP.                ELUABEND
00131      05  WS-TSQ-LENGTH             PIC S9(4) COMP.                ELUABEND
00132      05  WS-TSQ-POINTER            POINTER.                       ELUABEND
00133      05  WS-HOLD-CIA-DDNAME        PIC X(8).                      ELUABEND
00134      05  WS-USER-ID-WORK-AREA.                                    ELUABEND
00135          10  WS-USER-ID-1          PIC X(5).                      ELUABEND
00136          10  WS-USER-ID-2          PIC X(15).                     ELUABEND
00137      05  WS-USER-ID-WORK-AREA-2  REDEFINES WS-USER-ID-WORK-AREA.  ELUABEND
00138          10  FILLER                PIC X.                         ELUABEND
00139          10  WS-USER-ID            PIC X(5).                      ELUABEND
00140          10  FILLER                PIC X(14).                     ELUABEND
00141 /                                                                 ELUABEND
00142      05  WS-EIBDATE-CC.                                           ELUABEND
00143          10  WS-EIBDATE-CENTURY.                                  ELUABEND
00144             15 WS-EIBDATE-1A          PIC 9(01).                  ELUABEND
00145             15 WS-EIBDATE-1B          PIC 9(01).                  ELUABEND
00146          10  WS-EIBDATE-DATE          PIC 9(05).                  ELUABEND
00147      05  WS-EIBDATE-CEN REDEFINES WS-EIBDATE-CC                   ELUABEND
00148                                    PIC 9(07).                     ELUABEND
00149  01  WS-AREA-TOO-LONG-MSG.                                        ELUABEND
00150      05  FILLER                    PIC X(35)  VALUE               ELUABEND
00151          'THIS AREA IS EXCEEDS 65K - SIZE IS'.                    ELUABEND
00152      05  WS-AREA-TOO-LONG-LEN      PIC ---,---,--9.               ELUABEND
00153                                                                   ELUABEND
00154  01  WS-ABEND-CONTROL-AREA.                                       ELUABEND
00155      05  WS-ACA-TBL               OCCURS 101 TIMES                ELUABEND
00156                                   INDEXED BY WS-ACA-IDX.          ELUABEND
00157          10  WS-ACA-CODE           PIC X(4).                      ELUABEND
00158          10  WS-ACA-COUNT          PIC S9(7) COMP-3.              ELUABEND
00159              88  WS-ACA-DUMP           VALUE +0 THRU +10.         ELUABEND
00160          10  WS-ACA-LAST-TERM      PIC X(4).                      ELUABEND
00161          10  WS-ACA-LAST-TIME      PIC S9(7) COMP-3.              ELUABEND
00162                                                                   ELUABEND
00163  01  WS-ABEND-ADDRS-AREA.                                         ELUABEND
00164      05  WS-NBR-ADDRS          PICTURE S9(4) COMP VALUE ZERO.     ELUABEND
00165          88  WS-ABEND-ADDRS-FULL       VALUE +1000.               ELUABEND
00166      05  WS-ADDRS-ENTRY        OCCURS 1000 TIMES                  ELUABEND
00167                                INDEXED BY WS-ADDR-IDX.            ELUABEND
00168          10  WS-ADDRS-PTR      POINTER.                           ELUABEND
00169          10  WS-ADDRS-LEN      PIC    S9(8) COMP.                 ELUABEND
00170 /                                                                 ELUABEND
00171      COPY HEXCOBOL.                                               ELUABEND
00172 /                                                                 ELUABEND
00173      COPY ELSNAPSC.                                               ELUABEND
00174 /                                                                 ELUABEND
00175      COPY ELSNAPPC.                                               ELUABEND
00176 /                                                                 ELUABEND
00177      COPY DFHBMSCA.                                               ELUABEND
00178 /                                                                 ELUABEND
00179      COPY EL03SETC.                                               ELUABEND
00180 /            H G A D A T E S   C O M M A R E A                    ELUABEND
00181  01  WS-HGADATES-PARMS.                                           ELUABEND
00182      COPY HGCDAT01.                                               ELUABEND
00183 /                                                                 ELUABEND
00184  LINKAGE SECTION.                                                 ELUABEND
00185  01  DFHCOMMAREA.                                                 ELUABEND
00186      COPY ELSCOMMC.                                               ELUABEND
00187 /                                                                 ELUABEND
00188      COPY ELSTWAC.                                                ELUABEND
00189 /                                                                 ELUABEND
00190      COPY ELSCIA2C.                                               ELUABEND
00191 /                                                                 ELUABEND
00192      COPY ELSSMAC.                                                ELUABEND
00193 /                                                                 ELUABEND
00194      COPY ELSSSCBC.                                               ELUABEND
00195 /                                                                 ELUABEND
00196      COPY ELSIOPMC.                                               ELUABEND
00197 /                                                                 ELUABEND
00198      COPY ELSCMIFC.                                               ELUABEND
00199 /                                                                 ELUABEND
00200      COPY ELSCMDSC.                                               ELUABEND
00201 /                                                                 ELUABEND
00202      COPY ELSTCWAC.                                               ELUABEND
00203 /                                                                 ELUABEND
00204      COPY ELSCSACC.                                               ELUABEND
00205 /                                                                 ELUABEND
00206      COPY ELSCSBPC.                                               ELUABEND
00207 /                                                                 ELUABEND
00208      COPY ELSCSPTC.                                               ELUABEND
00209 /                                                                 ELUABEND
00210      COPY ELSIBGRC.                                               ELUABEND
00211 /                                                                 ELUABEND
00212      COPY ELSIPGNC.                                               ELUABEND
00213 /                                                                 ELUABEND
00214      COPY ELSIPGTC.                                               ELUABEND
00215 /                                                                 ELUABEND
00216  01  LS-RECORD.                                                   ELUABEND
00217      05  LS-BYTE                OCCURS 65536 TIMES                ELUABEND
00218                                 PIC X.                            ELUABEND
00219 *                                                                 ELUABEND
00220 *   ELSCSTPC-TABLE HARD CODED SINCE NO COPY BOOK EXISTS.          ELUABEND
00221 *                                                                 ELUABEND
00222  01  ELSCSTPC-TABLE.                                              ELUABEND
00223      05  FILLER                 PIC S9(4) COMP.                   ELUABEND
00224      05  ELSCSTPC-TBL-CNT       PIC S9(4) COMP.                   ELUABEND
00225      05  ELSCSTPC-TBL           PIC X(14)                         ELUABEND
00226                                 OCCURS 100 TIMES                  ELUABEND
00227                                 DEPENDING ON ELSCSTPC-TBL-CNT.    ELUABEND
00228 /                                                                 ELUABEND
00229      COPY TUACOBOL.                                               ELUABEND
00230 /                                                                 ELUABEND
00231 *                                                                 ELUABEND
00232 *   ITEMS REFERENCED ONLY FOR LENGTH PURPOSES ONLY                ELUABEND
00233 *                                                                 ELUABEND
00234      COPY COBXIO2  SUPPRESS.                                      ELUABEND
00235  01  DBPIOPMC.                                                    ELUABEND
00236      COPY DBPIOPMC SUPPRESS.                                      ELUABEND
00237      COPY ELSATBLC SUPPRESS.                                      ELUABEND
00238      COPY ELSCSADC SUPPRESS.                                      ELUABEND
00239      COPY ELSCSENC SUPPRESS.                                      ELUABEND
00240      COPY ELSCSPGC SUPPRESS.                                      ELUABEND
00241      COPY ELSKEYSC SUPPRESS.                                      ELUABEND
00242      COPY ELSKTBCC SUPPRESS.                                      ELUABEND
00243      COPY ELSKTBGC SUPPRESS.                                      ELUABEND
00244      COPY ELSKTBSC SUPPRESS.                                      ELUABEND
00245      COPY ELSMEMSC SUPPRESS.                                      ELUABEND
00246      COPY ELSMHDGC SUPPRESS.                                      ELUABEND
00247      COPY ELSMOPTC SUPPRESS.                                      ELUABEND
00248      COPY ELSOUTPC SUPPRESS.                                      ELUABEND
00249      COPY ELSPLGSW SUPPRESS.                                      ELUABEND
00250      COPY ELSPLGTB SUPPRESS.                                      ELUABEND
00251      COPY ELSPRVNC SUPPRESS.                                      ELUABEND
00252      COPY ELSRRBLC SUPPRESS.                                      ELUABEND
00253      COPY ELSSRTPC SUPPRESS.                                      ELUABEND
00254  01  CONTRACT-RECORD.                                             ELUABEND
00255      COPY GCCONTRC SUPPRESS.                                      ELUABEND
00256  01  GROUP-SPEC-RECORD.                                           ELUABEND
00257      COPY GCGROUPC SUPPRESS.                                      ELUABEND
00258  01  GCIBGR-RECORD.                                               ELUABEND
00259      COPY GCTIBGRC SUPPRESS.                                      ELUABEND
00260  01  GCIPGN-RECORD.                                               ELUABEND
00261      COPY GCTIPGNC SUPPRESS.                                      ELUABEND
00262  01  GCIPGT-RECORD.                                               ELUABEND
00263      COPY GCTIPGTC SUPPRESS.                                      ELUABEND
00264  01  RDMC4306-RECORD.                                             ELUABEND
00265      COPY RDMC4306 SUPPRESS.                                      ELUABEND
00266  01  RDMC4308-RECORD.                                             ELUABEND
00267      COPY RDMC4306 SUPPRESS.                                      ELUABEND
00268  01  RDMC4311-RECORD.                                             ELUABEND
00269      COPY RDMC4306 SUPPRESS.                                      ELUABEND
00270 /                                                                 ELUABEND
00271  PROCEDURE DIVISION.                                              ELUABEND
00272 ************************************************************      ELUABEND
00273 *                                                          *      ELUABEND
00274 *                    PROCEDURE DIVISION                    *      ELUABEND
00275 *                                                          *      ELUABEND
00276 ************************************************************      ELUABEND
00277      EXEC CICS HANDLE ABEND                                       ELUABEND
00278                LABEL(9990-GENERAL-ABEND)                          ELUABEND
00279      END-EXEC.                                                    ELUABEND
00280      PERFORM 0000-INITIALIZATION.                                 ELUABEND
00281      PERFORM 0100-PROCESS-ABEND.                                  ELUABEND
00282      PERFORM 0200-TERMINATION.                                    ELUABEND
00283      GOBACK.                                                      ELUABEND
00284      SKIP3                                                        ELUABEND
00285 ************************************************************      ELUABEND
00286 *                                                          *      ELUABEND
00287 *              INITIALIZE PROGRAM                          *      ELUABEND
00288 *                                                          *      ELUABEND
00289 ************************************************************      ELUABEND
00290  0000-INITIALIZATION.                                             ELUABEND
00291      PERFORM 0010-ESTABLISH-LINK-TO-TWA.                          ELUABEND
00292      PERFORM 0020-ESTABLISH-ADDR-OF-CIA.                          ELUABEND
00293      PERFORM 0030-ESTABLISH-ADDR-OF-SMA.                          ELUABEND
00294      PERFORM 0040-INITIALIZE-SNAPSHOT.                            ELUABEND
00295      PERFORM 0050-DETERMINE-SNAPSHOT-TYPE.                        ELUABEND
00296      PERFORM 0060-CREATE-HEADER-PREFIX.                           ELUABEND
00297      MOVE LOW-VALUES TO WS-ABEND-ADDRS-AREA.                      ELUABEND
00298 /                                                                 ELUABEND
00299  0010-ESTABLISH-LINK-TO-TWA.                                      ELUABEND
00300      EXEC CICS ASSIGN                                             ELUABEND
00301           TWALENG(WS-TWA-LEN)                                     ELUABEND
00302      END-EXEC.                                                    ELUABEND
00303      IF WS-TWA-LEN < LENGTH OF TWA-TRANSACTION-WORK-AREA          ELUABEND
00304           SET WS-TERMINATE-PGM TO TRUE                            ELUABEND
00305           MOVE 'INVALID TRANS WORK AREA DETECTED'                 ELUABEND
00306                TO  WS-TERMINATION-MSG                             ELUABEND
00307           PERFORM 9999-UNABLE-TO-CONTINUE.                        ELUABEND
00308      EXEC CICS ADDRESS                                            ELUABEND
00309           TWA(WS-TWA-PTR)                                         ELUABEND
00310      END-EXEC.                                                    ELUABEND
00311      IF WS-TWA-PTR = NULL                                         ELUABEND
00312           SET WS-TERMINATE-PGM TO TRUE                            ELUABEND
00313           MOVE 'TRANS WORK AREA NOT FOUND'                        ELUABEND
00314                TO WS-TERMINATION-MSG                              ELUABEND
00315           PERFORM 9999-UNABLE-TO-CONTINUE.                        ELUABEND
00316      SET ADDRESS OF TWA-TRANSACTION-WORK-AREA TO WS-TWA-PTR.      ELUABEND
00317      SKIP3                                                        ELUABEND
00318  0020-ESTABLISH-ADDR-OF-CIA.                                      ELUABEND
00319      IF TWA-ELSCOMM-PTR = NULL                                    ELUABEND
00320          SET WS-TERMINATE-PGM TO TRUE                             ELUABEND
00321          MOVE 'UNABLE TO LOCATE COMMAREA'                         ELUABEND
00322               TO WS-TERMINATION-MSG                               ELUABEND
00323           PERFORM 9999-UNABLE-TO-CONTINUE.                        ELUABEND
00324      SET ADDRESS OF DFHCOMMAREA                                   ELUABEND
00325          TO TWA-ELSCOMM-PTR.                                      ELUABEND
00326      IF ECA-CIA-PTR = NULL                                        ELUABEND
00327          SET WS-TERMINATE-PGM TO TRUE                             ELUABEND
00328          MOVE 'UNABLE TO LOCATE COMMAREA'                         ELUABEND
00329               TO WS-TERMINATION-MSG                               ELUABEND
00330           PERFORM 9999-UNABLE-TO-CONTINUE.                        ELUABEND
00331      SET ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA                 ELUABEND
00332          TO ECA-CIA-PTR.                                          ELUABEND
00333      MOVE CIA-DDNAME TO WS-HOLD-CIA-DDNAME.                       ELUABEND
00334      EXEC CICS ASSIGN                                             ELUABEND
00335               ABCODE(WS-ABCODE)                                   ELUABEND
00336      END-EXEC.                                                    ELUABEND
00337 /                                                                 ELUABEND
00338  0030-ESTABLISH-ADDR-OF-SMA.                                      ELUABEND
00339      CALL 'ELUADDRS' USING CIA-STG-MGT-TABLE                      ELUABEND
00340           ADDRESS OF SMA-STORAGE-MANAGEMENT-AREA.                 ELUABEND
00341       SKIP3                                                       ELUABEND
00342  0040-INITIALIZE-SNAPSHOT.                                        ELUABEND
00343      SET SSR-MAX-SUB-SIZE TO TRUE.                                ELUABEND
00344      MOVE SSR-SUB-SIZE TO WS-MAX-LEN.                             ELUABEND
00345      MOVE LOW-VALUES TO SSR-SNAP-SHOT-RECORD.                     ELUABEND
00346      EXEC CICS ASSIGN                                             ELUABEND
00347           SYSID(SSR-CICS-SYSTEM-ID)                               ELUABEND
00348      END-EXEC.                                                    ELUABEND
00349      EXEC CICS ASSIGN                                             ELUABEND
00350           APPLID(SSR-CICS-APPL-ID)                                ELUABEND
00351      END-EXEC.                                                    ELUABEND
00352      MOVE EIBDATE   TO SSR-ABEND-DATE.                            ELUABEND
00353      MOVE EIBTIME   TO SSR-ABEND-TIME.                            ELUABEND
00354      MOVE EIBTRMID  TO SSR-TERMINAL-ID.                           ELUABEND
00355      MOVE WS-ABCODE TO SSR-ABEND-CODE.                            ELUABEND
00356 /                                                                 ELUABEND
00357                                                                   ELUABEND
00358  0050-DETERMINE-SNAPSHOT-TYPE.                                    ELUABEND
00359      EXEC CICS IGNORE CONDITION                                   ELUABEND
00360           NOSPACE                                                 ELUABEND
00361           QIDERR                                                  ELUABEND
00362           ITEMERR                                                 ELUABEND
00363      END-EXEC.                                                    ELUABEND
00364                                                                   ELUABEND
00365      EXEC CICS READQ TS                                           ELUABEND
00366           QUEUE(WS-ABEND-QUEUE-ID)                                ELUABEND
00367           INTO(WS-ABEND-CONTROL-AREA)                             ELUABEND
00368           ITEM(WS-ONE)                                            ELUABEND
00369      END-EXEC.                                                    ELUABEND
00370                                                                   ELUABEND
00371      IF EIBRCODE NOT = LOW-VALUES                                 ELUABEND
00372          PERFORM VARYING WS-SUB FROM 1 BY 1                       ELUABEND
00373              UNTIL WS-SUB > 101                                   ELUABEND
00374                INITIALIZE WS-ACA-TBL (WS-SUB)                     ELUABEND
00375          END-PERFORM                                              ELUABEND
00376          EXEC CICS WRITEQ TS                                      ELUABEND
00377               QUEUE(WS-ABEND-QUEUE-ID)                            ELUABEND
00378               FROM(WS-ABEND-CONTROL-AREA)                         ELUABEND
00379               ITEM(WS-ONE)                                        ELUABEND
00380          END-EXEC                                                 ELUABEND
00381      END-IF.                                                      ELUABEND
00382                                                                   ELUABEND
00383      SET WS-ACA-IDX TO 1.                                         ELUABEND
00384      SEARCH WS-ACA-TBL  VARYING WS-ACA-IDX                        ELUABEND
00385          AT END                                                   ELUABEND
00386              SET WS-ACA-IDX TO 101                                ELUABEND
00387          WHEN WS-ACA-CODE (WS-ACA-IDX) = WS-ABCODE                ELUABEND
00388              NEXT SENTENCE                                        ELUABEND
00389          WHEN WS-ACA-CODE (WS-ACA-IDX) = SPACES                   ELUABEND
00390              MOVE WS-ABCODE TO WS-ACA-CODE (WS-ACA-IDX)           ELUABEND
00391      END-SEARCH.                                                  ELUABEND
00392                                                                   ELUABEND
00393      ADD 1 TO WS-ACA-COUNT (WS-ACA-IDX).                          ELUABEND
00394      MOVE EIBTIME  TO WS-ACA-LAST-TIME (WS-ACA-IDX).              ELUABEND
00395      MOVE EIBTRMID TO WS-ACA-LAST-TERM (WS-ACA-IDX).              ELUABEND
00396                                                                   ELUABEND
00397      EXEC CICS WRITEQ TS                                          ELUABEND
00398           QUEUE(WS-ABEND-QUEUE-ID)                                ELUABEND
00399           FROM(WS-ABEND-CONTROL-AREA)                             ELUABEND
00400           ITEM(WS-ONE) REWRITE                                    ELUABEND
00401      END-EXEC.                                                    ELUABEND
00402 /                                                                 ELUABEND
00403  0060-CREATE-HEADER-PREFIX.                                       ELUABEND
00404      EXEC CICS ADDRESS                                            ELUABEND
00405              TCTUA(ADDRESS OF TUACOBOL)                           ELUABEND
00406      END-EXEC.                                                    ELUABEND
00407      MOVE SPACES TO SSP-FIXED-PORTION.                            ELUABEND
00408                                                                   ELUABEND
00409      EXEC CICS ASSIGN                                             ELUABEND
00410                FACILITY(SSP-FACILITY)                             ELUABEND
00411      END-EXEC.                                                    ELUABEND
00412                                                                   ELUABEND
00413      EXEC CICS ASSIGN                                             ELUABEND
00414                OPID(SSP-OPERATOR-INITIALS)                        ELUABEND
00415      END-EXEC.                                                    ELUABEND
00416                                                                   ELUABEND
00417      MOVE TUASUSER   TO  WS-USER-ID-1.                            ELUABEND
00418      MOVE TUAUSER2   TO  WS-USER-ID-2.                            ELUABEND
00419      MOVE WS-USER-ID TO  SSP-USER-ID.                             ELUABEND
00420      MOVE TUASDEPT   TO  SSP-USER-DEPT-NO.                        ELUABEND
00421      MOVE EIBTASKN   TO  SSP-TASK-NUMBER.                         ELUABEND
00422 /                                                                 ELUABEND
00423  0100-PROCESS-ABEND.                                              ELUABEND
00424      PERFORM 0110-WRITE-HEADER.                                   ELUABEND
00425      PERFORM 0120-WRITE-CIA.                                      ELUABEND
00426      IF WS-ACA-DUMP (WS-ACA-IDX)                                  ELUABEND
00427          PERFORM 1000-FULL-DUMP.                                  ELUABEND
00428      PERFORM 9000-DISPLAY-SCREEN.                                 ELUABEND
00429                                                                   ELUABEND
00430  0110-WRITE-HEADER.                                               ELUABEND
00431      IF SMA-ELSSSCB-PTR = NULL                                    ELUABEND
00432           MOVE ' NO SSCB' TO SSR-DDNAME                           ELUABEND
00433           MOVE 'UNABLE TO LOCATE ELSSSCB' TO WS-TERMINATION-MSG   ELUABEND
00434           MOVE LENGTH OF WS-TERMINATION-MSG TO WS-MOVE-LEN        ELUABEND
00435                                                SSP-STATUS-SIZE    ELUABEND
00436           CALL 'ELUMVCL' USING  WS-TERMINATION-MSG                ELUABEND
00437                                 WS-MOVE-LEN                       ELUABEND
00438                                 SSP-STATUS-REC (1)                ELUABEND
00439                                 WS-MOVE-LEN                       ELUABEND
00440                                 WS-PAD-CHAR                       ELUABEND
00441      ELSE                                                         ELUABEND
00442           MOVE 'ELSSSCB ' TO SSR-DDNAME                           ELUABEND
00443           MOVE LENGTH OF SSB-SELECTOR-STATUS-CTL-BLK              ELUABEND
00444                       TO SSP-STATUS-SIZE                          ELUABEND
00445                          WS-MOVE-LEN                              ELUABEND
00446           SET ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK              ELUABEND
00447                       TO SMA-ELSSSCB-PTR                          ELUABEND
00448           CALL 'ELUMVCL' USING  SSB-SELECTOR-STATUS-CTL-BLK       ELUABEND
00449                                 WS-MOVE-LEN                       ELUABEND
00450                                 SSP-STATUS-REC (1)                ELUABEND
00451                                 WS-MOVE-LEN                       ELUABEND
00452                                 WS-PAD-CHAR                       ELUABEND
00453      END-IF.                                                      ELUABEND
00454      SET SSR-DUMP-HEADER TO TRUE.                                 ELUABEND
00455      SET SSR-SORT-HEADER TO TRUE.                                 ELUABEND
00456      MOVE ZERO TO SSR-SEQUENCE-NUM.                               ELUABEND
00457      CALL 'ELUADDRS' USING SSP-SNAP-SHOT-PREFIX                   ELUABEND
00458                            SSR-AREA-PTR.                          ELUABEND
00459      MOVE LENGTH OF SSP-SNAP-SHOT-PREFIX TO SSR-AREA-LENGTH.      ELUABEND
00460      PERFORM 8100-WRITE-MEM-WITH-CHECK.                           ELUABEND
00461 /                                                                 ELUABEND
00462 ****                                                              ELUABEND
00463 ****  THE CIA HAS A SPECIAL WRITE SINCE IT CAN BE ALTERED DURING  ELUABEND
00464 ****  PROCESSING OF THE SMA, THUS CHANGING THE CONTENTS OF THE    ELUABEND
00465 ****  CIA BEFORE IT IS DUMPED.  THE CIA IS USED SINCE IT IS       ELUABEND
00466 ****  CONVIENIENT TO USE THE CIA-DDNAME AND ITS 88 LEVELS.        ELUABEND
00467 ****                                                              ELUABEND
00468                                                                   ELUABEND
00469  0120-WRITE-CIA.                                                  ELUABEND
00470      SET SSR-DATA-AREA   TO TRUE.                                 ELUABEND
00471      SET SSR-SORT-BLOCK  TO TRUE.                                 ELUABEND
00472      MOVE ' ELSCIA ' TO SSR-DDNAME.                               ELUABEND
00473      MOVE ZERO TO SSR-SEQUENCE-NUM.                               ELUABEND
00474      MOVE LENGTH OF CIA-ELS-COMMON-INTERFACE-AREA                 ELUABEND
00475                  TO SSR-AREA-LENGTH.                              ELUABEND
00476      SET  SSR-AREA-PTR   TO  ECA-CIA-PTR.                         ELUABEND
00477      PERFORM 8100-WRITE-MEM-WITH-CHECK.                           ELUABEND
00478                                                                   ELUABEND
00479  0200-TERMINATION.                                                ELUABEND
00480      EXEC CICS RETURN                                             ELUABEND
00481          TRANSID('ELIQ')                                          ELUABEND
00482      END-EXEC.                                                    ELUABEND
00483 /                                                                 ELUABEND
00484  1000-FULL-DUMP.                                                  ELUABEND
00485      PERFORM VARYING WS-SMA-SUB FROM 1 BY 1                       ELUABEND
00486         UNTIL WS-SMA-SUB > SMA-NUMBER-OF-ENTRIES                  ELUABEND
00487               OR WS-FILE-FULL                                     ELUABEND
00488            SET SMA-STG-MGT-IDX TO WS-SMA-SUB                      ELUABEND
00489            IF SMA-PTR (SMA-STG-MGT-IDX) NOT = NULL                ELUABEND
00490                 PERFORM 1010-DUMP-AREA                            ELUABEND
00491            END-IF                                                 ELUABEND
00492      END-PERFORM.                                                 ELUABEND
00493                                                                   ELUABEND
00494  1010-DUMP-AREA.                                                  ELUABEND
00495      EVALUATE TRUE                                                ELUABEND
00496      WHEN SMA-TYP-DATA  (SMA-STG-MGT-IDX)                         ELUABEND
00497          PERFORM 2000-WRITE-DATA                                  ELUABEND
00498      WHEN SMA-TYP-STATIC (SMA-STG-MGT-IDX)                        ELUABEND
00499          PERFORM 2000-WRITE-DATA                                  ELUABEND
00500      WHEN SMA-TYP-IO (SMA-STG-MGT-IDX)                            ELUABEND
00501          PERFORM 3000-WRITE-IO                                    ELUABEND
00502      WHEN SMA-TYP-TEMPSTG (SMA-STG-MGT-IDX)                       ELUABEND
00503          PERFORM 4000-WRITE-TS                                    ELUABEND
00504      WHEN OTHER                                                   ELUABEND
00505          CONTINUE                                                 ELUABEND
00506      END-EVALUATE.                                                ELUABEND
00507 /                                                                 ELUABEND
00508  2000-WRITE-DATA.                                                 ELUABEND
00509      IF SMA-PTR (SMA-STG-MGT-IDX) NOT = NULL                      ELUABEND
00510          PERFORM 2010-WRITE-DATA.                                 ELUABEND
00511                                                                   ELUABEND
00512  2010-WRITE-DATA.                                                 ELUABEND
00513      SET SSR-DATA-AREA  TO TRUE.                                  ELUABEND
00514      MOVE SMA-DDN (SMA-STG-MGT-IDX) TO SSR-DDNAME                 ELUABEND
00515                                        CIA-DDNAME.                ELUABEND
00516      SET SSR-SORT-BLOCK TO TRUE.                                  ELUABEND
00517      SET SSR-AREA-PTR   TO SMA-PTR (SMA-STG-MGT-IDX).             ELUABEND
00518      MOVE ZERO          TO SSR-SEQUENCE-NUM.                      ELUABEND
00519                                                                   ELUABEND
00520      EVALUATE TRUE                                                ELUABEND
00521      WHEN CIA-ELSCSPTC-DDN                                        ELUABEND
00522          PERFORM 2020-WRITE-CSPT                                  ELUABEND
00523      WHEN CIA-ELSCSBPC-DDN                                        ELUABEND
00524          PERFORM 2030-WRITE-CSBP                                  ELUABEND
00525      WHEN CIA-ELSCSAC-DDN                                         ELUABEND
00526          PERFORM 2040-WRITE-CSAC                                  ELUABEND
00527      WHEN CIA-ELSIBGR-DDN                                         ELUABEND
00528          PERFORM 2050-WRITE-IBGR                                  ELUABEND
00529      WHEN CIA-ELSIPGN-DDN                                         ELUABEND
00530          PERFORM 2060-WRITE-IPGN                                  ELUABEND
00531      WHEN CIA-ELSIPGT-DDN                                         ELUABEND
00532          PERFORM 2070-WRITE-IPGT                                  ELUABEND
00533      WHEN OTHER                                                   ELUABEND
00534          PERFORM 2500-DUMP-NON-TABLE-AREA                         ELUABEND
00535      END-EVALUATE.                                                ELUABEND
00536 /***************************************************************  ELUABEND
00537 *                                                              *  ELUABEND
00538 *    THIS PARAGRAPH PRODUCES THE DUMP FOR THE CSPT TABLE.      *  ELUABEND
00539 *    THIS TABLE CONTAINS A GROUP OF POINTERS WHICH             *  ELUABEND
00540 *    CONTAIN THE ADDRESSES OF CONTRACT SUMMARY DATA            *  ELUABEND
00541 *    STRUCTURES.                                               *  ELUABEND
00542 *                                                              *  ELUABEND
00543 *    CPST IS UNIQUE SINCE IT HAS A POINTER (BP-TBL-PTR)        *  ELUABEND
00544 *    TO A TABLE OF POINTERS AS WELL (CSBP).                    *  ELUABEND
00545 *    THEREFORE, IT IS NECCESSARY TO PRODUCE THIS MEMORY        *  ELUABEND
00546 *    CHAIN IN A SLIGHTLY DIFFERENT MANNER.                     *  ELUABEND
00547 *                                                              *  ELUABEND
00548 ****************************************************************  ELUABEND
00549  2020-WRITE-CSPT.                                                 ELUABEND
00550      SET ADDRESS OF CSPT-POINTER-LIST                             ELUABEND
00551           TO SSR-AREA-PTR.                                        ELUABEND
00552      MOVE LENGTH OF CSPT-POINTER-LIST                             ELUABEND
00553           TO SSR-AREA-LENGTH.                                     ELUABEND
00554      PERFORM 8100-WRITE-MEM-WITH-CHECK.                           ELUABEND
00555      PERFORM VARYING WS-SUB FROM 1 BY 1                           ELUABEND
00556          UNTIL WS-SUB > 5                                         ELUABEND
00557          SET CSPT-IDX TO WS-SUB                                   ELUABEND
00558          IF CSPT-BP-TBL-PTR (CSPT-IDX) NOT = NULL                 ELUABEND
00559              SET SSR-AREA-PTR TO CSPT-BP-TBL-PTR (CSPT-IDX)       ELUABEND
00560              SET SSR-SORT-BLOCK TO TRUE                           ELUABEND
00561              SET CIA-ELSCSBPC-DDN TO TRUE                         ELUABEND
00562              MOVE CIA-DDNAME TO SSR-DDNAME                        ELUABEND
00563              PERFORM 2030-WRITE-CSBP                              ELUABEND
00564              SET CIA-ELSCSPTC-DDN TO TRUE                         ELUABEND
00565              MOVE CIA-DDNAME TO SSR-DDNAME                        ELUABEND
00566          END-IF                                                   ELUABEND
00567          IF CSPT-REQ-LIST-PTR (CSPT-IDX) NOT = NULL               ELUABEND
00568              SET SSR-SORT-TABLE-2 TO TRUE                         ELUABEND
00569              SET SSR-AREA-PTR TO CSPT-REQ-LIST-PTR (CSPT-IDX)     ELUABEND
00570              SET ADDRESS OF ELSCSTPC-TABLE                        ELUABEND
00571                   TO SSR-AREA-PTR                                 ELUABEND
00572              MOVE LENGTH OF ELSCSTPC-TABLE                        ELUABEND
00573                   TO SSR-AREA-LENGTH                              ELUABEND
00574              PERFORM 8100-WRITE-MEM-WITH-CHECK                    ELUABEND
00575          END-IF                                                   ELUABEND
00576          IF CSPT-PROV-GRP-PTR (CSPT-IDX) NOT = NULL               ELUABEND
00577              SET SSR-SORT-TABLE-3 TO TRUE                         ELUABEND
00578              SET SSR-AREA-PTR TO CSPT-PROV-GRP-PTR (CSPT-IDX)     ELUABEND
00579              SET ADDRESS OF CSPG-PROVISION-GROUP-TABLE            ELUABEND
00580                   TO SSR-AREA-PTR                                 ELUABEND
00581              MOVE LENGTH OF CSPG-PROVISION-GROUP-TABLE            ELUABEND
00582                   TO SSR-AREA-LENGTH                              ELUABEND
00583              PERFORM 8100-WRITE-MEM-WITH-CHECK                    ELUABEND
00584          END-IF                                                   ELUABEND
00585      END-PERFORM.                                                 ELUABEND
00586 /***************************************************************  ELUABEND
00587 *                                                              *  ELUABEND
00588 *    THIS PARAGRAPH PRODUCES THE DUMP FOR THE CSBP TABLE.      *  ELUABEND
00589 *    THIS TABLE CONTAINS A GROUP OF POINTERS WHICH             *  ELUABEND
00590 *    CONTAIN THE ADDRESSES OF CONTRACT SUMMARY DATA            *  ELUABEND
00591 *    STRUCTURES.                                               *  ELUABEND
00592 *                                                              *  ELUABEND
00593 *    CSBP IS UNIQUE SINCE IT CAN BE POINTED TO BY THE          *  ELUABEND
00594 *    SMA OR BY THE CSPT.  SEE COMMENTS IN THE AREA WRITING     *  ELUABEND
00595 *    THE CSPT FOR MORE INFO.                                   *  ELUABEND
00596 *                                                              *  ELUABEND
00597 ****************************************************************  ELUABEND
00598  2030-WRITE-CSBP.                                                 ELUABEND
00599      SET ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE                  ELUABEND
00600           TO SSR-AREA-PTR                                         ELUABEND
00601      MOVE LENGTH OF CSBP-BENEFIT-PROVISION-TABLE                  ELUABEND
00602           TO SSR-AREA-LENGTH                                      ELUABEND
00603      PERFORM 8100-WRITE-MEM-WITH-CHECK.                           ELUABEND
00604      PERFORM VARYING WS-SUB FROM 1 BY 1                           ELUABEND
00605          UNTIL WS-SUB > CSBP-TBL-CNT                              ELUABEND
00606          SET CSBP-X-IDX TO WS-SUB                                 ELUABEND
00607          IF CSBP-SENTENCE-PTR (CSBP-X-IDX) NOT = NULL             ELUABEND
00608              SET SSR-SORT-TABLE-1 TO TRUE                         ELUABEND
00609              SET SSR-AREA-PTR TO CSBP-SENTENCE-PTR (CSBP-X-IDX)   ELUABEND
00610              SET ADDRESS OF CSEN-SENTENCE-TABLE                   ELUABEND
00611                   TO SSR-AREA-PTR                                 ELUABEND
00612              MOVE LENGTH OF CSEN-SENTENCE-TABLE                   ELUABEND
00613                   TO SSR-AREA-LENGTH                              ELUABEND
00614              PERFORM 8100-WRITE-MEM-WITH-CHECK                    ELUABEND
00615          END-IF                                                   ELUABEND
00616      END-PERFORM.                                                 ELUABEND
00617 /***************************************************************  ELUABEND
00618 *                                                              *  ELUABEND
00619 *    THIS PARAGRAPH PRODUCES THE DUMP FOR THE CSAC TABLE.      *  ELUABEND
00620 *    THIS TABLE CONTAINS A GROUP OF POINTERS WHICH             *  ELUABEND
00621 *    CONTAIN THE ADDRESSES OF CONTRACT SUMMARY DATA            *  ELUABEND
00622 *    STRUCTURES.                                               *  ELUABEND
00623 *                                                              *  ELUABEND
00624 ****************************************************************  ELUABEND
00625  2040-WRITE-CSAC.                                                 ELUABEND
00626      SET ADDRESS OF CSAC-ACCUMULATOR-TABLE                        ELUABEND
00627           TO SSR-AREA-PTR.                                        ELUABEND
00628      MOVE LENGTH OF CSAC-ACCUMULATOR-TABLE                        ELUABEND
00629           TO SSR-AREA-LENGTH.                                     ELUABEND
00630      PERFORM 8100-WRITE-MEM-WITH-CHECK.                           ELUABEND
00631      PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 4          ELUABEND
00632          SET CSAC-X-IDX TO WS-SUB                                 ELUABEND
00633          IF CSAC-GC-TBL-PTR (CSAC-X-IDX) NOT = NULL               ELUABEND
00634              SET SSR-SORT-TABLE-1 TO TRUE                         ELUABEND
00635              SET SSR-AREA-PTR TO CSAC-GC-TBL-PTR (CSAC-X-IDX)     ELUABEND
00636              SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                ELUABEND
00637                   TO SSR-AREA-PTR                                 ELUABEND
00638              MOVE LENGTH OF ATBL-ACCUMULATOR-TABLE                ELUABEND
00639                   TO SSR-AREA-LENGTH                              ELUABEND
00640              PERFORM 8100-WRITE-MEM-WITH-CHECK                    ELUABEND
00641          END-IF                                                   ELUABEND
00642          IF CSAC-BP-TBL-PTR (CSAC-X-IDX) NOT = NULL               ELUABEND
00643              SET SSR-SORT-TABLE-1 TO TRUE                         ELUABEND
00644              SET SSR-AREA-PTR TO CSAC-BP-TBL-PTR (CSAC-X-IDX)     ELUABEND
00645              SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                ELUABEND
00646                   TO SSR-AREA-PTR                                 ELUABEND
00647              MOVE LENGTH OF ATBL-ACCUMULATOR-TABLE                ELUABEND
00648                   TO SSR-AREA-LENGTH                              ELUABEND
00649              PERFORM 8100-WRITE-MEM-WITH-CHECK                    ELUABEND
00650          END-IF                                                   ELUABEND
00651          IF CSAC-RR-PTR (CSAC-X-IDX) NOT = NULL                   ELUABEND
00652              SET SSR-SORT-TABLE-2 TO TRUE                         ELUABEND
00653              SET SSR-AREA-PTR TO CSAC-RR-PTR (CSAC-X-IDX)         ELUABEND
00654              SET ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST              ELUABEND
00655                   TO SSR-AREA-PTR                                 ELUABEND
00656              MOVE LENGTH OF RRBL-RANK-REQ-BLOCK-LIST              ELUABEND
00657                   TO SSR-AREA-LENGTH                              ELUABEND
00658              PERFORM 8100-WRITE-MEM-WITH-CHECK                    ELUABEND
00659          END-IF                                                   ELUABEND
00660      END-PERFORM.                                                 ELUABEND
00661 /***************************************************************  ELUABEND
00662 *                                                              *  ELUABEND
00663 *    THIS PARAGRAPH PRODUCES THE DUMP FOR THE IBGR TABLE.      *  ELUABEND
00664 *    THIS TABLE CONTAINS A GROUP OF POINTERS WHICH             *  ELUABEND
00665 *    CONTAIN THE ADDRESSES OF CONTRACT SUMMARY DATA            *  ELUABEND
00666 *    STRUCTURES.                                               *  ELUABEND
00667 *                                                              *  ELUABEND
00668 ****************************************************************  ELUABEND
00669  2050-WRITE-IBGR.                                                 ELUABEND
00670      SET ADDRESS OF IBGR-INTERNAL-TABS-TABLE                      ELUABEND
00671           TO SSR-AREA-PTR.                                        ELUABEND
00672      MOVE LENGTH OF IBGR-INTERNAL-TABS-TABLE                      ELUABEND
00673           TO SSR-AREA-LENGTH.                                     ELUABEND
00674      PERFORM 8100-WRITE-MEM-WITH-CHECK.                           ELUABEND
00675      PERFORM VARYING WS-SUB FROM 1 BY 1                           ELUABEND
00676          UNTIL WS-SUB > IBGR-TBL-CNT                              ELUABEND
00677          SET IBGR-X-IDX TO WS-SUB                                 ELUABEND
00678          IF IBGR-TABULAR-PTR (IBGR-X-IDX) NOT = NULL              ELUABEND
00679              SET SSR-SORT-TABLE-1 TO TRUE                         ELUABEND
00680              SET SSR-AREA-PTR TO IBGR-TABULAR-PTR (IBGR-X-IDX)    ELUABEND
00681              SET ADDRESS OF GCIBGR-RECORD                         ELUABEND
00682                   TO SSR-AREA-PTR                                 ELUABEND
00683              MOVE LENGTH OF GCIBGR-RECORD                         ELUABEND
00684                   TO SSR-AREA-LENGTH                              ELUABEND
00685              PERFORM 8100-WRITE-MEM-WITH-CHECK                    ELUABEND
00686          END-IF                                                   ELUABEND
00687      END-PERFORM.                                                 ELUABEND
00688 /***************************************************************  ELUABEND
00689 *                                                              *  ELUABEND
00690 *    THIS PARAGRAPH PRODUCES THE DUMP FOR THE IPGN TABLE.      *  ELUABEND
00691 *    THIS TABLE CONTAINS A GROUP OF POINTERS WHICH             *  ELUABEND
00692 *    CONTAIN THE ADDRESSES OF CONTRACT SUMMARY DATA            *  ELUABEND
00693 *    STRUCTURES.                                               *  ELUABEND
00694 *                                                              *  ELUABEND
00695 ****************************************************************  ELUABEND
00696  2060-WRITE-IPGN.                                                 ELUABEND
00697      SET ADDRESS OF IPGN-INTERNAL-TABS-TABLE                      ELUABEND
00698           TO SSR-AREA-PTR.                                        ELUABEND
00699      MOVE LENGTH OF IPGN-INTERNAL-TABS-TABLE                      ELUABEND
00700           TO SSR-AREA-LENGTH.                                     ELUABEND
00701      PERFORM 8100-WRITE-MEM-WITH-CHECK.                           ELUABEND
00702      PERFORM VARYING WS-SUB FROM 1 BY 1                           ELUABEND
00703          UNTIL WS-SUB > IPGN-TBL-CNT                              ELUABEND
00704          SET IPGN-X-IDX TO WS-SUB                                 ELUABEND
00705          IF IPGN-TABULAR-PTR (IPGN-X-IDX) NOT = NULL              ELUABEND
00706              SET SSR-SORT-TABLE-1 TO TRUE                         ELUABEND
00707              SET SSR-AREA-PTR TO IPGN-TABULAR-PTR (IPGN-X-IDX)    ELUABEND
00708              SET ADDRESS OF GCIPGN-RECORD                         ELUABEND
00709                   TO SSR-AREA-PTR                                 ELUABEND
00710              MOVE LENGTH OF GCIPGN-RECORD                         ELUABEND
00711                   TO SSR-AREA-LENGTH                              ELUABEND
00712              PERFORM 8100-WRITE-MEM-WITH-CHECK                    ELUABEND
00713          END-IF                                                   ELUABEND
00714      END-PERFORM.                                                 ELUABEND
00715 /***************************************************************  ELUABEND
00716 *                                                              *  ELUABEND
00717 *    THIS PARAGRAPH PRODUCES THE DUMP FOR THE IPGT TABLE.      *  ELUABEND
00718 *    THIS TABLE CONTAINS A GROUP OF POINTERS WHICH             *  ELUABEND
00719 *    CONTAIN THE ADDRESSES OF CONTRACT SUMMARY DATA            *  ELUABEND
00720 *    STRUCTURES.                                               *  ELUABEND
00721 *                                                              *  ELUABEND
00722 ****************************************************************  ELUABEND
00723  2070-WRITE-IPGT.                                                 ELUABEND
00724      SET ADDRESS OF IPGT-INTERNAL-TABS-TABLE                      ELUABEND
00725           TO SSR-AREA-PTR.                                        ELUABEND
00726      MOVE LENGTH OF IPGT-INTERNAL-TABS-TABLE                      ELUABEND
00727           TO SSR-AREA-LENGTH.                                     ELUABEND
00728      PERFORM 8100-WRITE-MEM-WITH-CHECK.                           ELUABEND
00729      PERFORM VARYING WS-SUB FROM 1 BY 1                           ELUABEND
00730          UNTIL WS-SUB > IPGT-TBL-CNT                              ELUABEND
00731          SET IPGT-X-IDX TO WS-SUB                                 ELUABEND
00732          IF IPGT-TABULAR-PTR (IPGT-X-IDX) NOT = NULL              ELUABEND
00733              SET SSR-SORT-TABLE-1 TO TRUE                         ELUABEND
00734              SET SSR-AREA-PTR TO IPGT-TABULAR-PTR (IPGT-X-IDX)    ELUABEND
00735              SET ADDRESS OF GCIPGT-RECORD                         ELUABEND
00736                   TO SSR-AREA-PTR                                 ELUABEND
00737              MOVE LENGTH OF GCIPGT-RECORD                         ELUABEND
00738                   TO SSR-AREA-LENGTH                              ELUABEND
00739              PERFORM 8100-WRITE-MEM-WITH-CHECK                    ELUABEND
00740          END-IF                                                   ELUABEND
00741      END-PERFORM.                                                 ELUABEND
00742 /                                                                 ELUABEND
00743  2500-DUMP-NON-TABLE-AREA.                                        ELUABEND
00744      PERFORM 2510-DETERMINE-LENGTH.                               ELUABEND
00745      IF SSR-AREA-LENGTH > ZERO                                    ELUABEND
00746          PERFORM 8100-WRITE-MEM-WITH-CHECK.                       ELUABEND
00747                                                                   ELUABEND
00748  2510-DETERMINE-LENGTH.                                           ELUABEND
00749      EVALUATE TRUE                                                ELUABEND
00750      WHEN CIA-ELSSMA-DDN                                          ELUABEND
00751          MOVE LENGTH OF SMA-STORAGE-MANAGEMENT-AREA               ELUABEND
00752               TO SSR-AREA-LENGTH                                  ELUABEND
00753      WHEN CIA-ELSCIA-DDN                                          ELUABEND
00754          MOVE LENGTH OF CIA-ELS-COMMON-INTERFACE-AREA             ELUABEND
00755               TO SSR-AREA-LENGTH                                  ELUABEND
00756      WHEN CIA-COBXIO-DDN                                          ELUABEND
00757          SET ADDRESS OF CSEXECIO-CONTROL                          ELUABEND
00758               TO SSR-AREA-PTR                                     ELUABEND
00759          MOVE LENGTH OF CSEXECIO-CONTROL                          ELUABEND
00760               TO SSR-AREA-LENGTH                                  ELUABEND
00761      WHEN CIA-DBPIOPM-DDN                                         ELUABEND
00762          SET ADDRESS OF DBPIOPMC                                  ELUABEND
00763               TO SSR-AREA-PTR                                     ELUABEND
00764          MOVE LENGTH OF DBPIOPMC                                  ELUABEND
00765               TO SSR-AREA-LENGTH                                  ELUABEND
00766      WHEN CIA-ELSATBL-DDN                                         ELUABEND
00767          SET ADDRESS OF ATBL-ACCUMULATOR-TABLE                    ELUABEND
00768               TO SSR-AREA-PTR                                     ELUABEND
00769          MOVE LENGTH OF ATBL-ACCUMULATOR-TABLE                    ELUABEND
00770               TO SSR-AREA-LENGTH                                  ELUABEND
00771      WHEN CIA-ELSCMDSC-DDN                                        ELUABEND
00772          SET ADDRESS OF CMF-DESCR                                 ELUABEND
00773               TO SSR-AREA-PTR                                     ELUABEND
00774          MOVE LENGTH OF CMF-DESCR                                 ELUABEND
00775               TO SSR-AREA-LENGTH                                  ELUABEND
00776      WHEN CIA-ELSCMIF-DDN                                         ELUABEND
00777          SET ADDRESS OF CMF-CODES-MANUAL-INTERFACE                ELUABEND
00778               TO SSR-AREA-PTR                                     ELUABEND
00779          MOVE LENGTH OF CMF-CODES-MANUAL-INTERFACE                ELUABEND
00780               TO SSR-AREA-LENGTH                                  ELUABEND
00781      WHEN CIA-ELSCONIB-DDN                                        ELUABEND
00782        OR CIA-ELSCONIS-DDN                                        ELUABEND
00783        OR CIA-ELSCONPB-DDN                                        ELUABEND
00784        OR CIA-ELSCONPS-DDN                                        ELUABEND
00785          SET ADDRESS OF CONTRACT-RECORD                           ELUABEND
00786               TO SSR-AREA-PTR                                     ELUABEND
00787          MOVE LENGTH OF CONTRACT-RECORD                           ELUABEND
00788               TO SSR-AREA-LENGTH                                  ELUABEND
00789      WHEN CIA-ELSCSADC-DDN                                        ELUABEND
00790          SET ADDRESS OF CSAD-ACL-TABLE                            ELUABEND
00791               TO SSR-AREA-PTR                                     ELUABEND
00792          MOVE LENGTH OF CSAD-ACL-TABLE                            ELUABEND
00793               TO SSR-AREA-LENGTH                                  ELUABEND
00794      WHEN CIA-ELSCSENC-DDN                                        ELUABEND
00795          SET ADDRESS OF CSEN-SENTENCE-TABLE                       ELUABEND
00796               TO SSR-AREA-PTR                                     ELUABEND
00797          MOVE LENGTH OF CSEN-SENTENCE-TABLE                       ELUABEND
00798               TO SSR-AREA-LENGTH                                  ELUABEND
00799      WHEN CIA-ELSCSPG-DDN                                         ELUABEND
00800          SET ADDRESS OF CSPG-PROVISION-GROUP-TABLE                ELUABEND
00801               TO SSR-AREA-PTR                                     ELUABEND
00802          MOVE LENGTH OF CSPG-PROVISION-GROUP-TABLE                ELUABEND
00803               TO SSR-AREA-LENGTH                                  ELUABEND
00804      WHEN CIA-ELSGRPSP-DDN                                        ELUABEND
00805          SET ADDRESS OF GROUP-SPEC-RECORD                         ELUABEND
00806               TO SSR-AREA-PTR                                     ELUABEND
00807          MOVE LENGTH OF GROUP-SPEC-RECORD                         ELUABEND
00808               TO SSR-AREA-LENGTH                                  ELUABEND
00809      WHEN CIA-ELSIOPM-DDN                                         ELUABEND
00810          SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS               ELUABEND
00811               TO SSR-AREA-PTR                                     ELUABEND
00812          MOVE LENGTH OF IOP-INPUT-OUTPUT-PARAMETERS               ELUABEND
00813               TO SSR-AREA-LENGTH                                  ELUABEND
00814      WHEN CIA-ELSKEYS-DDN                                         ELUABEND
00815          SET ADDRESS OF KWA-FILE-KEY-WORK-AREA                    ELUABEND
00816               TO SSR-AREA-PTR                                     ELUABEND
00817          MOVE LENGTH OF KWA-FILE-KEY-WORK-AREA                    ELUABEND
00818               TO SSR-AREA-LENGTH                                  ELUABEND
00819      WHEN CIA-ELSKTBC-DDN                                         ELUABEND
00820          SET ADDRESS OF KTC-GCCONTR-KEY-TABLE                     ELUABEND
00821               TO SSR-AREA-PTR                                     ELUABEND
00822          MOVE LENGTH OF KTC-GCCONTR-KEY-TABLE                     ELUABEND
00823               TO SSR-AREA-LENGTH                                  ELUABEND
00824      WHEN CIA-ELSKTBG-DDN                                         ELUABEND
00825          SET ADDRESS OF KTG-GCGRPSPC-KEY-TABLE                    ELUABEND
00826               TO SSR-AREA-PTR                                     ELUABEND
00827          MOVE LENGTH OF KTG-GCGRPSPC-KEY-TABLE                    ELUABEND
00828               TO SSR-AREA-LENGTH                                  ELUABEND
00829      WHEN CIA-ELSKTBS-DDN                                         ELUABEND
00830          SET ADDRESS OF KTS-SECTIONS-KEY-TABLE                    ELUABEND
00831               TO SSR-AREA-PTR                                     ELUABEND
00832          MOVE LENGTH OF KTS-SECTIONS-KEY-TABLE                    ELUABEND
00833               TO SSR-AREA-LENGTH                                  ELUABEND
00834      WHEN CIA-ELSMEMS-DDN                                         ELUABEND
00835          SET ADDRESS OF MSI-MEMBERSHIP-INTERFACE                  ELUABEND
00836               TO SSR-AREA-PTR                                     ELUABEND
00837          MOVE LENGTH OF MSI-MEMBERSHIP-INTERFACE                  ELUABEND
00838               TO SSR-AREA-LENGTH                                  ELUABEND
00839      WHEN CIA-ELSMHDG-DDN                                         ELUABEND
00840          SET ADDRESS OF MHD-MENU-HEADINGS                         ELUABEND
00841               TO SSR-AREA-PTR                                     ELUABEND
00842          MOVE LENGTH OF MHD-MENU-HEADINGS                         ELUABEND
00843               TO SSR-AREA-LENGTH                                  ELUABEND
00844      WHEN CIA-ELSMOPT-DDN                                         ELUABEND
00845          SET ADDRESS OF MSO-MENU-SELECTION-VALUES                 ELUABEND
00846               TO SSR-AREA-PTR                                     ELUABEND
00847          MOVE LENGTH OF MSO-MENU-SELECTION-VALUES                 ELUABEND
00848               TO SSR-AREA-LENGTH                                  ELUABEND
00849      WHEN CIA-ELSOUTP-DDN                                         ELUABEND
00850          SET ADDRESS OF COF-OUTPUT-INTERFACE                      ELUABEND
00851               TO SSR-AREA-PTR                                     ELUABEND
00852          MOVE LENGTH OF COF-OUTPUT-INTERFACE                      ELUABEND
00853               TO SSR-AREA-LENGTH                                  ELUABEND
00854      WHEN CIA-ELSPGMW1-DDN                                        ELUABEND
00855        OR CIA-ELSPGMW2-DDN                                        ELUABEND
00856        OR CIA-ELSPGMW3-DDN                                        ELUABEND
00857        OR CIA-ELSPGMW4-DDN                                        ELUABEND
00858          MOVE 128 TO SSR-AREA-LENGTH                              ELUABEND
00859 *******       128 USED SINCE THE EXACT NATURE OF THESE WORK       ELUABEND
00860 *******           AREAS VARIES BY THE PROGRAMS WHICH USE THEM     ELUABEND
00861      WHEN CIA-ELSPLGSW-DDN                                        ELUABEND
00862          SET ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES                ELUABEND
00863               TO SSR-AREA-PTR                                     ELUABEND
00864          MOVE LENGTH OF PLS-PAYMENT-LEVEL-SWITCHES                ELUABEND
00865               TO SSR-AREA-LENGTH                                  ELUABEND
00866      WHEN CIA-ELSPLGTB-DDN                                        ELUABEND
00867          SET ADDRESS OF PLT-PAYMENT-LEVEL-TABLE                   ELUABEND
00868               TO SSR-AREA-PTR                                     ELUABEND
00869          MOVE LENGTH OF PLT-PAYMENT-LEVEL-TABLE                   ELUABEND
00870               TO SSR-AREA-LENGTH                                  ELUABEND
00871      WHEN CIA-ELSPRVN-DDN                                         ELUABEND
00872          SET ADDRESS OF PVN-BENEFIT-PROVISION-LIST                ELUABEND
00873               TO SSR-AREA-PTR                                     ELUABEND
00874          MOVE LENGTH OF PVN-BENEFIT-PROVISION-LIST                ELUABEND
00875               TO SSR-AREA-LENGTH                                  ELUABEND
00876      WHEN CIA-ELSRRBL-DDN                                         ELUABEND
00877          SET ADDRESS OF RRBL-RANK-REQ-BLOCK-LIST                  ELUABEND
00878               TO SSR-AREA-PTR                                     ELUABEND
00879          MOVE LENGTH OF RRBL-RANK-REQ-BLOCK-LIST                  ELUABEND
00880               TO SSR-AREA-LENGTH                                  ELUABEND
00881      WHEN CIA-ELSSRTP-DDN                                         ELUABEND
00882          SET ADDRESS OF SRP-SUBROUTINE-PARAMETERS                 ELUABEND
00883               TO SSR-AREA-PTR                                     ELUABEND
00884          MOVE LENGTH OF SRP-SUBROUTINE-PARAMETERS                 ELUABEND
00885               TO SSR-AREA-LENGTH                                  ELUABEND
00886      WHEN CIA-ELSTCWA-DDN                                         ELUABEND
00887          SET ADDRESS OF TCAR-COMPRESSION-WORK-AREA                ELUABEND
00888               TO SSR-AREA-PTR                                     ELUABEND
00889          MOVE LENGTH OF TCAR-COMPRESSION-WORK-AREA                ELUABEND
00890               TO SSR-AREA-LENGTH                                  ELUABEND
00891      WHEN CIA-RDMC4306-DDN                                        ELUABEND
00892          SET ADDRESS OF RDMC4306-RECORD                           ELUABEND
00893               TO SSR-AREA-PTR                                     ELUABEND
00894          MOVE LENGTH OF RDMC4306-RECORD                           ELUABEND
00895               TO SSR-AREA-LENGTH                                  ELUABEND
00896      WHEN CIA-RDMC4308-DDN                                        ELUABEND
00897          SET ADDRESS OF RDMC4308-RECORD                           ELUABEND
00898               TO SSR-AREA-PTR                                     ELUABEND
00899          MOVE LENGTH OF RDMC4308-RECORD                           ELUABEND
00900               TO SSR-AREA-LENGTH                                  ELUABEND
00901      WHEN CIA-RDMC4311-DDN                                        ELUABEND
00902          SET ADDRESS OF RDMC4311-RECORD                           ELUABEND
00903               TO SSR-AREA-PTR                                     ELUABEND
00904          MOVE LENGTH OF RDMC4311-RECORD                           ELUABEND
00905               TO SSR-AREA-LENGTH                                  ELUABEND
00906      WHEN OTHER                                                   ELUABEND
00907          MOVE ZERO TO SSR-AREA-LENGTH                             ELUABEND
00908      END-EVALUATE.                                                ELUABEND
00909 /                                                                 ELUABEND
00910  3000-WRITE-IO.                                                   ELUABEND
00911      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS TO                ELUABEND
00912           SMA-PTR (SMA-STG-MGT-IDX).                              ELUABEND
00913      SET SSR-AREA-PTR TO SMA-PTR (SMA-STG-MGT-IDX).               ELUABEND
00914      SET SSR-I-O-ITEM    TO TRUE.                                 ELUABEND
00915      SET SSR-SORT-BLOCK  TO TRUE.                                 ELUABEND
00916      MOVE ZERO TO SSR-SEQUENCE-NUM.                               ELUABEND
00917      MOVE LENGTH OF IOP-INPUT-OUTPUT-PARAMETERS                   ELUABEND
00918           TO SSR-AREA-LENGTH.                                     ELUABEND
00919      MOVE SMA-DDN    (SMA-STG-MGT-IDX) TO SSR-DDNAME.             ELUABEND
00920      PERFORM 8110-WRITE-MEM-NO-CHECK.                             ELUABEND
00921      PERFORM 5000-WRITE-RECORDS.                                  ELUABEND
00922                                                                   ELUABEND
00923  4000-WRITE-TS.                                                   ELUABEND
00924      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS TO                ELUABEND
00925           SMA-PTR (SMA-STG-MGT-IDX).                              ELUABEND
00926      SET SSR-AREA-PTR TO SMA-PTR (SMA-STG-MGT-IDX).               ELUABEND
00927      SET SSR-T-S-ITEM    TO TRUE.                                 ELUABEND
00928      SET SSR-SORT-BLOCK  TO TRUE.                                 ELUABEND
00929      MOVE ZERO TO SSR-SEQUENCE-NUM.                               ELUABEND
00930      MOVE LENGTH OF IOP-INPUT-OUTPUT-PARAMETERS                   ELUABEND
00931           TO SSR-AREA-LENGTH.                                     ELUABEND
00932      MOVE SMA-DDN    (SMA-STG-MGT-IDX) TO SSR-DDNAME.             ELUABEND
00933      PERFORM 8100-WRITE-MEM-WITH-CHECK.                           ELUABEND
00934      PERFORM 5000-WRITE-RECORDS.                                  ELUABEND
00935                                                                   ELUABEND
00936      PERFORM 4010-COPY-TSQ                                        ELUABEND
00937          WITH TEST AFTER                                          ELUABEND
00938          VARYING WS-TSQ-COUNT FROM 1 BY 1                         ELUABEND
00939          UNTIL EIBRCODE NOT = LOW-VALUES.                         ELUABEND
00940                                                                   ELUABEND
00941  4010-COPY-TSQ.                                                   ELUABEND
00942      EXEC CICS READQ TS                                           ELUABEND
00943           QUEUE(IOP-TSQ-ID)                                       ELUABEND
00944           SET(WS-TSQ-POINTER)                                     ELUABEND
00945           LENGTH(WS-TSQ-LENGTH)                                   ELUABEND
00946           ITEM(WS-TSQ-COUNT)                                      ELUABEND
00947      END-EXEC.                                                    ELUABEND
00948      IF EIBRCODE = LOW-VALUES                                     ELUABEND
00949          SET SSR-AREA-PTR      TO WS-TSQ-POINTER                  ELUABEND
00950          SET SSR-T-S-ITEM      TO TRUE                            ELUABEND
00951          SET SSR-SORT-TS-QUEUE TO TRUE                            ELUABEND
00952          MOVE WS-TSQ-LENGTH    TO SSR-AREA-LENGTH                 ELUABEND
00953          PERFORM 8110-WRITE-MEM-NO-CHECK                          ELUABEND
00954      END-IF.                                                      ELUABEND
00955 /                                                                 ELUABEND
00956  5000-WRITE-RECORDS.                                              ELUABEND
00957      IF IOP-REC-PTR NOT = NULL                                    ELUABEND
00958          SET SSR-AREA-PTR TO IOP-REC-PTR                          ELUABEND
00959          SET SSR-SORT-RECORD TO TRUE                              ELUABEND
00960          MOVE IOP-REC-LEN TO SSR-AREA-LENGTH                      ELUABEND
00961          PERFORM 8100-WRITE-MEM-WITH-CHECK                        ELUABEND
00962      END-IF.                                                      ELUABEND
00963                                                                   ELUABEND
00964      IF IOP-DUP-REC-PTR NOT = NULL                                ELUABEND
00965          SET SSR-AREA-PTR TO IOP-DUP-REC-PTR                      ELUABEND
00966          SET SSR-SORT-RECORD TO TRUE                              ELUABEND
00967          MOVE IOP-DUP-REC-LEN TO SSR-AREA-LENGTH                  ELUABEND
00968          PERFORM 8100-WRITE-MEM-WITH-CHECK                        ELUABEND
00969      END-IF.                                                      ELUABEND
00970 /***********************************************************      ELUABEND
00971 *                                                          *      ELUABEND
00972 *              8100-WRITE-MEM-WITH-CHECK                   *      ELUABEND
00973 *                                                          *      ELUABEND
00974 ************************************************************      ELUABEND
00975 *                                                          *      ELUABEND
00976 *   THIS PARAGRAPH WRITES A DATA AREA TO A ESDS FILE AND   *      ELUABEND
00977 *   CHECKS FOR DUPLICATE RECORDS.  IF THE AREA HAS BEEN    *      ELUABEND
00978 *   ALREADY WRIITEN, THE WRITE IS SUPPRESSED.              *      ELUABEND
00979 *   TO BYPASS THE DUPLICATE RECORD CHECK, PERFORM          *      ELUABEND
00980 *   PARAGRAPH 8110-WRITE-MEM-NO-CHECK.                     *      ELUABEND
00981 *                                                          *      ELUABEND
00982 *   BEFORE CALLING, SSR-AREA-PTR MUST BE SET TO THE AREA   *      ELUABEND
00983 *   TO WRITE TO THE FILE.  ALSO, SSR-AREA-LENGTH MUST BE   *      ELUABEND
00984 *   SET TO THE LENGTH OF THE AREA.                         *      ELUABEND
00985 *                                                          *      ELUABEND
00986 ************************************************************      ELUABEND
00987                                                                   ELUABEND
00988  8100-WRITE-MEM-WITH-CHECK.                                       ELUABEND
00989      PERFORM VARYING WS-CHECK-SUB FROM 1 BY 1                     ELUABEND
00990        UNTIL WS-CHECK-SUB > WS-NBR-ADDRS                          ELUABEND
00991           OR (WS-ADDRS-PTR (WS-CHECK-SUB) = SSR-AREA-PTR AND      ELUABEND
00992               WS-ADDRS-LEN (WS-CHECK-SUB) = SSR-AREA-LENGTH)      ELUABEND
00993             CONTINUE                                              ELUABEND
00994      END-PERFORM.                                                 ELUABEND
00995      IF WS-CHECK-SUB > WS-NBR-ADDRS                               ELUABEND
00996          IF NOT WS-ABEND-ADDRS-FULL                               ELUABEND
00997              ADD 1 TO WS-NBR-ADDRS                                ELUABEND
00998              SET WS-ADDRS-PTR (WS-NBR-ADDRS) TO                   ELUABEND
00999                  SSR-AREA-PTR                                     ELUABEND
01000              MOVE SSR-AREA-LENGTH TO                              ELUABEND
01001                   WS-ADDRS-LEN (WS-NBR-ADDRS)                     ELUABEND
01002          END-IF                                                   ELUABEND
01003          PERFORM 8110-WRITE-MEM-NO-CHECK                          ELUABEND
01004      END-IF.                                                      ELUABEND
01005 /***********************************************************      ELUABEND
01006 *                                                          *      ELUABEND
01007 *              8110-WRITE-MEM-NO-CHECK                     *      ELUABEND
01008 *                                                          *      ELUABEND
01009 ************************************************************      ELUABEND
01010 *                                                          *      ELUABEND
01011 *   THIS PARAGRAPH WRITES A DATA AREA TO A ESDS FILE AND   *      ELUABEND
01012 *   BYPASSES THE DUPLICATE RECORD CHECK.  TO DO DUP RECORD *      ELUABEND
01013 *   CHECKS, PERFORM 8100-WRITE-MEM-WITH-CHECK              *      ELUABEND
01014 *                                                          *      ELUABEND
01015 *   BEFORE CALLING, SSR-AREA-PTR MUST BE SET TO THE AREA   *      ELUABEND
01016 *   TO WRITE TO THE FILE.  ALSO, SSR-AREA-LENGTH MUST BE   *      ELUABEND
01017 *   SET TO THE LENGTH OF THE AREA.  IF THE AREA IS TOO     *      ELUABEND
01018 *   LONG, A MESSAGE RECORD WILL BE WRITTEN INSTEAD.        *      ELUABEND
01019 *                                                          *      ELUABEND
01020 ************************************************************      ELUABEND
01021                                                                   ELUABEND
01022  8110-WRITE-MEM-NO-CHECK.                                         ELUABEND
01023      IF SSR-AREA-LENGTH > LENGTH OF LS-RECORD                     ELUABEND
01024          MOVE SSR-AREA-LENGTH TO WS-AREA-TOO-LONG-LEN             ELUABEND
01025          CALL 'ELUADDRS' USING WS-AREA-TOO-LONG-MSG               ELUABEND
01026                                WS-RECORD-PTR                      ELUABEND
01027          MOVE LENGTH OF WS-AREA-TOO-LONG-MSG TO WS-RECORD-LEN     ELUABEND
01028                                                 WS-REMAIN-LEN     ELUABEND
01029      ELSE                                                         ELUABEND
01030          SET WS-RECORD-PTR TO SSR-AREA-PTR                        ELUABEND
01031          MOVE SSR-AREA-LENGTH TO WS-RECORD-LEN                    ELUABEND
01032                                  WS-REMAIN-LEN                    ELUABEND
01033      END-IF.                                                      ELUABEND
01034      SET ADDRESS OF LS-RECORD TO WS-RECORD-PTR.                   ELUABEND
01035      MOVE 1 TO WS-DUMP-POS.                                       ELUABEND
01036      PERFORM 8120-WRITE-SNAPSHOT                                  ELUABEND
01037         UNTIL WS-REMAIN-LEN = ZERO                                ELUABEND
01038            OR WS-FILE-FULL.                                       ELUABEND
01039                                                                   ELUABEND
01040  8120-WRITE-SNAPSHOT.                                             ELUABEND
01041      IF WS-REMAIN-LEN > WS-MAX-LEN                                ELUABEND
01042          MOVE WS-MAX-LEN    TO WS-MOVE-LEN                        ELUABEND
01043      ELSE                                                         ELUABEND
01044          MOVE WS-REMAIN-LEN TO WS-MOVE-LEN.                       ELUABEND
01045                                                                   ELUABEND
01046      MOVE WS-MOVE-LEN       TO SSR-SUB-SIZE.                      ELUABEND
01047      CALL 'ELUMVCL' USING  LS-BYTE (WS-DUMP-POS)                  ELUABEND
01048                            WS-MOVE-LEN                            ELUABEND
01049                            SSR-SUB-REC (1)                        ELUABEND
01050                            WS-MOVE-LEN                            ELUABEND
01051                            WS-PAD-CHAR.                           ELUABEND
01052      EXEC CICS WRITE                                              ELUABEND
01053          DATASET(WS-SNAPSHOT-FILE-ID)                             ELUABEND
01054          FROM(SSR-SNAP-SHOT-RECORD)                               ELUABEND
01055          RIDFLD(WS-RBA)                                           ELUABEND
01056          RBA                                                      ELUABEND
01057      END-EXEC.                                                    ELUABEND
01058      ADD 1 TO SSR-SEQUENCE-NUM.                                   ELUABEND
01059      ADD WS-MOVE-LEN TO WS-DUMP-POS.                              ELUABEND
01060      SUBTRACT WS-MOVE-LEN FROM WS-REMAIN-LEN.                     ELUABEND
01061      IF EIBRCODE NOT = LOW-VALUES                                 ELUABEND
01062           SET WS-FILE-FULL TO TRUE.                               ELUABEND
01063 /***********************************************************      ELUABEND
01064 *                                                          *      ELUABEND
01065 *              9000-DISPLAY-SCREEN                         *      ELUABEND
01066 *                                                          *      ELUABEND
01067 ************************************************************      ELUABEND
01068 *                                                          *      ELUABEND
01069 *   THIS PARAGRAPH DISPLAYS THE ABEND NOTICE SCREEN TO     *      ELUABEND
01070 *   THE USER.                                              *      ELUABEND
01071 *                                                          *      ELUABEND
01072 ************************************************************      ELUABEND
01073                                                                   ELUABEND
01074  9000-DISPLAY-SCREEN.                                             ELUABEND
01075      PERFORM 9010-INITIALIZE-MAP.                                 ELUABEND
01076      PERFORM 9100-FORMAT-COMMON-HEADING.                          ELUABEND
01077      PERFORM 9200-DISPLAY-USER-ABEND-MSG.                         ELUABEND
01078      PERFORM 9020-SEND-SCREEN.                                    ELUABEND
01079                                                                   ELUABEND
01080  9010-INITIALIZE-MAP.                                             ELUABEND
01081      MOVE SPACES TO EL03MAPO.                                     ELUABEND
01082      MOVE DFHBMPRO TO MAPID3A                                     ELUABEND
01083                       PGNBRA                                      ELUABEND
01084                       PGMSGA                                      ELUABEND
01085                       DATE3A                                      ELUABEND
01086                       TEXTA (2).                                  ELUABEND
01087      MOVE DFHBMASB TO TEXTA (1).                                  ELUABEND
01088      PERFORM VARYING WS-NEXT-TEXT-LINE FROM 3 BY 1                ELUABEND
01089              UNTIL WS-NEXT-TEXT-LINE > 22                         ELUABEND
01090          MOVE DFHBMASB TO TEXTA (WS-NEXT-TEXT-LINE)               ELUABEND
01091      END-PERFORM.                                                 ELUABEND
01092      IF WS-FILE-FULL                                              ELUABEND
01093          MOVE WS-MSG8-1 TO TEXTO (1).                             ELUABEND
01094      MOVE 2 TO WS-NEXT-TEXT-LINE.                                 ELUABEND
01095                                                                   ELUABEND
01096  9020-SEND-SCREEN.                                                ELUABEND
01097      MOVE WS-MSG4-1  TO  TEXTO (21).                              ELUABEND
01098      MOVE WS-MSG5-1  TO  TEXTO (22).                              ELUABEND
01099      MOVE -1         TO  MAPID3L.                                 ELUABEND
01100      EXEC CICS SEND                                               ELUABEND
01101                MAP('EL03MAP')                                     ELUABEND
01102                MAPSET('EL03SET')                                  ELUABEND
01103                ERASE                                              ELUABEND
01104      END-EXEC.                                                    ELUABEND
01105                                                                   ELUABEND
01106  9100-FORMAT-COMMON-HEADING.                                      ELUABEND
01107      MOVE 'ELUABEND' TO MAPID3O.                                  ELUABEND
01108      MOVE 1 TO PGNBRO.                                            ELUABEND
01109      MOVE 'LAST' TO PGMSGO.                                       ELUABEND
01110      MOVE EIBDATE TO HGADATE-JULIAN1.                             ELUABEND
01111 *    MOVE EIBDATE TO WS-EIBDATE-CEN.                              ELUABEND
01112 *    MOVE WS-EIBDATE-DATE TO HGADATE-JULIAN1.                     ELUABEND
01113      PERFORM 9900-CONVERT-DATE.                                   ELUABEND
01114      MOVE HGADATE-DATE2  TO WS-DATE-EDIT.                         ELUABEND
01115      MOVE WS-DATE-EDIT   TO DATE3O.                               ELUABEND
01116      IF SMA-ELSSSCB-PTR NOT = NULL                                ELUABEND
01117          PERFORM 9110-MOVE-SSCB-VALUES.                           ELUABEND
01118      MOVE WS-KEY-INFO-1 TO TEXTO (WS-NEXT-TEXT-LINE).             ELUABEND
01119      ADD 1 TO WS-NEXT-TEXT-LINE.                                  ELUABEND
01120      PERFORM 9930-ESTABLISH-COMPRESS-AREA.                        ELUABEND
01121                                                                   ELUABEND
01122      IF WS-ABCODE = 'EL09' OR 'EL10'                              ELUABEND
01123           PERFORM 9140-SPECIAL-MSGS                               ELUABEND
01124      ELSE                                                         ELUABEND
01125          IF WS-ABCODE-ELS                                         ELUABEND
01126               PERFORM 9130-FORMAT-USER-ABEND-MSG                  ELUABEND
01127          ELSE                                                     ELUABEND
01128               PERFORM 9120-FORMAT-SYSTEM-ABEND-MSG.               ELUABEND
01129      PERFORM 9940-TEXT-COMPRESSION.                               ELUABEND
01130                                                                   ELUABEND
01131  9110-MOVE-SSCB-VALUES.                                           ELUABEND
01132      SET ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK                   ELUABEND
01133          TO SMA-ELSSSCB-PTR.                                      ELUABEND
01134      MOVE SSB-GRP-NO TO WS-HDR1-GROUP-NO.                         ELUABEND
01135      MOVE SSB-SECT-NO TO WS-HDR1-SECT-NO.                         ELUABEND
01136                                                                   ELUABEND
01137      IF SSB-SRV-FROM-DATE IS NUMERIC                              ELUABEND
01138          MOVE SSB-SRV-FROM-DATE TO HGADATE-JULIAN1                ELUABEND
01139          PERFORM 9900-CONVERT-DATE                                ELUABEND
01140          MOVE HGADATE-DATE2 TO WS-HDR1-FROM-DATE.                 ELUABEND
01141      IF SSB-SRV-TO-DATE IS NUMERIC                                ELUABEND
01142          MOVE SSB-SRV-TO-DATE TO HGADATE-JULIAN1                  ELUABEND
01143          PERFORM 9900-CONVERT-DATE                                ELUABEND
01144          MOVE HGADATE-DATE2 TO WS-HDR1-TO-DATE.                   ELUABEND
01145                                                                   ELUABEND
01146      MOVE SSB-TOPIC-PHRASE  TO    WS-TOPIC-TITLE.                 ELUABEND
01147      PERFORM 9190-DETERMINE-FAM-REL.                              ELUABEND
01148                                                                   ELUABEND
01149  9120-FORMAT-SYSTEM-ABEND-MSG.                                    ELUABEND
01150      MOVE WS-MSG1-1         TO TCAR-FROM-LINE (1).                ELUABEND
01151      MOVE 'AN'              TO TCAR-FROM-LINE (2).                ELUABEND
01152      MOVE WS-MSG1-2         TO TCAR-FROM-LINE (3).                ELUABEND
01153      MOVE WS-MSG3-1         TO TCAR-FROM-LINE (4).                ELUABEND
01154      MOVE WS-MSG3-2         TO TCAR-FROM-LINE (5).                ELUABEND
01155      MOVE WS-ABCODE         TO TCAR-FROM-LINE (6).                ELUABEND
01156      MOVE '.'               TO TCAR-FROM-LINE-LAST-DIGIT (6).     ELUABEND
01157                                                                   ELUABEND
01158  9130-FORMAT-USER-ABEND-MSG.                                      ELUABEND
01159      MOVE WS-MSG2-1         TO TCAR-FROM-LINE (1).                ELUABEND
01160      MOVE WS-TOPIC-TITLE    TO TCAR-FROM-LINE (2).                ELUABEND
01161      MOVE WS-MSG2-2         TO TCAR-FROM-LINE (3).                ELUABEND
01162      MOVE WS-MSG1-1         TO TCAR-FROM-LINE (4).                ELUABEND
01163      STRING 'A CODE '          DELIMITED BY SIZE                  ELUABEND
01164              WS-ABCODE         DELIMITED BY SIZE                  ELUABEND
01165        INTO                    TCAR-FROM-LINE (5).                ELUABEND
01166      MOVE WS-MSG1-2         TO TCAR-FROM-LINE (6).                ELUABEND
01167                                                                   ELUABEND
01168  9140-SPECIAL-MSGS.                                               ELUABEND
01169      MOVE WS-MSG6-1             TO TCAR-FROM-LINE (1).            ELUABEND
01170      MOVE WS-HOLD-CIA-DDNAME    TO TCAR-FROM-LINE (2).            ELUABEND
01171      IF WS-ABCODE = 'EL09'                                        ELUABEND
01172          MOVE WS-MSG6-2         TO TCAR-FROM-LINE (3)             ELUABEND
01173      ELSE                                                         ELUABEND
01174          MOVE WS-MSG6-3         TO TCAR-FROM-LINE (3)             ELUABEND
01175      END-IF.                                                      ELUABEND
01176      MOVE WS-MSG3-1             TO TCAR-FROM-LINE (4).            ELUABEND
01177      MOVE '.'                   TO TCAR-FROM-LINE-LAST-DIGIT (4). ELUABEND
01178 /***********************************************************      ELUABEND
01179 *                                                          *      ELUABEND
01180 *        DETERMINE FAMILY RELATIONSHIP PHRASE              *      ELUABEND
01181 *                                                          *      ELUABEND
01182 ************************************************************      ELUABEND
01183  9190-DETERMINE-FAM-REL.                                          ELUABEND
01184      EVALUATE TRUE                                                ELUABEND
01185      WHEN SSB-MEDCA-ELIG                                          ELUABEND
01186              MOVE 'ALL MEDICARE' TO WS-HDR1-FAM-REL               ELUABEND
01187      WHEN SSB-MEDCA-INELIG AND                                    ELUABEND
01188           SSB-FR-UNDEF     AND                                    ELUABEND
01189           SSB-PT-AGE-UNDEF                                        ELUABEND
01190              MOVE 'ALL NON-MEDICARE' TO WS-HDR1-FAM-REL           ELUABEND
01191      WHEN SSB-MEDCA-UNDEF AND                                     ELUABEND
01192           SSB-FR-UNDEF    AND                                     ELUABEND
01193           SSB-PT-AGE-UNDEF                                        ELUABEND
01194              MOVE 'ALL FAMILY MEMBERS' TO WS-HDR1-FAM-REL         ELUABEND
01195      WHEN SSB-PT-AGE-UNDEF                                        ELUABEND
01196          PERFORM 9191-SIMPLE-SELECTED-FAMILY-P                    ELUABEND
01197      WHEN OTHER                                                   ELUABEND
01198          PERFORM 9192-AGE-CONTROLLED-FAMILY-PH                    ELUABEND
01199      END-EVALUATE.                                                ELUABEND
01200                                                                   ELUABEND
01201 ************************************************************      ELUABEND
01202 *                                                          *      ELUABEND
01203 *        SIMPLE SELECTED FAMILY PHRASE                     *      ELUABEND
01204 *                                                          *      ELUABEND
01205 ************************************************************      ELUABEND
01206  9191-SIMPLE-SELECTED-FAMILY-P.                                   ELUABEND
01207      EVALUATE TRUE                                                ELUABEND
01208      WHEN SSB-MEMBER                                              ELUABEND
01209          MOVE 'MEMBER' TO WS-HDR1-FAM-REL                         ELUABEND
01210      WHEN SSB-SPOUSE                                              ELUABEND
01211          MOVE 'SPOUSE' TO WS-HDR1-FAM-REL                         ELUABEND
01212      WHEN SSB-DEPENDENT                                           ELUABEND
01213          MOVE 'DEPENDENT' TO WS-HDR1-FAM-REL                      ELUABEND
01214      WHEN OTHER                                                   ELUABEND
01215          MOVE 'UNDEFINED' TO WS-HDR1-FAM-REL                      ELUABEND
01216      END-EVALUATE.                                                ELUABEND
01217 /***********************************************************      ELUABEND
01218 *                                                          *      ELUABEND
01219 *        AGE CONTROLLED FAMILY PHRASE                      *      ELUABEND
01220 *                                                          *      ELUABEND
01221 ************************************************************      ELUABEND
01222  9192-AGE-CONTROLLED-FAMILY-PH.                                   ELUABEND
01223      EVALUATE TRUE                                                ELUABEND
01224      WHEN SSB-MEMBER                                              ELUABEND
01225          MOVE 'MEMBER '    TO WS-HDR1-FAM-REL                     ELUABEND
01226      WHEN SSB-SPOUSE                                              ELUABEND
01227          MOVE 'SPOUSE '    TO WS-HDR1-FAM-REL                     ELUABEND
01228      WHEN SSB-DEPENDENT                                           ELUABEND
01229          MOVE 'DEPENDENT ' TO WS-HDR1-FAM-REL                     ELUABEND
01230      WHEN OTHER                                                   ELUABEND
01231          MOVE 'PATIENT '   TO WS-HDR1-FAM-REL                     ELUABEND
01232      END-EVALUATE.                                                ELUABEND
01233                                                                   ELUABEND
01234      PERFORM 9910-ESTABLISH-CM-INTERFACE.                         ELUABEND
01235      MOVE '@ELS'           TO CMF-RECORD-PREFIX.                  ELUABEND
01236      MOVE 'SSB-PT-AGE'     TO CMF-ELEMENT-SYSTEM-NAME.            ELUABEND
01237      MOVE SSB-PT-AGE       TO CMF-CODE-VALUE.                     ELUABEND
01238      PERFORM 9920-LINK-TO-CODES-MANUAL.                           ELUABEND
01239                                                                   ELUABEND
01240      STRING WS-HDR1-FAM-REL DELIMITED BY '  '                     ELUABEND
01241             ' ' CMF-DESCR-LINE (1) DELIMITED BY SIZE              ELUABEND
01242             INTO WS-HDR1-FAM-REL.                                 ELUABEND
01243 /***********************************************************      ELUABEND
01244 *                                                          *      ELUABEND
01245 *        LOOKUP ELIQ ABEND CODE MESSAGE                    *      ELUABEND
01246 *                                                          *      ELUABEND
01247 ************************************************************      ELUABEND
01248  9200-DISPLAY-USER-ABEND-MSG.                                     ELUABEND
01249      PERFORM 9910-ESTABLISH-CM-INTERFACE.                         ELUABEND
01250      MOVE '@ELS'        TO CMF-RECORD-PREFIX.                     ELUABEND
01251      MOVE 'SSB-ABCODE'  TO CMF-ELEMENT-SYSTEM-NAME.               ELUABEND
01252      MOVE WS-ABCODE     TO CMF-CODE-VALUE.                        ELUABEND
01253      PERFORM 9920-LINK-TO-CODES-MANUAL.                           ELUABEND
01254                                                                   ELUABEND
01255      ADD 1 TO WS-NEXT-TEXT-LINE.                                  ELUABEND
01256      IF CMF-RC-OK                                                 ELUABEND
01257          PERFORM VARYING WS-MOVE-SUB FROM 1 BY 1                  ELUABEND
01258             UNTIL WS-MOVE-SUB > CMF-NBR-DESCR-LINES               ELUABEND
01259                OR WS-NEXT-TEXT-LINE > 20                          ELUABEND
01260             MOVE CMF-DESCR-LINE (WS-MOVE-SUB)                     ELUABEND
01261               TO TEXTO (WS-NEXT-TEXT-LINE)                        ELUABEND
01262             ADD 1 TO WS-NEXT-TEXT-LINE                            ELUABEND
01263          END-PERFORM                                              ELUABEND
01264      ELSE                                                         ELUABEND
01265          MOVE WS-ABCODE TO WS-MSG7-ABCODE                         ELUABEND
01266          MOVE WS-MSG7-1 TO TEXTO (WS-NEXT-TEXT-LINE)              ELUABEND
01267          ADD 1 TO WS-NEXT-TEXT-LINE                               ELUABEND
01268      END-IF.                                                      ELUABEND
01269 /                                                                 ELUABEND
01270  9900-CONVERT-DATE.                                               ELUABEND
01271      MOVE 'CNV' TO HGADATE-FUNC.                                  ELUABEND
01272      MOVE 'J'   TO HGADATE-FORM1.                                 ELUABEND
01273      MOVE 'M'   TO HGADATE-FORM2.                                 ELUABEND
01274      EXEC CICS LINK                                               ELUABEND
01275          PROGRAM('HGADATES')                                      ELUABEND
01276          COMMAREA(WS-HGADATES-PARMS)                              ELUABEND
01277      END-EXEC.                                                    ELUABEND
01278 /                                                                 ELUABEND
01279  9910-ESTABLISH-CM-INTERFACE.                                     ELUABEND
01280      IF SMA-ELSCMIF-PTR = NULL                                    ELUABEND
01281          MOVE LENGTH OF CMF-CODES-MANUAL-INTERFACE                ELUABEND
01282            TO WS-RECORD-LEN                                       ELUABEND
01283          EXEC CICS GETMAIN                                        ELUABEND
01284                    SET(SMA-ELSCMIF-PTR)                           ELUABEND
01285                    FLENGTH(WS-RECORD-LEN)                         ELUABEND
01286          END-EXEC                                                 ELUABEND
01287      END-IF.                                                      ELUABEND
01288      SET ADDRESS OF CMF-CODES-MANUAL-INTERFACE TO                 ELUABEND
01289          SMA-ELSCMIF-PTR.                                         ELUABEND
01290                                                                   ELUABEND
01291  9920-LINK-TO-CODES-MANUAL.                                       ELUABEND
01292      EXEC CICS PUSH HANDLE END-EXEC.                              ELUABEND
01293      EXEC CICS HANDLE ABEND                                       ELUABEND
01294                LABEL(9991-CODES-MANUAL-ABEND)                     ELUABEND
01295      END-EXEC.                                                    ELUABEND
01296      EXEC CICS LINK                                               ELUABEND
01297          PROGRAM('ELUCMIF')                                       ELUABEND
01298          COMMAREA(DFHCOMMAREA)                                    ELUABEND
01299      END-EXEC.                                                    ELUABEND
01300      EXEC CICS POP HANDLE END-EXEC.                               ELUABEND
01301      SET ADDRESS OF CMF-DESCR TO SMA-ELSCMDSC-PTR.                ELUABEND
01302 /                                                                 ELUABEND
01303  9930-ESTABLISH-COMPRESS-AREA.                                    ELUABEND
01304      IF SMA-ELSTCWA-PTR = NULL                                    ELUABEND
01305          MOVE LENGTH OF TCAR-COMPRESSION-WORK-AREA                ELUABEND
01306            TO WS-RECORD-LEN                                       ELUABEND
01307          EXEC CICS GETMAIN                                        ELUABEND
01308                    SET(SMA-ELSTCWA-PTR)                           ELUABEND
01309                    FLENGTH(WS-RECORD-LEN)                         ELUABEND
01310          END-EXEC                                                 ELUABEND
01311      END-IF.                                                      ELUABEND
01312      SET ADDRESS OF TCAR-COMPRESSION-WORK-AREA TO                 ELUABEND
01313          SMA-ELSTCWA-PTR.                                         ELUABEND
01314      INITIALIZE TCAR-COMPRESSION-WORK-AREA.                       ELUABEND
01315                                                                   ELUABEND
01316  9940-TEXT-COMPRESSION.                                           ELUABEND
01317      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELUABEND
01318      MOVE 4  TO TCAR-OUTPUT-FIELD-COUNT.                          ELUABEND
01319      MOVE 79 TO TCAR-OUTPUT-FIELD-1-LEN                           ELUABEND
01320                 TCAR-OUTPUT-FIELD-2-LEN                           ELUABEND
01321                 TCAR-OUTPUT-FIELD-3-LEN                           ELUABEND
01322                 TCAR-OUTPUT-FIELD-4-LEN.                          ELUABEND
01323      PERFORM TCPR-000-TEXT-UNSTRING.                              ELUABEND
01324      ADD 1 TO WS-NEXT-TEXT-LINE.                                  ELUABEND
01325      PERFORM VARYING WS-MOVE-SUB FROM 1 BY 1                      ELUABEND
01326         UNTIL WS-MOVE-SUB > TCAR-OUTPUT-FIELDS-USED               ELUABEND
01327            OR WS-NEXT-TEXT-LINE > 20                              ELUABEND
01328             MOVE TCAR-OPF-DATA (WS-MOVE-SUB)                      ELUABEND
01329               TO TEXTO (WS-NEXT-TEXT-LINE)                        ELUABEND
01330             ADD 1 TO WS-NEXT-TEXT-LINE                            ELUABEND
01331      END-PERFORM.                                                 ELUABEND
01332      COPY ELSTCOMP.                                               ELUABEND
01333 /                                                                 ELUABEND
01334 **************************************************************    ELUABEND
01335 *                                                            *    ELUABEND
01336 *   THIS IS THE ABEND HANDLERS ABEND RECOVERY SECTION.       *    ELUABEND
01337 *                                                            *    ELUABEND
01338 *   THIS AREA OF THE PROGRAM IS ENTERED ONLY IF SOME         *    ELUABEND
01339 *   CONDITION ARISES UNDER WHICH RECOVERY CANNOT CONTINUE.   *    ELUABEND
01340 *   THE CASES PROVIDED FOR ARE A GENERAL ABEND, A CODES      *    ELUABEND
01341 *   MANUAL INTERFACE ABEND AND SOME ERROR CONDITION          *    ELUABEND
01342 *   DETECTED BY THIS PROGRAM.                                *    ELUABEND
01343 *                                                            *    ELUABEND
01344 *   DUE TO THE UNUSUAL NATURE UNDER WHICH THIS SECTION IS    *    ELUABEND
01345 *   ENTERED, IS SHOULD BE CONSIDERED AS A BLACK HOLE - YOU   *    ELUABEND
01346 *   WILL NEVER RETURN ONCE YOU ENTER HERE.                   *    ELUABEND
01347 *                                                            *    ELUABEND
01348 *                                                            *    ELUABEND
01349 **************************************************************    ELUABEND
01350  9990-GENERAL-ABEND.                                              ELUABEND
01351      EXEC CICS ASSIGN                                             ELUABEND
01352                ABCODE(WS-RECOVERY-ABCODE)                         ELUABEND
01353      END-EXEC.                                                    ELUABEND
01354      STRING 'A CODE '                 DELIMITED BY SIZE           ELUABEND
01355             WS-RECOVERY-ABCODE        DELIMITED BY SIZE           ELUABEND
01356             ' ABEND OCCURRED DURRING ERROR RECOVERY.'             ELUABEND
01357                                       DELIMITED BY SIZE           ELUABEND
01358        INTO WS-TERMINATION-MSG.                                   ELUABEND
01359      GO TO 9999-UNABLE-TO-CONTINUE.                               ELUABEND
01360                                                                   ELUABEND
01361  9991-CODES-MANUAL-ABEND.                                         ELUABEND
01362      EXEC CICS ASSIGN                                             ELUABEND
01363                ABCODE(WS-RECOVERY-ABCODE)                         ELUABEND
01364      END-EXEC.                                                    ELUABEND
01365      STRING 'A CODE '                 DELIMITED BY SIZE           ELUABEND
01366             WS-RECOVERY-ABCODE        DELIMITED BY SIZE           ELUABEND
01367             ' ABEND OCCURRED DURRING CODES MANUAL ACCESS.'        ELUABEND
01368                                       DELIMITED BY SIZE           ELUABEND
01369        INTO WS-TERMINATION-MSG.                                   ELUABEND
01370      GO TO 9999-UNABLE-TO-CONTINUE.                               ELUABEND
01371                                                                   ELUABEND
01372  9999-UNABLE-TO-CONTINUE.                                         ELUABEND
01373      PERFORM 9010-INITIALIZE-MAP.                                 ELUABEND
01374      MOVE 'ELUABEND'  TO MAPID3O.                                 ELUABEND
01375      MOVE 1 TO PGNBRO.                                            ELUABEND
01376      MOVE 'LAST' TO PGMSGO.                                       ELUABEND
01377 *    MOVE EIBDATE TO WS-EIBDATE-CEN.                              ELUABEND
01378 *    MOVE WS-EIBDATE-DATE TO HGADATE-JULIAN1.                     ELUABEND
01379      MOVE EIBDATE TO HGADATE-JULIAN1.                             ELUABEND
01380      PERFORM 9900-CONVERT-DATE.                                   ELUABEND
01381      MOVE HGADATE-DATE2  TO WS-DATE-EDIT.                         ELUABEND
01382      MOVE WS-DATE-EDIT   TO DATE3O.                               ELUABEND
01383      MOVE WS-TERMINATION-MSG TO TEXTO (3).                        ELUABEND
01384      STRING 'THE ORIGINAL ABEND CODES WAS ' DELIMITED BY SIZE     ELUABEND
01385             WS-ABCODE                       DELIMITED BY SIZE     ELUABEND
01386             '.'                             DELIMITED BY SIZE     ELUABEND
01387        INTO TEXTO (4).                                            ELUABEND
01388      MOVE WS-MSG3-1          TO TEXTO (6).                        ELUABEND
01389      MOVE WS-MSG4-1          TO TEXTO (21).                       ELUABEND
01390      MOVE WS-MSG5-1          TO TEXTO (22).                       ELUABEND
01391                                                                   ELUABEND
01392      EXEC CICS DUMP                                               ELUABEND
01393                DUMPCODE(WS-ABCODE)                                ELUABEND
01394                TASK                                               ELUABEND
01395      END-EXEC.                                                    ELUABEND
01396                                                                   ELUABEND
01397      EXEC CICS SEND                                               ELUABEND
01398                MAP('EL03MAP')                                     ELUABEND
01399                MAPSET('EL03SET')                                  ELUABEND
01400                ERASE                                              ELUABEND
01401      END-EXEC.                                                    ELUABEND
01402      EXEC CICS RETURN                                             ELUABEND
01403                TRANSID('ELIQ')                                    ELUABEND
01404      END-EXEC.                                                    ELUABEND
