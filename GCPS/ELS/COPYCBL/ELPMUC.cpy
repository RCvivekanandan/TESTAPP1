000100***************** BEGINNING OF ELPMU MEMBER *******************   00010002
000101****** HEADER RECORD ******                                       00020002
000102 01  HEADER-RECORD.                                               00030008
000200     05  HEADER-SORT-KEY      PIC X(96).                          00040011
000300     05  HEADER-TYPE-RUN      PIC X.                              00050008
000400         88  LOAD-FILE        VALUE  'F'.                         00060008
000500         88  LOAD-RECORD      VALUE  'R'.                         00070008
000600     05  FILLER               PIC X(06).                          00080008
000700****** RECORD LIST ******                                         00090008
000800 01  RECORD-LIST-LOAD.                                            00100008
000900     05  SORT-KEY-RL.                                             00110008
001000         10  PREFIX-RL         PIC X(08).                         00120008
001100         10  NAME-RL           PIC X(75).                         00130008
001200         10  RECORD-TYPE-RL    PIC X.                             00140008
001300         10  CODE-VALUE-RL     PIC X(10).                         00150008
               10  CODE-DESC-SEQ-RL  PIC 99.                            00151012
001400     05  RECORD-NAME-RL        PIC X(50).                         00160008
001500     05  RECORD-FILE-TYPE-RL   PIC X.                             00170008
001600     05  AUTO-REPRINT-FLAG-RL  PIC X.                             00180008
001700     05  DELETE-RECORD-FLAG-RL PIC X.                             00190008
001710****** DATA ELEMENT ******                                        00200008
001800 01  DATA-ELEMENT-LOAD.                                           00210008
001900     05  SORT-KEY-DE.                                             00220008
002000         10  PREFIX-DE           PIC X(08).                       00230008
002100         10  NAME-DE             PIC X(75).                       00240008
002200         10  RECORD-TYPE-DE      PIC X.                           00250008
002300         10  CODE-VALUE-DE       PIC X(10).                       00260008
               10  CODE-DESC-SEQ-DE    PIC 99.                          00261012
002400     05  ELEMENT-FORMAT-DE       PIC XX.                          00270008
002500     05  ELEMENT-LENGTH-DE       PIC S9(03)  COMP-3.              00280008
002600     05  FORMAT-COMMENT-DE       PIC X(21).                       00290008
002700     05  DELETE-ELEMENT-FLAG-DE  PIC X.                           00300013
002800     05  AUTO-REPRINT-FLAG-DE    PIC X.                           00310008
002900     05  RECORD-POS-DE           PIC S9(03)  COMP-3.              00320008
003000     05  STORED-LENGTH-DE        PIC S9(03)  COMP-3.              00330008
003100     05  STORED-DECIMALS-DE      PIC S9(03)  COMP-3.              00340008
           05  STORED-FORMAT-DE        PIC X.                           00341014
003200     05  CODES-FLAG-DE           PIC X.                           00350008
003300     05  COBOL-NAME-DE           PIC X(30).                       00360008
003400     05  BAL-NAME-DE             PIC X(08).                       00370008
003500     05  NBR-DESC-LINES-DE       PIC S9(03)  COMP-3.              00380008
003600     05  DESC-LINE-DE            PIC X(79)                        00390008
003700          OCCURS 1 TO 11 TIMES DEPENDING ON NBR-DESC-LINES-DE.    00400009
003800****** CODE VALUE ******                                          00410008
003900 01  CODE-VALUE-LOAD.                                             00420008
004000     05  SORT-KEY-CV.                                             00430008
004100         10  PREFIX-CV               PIC X(08).                   00440008
004200         10  NAME-CV                 PIC X(75).                   00450008
004300         10  RECORD-TYPE-CV          PIC X.                       00460008
004400         10  CODE-VALUE-CV           PIC X(10).                   00470008
               10  CODE-DESC-SEQ-CV        PIC 99.                      00471012
004500     05  CODE-NAME-CV                PIC X(50).                   00480008
004600     05  DELETE-CODE-FLAG-CV         PIC X.                       00490008
004700     05  NBR-VALUE-DESC-LINES-CV     PIC S9(03)   COMP-3.         00500008
004800     05  VALUE-DESC-LINE-CV          PIC X(79)                    00510008
004900           OCCURS 1 TO 12 TIMES DEPENDING                         00520010
004950               ON NBR-VALUE-DESC-LINES-CV.                        00521008
005000****************** END OF COPY MEMBER ELPMU *******************   00530008
