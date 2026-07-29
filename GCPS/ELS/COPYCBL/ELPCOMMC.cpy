000100***********************************************************       00001002
000200*                       DFHCOMMAREA                       *       00002002
000300*              ENGLISH LANGUAGE PROTOTYPE                 *       00003002
000400*                                                         *       00004002
000500*                        LENGTH=500                       *       00004105
      *                                                                *        
      *    COPYBOOK:   ELPCOMMC                                        *        
      *    DATE:       ??-???-????                                     *        
      *    AUTHOR:     ??????????????                                  *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 09-NOV-1987 LET       ADDED AN '88' LEVEL VALUE 'L'      *        
      *                             UNDER CA-CURRENT-FUNCTION AS       *        
      *                             REQUESTED BY AKK.                  *        
      * 02.00 28-NOV-90   RKH       CHANGED CA-CIA-POINTER TO          *        
      *                             USAGE IS POINTER.                  *        
      ******************************************************************        
           02  CA-CIA-POINTER          USAGE IS POINTER  SYNC.                  
000602     02  CA-CURRENT-PGM          PICTURE  X(01).                  00020000
000603         88  CA-SIGNON           VALUE ' '.                       00030000
000604         88  CA-RECORD-LIST      VALUE 'A'.                       00040000
000605         88  CA-SELECT-DE        VALUE 'B'.                       00050000
000606         88  CA-DE-DEFINE        VALUE 'C'.                       00060000
000700         88  CA-SELECT-CV        VALUE 'D'.                       00070002
000800         88  CA-CV-EDIT          VALUE 'E'.                       00080000
000900     02  CA-CURRENT-FUNCTION     PICTURE  X(01).                  00090000
001000         88  CA-ADD              VALUE 'A'.                       00100000
001100         88  CA-CHANGE           VALUE 'C'.                       00110000
001200         88  CA-DELETE           VALUE 'D'.                       00120000
               88  CA-LOCATE           VALUE 'L'.                               
001300         88  CA-MAP-FROM         VALUE 'M'.                       00130000
001400         88  CA-SELECT           VALUE 'S'.                       00140000
001500         88  CA-UNDELETE         VALUE 'U'.                       00150000
001600     02  CA-SECURITY-DATA.                                        00160000
001700         03  CA-USER-LEVEL       PICTURE  X(01).                  00170000
001800             88  CA-INQUIRY      VALUE 'I'.                       00180000
001900             88  CA-MAINTENANCE  VALUE 'M'.                       00190000
002000             88  CA-SUPERVISORY  VALUE 'S'.                       00200000
002100         03  CA-USER-ID          PICTURE  X(08).                  00210000
002200         03  CA-USER-NAME        PICTURE  X(30).                  00220000
002300     02  CA-SELECTED-KEYS.                                        00230000
002400         03  CA-SELECTED-CV-KEY.                                  00240000
002500             04  CA-SELECTED-DE-KEY.                              00250000
002600                 05  CA-SELECTED-RL-KEY.                          00260000
002700                     06  CA-SEL-RECORD-PREFIX                     00270000
002800                                 PICTURE  X(08).                  00280000
002900                 05  CA-SEL-ELEMENT-NBR                           00290000
002910                                 PICTURE S9(03)V9(02)    COMP-3.  00291000
002920                 05  CA-SEL-ELEMENT-NBR-X  REDEFINES              00292002
002930                         CA-SEL-ELEMENT-NBR                       00293002
002940                                 PICTURE XXX.                     00294002
003000             04  CA-SEL-CODE-VALUE                                00300000
003100                                 PICTURE  X(10).                  00310000
003110             04  CA-SEL-CODE-SEQ PICTURE  9(02).                          
003120             04  CA-SEL-CODE-SEQ-X  REDEFINES CA-SEL-CODE-SEQ             
003130                                 PICTURE XX.                              
003200     02  CA-SELECTED-NAMES.                                       00320000
003300         03  CA-SEL-RECORD-NAME  PICTURE  X(50).                  00330000
003400         03  CA-SEL-ELEMENT-NAME PICTURE  X(75).                  00340004
003500         03  CA-SEL-CODE-NAME    PICTURE  X(50).                  00350000
003600     02  CA-RECORD-SCROLL-KEYS.                                   00360000
003700         03  CA-FIRST-RECORD     PICTURE  X(08).                  00370000
003800         03  CA-LAST-RECORD      PICTURE  X(08).                  00380000
003900     02  CA-ELEMENT-SCROLL-KEYS.                                  00390002
004000         03  CA-FIRST-ELEMENT.                                    00400000
004100             04  CA-FE-RECORD-PREFIX                              00410000
004200                                 PICTURE  X(08).                  00420000
004300             04  CA-FE-ELEMENT-NBR                                00430000
004400                                 PICTURE S9(03)V9(02)    COMP-3.  00440000
004500             04  CA-FE-ELEMENT-NBR-X  REDEFINES                   00440102
004600                                  CA-FE-ELEMENT-NBR               00440202
004601                                 PICTURE XXX.                     00440302
004602         03  CA-LAST-ELEMENT.                                     00441000
004603             04  CA-LE-RECORD-PREFIX                              00450000
004604                                 PICTURE  X(08).                  00460000
004700             04  CA-LE-ELEMENT-NBR                                00470000
004800                                 PICTURE S9(03)V9(02)    COMP-3.  00480000
004900             04  CA-LE-ELEMENT-NBR-X  REDEFINES                   00481002
005000                                  CA-LE-ELEMENT-NBR               00482002
005001                                 PICTURE XXX.                     00483002
005002     02  CA-CODE-VALUE-SCROLL-KEYS.                               00490000
005003         03  CA-FIRST-CODE.                                       00500000
005100             04  CA-FC-RECORD-PREFIX                              00510000
005200                                 PICTURE  X(08).                  00520000
005300             04  CA-FC-ELEMENT-NBR                                00530000
005400                                 PICTURE S9(03)V9(02)    COMP-3.  00540000
005500             04  CA-FC-ELEMENT-NBR-X  REDEFINES                   00540102
005600                                  CA-FC-ELEMENT-NBR               00540202
005601                                 PICTURE XXX.                     00540302
005602             04  CA-FC-CODE-VALUE                                 00541000
005603                                 PICTURE  X(10).                  00542000
005604             04  CA-FC-CODE-SEQ  PICTURE  9(02).                          
005605             04  CA-FC-CODE-SEQ-X  REDEFINES  CA-FC-CODE-SEQ              
005606                                 PICTURE XX.                              
005607         03  CA-LAST-CODE.                                        00550000
005608             04  CA-LC-RECORD-PREFIX                              00560000
005700                                 PICTURE  X(08).                  00570000
005800             04  CA-LC-ELEMENT-NBR                                00580000
005900                                 PICTURE S9(03)V9(02)    COMP-3.  00590000
006000             04  CA-LC-ELEMENT-NBR-X  REDEFINES                   00591002
006100                                  CA-LC-ELEMENT-NBR               00592002
006101                                 PICTURE XXX.                     00593002
006102             04  CA-LC-CODE-VALUE                                 00600000
006103                                 PICTURE  X(10).                  00610000
006104             04  CA-LC-CODE-SEQ  PICTURE  9(02).                          
006105             04  CA-LC-CODE-SEQ-X  REDEFINES  CA-LC-CODE-SEQ              
006106                                 PICTURE XX.                              
006107     02  CA-MAPFROM-KEYS.                                         00230000
006108         03  CA-MAPFROM-CV-KEY.                                   00240000
006109             04  CA-MAPFROM-DE-KEY.                               00250000
006110                 05  CA-MAPFROM-RL-KEY.                           00260000
006111                     06  CA-MF-RECORD-PREFIX                      00270000
006112                                 PICTURE  X(08).                  00280000
006113                 05  CA-MF-ELEMENT-NBR                            00290000
006114                                 PICTURE S9(03)V9(02)    COMP-3.  00291000
006115                 05  CA-MF-ELEMENT-NBR-X   REDEFINES              00292002
006116                         CA-MF-ELEMENT-NBR                        00293002
006117                                 PICTURE XXX.                     00294002
006118             04  CA-MF-CODE-VALUE                                 00300000
006119                                 PICTURE  X(10).                  00310000
006120             04  CA-MF-CODE-SEQ PICTURE   9(02).                          
006121             04  CA-MF-CODE-SEQ-X   REDEFINES CA-MF-CODE-SEQ              
006122                                 PICTURE XX.                              
006123     02  CA-MAPFROM-ELEMENT-NAME PICTURE  X(75).                          
006124     02  CA-TRANS-HDR            PICTURE  X(36).                          
006125     02  FILLER                  PICTURE  X(39).                          
