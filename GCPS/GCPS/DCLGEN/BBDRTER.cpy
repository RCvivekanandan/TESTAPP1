      ******************************************************************00000100
      * DCLGEN TABLE(BNFT_DTRM_RTER_CSMR)                              *00000200
      *        LIBRARY(I303850.TEST.PROCLIB(BBDRTR)) NEW COMPONENT 03/2100000300
      *        ACTION(REPLACE)                                         *00000400
      *        LANGUAGE(COBOL)                                         *00000500
      *        APOST                                                   *00000600
      * ... IS THE2DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *00000700
      ******************************************************************00000800
           EXEC SQL DECLARE BNFT_DTRM_RTER_CSMR TABLE                   00000900
           ( BSTAR_BNFT_AGRMT_NBR           CHAR(10) NOT NULL,          00001000
             BSTAR_ACCT_NBR                 CHAR(10) NOT NULL,          00001100
             CORP_ENT_CD                    CHAR(3) NOT NULL,           00001200
             IPAR_XMITSN_RUL_CD             CHAR(1) NOT NULL,           00001300
             GRP_NBR                        CHAR(10) NOT NULL,          00001400
             SECT_NBR                       CHAR(10) NOT NULL,          00001500
             EFF_DT                         DATE NOT NULL,              00001600
             END_DT                         DATE NOT NULL,              00001700
             MBR_SRC_SYS_NM                 VARCHAR(20) NOT NULL,       00001800
             APPLCTN_SYS_ID                 INTEGER NOT NULL            00001900
           ) END-EXEC.                                                  00002000
      ******************************************************************00002100
      * COBOL DECLARATION FOR TABLE TBDDB07.BNFT_DTRM_RTER_CSMR        *00002200
      ******************************************************************00002300
       01  DCLBNFT-DTRM-RTER-CSMR.                                      00002400
           10 BSTAR-BNFT-AGRMT-NBR                                      00002500
              PIC X(10).                                                00002600
           10 BSTAR-ACCT-NBR       PIC X(10).                           00002700
           10 CORP-ENT-CD          PIC X(3).                            00002800
           10 IPAR-XMITSN-RUL-CD   PIC X(1).                            00002900
           10 GRP-NBR              PIC X(10).                           00003000
           10 SECT-NBR             PIC X(10).                           00003100
           10 EFF-DT               PIC X(10).                           00003200
           10 END-DT               PIC X(10).                           00003300
           10 MBR-SRC-SYS-NM.                                           00003400
              49 MBR-SRC-SYS-NM-LEN                                     00003500
                 PIC S9(4) USAGE COMP.                                  00003600
              49 MBR-SRC-SYS-NM-TEXT                                    00003700
                 PIC X(20).                                             00003800
           10 APPLCTN-SYS-ID       PIC S9(9) USAGE COMP.                00003900
      ******************************************************************00004000
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *00004100
      ******************************************************************00004200
