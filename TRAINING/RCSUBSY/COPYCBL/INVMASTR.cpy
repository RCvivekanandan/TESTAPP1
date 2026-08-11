000200***************************************************************   00000200
000300*    C O P Y L I B   M E M B E R                              *   00000300
000400*    INVMASTR  -  RCSUBSYTORY MASTER FILE RECORD              *   00000400
000600*    CHANGED MEMBER NAME FROM RDMC4465 TO INVMASTR            *   00000600
016900***************************************************************   00016900
017000  03 WS100-RCSUBSYTORY-FILE.                                      00017000
017100     05  WS100-RCN.                                               00017100
017200         10  WS100-CLAIM-NBR     PIC X(17).                       00017200
017300         10  WS100-CLAIM-INC     PIC X(01).                       00017300
017400         10  WS100-ADJ-SUF       PIC X(02).                       00017400
017500     05  WS100-GROUP-NO          PIC X(09)       JUSTIFIED RIGHT. 00017500
035400     05  WS100-UPP-PROG-IND                  PIC X(1).            00035400
035500     05  WS100-ADM-DTCC                    PIC S9(07) COMP-3.     00035500
035600     05  WS100-PROMPT-PAY-IND                PIC X(1).            00035600
035600     05  WS100-PROV-DESGN-CD                 PIC X(5).            00035610
035600     05  WS100-IPAR-PLAN-DEFER-RSN           PIC X(3).            00035620
035700     05  FILLER                              PIC X(88).           00035700
