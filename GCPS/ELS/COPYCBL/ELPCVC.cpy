           05  ELEMENT-CODE-VALUE.                                      00010000
               10  CV-CODE-KEY.                                         00020000
                   15  CV-RECORD-PREFIX    PIC X(08).                   00030000
                   15  CV-ELEMENT-NBR      PIC S9(03)V9(02)    COMP-3.  00040000
                   15  CV-CODE-VALUE       PIC X(10).                   00050000
                   15  CV-CODE-DESC-SEQ    PIC 9(02).                           
               10  CV-CODE-NAME            PIC X(50).                   00060000
               10  CV-DELETE-CODE-FLAG     PIC X(01).                   00070000
                   88  CV-DELETE           VALUE 'D'.                   00080000
               10  CV-NBR-VALUE-DESC-LINES PIC S9(03)          COMP-3.  00090000
               10  CV-VALUE-DESC-LINE      PIC X(79)                    00100000
                                           OCCURS 1 TO 12 TIMES         00110000
                                           DEPENDING ON                 00120000
                                           CV-NBR-VALUE-DESC-LINES.     00130000
