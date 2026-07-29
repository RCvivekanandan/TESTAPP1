      ***********************************************************       00001000
      *                 DATA ELEMENT RECORD                     *       00002000
      *              ENGLISH LANGUAGE PROTOTYPE                 *       00003000
      *                                                         *       00004000
      *                  MAX LENGTH=1043                        *       00005000
      ***********************************************************       00006000
            05  DATA-ELEMENT.                                           00010001
               10  DE-PRIMARY-KEY.                                      00020000
                   15  DE-RECORD-PREFIX    PIC X(08).                   00030000
                   15  DE-ELEMENT-NBR      PIC S9(03)V9(02)   COMP-3.   00040000
               10  DE-SECONDARY-NAME-KEY.                               00050000
                   15  DE-RECORD-PREFIX-N  PIC X(08).                   00060000
                   15  DE-ELEMENT-NAME     PIC X(75).                   00070000
               10  DE-ELEMENT-FORMAT       PIC X(02).                   00080000
                   88  DE-ALPHA            VALUE 'A '.                  00090000
                   88  DE-ALPHANUM         VALUE 'AN'.                  00100000
                   88  DE-DATE             VALUE 'DT'.                  00110000
                   88  DE-NUM              VALUE 'N '.                  00120000
               10  DE-ELEMENT-LENGTH       PIC S9(03)         COMP-3.   00130000
               10  DE-FORMAT-COMMENT       PIC X(21).                   00140000
               10  DE-DELETE-ELEMENT-FLAG  PIC X(01).                   00150000
                   88  DE-DELETE           VALUE 'D'.                   00160000
               10  DE-AUTO-REPRINT-FLAG    PIC X(01).                   00170000
                   88  DE-REPRINT          VALUE 'Y'.                   00180000
                   88  DE-ENG-NAME-CHG     VALUE 'E'.                   00181002
               10  DE-RECORD-POS           PIC S9(05)         COMP-3.   00190000
               10  DE-STORED-LENGTH        PIC S9(03)         COMP-3.   00200000
               10  DE-STORED-DECIMALS      PIC S9(03)         COMP-3.   00210000
               10  DE-STORED-FORMAT        PIC X(01).                   00220000
                   88  DE-BIN              VALUE 'B'.                   00230000
                   88  DE-CHAR             VALUE 'C'.                   00240000
                   88  DE-DISPLAY          VALUE 'D'.                   00250000
                   88  DE-PACKED           VALUE 'P'.                   00260000
                   88  DE-HEX              VALUE 'X'.                   00270000
               10  DE-CODES-FLAG           PIC X(01).                   00280000
                   88  DE-HAS-CODE-VALUES  VALUE 'Y'.                   00290000
               10  DE-CODE-VALUES-CT       PIC S9(04)         COMP.     00300000
               10  DE-COBOL-NAME           PIC X(30).                   00310000
               10  DE-BAL-NAME             PIC X(08).                   00320000
               10  DE-NBR-DESC-LINES       PIC S9(03)         COMP-3.   00330000
               10  DE-DESC-LINE            PIC X(79)                    00340000
                                       OCCURS 1 TO 11 TIMES             00350000
                                       DEPENDING ON DE-NBR-DESC-LINES.  00360000
