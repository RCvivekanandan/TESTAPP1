      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELSMSGRC                                        *00030000
      *    DATE:       22-JUN-1988                                     *00040000
      *    AUTHOR:     RICK E. BARILEAU                                *00050000
      *    FUNCTION:   SPECIAL MESSAGE RECORD LAYOUT                   *00060000
      *                                                                *00070000
      *                THIS COPY MEMBER WILL LOAD A SPECIAL            *00080000
      *                MESSAGE AND IDENTIFY IT WITH A UNIQUE KEY.      *00090000
      *                IT WILL ALSO KEEP TRACK OF HOW MANY TIMES       *00100000
      *                A PARTICULAR MESSAGE IS REFERRED TO.            *00110000
      *                                                                *00120000
      ******************************************************************00130000
      *                                                                *00140000
      *                      MAINTENANCE HISTORY                       *00150000
      *                                                                *00160000
      *  MOD     DATE     BY  DRPT                ACTION               *00170000
      * ----- ----------- --- ----- ---------------------------------- *00180000
      * 01.00 22-JUN-1988 REB       CREATED                            *00190000
      * 01.01 05-OCT-1988 LET       REFORMATTED AND ADDED AN INDICATOR *00200000
      *                             THAT INFORMS IF THE MESSAGE WAS    *00210000
      *                             PREVIOUSLY RETRIEVED.              *00220000
      *                                                                *00230000
      * 01.02 11-OCT-1988 LET       ADDED A LEVEL IN FRONT OF TEXT AREA*00240000
      *                                                                *00230000
      * 01.03 25-OCT-1988 LET       ADDED A 10 BYTE FILLER FOR FUTURE. *00240000
      ******************************************************************00250000
       01  SPECIAL-MESSAGE-RECORD.                                      00260000
           05  MESSAGE-KEY.                                             00270000
               10  MESSAGE-TYPE                PIC  X(01).              00280000
                   88  MESSAGE-COMMON                     VALUE 'C'.    00290000
                   88  MESSAGE-UNIQUE                     VALUE 'X'.    00300000
               10  MESSAGE-NUMBER              PIC  X(04).              00310000
           05  MESSAGE-RETRIEVED-IND           PIC  X(01).              00320000
               88  MSG-RETRIEVED                          VALUE 'R'.    00330000
               88  MSG-NOT-RETRIEVED                      VALUE ' '.    00340000
           05  FILLER                          PIC  X(10) VALUE SPACES.         
           05  MESSAGE-REFERENCE-COUNT         PIC S9(04)   COMP.       00350000
           05  MESSAGE-LINE-COUNT              PIC S9(04)   COMP.       00360000
               88  MAXIMUM-MESSAGE-LINES                    VALUE +015. 00370000
           05  MESSAGE-TEXT-AREA.                                       00380000
               10  MESSAGE-LINE-ENTRY          PIC  X(72)               00390000
                                               OCCURS 1 TO 15 TIMES     00400000
                                               DEPENDING ON             00410000
                                               MESSAGE-LINE-COUNT       00420000
                                               INDEXED BY MSG-LINE-IDX. 00430000
