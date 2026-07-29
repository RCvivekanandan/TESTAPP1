000100******************************************************************00010002
000200*                                                                *00020002
000300*    COPYBOOK:   ELXLOBTC                                        *00030002
000400*    DATE:       21-JUL-1992                                     *00040002
000500*    AUTHOR:     BARBARA KEIB                                     00050002
000600*    FUNCTION:   LINE OF BUSINESS TABLE FOR GROUP SPECIFIC       *00060002
      *                GCG-L-O-B-CONTRACT-LEVEL-IND.                   *00070002
000800******************************************************************00080002
000900*                                                                *00090002
001000*                      MAINTENANCE HISTORY                       *00100002
001100*                                                                *00110002
001200*  MOD     DATE     BY  DRPT                ACTION               *00120002
001300* ----- ----------- --- ----- ---------------------------------- *00130002
001400* 01.00 21-JUL-1992 BAK       CREATED                            *00140002
001400* 02.00 21-MAY-1993 BAK       ADD MAJOR MEDICAL SUPPORT 3RD ENTRY*00140003
002920*                                                                *00292002
003000******************************************************************00300002
003200 01  ELS-LOB-LOOKUP-TABLE.                                        00300003
001100                                                                  00300004
001000    02  LOB-VALUES.                                               00301300
001100                                                                  00301400
001300      03  FILLER.                                                 00302400
001300          04  FILLER              PIC X(02)  VALUE '00'.          00302410
001300          04  FILLER              PIC X(01)  VALUE 'R'.           00302420
001300          04  FILLER              PIC X(01)  VALUE 'R'.           00302430
001300          04  FILLER              PIC X(01)  VALUE 'R'.           00302440
001100                                                                  00302500
001300      03  FILLER.                                                 00302600
001300          04  FILLER              PIC X(02)  VALUE '01'.          00302700
001300          04  FILLER              PIC X(01)  VALUE '1'.           00302800
001300          04  FILLER              PIC X(01)  VALUE 'R'.           00302900
001300          04  FILLER              PIC X(01)  VALUE 'R'.           00302910
001100                                                                  00303000
001300      03  FILLER.                                                 00303100
001300          04  FILLER              PIC X(02)  VALUE '02'.          00303200
001300          04  FILLER              PIC X(01)  VALUE '1'.           00303300
001300          04  FILLER              PIC X(01)  VALUE '2'.           00303400
001300          04  FILLER              PIC X(01)  VALUE 'R'.           00303410
001100                                                                  00303500
001300      03  FILLER.                                                 00303600
001300          04  FILLER              PIC X(02)  VALUE '03'.          00303700
001300          04  FILLER              PIC X(01)  VALUE '1'.           00303800
001300          04  FILLER              PIC X(01)  VALUE '2'.           00303900
001300          04  FILLER              PIC X(01)  VALUE '3'.           00303910
001100                                                                  00304000
001300      03  FILLER.                                                 00304100
001300          04  FILLER              PIC X(02)  VALUE '04'.          00304200
001300          04  FILLER              PIC X(01)  VALUE '1'.           00304300
001300          04  FILLER              PIC X(01)  VALUE 'R'.           00304400
001300          04  FILLER              PIC X(01)  VALUE '3'.           00304410
001100                                                                  00304500
001300      03  FILLER.                                                 00304600
001300          04  FILLER              PIC X(02)  VALUE '05'.          00304700
001300          04  FILLER              PIC X(01)  VALUE 'R'.           00304800
001300          04  FILLER              PIC X(01)  VALUE '2'.           00304900
001300          04  FILLER              PIC X(01)  VALUE 'R'.           00304910
001100                                                                  00305000
001300      03  FILLER.                                                 00305100
001300          04  FILLER              PIC X(02)  VALUE '06'.          00305200
001300          04  FILLER              PIC X(01)  VALUE 'R'.           00305300
001300          04  FILLER              PIC X(01)  VALUE '2'.           00305400
001300          04  FILLER              PIC X(01)  VALUE '3'.           00305410
001100                                                                  00305500
001300      03  FILLER.                                                 00305600
001300          04  FILLER              PIC X(02)  VALUE '07'.          00305700
001300          04  FILLER              PIC X(01)  VALUE '4'.           00305800
001300          04  FILLER              PIC X(01)  VALUE '4'.           00305900
001300          04  FILLER              PIC X(01)  VALUE 'R'.           00305910
001100                                                                  00306000
001300      03  FILLER.                                                 00306100
001300          04  FILLER              PIC X(02)  VALUE '08'.          00306200
001300          04  FILLER              PIC X(01)  VALUE 'R'.           00306300
001300          04  FILLER              PIC X(01)  VALUE 'R'.           00306400
001300          04  FILLER              PIC X(01)  VALUE '3'.           00306410
001100                                                                  00306500
058900    02    LOB-TABLE               REDEFINES  LOB-VALUES.          00307200
001300      03  LOB-TBL                 OCCURS 9 TIMES                  00307210
059200                                  INDEXED BY LOB-INDX             00307500
059200                                             LOB-MAX-INDX.        00307501
001300          04  LOB-KEY             PIC X(02).                      00307502
001300          04  LOB-INST            PIC X(01).                      00307503
001300          04  LOB-PROF            PIC X(01).                      00307504
001300          04  LOB-MAJ-MED         PIC X(01).                      00307505
001100                                                                  00307506
058900    02    LOB-COUNTS.                                             00307507
   300      03  LOB-ENTRY-CNT      PIC S9(04) COMP  VALUE 9.            00307508
 03100                                                                  00310002
