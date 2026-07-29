      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELAFPCNC                                        *00030000
      *    DATE:       09-JAN-1990                                     *00040000
      *    AUTHOR:     EDWARD G LISS                                   *00050000
      *    FUNCTION:   DEFINES THE CONSTANTS TO BE USED IN CALLING     *00060000
      *                AFP INTERFACE PROGRAM(S).                       *00070000
      *                                                                *00080000
      *                                                                *00090000
      ******************************************************************00100000
      *                                                                *00110000
      *                      MAINTENANCE HISTORY                       *00120000
      *                                                                *00130000
      *  MOD     DATE     BY  DRPT                ACTION               *00140000
      * ----- ----------- --- ----- ---------------------------------- *00150000
      * 01.00 09-JAN-1990 EGL       CREATED                            *00160000
      *                                                                *00170000
      ******************************************************************00180000
                                                                        00190000
       01  AFP-PROGRAM-CONSTANTS.                                       00200000
           02  AFPC-MACROS-ABS.                                         00210001
               04  AFPC-DRAW-BOX-ABS               PIC X(3) VALUE '*BX'.00220001
               04  AFPC-DRAW-HOR-LINE-ABS          PIC X(3) VALUE '*HL'.00230001
               04  AFPC-DRAW-VER-LINE-ABS          PIC X(3) VALUE '*VL'.00240001
