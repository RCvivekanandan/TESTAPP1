000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELCDTCPR                                        *00030000
000400*    DATE:       29-SEP-1986                                     *00040000
000500*    AUTHOR:     JERRY L. ARKEMA                                 *00050000
000600*                RICHARD J. LUKETICH                             *00060000
000700*    FUNCTION:   EXPAND AND COMPRESS TEXT FOR ENGLISH CONTRACT   *00070000
000800*                INQUIRY TOPIC PROGRAMS.                         *00080000
000900*                                                                *00090000
001000******************************************************************00100000
001100*                                                                *00110000
001200*                      MAINTENANCE HISTORY                       *00120000
001300*                                                                *00130000
001400*  MOD     DATE     BY  DRPT                ACTION               *00140000
001500* ----- ----------- --- ----- ---------------------------------- *00150000
001600* 01.00 29-SEP-1986 RJL       RECREATED                          *00160000
001700*                                                                *00170000
001800* 01.01 09-DEC-1986 JTC       CORRECTED FIRST PERFORM BY         *00180000
001900*                             CHANGING 'FROM TCAR-AREA-LENGTH'   *00190000
002000*                             TO 'FROM LENGTH OF                 *00200000
002100*                                 TCAR-FROM-AREA'                *00210000
002200* 01.02 29-DEC-1986 EGL       ADDED A DUMMY SECTION TO THE END   *00220000
002300*                             TO ALLOW COMPATABILITY WITH THE    *00230000
002400*                             CODE GENERATOR.  THIS CHANGE WILL  *00240000
002500*                             ELIMINATE THE NEED FOR THIS COPY   *00250000
002600*                             TO BE IN THE LAST SET OF A         *00260000
002700*                             GENERATED PROGRAM.                 *00270000
002800* 01.03 29-DEC-1986 EGL       MODIFY LOGIC TO USE                *00280000
002900*                             TCAR-AREA-LENGTH, BUT SET SAME IF  *00290000
003000*                             ZERO ON ENTRY.                     *00300000
003100* 02.00 23-FEB-1987 EGL       CHANGED TO INTERFACE WITH TEXT     *00310000
003200*                             PROCESSING SUBROUTINES.            *00320000
003300*                             PARAMETER LIST DID NOT CHANGE      *00330000
003400******************************************************************00340000
003500                                                                  00350000
003600 TCPR-000-TEXT-COMPRESSION.                                       00360000
003700                                                                  00370000
003800     CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            00380000
003900                                                                  00390000
004000 TCPR-000-TEXT-UNSTRING.                                          00400000
004100                                                                  00410000
004200     CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            00420000
004300                                                                  00430000
