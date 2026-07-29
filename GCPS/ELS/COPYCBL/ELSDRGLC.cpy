000100******************************************************************        
000200*    COPYBOOK:   ELSDRGLC                                        *        
000300*    AUTHOR:     CHARLES LAPEDUS.                                *        
000400*    DATE:       01-MAR-1993                                     *        
000500*    FUNCTION:   LIST OF DRUG ABUSE DIAGNOSIS CODES. THIS LIST   *        
000600*                IS BEING USED BY THE NA/ES SYSTEM TO HELP       *        
000700*                IN DETERMINING WHETHER OUTPATIENT SERVICES      *        
000800*                ARE COVERED.  AND IN DETERMINING HOW            *        
000900*                HOW 'CONFIDENT' WE ARE THAT DRUG ABUSE IS       *        
001000*                COVERED FOR A GIVEN PATIENT.                    *        
001100*                                                                *        
001200******************************************************************        
001300*                      MAINTENANCE HISTORY                       *        
001400*                                                                *        
001500*  MOD     DATE     BY  DRPT                ACTION               *        
001600* ----- ----------- --- ----- ---------------------------------- *        
001700* 1.00  01-MAR-1993 CGL       CREATED                            *        
001800******************************************************************        
001900 01  DRGL-DIAGNOSES-TABLE.                                                
002000     05 DRGL-DIAGNOSES-COUNT     PIC S9(04) COMP VALUE +39.               
002100     05 DRGL-DIAGNOSES-LIST.                                              
002200        10 FILLER                PIC  X(06)      VALUE 'V113 '.           
002300        10 FILLER                PIC  X(06)      VALUE 'V791 '.           
002400        10 FILLER                PIC  X(06)      VALUE '291  '.           
002500        10 FILLER                PIC  X(06)      VALUE '2910 '.           
002600        10 FILLER                PIC  X(06)      VALUE '2911 '.           
002700        10 FILLER                PIC  X(06)      VALUE '2912 '.           
002800        10 FILLER                PIC  X(06)      VALUE '2913 '.           
002900        10 FILLER                PIC  X(06)      VALUE '2914 '.           
003000        10 FILLER                PIC  X(06)      VALUE '2915 '.           
003100        10 FILLER                PIC  X(06)      VALUE '2918 '.           
003200        10 FILLER                PIC  X(06)      VALUE '2919 '.           
003300        10 FILLER                PIC  X(06)      VALUE '303  '.           
003400        10 FILLER                PIC  X(06)      VALUE '3030 '.           
003500        10 FILLER                PIC  X(06)      VALUE '30300'.           
003600        10 FILLER                PIC  X(06)      VALUE '30301'.           
003700        10 FILLER                PIC  X(06)      VALUE '30302'.           
003800        10 FILLER                PIC  X(06)      VALUE '30303'.           
003900        10 FILLER                PIC  X(06)      VALUE '3039 '.           
004000        10 FILLER                PIC  X(06)      VALUE '30390'.           
004100        10 FILLER                PIC  X(06)      VALUE '30391'.           
004200        10 FILLER                PIC  X(06)      VALUE '30392'.           
004300        10 FILLER                PIC  X(06)      VALUE '30393'.           
004400        10 FILLER                PIC  X(06)      VALUE '3050 '.           
004500        10 FILLER                PIC  X(06)      VALUE '30500'.           
004600        10 FILLER                PIC  X(06)      VALUE '30501'.           
004700        10 FILLER                PIC  X(06)      VALUE '30502'.           
004800        10 FILLER                PIC  X(06)      VALUE '30503'.           
004900        10 FILLER                PIC  X(06)      VALUE '3575 '.           
005000        10 FILLER                PIC  X(06)      VALUE '4255 '.           
005100        10 FILLER                PIC  X(06)      VALUE '5353 '.           
005200        10 FILLER                PIC  X(06)      VALUE '53530'.           
005300        10 FILLER                PIC  X(06)      VALUE '53531'.           
005400        10 FILLER                PIC  X(06)      VALUE '571  '.           
005500        10 FILLER                PIC  X(06)      VALUE '5710 '.           
005600        10 FILLER                PIC  X(06)      VALUE '5711 '.           
005700        10 FILLER                PIC  X(06)      VALUE '5712 '.           
005800        10 FILLER                PIC  X(06)      VALUE '5713 '.           
005900        10 FILLER                PIC  X(06)      VALUE '57141'.           
006000        10 FILLER                PIC  X(06)      VALUE '7903 '.           
