000100******************************************************************        
000200*    COPYBOOK:   ELSALCLC                                        *        
000300*    AUTHOR:     CHARLES LAPEDUS.                                *        
000400*    DATE:       01-MAR-1993                                     *        
000500*    FUNCTION:   LIST OF ALCOHOL ABUSE DIAGNOSIS CODES. THIS     *        
000600*                IS BEING USED BY THE NA/ES SYSTEM TO HELP       *        
000700*                IN DETERMINING WHETHER OUTPATIENT SERVICES      *        
000800*                ARE COVERED.  AND IN DETERMINING HOW            *        
000900*                HOW 'CONFIDENT' WE ARE THAT ALCOHOL ABUSE IS    *        
001000*                COVERED FOR A GIVEN PATIENT.                    *        
001100*                                                                *        
001200******************************************************************        
001300*                      MAINTENANCE HISTORY                       *        
001400*                                                                *        
001500*  MOD     DATE     BY  DRPT                ACTION               *        
001600* ----- ----------- --- ----- ---------------------------------- *        
001700* 1.00  01-MAR-1993 CGL       CREATED                            *        
001800******************************************************************        
001900 01  ALCL-DIAGNOSES-TABLE.                                                
002000     05 ALCL-DIAGNOSES-COUNT     PIC S9(04) COMP VALUE +139.              
002100     05 ALCL-DIAGNOSES-LIST.                                              
002200        10 FILLER                PIC  X(06)      VALUE '292  '.           
002300        10 FILLER                PIC  X(06)      VALUE '2920 '.           
002400        10 FILLER                PIC  X(06)      VALUE '2921 '.           
002500        10 FILLER                PIC  X(06)      VALUE '29211'.           
002600        10 FILLER                PIC  X(06)      VALUE '29212'.           
002700        10 FILLER                PIC  X(06)      VALUE '2922 '.           
002800        10 FILLER                PIC  X(06)      VALUE '2928 '.           
002900        10 FILLER                PIC  X(06)      VALUE '29281'.           
003000        10 FILLER                PIC  X(06)      VALUE '29282'.           
003100        10 FILLER                PIC  X(06)      VALUE '29283'.           
003200        10 FILLER                PIC  X(06)      VALUE '29284'.           
003300        10 FILLER                PIC  X(06)      VALUE '29289'.           
003300        10 FILLER                PIC  X(06)      VALUE '2929 '.           
003400        10 FILLER                PIC  X(06)      VALUE '304  '.           
003500        10 FILLER                PIC  X(06)      VALUE '3040 '.           
003600        10 FILLER                PIC  X(06)      VALUE '30400'.           
003700        10 FILLER                PIC  X(06)      VALUE '30401'.           
003800        10 FILLER                PIC  X(06)      VALUE '30402'.           
003800        10 FILLER                PIC  X(06)      VALUE '30403'.           
003900        10 FILLER                PIC  X(06)      VALUE '3041 '.           
004000        10 FILLER                PIC  X(06)      VALUE '30410'.           
004100        10 FILLER                PIC  X(06)      VALUE '30411'.           
004200        10 FILLER                PIC  X(06)      VALUE '30412'.           
004300        10 FILLER                PIC  X(06)      VALUE '30413'.           
004400        10 FILLER                PIC  X(06)      VALUE '3042 '.           
004500        10 FILLER                PIC  X(06)      VALUE '30420'.           
004600        10 FILLER                PIC  X(06)      VALUE '30421'.           
004700        10 FILLER                PIC  X(06)      VALUE '30422'.           
004800        10 FILLER                PIC  X(06)      VALUE '30423'.           
004800        10 FILLER                PIC  X(06)      VALUE '3043 '.           
005000        10 FILLER                PIC  X(06)      VALUE '30430'.           
005100        10 FILLER                PIC  X(06)      VALUE '30431'.           
005200        10 FILLER                PIC  X(06)      VALUE '30432'.           
005300        10 FILLER                PIC  X(06)      VALUE '30433'.           
005400        10 FILLER                PIC  X(06)      VALUE '3044 '.           
005500        10 FILLER                PIC  X(06)      VALUE '30440'.           
005600        10 FILLER                PIC  X(06)      VALUE '30441'.           
005700        10 FILLER                PIC  X(06)      VALUE '30442'.           
005800        10 FILLER                PIC  X(06)      VALUE '30443'.           
              10 FILLER                PIC  X(06)      VALUE '3045 '.           
              10 FILLER                PIC  X(06)      VALUE '30450'.           
              10 FILLER                PIC  X(06)      VALUE '30451'.           
              10 FILLER                PIC  X(06)      VALUE '30452'.           
              10 FILLER                PIC  X(06)      VALUE '30453'.           
              10 FILLER                PIC  X(06)      VALUE '3046 '.           
              10 FILLER                PIC  X(06)      VALUE '30460'.           
              10 FILLER                PIC  X(06)      VALUE '30461'.           
              10 FILLER                PIC  X(06)      VALUE '30462'.           
              10 FILLER                PIC  X(06)      VALUE '30463'.           
003900        10 FILLER                PIC  X(06)      VALUE '3047 '.           
004000        10 FILLER                PIC  X(06)      VALUE '30470'.           
004100        10 FILLER                PIC  X(06)      VALUE '30471'.           
004200        10 FILLER                PIC  X(06)      VALUE '30472'.           
004300        10 FILLER                PIC  X(06)      VALUE '30473'.           
004400        10 FILLER                PIC  X(06)      VALUE '3048 '.           
004500        10 FILLER                PIC  X(06)      VALUE '30480'.           
004600        10 FILLER                PIC  X(06)      VALUE '30481'.           
004700        10 FILLER                PIC  X(06)      VALUE '30482'.           
004800        10 FILLER                PIC  X(06)      VALUE '30483'.           
004800        10 FILLER                PIC  X(06)      VALUE '3049 '.           
005000        10 FILLER                PIC  X(06)      VALUE '30490'.           
005100        10 FILLER                PIC  X(06)      VALUE '30491'.           
005200        10 FILLER                PIC  X(06)      VALUE '30492'.           
005300        10 FILLER                PIC  X(06)      VALUE '30493'.           
005400        10 FILLER                PIC  X(06)      VALUE '305  '.           
005500        10 FILLER                PIC  X(06)      VALUE '3051 '.           
005600        10 FILLER                PIC  X(06)      VALUE '30510'.           
005600        10 FILLER                PIC  X(06)      VALUE '30511'.           
005700        10 FILLER                PIC  X(06)      VALUE '30512'.           
005800        10 FILLER                PIC  X(06)      VALUE '30513'.           
              10 FILLER                PIC  X(06)      VALUE '3052 '.           
              10 FILLER                PIC  X(06)      VALUE '30520'.           
              10 FILLER                PIC  X(06)      VALUE '30521'.           
              10 FILLER                PIC  X(06)      VALUE '30522'.           
              10 FILLER                PIC  X(06)      VALUE '30523'.           
              10 FILLER                PIC  X(06)      VALUE '3053 '.           
              10 FILLER                PIC  X(06)      VALUE '30530'.           
              10 FILLER                PIC  X(06)      VALUE '30531'.           
              10 FILLER                PIC  X(06)      VALUE '30532'.           
              10 FILLER                PIC  X(06)      VALUE '30533'.           
              10 FILLER                PIC  X(06)      VALUE '3054 '.           
005000        10 FILLER                PIC  X(06)      VALUE '30540'.           
005100        10 FILLER                PIC  X(06)      VALUE '30541'.           
005200        10 FILLER                PIC  X(06)      VALUE '30542'.           
005300        10 FILLER                PIC  X(06)      VALUE '30543'.           
005400        10 FILLER                PIC  X(06)      VALUE '3055 '.           
005500        10 FILLER                PIC  X(06)      VALUE '30550'.           
005600        10 FILLER                PIC  X(06)      VALUE '30551'.           
005700        10 FILLER                PIC  X(06)      VALUE '30552'.           
005800        10 FILLER                PIC  X(06)      VALUE '30553'.           
              10 FILLER                PIC  X(06)      VALUE '3056 '.           
              10 FILLER                PIC  X(06)      VALUE '30560'.           
              10 FILLER                PIC  X(06)      VALUE '30561'.           
              10 FILLER                PIC  X(06)      VALUE '30562'.           
              10 FILLER                PIC  X(06)      VALUE '30563'.           
              10 FILLER                PIC  X(06)      VALUE '3057 '.           
              10 FILLER                PIC  X(06)      VALUE '30570'.           
              10 FILLER                PIC  X(06)      VALUE '30571'.           
              10 FILLER                PIC  X(06)      VALUE '30572'.           
              10 FILLER                PIC  X(06)      VALUE '30573'.           
              10 FILLER                PIC  X(06)      VALUE '3058 '.           
              10 FILLER                PIC  X(06)      VALUE '30580'.           
              10 FILLER                PIC  X(06)      VALUE '30581'.           
              10 FILLER                PIC  X(06)      VALUE '30582'.           
              10 FILLER                PIC  X(06)      VALUE '30583'.           
              10 FILLER                PIC  X(06)      VALUE '3059 '.           
              10 FILLER                PIC  X(06)      VALUE '30590'.           
              10 FILLER                PIC  X(06)      VALUE '30591'.           
              10 FILLER                PIC  X(06)      VALUE '30592'.           
              10 FILLER                PIC  X(06)      VALUE '30593'.           
              10 FILLER                PIC  X(06)      VALUE '3576 '.           
              10 FILLER                PIC  X(06)      VALUE '3577 '.           
              10 FILLER                PIC  X(06)      VALUE '6483 '.           
              10 FILLER                PIC  X(06)      VALUE '64830'.           
              10 FILLER                PIC  X(06)      VALUE '64831'.           
              10 FILLER                PIC  X(06)      VALUE '64832'.           
              10 FILLER                PIC  X(06)      VALUE '64833'.           
              10 FILLER                PIC  X(06)      VALUE '64834'.           
              10 FILLER                PIC  X(06)      VALUE '9650 '.           
              10 FILLER                PIC  X(06)      VALUE '96500'.           
              10 FILLER                PIC  X(06)      VALUE '96501'.           
              10 FILLER                PIC  X(06)      VALUE '96502'.           
              10 FILLER                PIC  X(06)      VALUE '96509'.           
              10 FILLER                PIC  X(06)      VALUE '967  '.           
              10 FILLER                PIC  X(06)      VALUE '9670 '.           
              10 FILLER                PIC  X(06)      VALUE '9671 '.           
              10 FILLER                PIC  X(06)      VALUE '9672 '.           
              10 FILLER                PIC  X(06)      VALUE '9673 '.           
              10 FILLER                PIC  X(06)      VALUE '9674 '.           
              10 FILLER                PIC  X(06)      VALUE '9675 '.           
              10 FILLER                PIC  X(06)      VALUE '9676 '.           
              10 FILLER                PIC  X(06)      VALUE '9678 '.           
              10 FILLER                PIC  X(06)      VALUE '9679 '.           
              10 FILLER                PIC  X(06)      VALUE '9693 '.           
              10 FILLER                PIC  X(06)      VALUE '9694 '.           
              10 FILLER                PIC  X(06)      VALUE '9695 '.           
              10 FILLER                PIC  X(06)      VALUE '9696 '.           
              10 FILLER                PIC  X(06)      VALUE '9697 '.           
              10 FILLER                PIC  X(06)      VALUE '9894 '.           
