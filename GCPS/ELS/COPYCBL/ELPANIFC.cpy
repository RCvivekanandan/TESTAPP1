000100******************************************************************        
000200*                                                                *        
000300*    COPYBOOK:   ELSPANIFC                                       *        
000400*    DATE:       28-JAN-1988                                     *        
000500*    AUTHOR:     RICHARD J. LUKETICH                             *        
000600*    FUNCTION:   CONTAINS THE PARAMETER AND RECORD BLOCKS FOR    *        
000700*                USING THE PANVALET ACCESS METHOD.               *        
000800*                                                                *        
000900******************************************************************        
001000*                                                                *        
001100*                      MAINTENANCE HISTORY                       *        
001200*                                                                *        
001300*  MOD     DATE     BY  DRPT                ACTION               *        
001400* ----- ----------- --- ----- ---------------------------------- *        
001500* 01.00 28-JAN-1988 RJL       CREATED                            *        
001510*                                                                *        
001600******************************************************************        
001700                                                                          
001900 01  PAM-ACTION                  PICTURE S9(8)   COMP.                    
001901 01  PAM-BACKUP                  PICTURE  X(08).                          
001910 01  PAM-COMMENT                 PICTURE  X(52).                          
001920 01  PAM-DDNAME                  PICTURE  X(08).                          
002000 01  PAM-INCLUDES                PICTURE  X(08).                          
002300 01  PAM-NAME                    PICTURE  X(22).                          
002310 01  PAM-NAME1                   PICTURE  X(22).                          
002320 01  PAM-NAME2                   PICTURE  X(11).                          
002400 01  PAM-RECORD                  PICTURE  X(80).                          
002600 01  PAM-SUBSET                  PICTURE  X(27).                          
003110                                                                          
003200 01  PAM-DIRECTORY-RECORD.                                                
003300     02 PAM-DIR-MEMBER           PICTURE  X(10).                          
003400     02 PAM-DIR-LEVEL            PICTURE  9(03).                          
003500     02 PAM-DIR-USER             PICTURE  9(04).                          
003600     02 PAM-DIR-SECURITY         PICTURE  9(01).                          
003610     02 PAM-DIR-LANGUAGE         PICTURE  X(05).                          
003611        88 PAM-DIR-LAN-ANSCOBOL  VALUE 'ANSCB'.                           
003614        88 PAM-DIR-LAN-ASSEMBLER VALUE 'ASMB '.                           
003615        88 PAM-DIR-LAN-AUTOCODER VALUE 'AUTOC'.                           
003616        88 PAM-DIR-LAN-COBOL     VALUE 'COBOL'.                           
003618        88 PAM-DIR-LAN-COBOL-72  VALUE 'COB72'.                           
003619        88 PAM-DIR-LAN-DATA      VALUE 'DATA '.                           
003620        88 PAM-DIR-LAN-FORTRAN   VALUE 'FORT '.                           
003621        88 PAM-DIR-LAN-JCL       VALUE 'JCL  '.                           
003622        88 PAM-DIR-LAN-OBJECT    VALUE 'OBJCT'.                           
003623        88 PAM-DIR-LAN-OTHER     VALUE 'OTHER'.                           
003624        88 PAM-DIR-LAN-PL1       VALUE 'PL/1 '.                           
003625        88 PAM-DIR-LAN-RPG       VALUE 'RPG  '.                           
003626        88 PAM-DIR-LAN-UNSPEC    VALUE 'UNSP '.                           
003627        88 PAM-DIR-LAN-USER-180  VALUE 'USER1'.                           
003628        88 PAM-DIR-LAN-USER-780  VALUE 'USER2'.                           
003700     02 PAM-DIR-STATUS.                                                   
003800        03 PAM-DIR-STATUS-1      PICTURE  X(01).                          
003801           88 PAM-DIR-PRODUCTION VALUE 'P'.                               
003802           88 PAM-DIR-TEST       VALUE 'T'.                               
003810        03 PAM-DIR-STATUS-2      PICTURE  X(01).                          
003811           88 PAM-DIR-ACTIVE     VALUE 'A'.                               
003812           88 PAM-DIR-INACTIVE   VALUE 'I'.                               
003820        03 PAM-DIR-STATUS-3      PICTURE  X(01).                          
003830           88 PAM-DIR-ENABLE     VALUE 'E'.                               
003840           88 PAM-DIR-DISABLE    VALUE 'D'.                               
003900     02 PAM-DIR-LAST-MAINT-DT    PICTURE  X(08).                          
004000     02 PAM-DIR-LAST-ACCESS-DT   PICTURE  X(08).                          
004100     02 PAM-DIR-NBR-BLOCKS-USED  PICTURE  9(05).                          
004200     02 PAM-DIR-NBR-STATEMENTS   PICTURE  9(08).                          
004300     02 PAM-DIR-PROD-STAT-CHG    PICTURE  X(01).                          
004310        88 PAM-DIR-PROD-STAT-ACT VALUE '*'.                               
004400     02 PAM-DIR-LAST-ACTION      PICTURE  X(03).                          
004500     02 PAM-DIR-AVG-STMT-SIZE    PICTURE  9(02).                          
004600     02 PAM-DIR-NBR-SUBSETS      PICTURE  9(04).                          
004700     02 PAM-DIR-MEMBER-RT-JUST   PICTURE  X(10).                          
004800     02 FILLER                   PICTURE  X(01).                          
004900     02 PAM-DIR-FORMAT-OPTION    PICTURE  X(01).                          
004920        88 PAM-DIR-NO-FORMAT     VALUE 'N'.                               
004930        88 PAM-DIR-TSO-FORMAT    VALUE 'T'.                               
005000     02 FILLER                   PICTURE  X(03).                          
