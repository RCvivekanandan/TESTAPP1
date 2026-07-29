000100*01 RC-WORK-RECORD. (ALRCWRK2 COPYBOOK)                                   
000110    05 RC-WORK-AREA.                                                      
000200    10  RC-FILLA                             PIC  X(04).                  
000300    10  RC-GROUP                             PIC  X(09).                  
000400    10  RC-FILLB                             PIC  X(01).                  
000500    10  RC-SECTION                           PIC  X(04).                  
000600    10  RC-FILLC                             PIC  X(01).                  
000700    10  RC-MEMB-ID                           PIC  X(16).                  
000800    10  RC-FILLD                             PIC  X(01).                  
000900    10  RC-SSN                               PIC  X(09).                  
001000    10  RC-FILLE                             PIC  X(01).                  
001100    10  RC-PATIENT-SUB                       PIC  X(02).                  
001200    10  RC-FILLF                             PIC  X(01).                  
001300    10  RC-LAST-NAME                         PIC  X(10).                  
001400    10  RC-FILLG                             PIC  X(01).                  
001500    10  RC-FIRST-NAME                        PIC  X(10).                  
001600    10  RC-FILLH                             PIC  X(01).                  
001700    10  RC-CLAIMANT-BIRTH-DATE.                                           
001800        15 RC-CLM-BIRTH-MM                   PIC  9(02).                  
001900        15 RC-CLM-BIRTH-DD                   PIC  9(02).                  
002000        15 RC-CLM-BIRTH-CCYY                 PIC  9(04).                  
002200    10  RC-CLAIMANT-GENDER                   PIC  X(01).                  
002400    10  RC-REL-CODE                          PIC  X(01).                  
002500    10  RC-FILLM                             PIC  X(01).                  
002600    10  RC-BP                                PIC  X(02).                  
002600    10  RC-BEG-DATE                          PIC  9(08).                  
002600    10  RC-END-DATE                          PIC  9(08).                  
002700    10  RC-RELATIONSHIP-IND                  PIC  X(02).                  
002700    10  RC-FILLN                             PIC  X(04).                  
002800    10  RC-ACCUM-TYPE                        PIC  X(02).                  
002900    10  RC-LOB                               PIC  X(01).                  
003000    10  RC-INTL-DESCRIPTOR                   PIC  X(09).                  
003100    10  RC-TREATMENT-GROUP                   PIC  X(02).                  
003200    10  RC-PERCENTAGE                        PIC  X(04).                  
003400    10  RC-ACMS-VALUE-LIMIT                  PIC  9(07)V99.               
003500    10  RC-COST-CONTAIN-IND                  PIC  X(02).                  
003900    10  RC-ACMS-FAM-OR-INDIV                 PIC  X(01).                  
004000    10  RC-ACMS-VALUE-QUAL                   PIC  X(01).                  
003300    10  RC-FILLR                             PIC  X(01).                  
003600    10  RC-ACMS-ACCUM-AMT                    PIC S9(07)V99.               
003700    10  RC-ACMS-MANUALLY-ENTERED             PIC S9(07)V99.               
003800    10  RC-ACMS-FEAK-AMT                     PIC S9(07)V99.               
004100    10  RC-ACMS-VENDOR-AMT                   PIC S9(07)V99.               
003300    10  RC-FILLM                             PIC  X(01).                  
004200    10  RC-WORK-GROUP                        PIC X(09).                   
004300    10  RC-WORK-SUBSCRIBER                   PIC X(12).                   
004400    10  RC-WORK-SSN                          PIC X(12).                   
004500    10  RC-WORK-SECTION                      PIC X(04).                   
004600    10  RC-WORK-DATE-OF-BIRTH                PIC 9(08).                   
004700    10  RC-WORK-FIRST-NAME                   PIC X(09).                   
004800    10  RC-WORK-LAST-NAME                    PIC X(15).                   
004900    10  RC-WORK-GENDER                       PIC X(01).                   
005000    10  RC-WORK-FILE-STATUS                  PIC X(02).                   
005100    10  RC-WORK-PAID-TO-DATE                 PIC 9(08).                   
005200    10  RC-WORK-STATE                        PIC X(02).                   
005300    10  RC-WORK-ADDRESS                      PIC X(25).                   
005400    10  RC-WORK-ADDRESS-2.                                                
005400        15  RC-FILLA                         PIC X(20).                   
005400        15  RC-IMC-CODE                      PIC X(03).                   
005400            88  IMC-IS-INDIV                 VALUES  '100'                
005400                                                     '200'                
005400                                                     '300'                
005400                                                     '400'.               
005400        15  RC-FILLB                         PIC X(02).                   
005500    10  RC-WORK-CITY                         PIC X(18).                   
005600    10  RC-WORK-ZIP                          PIC X(09).                   
005700    10  RC-WORK-CANCEL-CODE                  PIC X(02).                   
005800    10  RC-WORK-CANCEL-DATE                  PIC 9(08).                   
005900    10  RC-FILL1                             PIC X(02).                   
006000    10  RC-WORK-NO-OF-DEPS                   PIC 9(02).                   
006100    10  RC-WORK-DEPENDENTS    OCCURS  15  TIMES.                          
006200        15 RC-WDEP-FIRST-NAME                PIC X(09).                   
006300        15 RC-WDEP-BIRTH-DATE                PIC 9(08).                   
006400        15 RC-WDEP-SSN                       PIC X(12).                   
006500        15 RC-WDEP-REL-CODE                  PIC X(01).                   
006600        15 RC-WDEP-SEX-CODE                  PIC X(01).                   
006700        15 RC-WDEP-APPROVAL                  PIC X(01).                   
006800    10  RC-PSEUDO-GROUP                      PIC X(09).                   
006800    10  RC-FILL2                             PIC X(13).                   
000100*01 RC-WORK-RECORD. END                                                   
