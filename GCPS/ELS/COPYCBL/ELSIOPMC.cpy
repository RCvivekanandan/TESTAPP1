000100******************************************************************        
000200*                                                                *        
000300*    COPYBOOK:   ELSIOPMC                                        *        
000400*    DATE:       22-SEP-1986                                     *        
000500*    AUTHOR:     RICHARD J. LUKETICH                             *        
000600*    FUNCTION:   INPUT/OUTPUT PARAMETERS BLOCK.                  *        
000700*                                                                *        
000800*    NOTES:      USED TO COMMUNICATE INPUT/OUTPUT REQUESTS TO    *        
000900*                ELUIOPGM AND TO REQUEST STORAGE MANAGEMENT      *        
001000*                FUNCTIONS FROM ELUIOGMN AND ELUIOFMN.  SEE      *        
001100*                INDIVIDUAL MODULE DOCUMENTATION FOR DETAILS OF  *        
001200*                THE USE OF THE VARIOUS FIELDS.                  *        
001300*                                                                *        
001400******************************************************************        
001500*                                                                *        
001600*                      MAINTENANCE HISTORY                       *        
001700*                                                                *        
001800*  MOD     DATE     BY  DRPT                ACTION               *        
001900* ----- ----------- --- ----- ---------------------------------- *        
002000* 01.00 22-SEP-1986 RJL       CREATED                            *        
002100* 02.00 30-NOV-1993 RJL       REVISED FOR CONSISTENCY.           *        
002200*                                                                *        
002300******************************************************************        
002400                                                                          
002500 01  IOP-INPUT-OUTPUT-PARAMETERS.                                         
002600     02 IOP-FILE-DDNAME                   PIC  X(08).                     
002700     02 IOP-TSQ-ID                                                        
002800           REDEFINES IOP-FILE-DDNAME.                                     
002900        03 IOP-TSQ-TERMID                 PIC  X(04).                     
003000        03 IOP-TSQ-SFX                    PIC  X(04).                     
003100     02 IOP-FUNCTION                      PIC  X(02).                     
003200        88 IOP-ADD                        VALUE 'AD'.                     
003300        88 IOP-DEL                        VALUE 'DL'.                     
003400        88 IOP-END-BR                     VALUE 'EB'.                     
003500        88 IOP-RD                         VALUE 'RD'.                     
003600        88 IOP-RD-NXT                     VALUE 'RN'.                     
003700        88 IOP-RD-PRV                     VALUE 'RP'.                     
003800        88 IOP-ST-BR                      VALUE 'SB'.                     
003900        88 IOP-UNLK                       VALUE 'UL'.                     
004000        88 IOP-UPD                        VALUE 'UP'.                     
004100        88 IOP-FCN-INPUT                  VALUE 'RD', 'RN', 'RP'.         
004200        88 IOP-PURGE-TSQ                  VALUE 'PU'.                     
004300        88 IOP-PRINT-TSQ                  VALUE 'PQ'.                     
004400     02 IOP-FUNCTION-QUAL                 PIC  X(01).                     
004500        88 IOP-FCQ-GEN                    VALUE 'G'.                      
004600        88 IOP-FCQ-MAS                    VALUE 'M'.                      
004700        88 IOP-FCQ-NONE                   VALUE SPACE.                    
004800        88 IOP-FCQ-UPD                    VALUE 'U'.                      
004900     02 IOP-KEY-VAL-QUAL                  PIC  X(01).                     
005000        88 IOP-KVQ-EQ                     VALUE 'E'.                      
005100        88 IOP-KVQ-GTE                    VALUE 'G'.                      
005200        88 IOP-KVQ-NONE                   VALUE SPACE.                    
005300     02 IOP-STG-MODE                      PIC  X(01).                     
005400        88 IOP-STG-MODE-LOCATE            VALUE 'L'.                      
005500        88 IOP-STG-MODE-MOVE              VALUE 'M'.                      
005600        88 IOP-STG-MODE-REUSE             VALUE 'R'.                      
005700     02 IOP-STG-MGT-IND                   PIC  X(01).                     
005800        88 IOP-FREEMAIN-ALL               VALUE 'A'.                      
005900        88 IOP-FREEMAIN-BOTH-REC          VALUE 'B'.                      
006000        88 IOP-FREEMAIN-DUP-REC           VALUE 'D'.                      
006100        88 IOP-FREEMAIN-REC               VALUE 'R'.                      
006200        88 IOP-GETMAIN-DUP-REC            VALUE 'D'.                      
006300        88 IOP-GETMAIN-REC                VALUE 'R'.                      
006400     02 IOP-CICS-FCN                      PIC  X(02).                     
006600     02    REDEFINES IOP-CICS-FCN         PIC S9(04)       COMP.          
006700        88 IOP-CICS-FC                    VALUE +1536 THRU +1791.         
006800        88 IOP-CICS-TS                    VALUE +2560 THRU +2815.         
006900     02 IOP-RET-CD                        PIC S9(04)       COMP.          
007000        88 IOP-RC-DSIDERR                 VALUE +0001.                    
007100        88 IOP-RC-DUPKEY                  VALUE +0132.                    
007200        88 IOP-RC-DUPREC                  VALUE +0130.                    
007300        88 IOP-RC-ENDFILE                 VALUE +0015.                    
007400        88 IOP-RC-ILLOGIC                 VALUE +0002.                    
007500        88 IOP-RC-INVREQ                  VALUE +0008.                    
007600        88 IOP-RC-IOERR                   VALUE +0004.                    
007700        88 IOP-RC-ISCINVREQ               VALUE +0209.                    
007800        88 IOP-RC-LENGERR                 VALUE +0225.                    
007900        88 IOP-RC-NOSPACE                 VALUE +0131.                    
008000        88 IOP-RC-NOTFND                  VALUE +0129.                    
008100        88 IOP-RC-NOTOPEN                 VALUE +0012.                    
008200        88 IOP-RC-OK                      VALUE +0000.                    
008300        88 IOP-RC-SEGIDERR                VALUE +0004.                    
008400        88 IOP-RC-SYSIDERR                VALUE +0208.                    
008500        88 IOP-RC-TS-INVREQ               VALUE +0032.                    
008600        88 IOP-RC-TS-IOERR                VALUE +0128.                    
008700        88 IOP-RC-TS-ITEMERR              VALUE +0001.                    
008800        88 IOP-RC-TS-NOSPACE              VALUE +0008.                    
008900        88 IOP-RC-TS-QIDERR               VALUE +0002.                    
009000     02 IOP-RET-CD-OVLY                                                   
009100           REDEFINES IOP-RET-CD.                                          
009200        03 IOP-RET-CD-HI                  PIC  X(01).                     
009300        03 IOP-RET-CD-LO                  PIC  X(01).                     
009400     02 IOP-REC-PTR                       POINTER.                        
009500     02 IOP-DUP-REC-PTR                   POINTER.                        
009600     02 IOP-FC-REC-PTR                    POINTER.                        
009700     02 IOP-ERR-MSG                       PIC  X(16).                     
009800     02 IOP-MAX-REC-LEN                   PIC S9(04)       COMP.          
009900     02 IOP-REC-LEN                       PIC S9(04)       COMP.          
010000     02 IOP-DUP-REC-LEN                   PIC S9(04)       COMP.          
010100     02 IOP-KEY-LEN                       PIC S9(04)       COMP.          
010200     02 IOP-BROWSE-REQ-ID                 PIC S9(04)       COMP.          
010300     02 IOP-FILE-KEY                      PIC  X(256).                    
010400     02 IOP-TSQ-KEY                                                       
010500           REDEFINES IOP-FILE-KEY.                                        
010600        03 IOP-TSQ-ITEM-NBR               PIC S9(04)       COMP.          
010700        03                                PIC  X(254).                    
010800     02 IOP-TSQ-PRINT-PARMS                                               
010900           REDEFINES IOP-TSQ-KEY.                                         
011000        03 IOP-TSQ-FIRST-PRINT            PIC S9(04)       COMP.          
011100        03 IOP-TSQ-LAST-PRINT             PIC S9(04)       COMP.          
011200        03 IOP-TSQ-PRINTER-ID             PIC  X(04).                     
011300        03 IOP-TSQ-PRINT-MSG              PIC  X(50).                     
011400        03                                PIC  X(198).                    
011500     02 IOP-AIX-DDNAME                    PIC  X(08).                     
011600     02 IOP-FC-REC-LEN                    PIC S9(04)       COMP.          
011700     02 IOP-NBR-RECS-DEL                  PIC S9(04)       COMP.          
