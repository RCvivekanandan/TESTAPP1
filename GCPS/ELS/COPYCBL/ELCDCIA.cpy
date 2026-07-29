      ******************************************************************        
      *  ELCDCIA    C O M M O N   I N T E R F A C E   A R E A          *        
      *                                                                *        
      *  CONTAINS POINTERS TO NUMEROUS DATA AREAS PASSED THROUGHOUT A  *        
      *  MYRIAD OF PROGRAMS LINKED TOGETHER TO PROCESS A TRANSACTION   *        
      *  OR POSSIBLY A SERIES OF TRANSACTIONS.                         *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
      *       *-*         U P D A T E   H I S T O R Y         *-*      *        
      *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
      *                                                                *        
      **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*        
      *                                                                *        
      *    XXXX    03/04/86  AMJ  ORIGINAL MODULE (LENGTH ??? BYTES)   *        
      *    0001    03/10/86  JTC  ADD IOPARM POINTER AREAS             *        
      *                           LENGTH NOW = 356                     *        
      *    0002    03/19/86  DES  ADDED CODES MANUAL INTERFACE &       *        
      *                           ELO CONTROL BLOCK (LENGTH NOW = 4000)*        
      *    0003    03/31/86  NAC  ADDED FIELD TO INDICATE WHAT LINE OF *        
      *                           BUSINESS THE SCREEN SHOULD CONTAIN.  *        
      *    0004    04/01/86  DES  REMOVED 1 HEADER LINE FROM OUTPUT    *        
      *                           AREA & ADDED BEN PROV LIST PNTRS     *        
      *    0005    04/02/86  JLA  ADDED POINTER FOR PAYMENT LEVEL      *        
      *                            SWITCHES AREA.                      *        
      *                           ADDED FOUR POINTERS FOR PAYMENT LEVEL*        
      *                            TABLES, PAIRED WITH FOUR BEN PROV   *        
      *                            LIST POINTERS.                      *        
      *    0006    04/03/86  DES  ADDED DECIMAL PLACE TO ELEMENT NO    *        
      *    0007    04/03/86  JTC  ADDED TOPIC NAME TO BE PASSED TO     *        
      *                           COVERAGE SUBROUTINE CIA-TOPIC-PHRASE *        
      *    0008    05/20/86  DES  ADDED CONDITION-NAME FOR THE RETURN- *        
      *                           FLAG ASSOCIATED WITH CODES MANAUL    *        
      *    0009    07/10/86  AMJ  FIRST STEP OF CONVERSION TO NEW      *        
      *                           CONTRACT POINTERS.                   *        
      *                                                                *        
      ******************************************************************        
           02  CIA-ELP-COMMON-INTERFACE-AREA.                                   
             05  CIA-IO-GETMAIN-DDNAME           PIC X(8).                      
             05  CIA-IO-PARM-AREA-PNTR           USAGE IS POINTER.              
             05  CIA-ERROR-MSG-REC-AREA-PNTR     USAGE IS POINTER.              
             05  CIA-ELPRL-IOPARM-AREA-PNTR      USAGE IS POINTER.              
             05  CIA-ELPRL-REC-AREA-PNTR         USAGE IS POINTER.              
             05  CIA-ELPDE-IOPARM-AREA-PNTR      USAGE IS POINTER.              
             05  CIA-ELPDE-REC-AREA-PNTR         USAGE IS POINTER.              
             05  CIA-ELPCV-IOPARM-AREA-PNTR      USAGE IS POINTER.              
             05  CIA-ELPCV-REC-AREA-PNTR         USAGE IS POINTER.              
             05  CIA-ELPEN-IOPARM-AREA-PNTR      USAGE IS POINTER.              
             05  CIA-ELPEN-REC-AREA-PNTR         USAGE IS POINTER.              
             05  CIA-ELPCN-IOPARM-AREA-PNTR      USAGE IS POINTER.              
             05  CIA-ELPCN-REC-AREA-PNTR         USAGE IS POINTER.              
             05  CIA-GRPSP-IOPARM-AREA-PNTR      USAGE IS POINTER.              
             05  CIA-GRPSP-REC-AREA-PNTR         USAGE IS POINTER.              
             05  CIA-CONTR-IOPARM-AREA-PNTR      USAGE IS POINTER.              
      ******************************************************************        
      ** THE NEXT FOUR POINTERS WILL BE REPLACED BY BASIC AND         **        
      ** SUPPLEMENTAL INSTITUTIONAL AND PROFESSIONAL POINTERS.        **        
      ******************************************************************        
             05  CIA-BC-CONTR-REC-PNTR           USAGE IS POINTER.              
             05  CIA-BS-CONTR-REC-PNTR           USAGE IS POINTER.              
             05  CIA-CMM-CONTR-REC-PNTR          USAGE IS POINTER.              
             05  CIA-SMM-CONTR-REC-PNTR          USAGE IS POINTER.              
             05  CIA-BENPV-IOPARM-AREA-PNTR      USAGE IS POINTER.              
             05  CIA-BENPV-REC-AREA-PNTR         USAGE IS POINTER.              
             05  CIA-TABLR-IOPARM-AREA-PNTR      USAGE IS POINTER.              
             05  CIA-TABLR-REC-AREA-PNTR         USAGE IS POINTER.              
             05  CIA-PAYMENT-LEVEL-SWITCH-PTR    USAGE IS POINTER.              
             05  CIA-BEN-PROV-PROF-IP-PTR        USAGE IS POINTER.              
             05  CIA-BEN-PROV-PROF-IP-PLT-PTR    USAGE IS POINTER.              
             05  CIA-BEN-PROV-PROF-OP-PTR        USAGE IS POINTER.              
             05  CIA-BEN-PROV-PROF-OP-PLT-PTR    USAGE IS POINTER.              
             05  CIA-BEN-PROV-INST-IP-PTR        USAGE IS POINTER.              
             05  CIA-BEN-PROV-INST-IP-PLT-PTR    USAGE IS POINTER.              
             05  CIA-BEN-PROV-INST-OP-PTR        USAGE IS POINTER.              
             05  CIA-BEN-PROV-INST-OP-PLT-PTR    USAGE IS POINTER.              
      ******************************************************************        
      ** THE NEXT FOUR POINTERS WILL BE REPLACED BY FILLER AFTER      **        
      ** THE FOUR ORIGINAL CONTRACT POINTERS ARE ELIMINATED.          **        
      ******************************************************************        
             05  CIA-INST-BASIC-CONTR-PNTR       USAGE IS POINTER.              
             05  CIA-INST-SUPP-CONTR-PNTR        USAGE IS POINTER.              
             05  CIA-PROF-BASIC-CONTR-PNTR       USAGE IS POINTER.              
             05  CIA-PROF-SUPP-CONTR-PNTR        USAGE IS POINTER.              
             05  CIA-GCPS-KEY-FIELDS.                                           
               10  CIA-GROUP                     PIC X(6).                      
               10  CIA-SECTION                   PIC X(4).                      
               10  CIA-LINE-OF-BUSINESS          PIC X.                         
               10  CIA-FAMILY-RELAT-LEVEL        PIC X.                         
               10  CIA-PROVIDER-CONTROL          PIC XX.                        
               10  CIA-EFFECTIVE-DATE            PIC S9(5)     COMP-3.          
             05  CIA-ELP-KEY-FIELDS.                                            
               10  CIA-ELPCV-KEY.                                               
                 15  CIA-ELPDE-KEY.                                             
                   20  CIA-ELPRL-KEY.                                           
                     25  CIA-ELPRL-RECORD-ID     PIC X(8).                      
                   20  CIA-ELPDE-ELEMENT-NO      PIC S9(3)V99  COMP-3.          
                 15  CIA-ELPCV-CODE-VALUE        PIC X(10).                     
                 15  CIA-ELPCV-SEQ-NO            PIC XX.                        
               10  CIA-ELPEN-KEY.                                               
                 15  CIA-ELPEN-RECORD-ID         PIC X(8).                      
                 15  CIA-ELPEN-ENG-NAME          PIC X(75).                     
               10  CIA-ELPCN-KEY.                                               
                 15  CIA-ELPCN-RECORD-ID         PIC X(8).                      
                 15  CIA-ELPCN-SYSTEM-NAME       PIC X(30).                     
             05  FILLER                          PIC X(55).                     
             05  CIA-CODES-MANUAL-INTERFACE.                                    
               10  CIA-RECORD-PREFIX             PIC  X(08).                    
               10  CIA-ELEMENT-SYSTEM-NAME       PIC  X(30).                    
               10  CIA-CODE-VALUE                PIC  X(10).                    
               10  CIA-CONDITION-BITS            PIC  X(20).                    
               10  CIA-DESCRIPTION-TYPE          PIC  X(01).                    
                   88  CIA-USE-SHORT-DESCRIPTION      VALUE 'S'.                
                   88  CIA-USE-LONG-DESCRIPTION       VALUE 'L'.                
               10  CIA-RETURN-FLAG               PIC  X(01).                    
                   88  CIA-RETURN-OK                  VALUE SPACE,  'M'.        
                   88  CIA-MORE-DESCRIPTION-LINES     VALUE 'M'.                
                   88  CIA-RETURN-NOTFND              VALUE 'N'.                
                   88  CIA-RETURN-ERROR               VALUE 'E'.                
               10  CIA-SHORT-DESCRIPTION         PIC  X(79).                    
               10  CIA-LONG-DESCRIPTION-TBL.                                    
                 15  CIA-NBR-DESCR-LINES         PIC S9(03)    COMP-3.          
                 15  CIA-DESCRIPTION-LINE        PIC  X(79)                     
                                                 OCCURS 20 TIMES.               
             05  CIA-CONTROL-BLOCK.                                             
               10  CIA-FUNCTION                  PIC  X(01).                    
                   88  CIA-CONTINUE                   VALUE ' '.                
                   88  CIA-NEW-PAGE                   VALUE 'P'.                
                   88  CIA-END                        VALUE 'E'.                
               10  CIA-RETURN                    PIC  X(01).                    
                   88  CIA-PROCESS-OK                 VALUE '0'.                
                   88  CIA-INVALID-REQUEST            VALUE '1'.                
               10  CIA-RECORD-PTR                PIC  S9(08) COMP SYNC.         
               10  CIA-FIRST-SCREEN-PTR          PIC  S9(08) COMP SYNC.         
               10  CIA-NBR-HEADER-LINES          PIC  S9(03)   COMP-3.          
               10  CIA-NBR-DETAIL-LINES          PIC  S9(03)   COMP-3.          
               10  CIA-HEADER.                                                  
                 15  CIA-HEADER-LINES            PIC  X(79)                     
                                                 OCCURS 2 TIMES.                
               10  CIA-DETAIL.                                                  
                 15  CIA-DETAIL-LINE             PIC  X(79)                     
                                                 OCCURS 20 TIMES.               
             05  CIA-SCREEN-LOB                  PIC X.                         
                 88  CIA-NOT-APPLICABLE          VALUE '0'.                     
                 88  CIA-BC                      VALUE '1'.                     
                 88  CIA-BS                      VALUE '2'.                     
                 88  CIA-MM                      VALUE '3'.                     
                 88  CIA-CMM                     VALUE '4'.                     
                 88  CIA-BC-BS                   VALUE '5'.                     
                 88  CIA-BC-MM                   VALUE '6'.                     
                 88  CIA-BS-MM                   VALUE '7'.                     
                 88  CIA-BC-BS-MM                VALUE '8'.                     
                 88  CIA-BC-BS-MM                VALUE '8'.                     
             05  CIA-TOPIC-PHRASE                PIC X(58).                     
             05  CIA-FIRST-TIME-SW               PIC X(01).                     
             05  FILLER                          PIC X(86).                     
