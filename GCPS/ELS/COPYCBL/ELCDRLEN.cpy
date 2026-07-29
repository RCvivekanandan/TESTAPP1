      ******************************************************************        
      * ELCDRLEN CONTAINS FILE DD NAMES, AS WELL AS RECORD LENGTHS FOR *        
      * APPLICATION FILES, FOR THE ELP COMMON INTERFACE AREA AND FOR   *        
      * THE ELP I/O PARM RECORD.  LENGTHS GIVEN FOR VARIABLE LENGTH    *        
      * RECORDS WILL BE THE MAXIMUM LENGTH.  BECAUSE OF THE FREQUENT   *        
      * CHANGES TO THESE FILES, CARE MUST BE TAKEN TO COMPARE THESE    *        
      * LENGTHS TO THE RDW'S AND TO OTHER DOCUMENTATION.  THIS COPY    *        
      * MEMBER WILL BE UPDATED AS THE NEED ARISES BUT UNTIL THE        *        
      * RECORDS STABILIZE IT WILL FREQUENTLY BE IN ERROR.              *        
      ******************************************************************        
      *                                                                *        
      *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
      *       *-*         U P D A T E   H I S T O R Y         *-*      *        
      *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
      *                                                                *        
      **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*        
      *                                                                *        
      *    XXXX    03/04/86  AMJ  ORIGINAL MODULE                      *        
      *    0001    03/19/86  DES  ADDED ELPAGEFL                       *        
      *    0002    03/24/86  AMJ  CHANGED ECI DFHCOMMAREA LGTH TO 1818 *        
      *    0003    03/26/86  AMJ  CHANGED ECI DFHCOMMAREA LGTH TO 1831 *        
      *    0004    03/27/86  AMJ  CHANGED ECI DFHCOMMAREA LGTH TO 1832 *        
      *    0005    03/31/86  AMJ  CHANGED ECI DFHCOMMAREA LGTH TO 1963 *        
      *    0006    03/31/86  AMJ  CHANGED ECI DFHCOMMAREA LGTH TO 2000 *        
      *    0007    04/02/86  JLA  ADDED LENGTHS FOR:                   *        
      *                             BEN PROV LIST(S) AREA              *        
      *                             PAYMENT LEVEL TABLE(S) AREA        *        
      *                             PAYMENT LEVEL SWITCHES AREA        *        
      *                                                                *        
      *    0008    04/03/86  JLA  CHANGED LENGTHS FOR BEN PROV LISTS   *        
      *    0009    07/21/86  AMJ  ADDED LENGTH FOR GCFLDVAL RECORD     *        
      *    0010    07/30/86  JTC  CHANGED LENGTHS FOR                  *        
      *                           EL-PAY-LVL-TBL-VARY-LEN AND          *        
      *                           EL-PAY-LVL-SWITCHES-LEN              *        
      *    0011    11/24/86  NAC  CHANGED LENGTHS FOR                  *        
      *                           EL-PAY-LVL-TBL-VARY-LEN AND          *        
      *                           EL-PAY-LVL-SWITCHES-LEN              *        
      ******************************************************************        
           05  EL-RECORD-LENGTHS.                                               
      * GCPS BENEFIT PROVISION                                                  
               10  EL-DSN-GCBENPRV          PIC X(8) VALUE 'GCBENPRV'.          
               10  EL-BENEFIT-PROV-REC-LEN  PIC S9(4) COMP SYNC                 
                                                VALUE +4000.                    
      * GCPS CONTRACT                                                           
               10  EL-DSN-GCCONTR           PIC X(8) VALUE 'GCCONTR'.           
               10  EL-CONTRACT-REC-LEN      PIC S9(4) COMP SYNC                 
                                                VALUE +8000.                    
      * GCPS TABULAR                                                            
               10  EL-DSN-GCTABULR          PIC X(8) VALUE 'GCTABULR'.          
               10  EL-TABULAR-REC-LEN       PIC S9(4) COMP SYNC                 
                                                VALUE +4000.                    
      * GCPS GROUP SPECIFIC                                                     
               10  EL-DSN-GCGRPSPC          PIC X(8) VALUE 'GCGRPSPC'.          
               10  EL-GROUP-SPECIFIC-REC-LEN                                    
                                            PIC S9(4) COMP SYNC                 
                                                VALUE +4000.                    
      * GCPS DATES CROSS-REFERENCE FILE                                         
               10  EL-DSN-GCDATES           PIC X(8) VALUE 'GCDATES '.          
               10  EL-DATES-FILE-REC-LEN    PIC S9(4) COMP SYNC                 
                                                VALUE +4000.                    
      * GCPS SYSTEM TABLE                                                       
               10  EL-DSN-GCSYSTBL          PIC X(8) VALUE 'GCSYSTBL'.          
               10  EL-GCPS-SYS-TBL-REC-LEN  PIC S9(4) COMP SYNC                 
                                                VALUE +4000.                    
      * ENGLISH LANGUAGE RECORD LIST                                            
               10  EL-DSN-ELPRL             PIC X(8) VALUE 'ELPRL   '.          
               10  EL-ELPRL-REC-LEN         PIC S9(4) COMP SYNC                 
                                                VALUE +61.                      
      * ENGLISH LANGUAGE DATA ELEMENT                                           
               10  EL-DSN-ELPDE             PIC X(8) VALUE 'ELPDE   '.          
               10  EL-ELPDE-REC-LEN         PIC S9(4) COMP SYNC                 
                                                VALUE +1041.                    
      * ENGLISH LANGUAGE CODE VALUE                                             
               10  EL-DSN-ELPCV             PIC X(8) VALUE 'ELPCV   '.          
               10  EL-ELPCV-REC-LEN         PIC S9(4) COMP SYNC                 
                                                VALUE +1024.                    
      * ENGLISH LANGUAGE ENGLISH NAME                                           
               10  EL-DSN-ELPEN             PIC X(8) VALUE 'ELPEN   '.          
               10  EL-ELPEN-REC-LEN         PIC S9(4) COMP SYNC                 
                                                VALUE +87.                      
      * ENGLISH LANGUAGE SYSTEM NAME                                            
               10  EL-DSN-ELPCN             PIC X(8) VALUE 'ELPCN   '.          
               10  EL-ELPCN-REC-LEN         PIC S9(4) COMP SYNC                 
                                                VALUE +42.                      
      * ENGLISH LANGUAGE PAGE FILE                                              
               10  EL-DSN-ELPAGEFL          PIC X(8) VALUE 'ELPAGEFL'.          
               10  EL-ELPAGEFL-REC-LEN      PIC S9(4) COMP SYNC                 
                                                VALUE +2048.                    
      * ENGLISH LANGUAGE IO PARM AREA                                           
               10  FILLER                   PIC X(8) VALUE '********'.          
               10  EL-IOPARMS-REC-LEN       PIC S9(4) COMP SYNC                 
                                                VALUE +572.                     
      * ENGLISH CONTRACT INQUIRY STANDARD DFHCOMMAREA                           
               10  FILLER                   PIC X(8) VALUE '********'.          
               10  EL-ECI-DFHCOMMAREA-LEN   PIC S9(4) COMP SYNC                 
                                                VALUE +2000.                    
      * ENGLISH LANGUAGE COMMON INTERFACE AREA                                  
               10  FILLER                   PIC X(8) VALUE '********'.          
               10  EL-CIA-REC-REC-LEN       PIC S9(4) COMP SYNC                 
                                                VALUE +4000.                    
      * ENGLISH LANGUAGE POINTER TO COMMON INTERFACE AREA                       
               10  FILLER                   PIC X(8) VALUE '********'.          
               10  EL-CIA-POINTER-LEN       PIC S9(4) COMP SYNC                 
                                                VALUE +4.                       
      * ENGLISH LANGUAGE BENEFIT PROVISION LIST(S) INFO:                        
      *                           1. LENGTH OF FIXED PORTION                    
      *                           2. LENGTH OF VARIABLE PORTION                 
      *                           3. MAXIMUM OCCURS OF VARIABLE PORTION         
               10  FILLER                   PIC X(8) VALUE '********'.          
               10  EL-PROV-LIST-FIXED-LEN   PIC S9(4) COMP SYNC                 
                                                VALUE +11.                      
               10  FILLER                   PIC X(8) VALUE '********'.          
               10  EL-PROV-LIST-VARY-LEN    PIC S9(4) COMP SYNC                 
                                                VALUE +16.                      
               10  FILLER                   PIC X(8) VALUE '********'.          
               10  EL-PROV-LIST-MAX-OCCURS  PIC S9(4) COMP SYNC                 
                                                VALUE +25.                      
      * ENGLISH LANGUAGE PAYMENT LEVEL TABLE(S) INFO:                           
      *                           1. LENGTH OF FIXED PORTION                    
      *                           2. LENGTH OF VARIABLE PORTION                 
      *                           3. MAXIMUM OCCURS OF VARIABLE PORTION         
               10  FILLER                   PIC X(8) VALUE '********'.          
               10  EL-PAY-LVL-TBL-FIXED-LEN PIC S9(4) COMP SYNC                 
                                                VALUE +10.                      
               10  FILLER                   PIC X(8) VALUE '********'.          
               10  EL-PAY-LVL-TBL-VARY-LEN  PIC S9(4) COMP SYNC                 
                                                VALUE +691.                     
               10  FILLER                   PIC X(8) VALUE '********'.          
               10  EL-PAY-LVL-TBL-MAX-OCCURS PIC S9(4) COMP SYNC                
                                                VALUE +25.                      
      * ENGLISH LANGUAGE PAYMENT LEVEL SWITCHES AREA                            
               10  FILLER                   PIC X(8) VALUE '********'.          
               10  EL-PAY-LVL-SWITCHES-LEN  PIC S9(4) COMP SYNC                 
                                                VALUE +164.                     
      * GCPS FIELD VALIDATION TABLE                                             
               10  EL-DSN-GCFLDVAL          PIC X(8) VALUE 'GCFLDVAL'.          
               10  EL-GCFLDVAL-REC-LEN                                          
                                            PIC S9(4) COMP SYNC                 
                                                VALUE +4000.                    
      * END OF TABLE SENTINEL                                                   
               10  FILLER                   PIC X(8) VALUE HIGH-VALUES.         
