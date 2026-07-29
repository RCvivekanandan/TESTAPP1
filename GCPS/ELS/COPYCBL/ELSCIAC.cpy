      ******************************************************************        
      *                                                                *        
      *   RRRRR   EEEEEE   AAAA   DDDDD         MM     MM  EEEEEE      *        
      *   R    R  E       A    A  D    D        M M   M M  E           *        
      *   R    R  E       A    A  D    D        M  M M  M  E           *        
      *   RRRRR   EEEE    A    A  D    D        M   M   M  EEEE        *        
      *   R  R    E       AAAAAA  D    D        M       M  E           *        
      *   R   R   E       A    A  D    D        M       M  E           *        
      *   R    R  EEEEEE  A    A  DDDDD         M       M  EEEEEE      *        
      *                                                                *        
      *   UNTIL THE TRANSITION TO THE NEW STORAGE MANAGEMENT IS        *        
      *   COMPLETED, IF YOU MAKE ANY CHANGES TO ANY ONE OF THE         *        
      *   FOLLOWING MEMBERS, YOU MUST MAKE SURE THAT THE CORRESPONDING *        
      *   CHANGE IS MADE IN ALL OTHER MEMBERS IN THIS GROUP:           *        
      *                                                                *        
      *     ELSCIAC  - COMMON INTERFACE AREA (OLD VERSION)             *        
      *     ELSCIA2C - COMMON INTERFACE AREA (NEW VERSION)             *        
      *     ELSCIA2D - COMMON INTERFACE AREA (NEW ASSEMBLER VERSION)   *        
      *     ELSSMAC  - STORAGE MANAGEMENT CONTROL TABLE (NEW VERSION)  *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSCIAC                                         *        
      *    DATE:       10-SEP-1986                                     *        
      *    AUTHOR:     RICHARD J. LUKETICH                             *        
      *    FUNCTION:   COMMON INTERFACE CONTROL BLOCK FOR ELS ENGLISH  *        
      *                CONTRACT INQUIRY PROGRAMS.                      *        
      *                                                                *        
      *                DEFINES INTERFACE TO INPUT/OUTPUT AND STORAGE   *        
      *                MANAGEMENT SUBROUTINES USED IN ELS.  ALSO       *        
      *                CONTAINS A TABLE INDICATING WHETHER A PARTICU-  *        
      *                LAR SELECTOR IS A KEY OR A TOPIC SELECTOR.      *        
      *                THIS TABLE MUST BE SYNCHRONIZED WITH THE MODULE *        
      *                STATUS TABLE IN THE SELECTOR STATUS CONTROL     *        
      *                BLOCK (ELSSSCB).                                *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 10-SEP-1986 RJL       CREATED                            *        
      * 01.55 21-JUL-1992 AKK       ADDED PMCCOMM.  USED WITH NOTICE   *        
      *                             OF ADMISSION/ ELIGIBILITY SUMMARY. *        
      * 01.56 22-JUL-1992 AKK       ADDED ELSPMCID. WILL USE TO HOLD   *        
      *                             AND PASS INFO ON BPS AND OTHER     *        
      *                             INTERMEDIATE RESULTS FOR NA/ES.    *        
      *                             INCREASE NUMBER OF OCCURENCES TO 70*        
      * 01.57 15-OCT-1992 BAK       ADDED ELSIDGD AND ELSIPGP INCREASE *        
      *                             TO 72 OCCURANCES.                  *        
      * 01.58 02-AUG-1993 BAK       ADDED GCPRVTB2 FOR GLV TABULARS %  *        
      *                             TO 73 OCCURANCES & EL49.           *        
      *                                                                *        
      * 01.59 23-SEP-1993 AKK       ADDED ELSSLNR TO USE WHEN SAVING   *        
      *                             ID AND SLOT NUMBERS -- FIRST USE   *        
      *                             IS WITH GVL                        *        
      *                                                                *        
      * 01.60 17-AUG-2000 AKK       ADDED SUPPORT FOR IPGS TAUBLAR     *        
      *                                                                *        
      * 01.61 22-SEP-2000  JP       ADDED ELSRLMED FOR REALMED.        *        
      *                                                                *        
      * 01.70 06-DEC-2005  AKK      CHANGE INTERNALCOUNT TO 150        *        
      *                             MATCH SMA. MAY FIX STORAGE VIOL    *        
      *                                                                *        
      ******************************************************************        
                                                                                
       01  CIA-ELS-COMMON-INTERFACE-AREA.                                       
                                                                                
           02 CIA-CONTROL-BLOCK.                                                
              03 CIA-DDNAME            PICTURE  X(08).                          
              03 CIA-AREA-LEN          PICTURE S9(08)          COMP.            
                 88 CIA-AREA-LEN-DFLT  VALUE +0000.                             
              03 CIA-STG-MGT-FCN       PICTURE  X(01).                          
                 88 CIA-STG-FREEMAIN   VALUE 'F'.                               
                 88 CIA-STG-GETMAIN    VALUE 'G'.                               
                 88 CIA-STG-INITIALIZE VALUE 'I'.                               
                 88 CIA-STG-PURGE      VALUE 'P'.                               
                 88 CIA-STG-RETRIEVE   VALUE 'R'.                               
                 88 CIA-STG-STOW       VALUE 'S'.                               
              03 CIA-STG-MGT-FCN-SAVE  PICTURE  X(01).                          
              03 CIA-AREA-LEN-16M      PICTURE S9(04)          COMP.            
              03 CIA-ABCODE            PICTURE  X(04).                          
                 88 CIA-AB-UNDEF                   VALUE 'EL00'.                
                 88 CIA-AB-DFHCOMMAREA             VALUE 'EL01'.                
                 88 CIA-AB-ELSCIA-PTR              VALUE 'EL02'.                
                 88 CIA-AB-AREANAME                VALUE 'EL03'.                
                 88 CIA-AB-DDNAME                  VALUE 'EL04'.                
                 88 CIA-AB-ELSIOPM-PTR             VALUE 'EL05'.                
                 88 CIA-AB-STG-INVREQ              VALUE 'EL06'.                
                 88 CIA-AB-IO-INVREQ               VALUE 'EL07'.                
                 88 CIA-AB-IOREC-PTR               VALUE 'EL08'.                
                 88 CIA-AB-NOTOPEN                 VALUE 'EL09'.                
                 88 CIA-AB-CRITIO                  VALUE 'EL10'.                
                 88 CIA-AB-MAPFAIL                 VALUE 'EL11'.                
                 88 CIA-AB-UNALLOC-AREA            VALUE 'EL12'.                
                 88 CIA-AB-NOT-UNIQUE              VALUE 'EL20'.                
                 88 CIA-AB-TOP-SEL                 VALUE 'EL21'.                
                 88 CIA-AB-TOP-SEL-SYNC            VALUE 'EL22'.                
                 88 CIA-AB-PARM-MISSING            VALUE 'EL23'.                
                 88 CIA-AB-GROUP-SPEC-NOT-AVAIL    VALUE 'EL24'.                
                 88 CIA-AB-CONTRACT-NOT-AVAIL      VALUE 'EL25'.                
                 88 CIA-AB-NOTFND-ELPCN            VALUE 'EL31'.                
                 88 CIA-AB-NOTFND-ELPCV            VALUE 'EL32'.                
                 88 CIA-AB-NOTFND-ELPDE            VALUE 'EL33'.                
                 88 CIA-AB-NOTFND-ELPEN            VALUE 'EL34'.                
                 88 CIA-AB-NOTFND-ELPRL            VALUE 'EL35'.                
                 88 CIA-AB-NOTFND-GCDATES          VALUE 'EL41'.                
                 88 CIA-AB-NOTFND-GCGRPSPC         VALUE 'EL42'.                
                 88 CIA-AB-NOTFND-GCCONTR          VALUE 'EL43'.                
                 88 CIA-AB-NOTFND-GCBENPRV         VALUE 'EL44'.                
                 88 CIA-AB-NOTFND-GCTABULR         VALUE 'EL45'.                
                 88 CIA-AB-NOTFND-GCSYSTBL         VALUE 'EL46'.                
                 88 CIA-AB-NOTFND-GCFLDVAL         VALUE 'EL47'.                
                 88 CIA-AB-NOTFND-GCFLDVL2         VALUE 'EL48'.                
                 88 CIA-AB-NOTFND-GCPRVTB2         VALUE 'EL49'.                
                 88 CIA-AB-TAB-UNDEF               VALUE 'EL50'.                
                 88 CIA-AB-INV-A-D-IND             VALUE 'EL51'.                
                 88 CIA-AB-A-D-NOTFND              VALUE 'EL52'.                
                 88 CIA-AB-NOTFND-GRP-SECT         VALUE 'EL53'.                
                 88 CIA-AB-NOTFND-CONT-SECT        VALUE 'EL54'.                
                 88 CIA-AB-CC-ACCUM-ON-CONTR       VALUE 'EL55'.                
                 88 CIA-AB-INCR-TBL-SIZE           VALUE 'EL60'.                
                 88 CIA-AB-ARGUMENT-NOTFND         VALUE 'EL61'.                
                 88 CIA-AB-PARM-ERR                VALUE 'EL98'.                
                 88 CIA-AB-PGM-LOGIC               VALUE 'EL99'.                
           02 CIA-TSQ-MGT.                                                      
              03 CIA-TSQ-ID.                                                    
                 04 CIA-TSQ-ID-PFX     PICTURE  X(04).                          
                 04 CIA-TSQ-ID-SFX     PICTURE  X(04).                          
              03 CIA-TSQ-ITEM-NBR      PICTURE S9(04)          COMP.            
              03 CIA-TSQ-TS-LEN        PICTURE S9(04)          COMP.            
              03 CIA-TSQ-TS-PTR        POINTER.                                 
              03 CIA-TSQ-MAX           PICTURE S9(04)          COMP             
                                       VALUE +0007.                             
              03 CIA-TSQ-TABLE.                                                 
                 04 FILLER     PICTURE  X(08)     VALUE 'ELSSSCB '.             
                 04 FILLER     PICTURE  X(08)     VALUE 'ELSKTBS '.             
                 04 FILLER     PICTURE  X(08)     VALUE 'ELSKTBG '.             
                 04 FILLER     PICTURE  X(08)     VALUE 'ELSKTBC '.             
                 04 FILLER     PICTURE  X(08)     VALUE 'ELSMEMS '.             
                 04 FILLER     PICTURE  X(08)     VALUE 'ELSMHDG '.             
                 04 FILLER     PICTURE  X(08)     VALUE 'ELSMOPT '.             
                 04 FILLER     PICTURE  X(08)     VALUE 'ELSSLNR '.             
              03 CIA-TSQ-TBL           REDEFINES CIA-TSQ-TABLE                  
                                       OCCURS 8 TIMES                           
                                       INDEXED BY CIA-TSQ-IDX.                  
                 04 CIA-TSQ-DDN        PICTURE  X(08).                          
                                                                                
              03 CIA-RETURN-CODE       PICTURE S9(04)          COMP.            
                 88 CIA-RC-OK                        VALUE +0000.               
                 88 CIA-RC-STG-DUP-REL               VALUE +0101.               
                 88 CIA-RC-STG-DUP-REQ               VALUE +0102.               
                 88 CIA-RC-MEMB-NON-RETN             VALUE +0201.               
                 88 CIA-RC-MEMB-NOT-IN-GRP           VALUE +0202.               
                 88 CIA-RC-MEMB-TBL-OVFL             VALUE +0203.               
                 88 CIA-RC-MEMB-GRP-NOTFND           VALUE +0204.               
                 88 CIA-RC-MEMB-NO-SECTN-INFO        VALUE +0205.               
                 88 CIA-RC-KTB-GRP-NOTFND            VALUE +0301.               
                 88 CIA-RC-KTB-SECTN-NOTFND          VALUE +0302.               
                 88 CIA-RC-CONDB-UNABLE              VALUE +0311.               
                                                                                
              03 FILLER                PICTURE  X(02).                          
                                                                                
                                                                                
           02 CIA-SEL-TYP-FLAGS.                                                
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'T'.          
              03 FILLER                PICTURE  X(01)       VALUE 'T'.          
              03 FILLER                PICTURE  X(01)       VALUE 'T'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'K'.          
              03 FILLER                PICTURE  X(01)       VALUE 'T'.          
              03 FILLER                PICTURE  X(01)       VALUE 'T'.          
              03 FILLER                PICTURE  X(01)       VALUE 'T'.          
              03 FILLER                PICTURE  X(01)       VALUE 'O'.          
              03 FILLER                PICTURE  X(01)       VALUE ' '.          
              03 FILLER                PICTURE  X(01)       VALUE ' '.          
                                                                                
           02 CIA-SEL-TYP-TABLE        REDEFINES CIA-SEL-TYP-FLAGS.             
              03 CIA-SEL-TYP-TBL       OCCURS 25 TIMES.                         
                 04 CIA-SEL-TYP        PICTURE  X(01).                          
                    88 CIA-SEL-TYP-KEY VALUE 'K'.                               
                    88 CIA-SEL-TYP-OTH VALUE 'O'.                               
                    88 CIA-SEL-TYP-TOP VALUE 'T'.                               
                                                                                
                                                                                
           02 CIA-STG-MGT.                                                      
              03 CIA-NBR-STG-MGT-BLKS  PICTURE S9(04)          COMP             
                                       VALUE +76.                               
                                                                                
              03 CIA-ELSSMA.                                                    
              04 CIA-ELSSMA-DDN   PICTURE  X(08)      VALUE ' ELS SMA'.         
              04 CIA-ELSSMA-LEN   PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSSMA-TYP   PICTURE  X(02)      VALUE 'SA'.               
              04 CIA-ELSSMA-PTR   POINTER             VALUE NULL.               
              04 CIA-ELSSMA-MVO   PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCIA.                                                    
              04 CIA-ELSCIA-DDN   PICTURE  X(08)      VALUE ' ELSCIA '.         
              04 CIA-ELSCIA-LEN   PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCIA-TYP   PICTURE  X(02)      VALUE 'SA'.               
              04 CIA-ELSCIA-PTR   POINTER             VALUE NULL.               
              04 CIA-ELSCIA-MVO   PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-COBXIO.                                                    
              04 CIA-COBXIO-DDN   PICTURE  X(08)      VALUE 'COBXIO  '.         
              04 CIA-COBXIO-LEN   PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-COBXIO-TYP   PICTURE  X(02)      VALUE 'DB'.               
              04 CIA-COBXIO-PTR   POINTER             VALUE NULL.               
              04 CIA-COBXIO-MVO   PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-DBPIOPM.                                                   
              04 CIA-DBPIOPM-DDN  PICTURE   X(08)     VALUE 'DBPIOPM'.          
              04 CIA-DBPIOPM-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-DBPIOPM-TYP  PICTURE   X(02)     VALUE 'DB'.               
              04 CIA-DBPIOPM-PTR  POINTER             VALUE NULL.               
              04 CIA-DBPIOPM-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELPCN.                                                     
              04 CIA-ELPCN-DDN    PICTURE  X(08)      VALUE 'ELPCN   '.         
              04 CIA-ELPCN-LEN    PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELPCN-TYP    PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-ELPCN-PTR    POINTER             VALUE NULL.               
              04 CIA-ELPCN-MVO    PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELPCV.                                                     
              04 CIA-ELPCV-DDN    PICTURE  X(08)      VALUE 'ELPCV   '.         
              04 CIA-ELPCV-LEN    PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELPCV-TYP    PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-ELPCV-PTR    POINTER             VALUE NULL.               
              04 CIA-ELPCV-MVO    PICTURE S9(04) COMP VALUE +0022.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELPDE.                                                     
              04 CIA-ELPDE-DDN    PICTURE  X(08)      VALUE 'ELPDE   '.         
              04 CIA-ELPDE-LEN    PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELPDE-TYP    PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-ELPDE-PTR    POINTER             VALUE NULL.               
              04 CIA-ELPDE-MVO    PICTURE S9(04) COMP VALUE +0022.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELPEN.                                                     
              04 CIA-ELPEN-DDN    PICTURE  X(08)      VALUE 'ELPEN   '.         
              04 CIA-ELPEN-LEN    PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELPEN-TYP    PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-ELPEN-PTR    POINTER             VALUE NULL.               
              04 CIA-ELPEN-MVO    PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELPGRPKY.                                                  
              04 CIA-ELPGRPKY-DDN PICTURE  X(08)      VALUE 'ELPGRPKY'.         
              04 CIA-ELPGRPKY-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELPGRPKY-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-ELPGRPKY-PTR POINTER             VALUE NULL.               
              04 CIA-ELPGRPKY-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELPMSGKY.                                                  
              04 CIA-ELPMSGKY-DDN PICTURE  X(08)      VALUE 'ELPMSGKY'.         
              04 CIA-ELPMSGKY-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELPMSGKY-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-ELPMSGKY-PTR POINTER             VALUE NULL.               
              04 CIA-ELPMSGKY-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELPRL.                                                     
              04 CIA-ELPRL-DDN    PICTURE  X(08)      VALUE 'ELPRL   '.         
              04 CIA-ELPRL-LEN    PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELPRL-TYP    PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-ELPRL-PTR    POINTER             VALUE NULL.               
              04 CIA-ELPRL-MVO    PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSATBL.                                                   
              04 CIA-ELSATBL-DDN  PICTURE  X(08)      VALUE 'ELSATBL '.         
              04 CIA-ELSATBL-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSATBL-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSATBL-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSATBL-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCMDSC.                                                  
              04 CIA-ELSCMDSC-DDN PICTURE  X(08)      VALUE 'ELSCMDSC'.         
              04 CIA-ELSCMDSC-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCMDSC-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSCMDSC-PTR POINTER             VALUE NULL.               
              04 CIA-ELSCMDSC-MVO PICTURE S9(04) COMP VALUE +0400.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCMIF.                                                   
              04 CIA-ELSCMIF-DDN  PICTURE  X(08)      VALUE 'ELSCMIF '.         
              04 CIA-ELSCMIF-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCMIF-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSCMIF-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSCMIF-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCOMM.                                                   
              04 CIA-ELSCOMM-DDN  PICTURE  X(08)      VALUE 'ELSCOMM '.         
              04 CIA-ELSCOMM-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCOMM-TYP  PICTURE  X(02)      VALUE 'SA'.               
              04 CIA-ELSCOMM-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSCOMM-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCONIB.                                                  
              04 CIA-ELSCONIB-DDN PICTURE  X(08)      VALUE 'ELSCONIB'.         
              04 CIA-ELSCONIB-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCONIB-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSCONIB-PTR POINTER             VALUE NULL.               
              04 CIA-ELSCONIB-MVO PICTURE S9(04) COMP VALUE +0450.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCONIS.                                                  
              04 CIA-ELSCONIS-DDN PICTURE  X(08)      VALUE 'ELSCONIS'.         
              04 CIA-ELSCONIS-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCONIS-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSCONIS-PTR POINTER             VALUE NULL.               
              04 CIA-ELSCONIS-MVO PICTURE S9(04) COMP VALUE +0450.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCONPB.                                                  
              04 CIA-ELSCONPB-DDN PICTURE  X(08)      VALUE 'ELSCONPB'.         
              04 CIA-ELSCONPB-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCONPB-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSCONPB-PTR POINTER             VALUE NULL.               
              04 CIA-ELSCONPB-MVO PICTURE S9(04) COMP VALUE +0450.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCONPS.                                                  
              04 CIA-ELSCONPS-DDN PICTURE  X(08)      VALUE 'ELSCONPS'.         
              04 CIA-ELSCONPS-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCONPS-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSCONPS-PTR POINTER             VALUE NULL.               
              04 CIA-ELSCONPS-MVO PICTURE S9(04) COMP VALUE +0450.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCSAC.                                                   
              04 CIA-ELSCSAC-DDN  PICTURE  X(08)      VALUE 'ELSCSA'.           
              04 CIA-ELSCSAC-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCSAC-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSCSAC-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSCSAC-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCSADC.                                                  
              04 CIA-ELSCSADC-DDN PICTURE  X(08)      VALUE 'ELSCSAD'.          
              04 CIA-ELSCSADC-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCSADC-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSCSADC-PTR POINTER             VALUE NULL.               
              04 CIA-ELSCSADC-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCSBPC.                                                  
              04 CIA-ELSCSBPC-DDN PICTURE  X(08)      VALUE 'ELSCSBP'.          
              04 CIA-ELSCSBPC-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCSBPC-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSCSBPC-PTR POINTER             VALUE NULL.               
              04 CIA-ELSCSBPC-MVO PICTURE S9(04) COMP VALUE +0050.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCSENC.                                                  
              04 CIA-ELSCSENC-DDN PICTURE  X(08)      VALUE 'ELSCSEN'.          
              04 CIA-ELSCSENC-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCSENC-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSCSENC-PTR POINTER             VALUE NULL.               
              04 CIA-ELSCSENC-MVO PICTURE S9(04) COMP VALUE +0050.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCSPG.                                                   
              04 CIA-ELSCSPG-DDN  PICTURE  X(08)      VALUE 'ELSCSPG'.          
              04 CIA-ELSCSPG-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCSPG-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSCSPG-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSCSPG-MVO  PICTURE S9(04) COMP VALUE +0050.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSCSPTC.                                                  
              04 CIA-ELSCSPTC-DDN PICTURE  X(08)      VALUE 'ELSCSPT'.          
              04 CIA-ELSCSPTC-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSCSPTC-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSCSPTC-PTR POINTER             VALUE NULL.               
              04 CIA-ELSCSPTC-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSELOG.                                                   
              04 CIA-ELSELOG-DDN PICTURE  X(08)       VALUE 'ELSELOG'.          
              04 CIA-ELSELOG-LEN PICTURE S9(04) COMP  VALUE +0000.              
              04 CIA-ELSELOG-TYP PICTURE  X(02)       VALUE 'DA'.               
              04 CIA-ELSELOG-PTR POINTER              VALUE NULL.               
              04 CIA-ELSELOG-MVO PICTURE S9(04) COMP  VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSGRPSP.                                                  
              04 CIA-ELSGRPSP-DDN PICTURE  X(08)      VALUE 'ELSGRPSP'.         
              04 CIA-ELSGRPSP-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSGRPSP-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSGRPSP-PTR POINTER             VALUE NULL.               
              04 CIA-ELSGRPSP-MVO PICTURE S9(04) COMP VALUE +0030.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSIBGR.                                                   
              04 CIA-ELSIBGR-DDN  PICTURE  X(08)      VALUE 'ELSIBGR '.         
              04 CIA-ELSIBGR-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSIBGR-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSIBGR-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSIBGR-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSIDGD.                                                   
              04 CIA-ELSIDGD-DDN  PICTURE  X(08)      VALUE 'ELSIDGD '.         
              04 CIA-ELSIDGD-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSIDGD-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSIDGD-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSIDGD-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSIOPM.                                                   
              04 CIA-ELSIOPM-DDN  PICTURE  X(08)      VALUE 'ELSIOPM '.         
              04 CIA-ELSIOPM-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSIOPM-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSIOPM-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSIOPM-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSIPGN.                                                   
              04 CIA-ELSIPGN-DDN  PICTURE  X(08)      VALUE 'ELSIPGN '.         
              04 CIA-ELSIPGN-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSIPGN-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSIPGN-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSIPGN-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSIPGP.                                                   
              04 CIA-ELSIPGP-DDN  PICTURE  X(08)      VALUE 'ELSIPGP '.         
              04 CIA-ELSIPGP-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSIPGP-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSIPGP-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSIPGP-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSIPGS.                                                   
              04 CIA-ELSIPGS-DDN  PICTURE  X(08)      VALUE 'ELSIPGS '.         
              04 CIA-ELSIPGS-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSIPGS-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSIPGS-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSIPGS-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSIPGT.                                                   
              04 CIA-ELSIPGT-DDN  PICTURE  X(08)      VALUE 'ELSIPGT '.         
              04 CIA-ELSIPGT-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSIPGT-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSIPGT-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSIPGT-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSKEYS.                                                   
              04 CIA-ELSKEYS-DDN  PICTURE  X(08)      VALUE 'ELSKEYS '.         
              04 CIA-ELSKEYS-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSKEYS-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSKEYS-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSKEYS-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSKTBC.                                                   
              04 CIA-ELSKTBC-DDN  PICTURE  X(08)      VALUE 'ELSKTBC '.         
              04 CIA-ELSKTBC-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSKTBC-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSKTBC-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSKTBC-MVO  PICTURE S9(04) COMP VALUE +0500.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSKTBG.                                                   
              04 CIA-ELSKTBG-DDN  PICTURE  X(08)      VALUE 'ELSKTBG '.         
              04 CIA-ELSKTBG-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSKTBG-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSKTBG-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSKTBG-MVO  PICTURE S9(04) COMP VALUE +0100.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSKTBS.                                                   
              04 CIA-ELSKTBS-DDN  PICTURE  X(08)      VALUE 'ELSKTBS '.         
              04 CIA-ELSKTBS-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSKTBS-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSKTBS-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSKTBS-MVO  PICTURE S9(04) COMP VALUE +0750.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSMEMS.                                                   
              04 CIA-ELSMEMS-DDN  PICTURE  X(08)      VALUE 'ELSMEMS '.         
              04 CIA-ELSMEMS-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSMEMS-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSMEMS-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSMEMS-MVO  PICTURE S9(04) COMP VALUE +0020.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSMHDG.                                                   
              04 CIA-ELSMHDG-DDN  PICTURE  X(08)      VALUE 'ELSMHDG '.         
              04 CIA-ELSMHDG-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSMHDG-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSMHDG-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSMHDG-MVO  PICTURE S9(04) COMP VALUE +0015.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSMOPT.                                                   
              04 CIA-ELSMOPT-DDN  PICTURE  X(08)      VALUE 'ELSMOPT '.         
              04 CIA-ELSMOPT-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSMOPT-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSMOPT-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSMOPT-MVO  PICTURE S9(04) COMP VALUE +0750.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSOUTP.                                                   
              04 CIA-ELSOUTP-DDN  PICTURE  X(08)      VALUE 'ELSOUTP '.         
              04 CIA-ELSOUTP-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSOUTP-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSOUTP-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSOUTP-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSPGMW1.                                                  
              04 CIA-ELSPGMW1-DDN PICTURE  X(08)      VALUE 'ELSPGMW1'.         
              04 CIA-ELSPGMW1-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSPGMW1-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSPGMW1-PTR POINTER             VALUE NULL.               
              04 CIA-ELSPGMW1-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSPGMW2.                                                  
              04 CIA-ELSPGMW2-DDN PICTURE  X(08)      VALUE 'ELSPGMW2'.         
              04 CIA-ELSPGMW2-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSPGMW2-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSPGMW2-PTR POINTER             VALUE NULL.               
              04 CIA-ELSPGMW2-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSPGMW3.                                                  
              04 CIA-ELSPGMW3-DDN PICTURE  X(08)      VALUE 'ELSPGMW3'.         
              04 CIA-ELSPGMW3-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSPGMW3-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSPGMW3-PTR POINTER             VALUE NULL.               
              04 CIA-ELSPGMW3-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSPGMW4.                                                  
              04 CIA-ELSPGMW4-DDN PICTURE  X(08)      VALUE 'ELSPGMW4'.         
              04 CIA-ELSPGMW4-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSPGMW4-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSPGMW4-PTR POINTER             VALUE NULL.               
              04 CIA-ELSPGMW4-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSPLGSW.                                                  
              04 CIA-ELSPLGSW-DDN PICTURE  X(08)      VALUE 'ELSPLGSW'.         
              04 CIA-ELSPLGSW-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSPLGSW-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSPLGSW-PTR POINTER             VALUE NULL.               
              04 CIA-ELSPLGSW-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSPLGTB.                                                  
              04 CIA-ELSPLGTB-DDN PICTURE  X(08)      VALUE 'ELSPLGTB'.         
              04 CIA-ELSPLGTB-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSPLGTB-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSPLGTB-PTR POINTER             VALUE NULL.               
              04 CIA-ELSPLGTB-MVO PICTURE S9(04) COMP VALUE +0025.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSPMCID.                                                  
              04 CIA-ELSPMCID-DDN PICTURE  X(08)      VALUE 'ELSPMCID'.         
              04 CIA-ELSPMCID-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSPMCID-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSPMCID-PTR POINTER             VALUE NULL.               
              04 CIA-ELSPMCID-MVO PICTURE S9(04) COMP VALUE +0025.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSPRVN.                                                   
              04 CIA-ELSPRVN-DDN  PICTURE  X(08)      VALUE 'ELSPRVN '.         
              04 CIA-ELSPRVN-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSPRVN-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSPRVN-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSPRVN-MVO  PICTURE S9(04) COMP VALUE +0050.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSMENU.                                                   
              04 CIA-ELSMENU-DDN  PICTURE  X(08)      VALUE 'ELSQMENU'.         
              04 CIA-ELSMENU-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSMENU-TYP  PICTURE  X(02)      VALUE 'TS'.               
              04 CIA-ELSMENU-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSMENU-MVO  PICTURE S9(04) COMP VALUE +0003.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSPAGE.                                                   
              04 CIA-ELSPAGE-DDN  PICTURE  X(08)      VALUE 'ELSQPAGE'.         
              04 CIA-ELSPAGE-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSPAGE-TYP  PICTURE  X(02)      VALUE 'TS'.               
              04 CIA-ELSPAGE-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSPAGE-MVO  PICTURE S9(04) COMP VALUE +0023.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSRLMED.                                                  
              04 CIA-ELSRLMED-DDN PICTURE  X(08)      VALUE 'ELSRLMED'.         
              04 CIA-ELSRLMED-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSRLMED-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSRLMED-PTR POINTER             VALUE NULL.               
              04 CIA-ELSRLMED-MVO PICTURE S9(04) COMP VALUE +0099.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSRRBL.                                                   
              04 CIA-ELSRRBL-DDN  PICTURE  X(08)      VALUE 'ELSRRBL '.         
              04 CIA-ELSRRBL-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSRRBL-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSRRBL-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSRRBL-MVO  PICTURE S9(04) COMP VALUE +0100.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSSLNR.                                                   
              04 CIA-ELSSLNR-DDN  PICTURE  X(08)      VALUE 'ELSSLNR '.         
              04 CIA-ELSSLNR-LEN  PICTURE S9(04) COMP VALUE +0600.              
              04 CIA-ELSSLNR-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSSLNR-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSSLNR-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSSRTP.                                                   
              04 CIA-ELSSRTP-DDN  PICTURE  X(08)      VALUE 'ELSSRTP '.         
              04 CIA-ELSSRTP-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSSRTP-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSSRTP-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSSRTP-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSSSCB.                                                   
              04 CIA-ELSSSCB-DDN  PICTURE  X(08)      VALUE 'ELSSSCB'.          
              04 CIA-ELSSSCB-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSSSCB-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSSSCB-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSSSCB-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSTCWA.                                                   
              04 CIA-ELSTCWA-DDN  PICTURE  X(08)      VALUE 'ELSTCWA'.          
              04 CIA-ELSTCWA-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSTCWA-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-ELSTCWA-PTR  POINTER             VALUE NULL.               
              04 CIA-ELSTCWA-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSTWA.                                                    
              04 CIA-ELSTWA-DDN   PICTURE  X(08)      VALUE 'ELSTWA '.          
              04 CIA-ELSTWA-LEN   PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSTWA-TYP   PICTURE  X(02)      VALUE 'SA'.               
              04 CIA-ELSTWA-PTR   POINTER             VALUE NULL.               
              04 CIA-ELSTWA-MVO   PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSWKFL1.                                                  
              04 CIA-ELSWKFL1-DDN PICTURE  X(08)      VALUE 'ELSWKFL1'.         
              04 CIA-ELSWKFL1-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSWKFL1-TYP PICTURE  X(02)      VALUE 'TS'.               
              04 CIA-ELSWKFL1-PTR POINTER             VALUE NULL.               
              04 CIA-ELSWKFL1-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSWKFL2.                                                  
              04 CIA-ELSWKFL2-DDN PICTURE  X(08)      VALUE 'ELSWKFL2'.         
              04 CIA-ELSWKFL2-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSWKFL2-TYP PICTURE  X(02)      VALUE 'TS'.               
              04 CIA-ELSWKFL2-PTR POINTER             VALUE NULL.               
              04 CIA-ELSWKFL2-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSWKFL3.                                                  
              04 CIA-ELSWKFL3-DDN PICTURE  X(08)      VALUE 'ELSWKFL3'.         
              04 CIA-ELSWKFL3-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSWKFL3-TYP PICTURE  X(02)      VALUE 'TS'.               
              04 CIA-ELSWKFL3-PTR POINTER             VALUE NULL.               
              04 CIA-ELSWKFL3-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-ELSWKFL4.                                                  
              04 CIA-ELSWKFL4-DDN PICTURE  X(08)      VALUE 'ELSWKFL4'.         
              04 CIA-ELSWKFL4-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-ELSWKFL4-TYP PICTURE  X(02)      VALUE 'TS'.               
              04 CIA-ELSWKFL4-PTR POINTER             VALUE NULL.               
              04 CIA-ELSWKFL4-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-GCBENPRV.                                                  
              04 CIA-GCBENPRV-DDN PICTURE  X(08)      VALUE 'GCBENPRV'.         
              04 CIA-GCBENPRV-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-GCBENPRV-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-GCBENPRV-PTR POINTER             VALUE NULL.               
              04 CIA-GCBENPRV-MVO PICTURE S9(04) COMP VALUE +0015.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-GCCONTR.                                                   
              04 CIA-GCCONTR-DDN  PICTURE  X(08)      VALUE 'GCCONTR '.         
              04 CIA-GCCONTR-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-GCCONTR-TYP  PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-GCCONTR-PTR  POINTER             VALUE NULL.               
              04 CIA-GCCONTR-MVO  PICTURE S9(04) COMP VALUE +0450.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-GCDATES.                                                   
              04 CIA-GCDATES-DDN  PICTURE  X(08)      VALUE 'GCDATES '.         
              04 CIA-GCDATES-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-GCDATES-TYP  PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-GCDATES-PTR  POINTER             VALUE NULL.               
              04 CIA-GCDATES-MVO  PICTURE S9(04) COMP VALUE +0440.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-GCFLDVAL.                                                  
              04 CIA-GCFLDVAL-DDN PICTURE  X(08)      VALUE 'GCFLDVAL'.         
              04 CIA-GCFLDVAL-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-GCFLDVAL-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-GCFLDVAL-PTR POINTER             VALUE NULL.               
              04 CIA-GCFLDVAL-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-GCFLDVL2.                                                  
              04 CIA-GCFLDVL2-DDN PICTURE  X(08)      VALUE 'GCFLDVL2'.         
              04 CIA-GCFLDVL2-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-GCFLDVL2-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-GCFLDVL2-PTR POINTER             VALUE NULL.               
              04 CIA-GCFLDVL2-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-GCGRPSPC.                                                  
              04 CIA-GCGRPSPC-DDN PICTURE  X(08)      VALUE 'GCGRPSPC'.         
              04 CIA-GCGRPSPC-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-GCGRPSPC-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-GCGRPSPC-PTR POINTER             VALUE NULL.               
              04 CIA-GCGRPSPC-MVO PICTURE S9(04) COMP VALUE +0030.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-GCPRVTB2.                                                  
              04 CIA-GCPRVTB2-DDN PICTURE  X(08)      VALUE 'GCPRVTB2'.         
              04 CIA-GCPRVTB2-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-GCPRVTB2-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-GCPRVTB2-PTR POINTER             VALUE NULL.               
              04 CIA-GCPRVTB2-MVO PICTURE S9(04) COMP VALUE +0100.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-GCSYSTBL.                                                  
              04 CIA-GCSYSTBL-DDN PICTURE  X(08)      VALUE 'GCSYSTBL'.         
              04 CIA-GCSYSTBL-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-GCSYSTBL-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-GCSYSTBL-PTR POINTER             VALUE NULL.               
              04 CIA-GCSYSTBL-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-GCTABULR.                                                  
              04 CIA-GCTABULR-DDN PICTURE  X(08)      VALUE 'GCTABULR'.         
              04 CIA-GCTABULR-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-GCTABULR-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 CIA-GCTABULR-PTR POINTER             VALUE NULL.               
              04 CIA-GCTABULR-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-PMCCOMM.                                                   
              04 CIA-PMCCOMM-DDN  PICTURE  X(08)      VALUE 'PMCCOMM '.         
              04 CIA-PMCCOMM-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-PMCCOMM-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-PMCCOMM-PTR  POINTER             VALUE NULL.               
              04 CIA-PMCCOMM-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-RDMC4306.                                                  
              04 CIA-RDMC4306-DDN PICTURE  X(08)      VALUE 'RDMC4306'.         
              04 CIA-RDMC4306-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-RDMC4306-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-RDMC4306-PTR POINTER             VALUE NULL.               
              04 CIA-RDMC4306-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-RDMC4308.                                                  
              04 CIA-RDMC4308-DDN PICTURE  X(08)      VALUE 'RDMC4308'.         
              04 CIA-RDMC4308-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-RDMC4308-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-RDMC4308-PTR POINTER             VALUE NULL.               
              04 CIA-RDMC4308-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 CIA-RDMC4311.                                                  
              04 CIA-RDMC4311-DDN PICTURE  X(08)      VALUE 'RDMC4311'.         
              04 CIA-RDMC4311-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 CIA-RDMC4311-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 CIA-RDMC4311-PTR POINTER             VALUE NULL.               
              04 CIA-RDMC4311-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
           02 CIA-STG-MGT-TABLE        REDEFINES CIA-STG-MGT.                   
              03 FILLER                PICTURE S9(04) COMP.                     
              03 CIA-STG-MGT-TBL       OCCURS 76 TIMES                          
                                       ASCENDING KEY CIA-DDN                    
                                       INDEXED BY CIA-STG-MGT-IDX.              
                 04 CIA-DDN            PICTURE  X(08).                          
                 04 CIA-TSN            REDEFINES CIA-DDN.                       
                    05 FILLER          PICTURE  X(04).                          
                    05 CIA-TS-SFX      PICTURE  X(04).                          
                 04 CIA-LEN            PICTURE S9(04)          COMP.            
                 04 CIA-TYP            PICTURE  X(02).                          
                    88 CIA-TYP-DATA    VALUE 'DA', 'DB'.                        
                    88 CIA-TYP-ABOVE-16M                                        
                                       VALUE 'DA'.                              
                    88 CIA-TYP-BELOW-16M                                        
                                       VALUE 'DB'.                              
                    88 CIA-TYP-IO      VALUE 'IO'.                              
                    88 CIA-TYP-STATIC  VALUE 'SA'.                              
                    88 CIA-TYP-TEMPSTG VALUE 'TS'.                              
                 04 CIA-PTR            POINTER.                                 
                 04 CIA-MVO            PICTURE S9(04)          COMP.            
                 04 FILLER             PICTURE  X(02).                          
