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
      *    COPYBOOK:   ELSCIA2C                                        *        
      *    DATE:       17-JUN-1988                                     *        
      *    AUTHOR:     EDWARD G. LISS                                  *        
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
      *                THIS IS A TOTAL REWORK OF ELSCIAC TO SUPPORT    *        
      *                A MORE FLEXIBLE STORAGE MANAGEMENT.             *        
      *                                                                *        
      * **NOTE:  IF YOU ADD TO THE CIA-CONTROL-BLOCK DON'T FORGET TO   *        
      *            UPDATE THE COUNT AT THE END OF THIS MEMBER//////    *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 14-SEP-1988 EGL       CREATED ELSCIA2C                   *        
      * 01.08 21-JUL-1992 AKK       ADD PMCCOMM.  THIS WILL WORK WITH  *        
      *                             NOTICE OF ADMISSION/ELIGIBILITY    *        
      *                             SUMMARY.  INCREASED OCCURENCES TO  *        
      *                             69.                                *        
      * 01.09 22-JUL-1992 AKK       ADD ELSPMCID.  THIS WILL BE USED   *        
      *                             TO HOLD BP S FOR NA/ES AND OTHER   *        
      *                             INTERMEDIATE RESULTS. OCCURENCES   *        
      *                             ARE BING INCREASED TO 70.          *        
      * 01.10 15-OCT-1992 BAK       ADD ELSIDGD AND ELSIPGP            *        
      * 01.11 02-AUG-1993 BAK       ADD GCPRVTB2 FOR GVL TABULARS      *        
      * 01.12 22-SEP-1993 AKK       ADD ELSSLNRC TO CIA-TSQ            *        
      * 01.13 19-SEP-1993 AKK       ADDED 2 NEW ABEND CODES EL97 AND   *        
      *                             EL98.  EL97- WHEN MORE THAN 1 'KEY-*        
      *                             SEL FOUND IN ELSKTGBC TABLE.  EL98-*        
      *                             WHEN NO 'KEY-SEL' FOUND IN ELSKTBGC*        
      *                             TABLE.                             *        
      * 01.14 17-AUG-2000 AKK       ADD SUPPORT FOR IPGS TABULAR       *        
      * 01.15 22-SEP-2000  JP       ADDED ELSRLMED FOR REALMED.        *        
      *                                                                *        
      ******************************************************************        
                                                                                
       01  CIA-ELS-COMMON-INTERFACE-AREA.                                       
                                                                                
           02 CIA-CONTROL-BLOCK.                                                
              03 CIA-DDNAME            PICTURE  X(08).                          
                  88 CIA-ELSSMA-DDN                   VALUE ' ELS SMA'.         
                  88 CIA-ELSCIA-DDN                   VALUE ' ELSCIA '.         
                  88 CIA-COBXIO-DDN                   VALUE 'COBXIO  '.         
                  88 CIA-DBPIOPM-DDN                  VALUE 'DBPIOPM'.          
                  88 CIA-ELPCN-DDN                    VALUE 'ELPCN   '.         
                  88 CIA-ELPCV-DDN                    VALUE 'ELPCV   '.         
                  88 CIA-ELPDE-DDN                    VALUE 'ELPDE   '.         
                  88 CIA-ELPEN-DDN                    VALUE 'ELPEN   '.         
                  88 CIA-ELPGRPKY-DDN                 VALUE 'ELPGRPKY'.         
                  88 CIA-ELPMSGKY-DDN                 VALUE 'ELPMSGKY'.         
                  88 CIA-ELPRL-DDN                    VALUE 'ELPRL   '.         
                  88 CIA-ELSATBL-DDN                  VALUE 'ELSATBL '.         
                  88 CIA-ELSCMDSC-DDN                 VALUE 'ELSCMDSC'.         
                  88 CIA-ELSCMIF-DDN                  VALUE 'ELSCMIF '.         
                  88 CIA-ELSCOMM-DDN                  VALUE 'ELSCOMM '.         
                  88 CIA-ELSCONIB-DDN                 VALUE 'ELSCONIB'.         
                  88 CIA-ELSCONIS-DDN                 VALUE 'ELSCONIS'.         
                  88 CIA-ELSCONPB-DDN                 VALUE 'ELSCONPB'.         
                  88 CIA-ELSCONPS-DDN                 VALUE 'ELSCONPS'.         
                  88 CIA-ELSCSAC-DDN                  VALUE 'ELSCSA'.           
                  88 CIA-ELSCSADC-DDN                 VALUE 'ELSCSAD'.          
                  88 CIA-ELSCSBPC-DDN                 VALUE 'ELSCSBP'.          
                  88 CIA-ELSCSENC-DDN                 VALUE 'ELSCSEN'.          
                  88 CIA-ELSCSPG-DDN                  VALUE 'ELSCSPG'.          
                  88 CIA-ELSCSPTC-DDN                 VALUE 'ELSCSPT'.          
                  88 CIA-ELSELOG-DDN                  VALUE 'ELSELOG'.          
                  88 CIA-ELSGRPSP-DDN                 VALUE 'ELSGRPSP'.         
                  88 CIA-ELSIBGR-DDN                  VALUE 'ELSIBGR '.         
                  88 CIA-ELSIDGD-DDN                  VALUE 'ELSIDGD '.         
                  88 CIA-ELSIOPM-DDN                  VALUE 'ELSIOPM '.         
                  88 CIA-ELSIPGN-DDN                  VALUE 'ELSIPGN '.         
                  88 CIA-ELSIPGP-DDN                  VALUE 'ELSIPGP '.         
                  88 CIA-ELSIPGS-DDN                  VALUE 'ELSIPGS '.         
                  88 CIA-ELSIPGT-DDN                  VALUE 'ELSIPGT '.         
                  88 CIA-ELSKEYS-DDN                  VALUE 'ELSKEYS '.         
                  88 CIA-ELSKTBC-DDN                  VALUE 'ELSKTBC '.         
                  88 CIA-ELSKTBG-DDN                  VALUE 'ELSKTBG '.         
                  88 CIA-ELSKTBS-DDN                  VALUE 'ELSKTBS '.         
                  88 CIA-ELSMEMS-DDN                  VALUE 'ELSMEMS '.         
                  88 CIA-ELSMENU-DDN                  VALUE 'ELSQMENU'.         
                  88 CIA-ELSMHDG-DDN                  VALUE 'ELSMHDG '.         
                  88 CIA-ELSMOPT-DDN                  VALUE 'ELSMOPT '.         
                  88 CIA-ELSOUTP-DDN                  VALUE 'ELSOUTP '.         
                  88 CIA-ELSPAGE-DDN                  VALUE 'ELSQPAGE'.         
                  88 CIA-ELSPGMW1-DDN                 VALUE 'ELSPGMW1'.         
                  88 CIA-ELSPGMW2-DDN                 VALUE 'ELSPGMW2'.         
                  88 CIA-ELSPGMW3-DDN                 VALUE 'ELSPGMW3'.         
                  88 CIA-ELSPGMW4-DDN                 VALUE 'ELSPGMW4'.         
                  88 CIA-ELSPLGSW-DDN                 VALUE 'ELSPLGSW'.         
                  88 CIA-ELSPLGTB-DDN                 VALUE 'ELSPLGTB'.         
                  88 CIA-ELSPMCID-DDN                 VALUE 'ELSPMCID'.         
                  88 CIA-ELSPRVN-DDN                  VALUE 'ELSPRVN '.         
                  88 CIA-ELSRLMED-DDN                 VALUE 'ELSRLMED'.         
                  88 CIA-ELSRRBL-DDN                  VALUE 'ELSRRBL '.         
                  88 CIA-ELSSLNR-DDN                  VALUE 'ELSSLNR '.         
                  88 CIA-ELSSRTP-DDN                  VALUE 'ELSSRTP '.         
                  88 CIA-ELSSSCB-DDN                  VALUE 'ELSSSCB'.          
                  88 CIA-ELSTCWA-DDN                  VALUE 'ELSTCWA'.          
                  88 CIA-ELSTWA-DDN                   VALUE 'ELSTWA '.          
                  88 CIA-ELSWKFL1-DDN                 VALUE 'ELSWKFL1'.         
                  88 CIA-ELSWKFL2-DDN                 VALUE 'ELSWKFL2'.         
                  88 CIA-ELSWKFL3-DDN                 VALUE 'ELSWKFL3'.         
                  88 CIA-ELSWKFL4-DDN                 VALUE 'ELSWKFL4'.         
                  88 CIA-GCBENPRV-DDN                 VALUE 'GCBENPRV'.         
                  88 CIA-GCCONTR-DDN                  VALUE 'GCCONTR '.         
                  88 CIA-GCDATES-DDN                  VALUE 'GCDATES '.         
                  88 CIA-GCFLDVAL-DDN                 VALUE 'GCFLDVAL'.         
                  88 CIA-GCFLDVL2-DDN                 VALUE 'GCFLDVL2'.         
                  88 CIA-GCGRPSPC-DDN                 VALUE 'GCGRPSPC'.         
                  88 CIA-GCPRVTB2-DDN                 VALUE 'GCPRVTB2'.         
                  88 CIA-GCSYSTBL-DDN                 VALUE 'GCSYSTBL'.         
                  88 CIA-GCTABULR-DDN                 VALUE 'GCTABULR'.         
                  88 CIA-PMCCOMM-DDN                  VALUE 'PMCCOMM '.         
                  88 CIA-RDMC4306-DDN                 VALUE 'RDMC4306'.         
                  88 CIA-RDMC4308-DDN                 VALUE 'RDMC4308'.         
                  88 CIA-RDMC4311-DDN                 VALUE 'RDMC4311'.         
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
                 88 CIA-AB-ARG-NOTFND              VALUE 'EL61'.                
                 88 CIA-AB-DUP-SLCTD-KEYS          VALUE 'EL96'.                
                 88 CIA-AB-NO-KEYS-SLCTD           VALUE 'EL97'.                
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
                                       VALUE +0008.                             
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
                 88 CIA-RC-NO-DDNAME                 VALUE +0001.               
                 88 CIA-RC-PTR-NULL                  VALUE +0002, +0001.        
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
                                                                                
              03 CIA-MVO               PICTURE  S9(4) COMP.                     
                                                                                
                                                                                
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
                                                                                
                                                                                
      ******************************************************************        
      ******************************************************************        
      ********* DO NOT USE THIS TABLE -  FOR COMPATABILITY ONLY ********        
      ******************************************************************        
      ******************************************************************        
           02 CIA-STG-MGT-TABLE.                                                
                                                                                
              03 FILLER                PICTURE S9(04)          COMP             
                                       VALUE +76.                               
              03 FILLER                OCCURS 76 TIMES.                         
      ********************             ASCENDING KEY CIA-DDN                    
      ********************             INDEXED BY CIA-STG-MGT-IDX.              
                 04 FILLER             PICTURE  X(08).                          
                 04 FILLER             PICTURE S9(04)          COMP.            
                 04 FILLER             PICTURE  X(02).                          
                 04 FILLER             POINTER.                                 
                 04 FILLER             PICTURE S9(04)          COMP.            
                 04 FILLER             PICTURE  X(02).                          
      ******************************************************************        
      ******************************************************************        
      ********* DO NOT USE THIS TABLE -  FOR COMPATABILITY ONLY ********        
      ******************************************************************        
      ******************************************************************        
