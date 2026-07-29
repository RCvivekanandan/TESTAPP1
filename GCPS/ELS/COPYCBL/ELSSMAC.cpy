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
      *    COPYBOOK:   ELSSMAC                                         *        
      *    DATE:       17-JUN-1986                                     *        
      *    AUTHOR:     EDWARD G. LISS                                  *        
      *    FUNCTION:   STORAGE MANAGEMENT AREA FOR ELS                 *        
      *                                                                *        
      *                CONTAINS A TABLE LISTING ALL THE VALID ELS      *        
      *                AREAS TO ENFORCE COMMON LOCATION OF STORAGE     *        
      *                TABLES.                                         *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 17-JUN-1988 EGL       CREATED                            *        
      * 01.10 21-JUL-1992 AKK       ADDED PMCCOMM AS DA TO HANDLE      *        
      *                             NOTICE OF ADMISSION/ELIGIBILITY    *        
      *                             SUMMARY.                           *        
      *                             INCREASE # OF OCCURENCES TO 69.    *        
      * 01.11 22-JUL-1992 AKK       ADDED ELSPMCID TO HOLD BP S AND    *        
      *                             OTHER INTERMEDIATE RESULTS FOR     *        
      *                             NA/ES.                             *        
      *                             INCREASE # OF OCCURENCES TO 70.    *        
      * 01.12 15-OCT-1992 BAK       ADDED ELSIDGD AND ELSIPGP AND      *        
      *                             INCREASE TO 72.                    *        
      * 01.13 02-AUG-1993 BAK       ADDED GCPRVTB2 FOR GVL TABULARS &  *        
      *                             INCREASE TO 73 OCCURANCES.         *        
      * 01.14 23-SEP-1993 AKK       ADDED ELSSLNR  FOR GVL TABULARS &  *        
      *                             INCREASE TO 74 OCCURANCES.         *        
      * 01.15 14-FEB-1995 AKK       CHANGED MVO FOR GCDATES TO 396  &  *        
      *                             GCCONTR TO 755.                    *        
      * 01.16 19-AUG-1997 AKK       CHANGED MVO FOR VARIOUS COPY       *        
      *                             MEMBERS                            *        
      * 01.17 17-AUG-2000 AKK       ADD SUPORT FOR IPGS                         
      * 01.18 22-SEP-2000  JP       ADD ELSRLMED FOR REALMED                    
      *                                                                *        
      ******************************************************************        
                                                                                
       01  SMA-STORAGE-MANAGEMENT-AREA.                                         
           02 SMA-NUMBER-OF-ENTRIES  PICTURE S9(4) COMP VALUE +76.              
                                                                                
           02 SMA-DEFINITION.                                                   
                                                                                
              03 SMA-ELSSMA.                                                    
              04 SMA-ELSSMA-DDN   PICTURE  X(08)      VALUE ' ELS SMA'.         
              04 SMA-ELSSMA-LEN   PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSSMA-TYP   PICTURE  X(02)      VALUE 'SA'.               
              04 SMA-ELSSMA-PTR   POINTER             VALUE NULL.               
              04 SMA-ELSSMA-MVO   PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCIA.                                                    
              04 SMA-ELSCIA-DDN   PICTURE  X(08)      VALUE ' ELSCIA '.         
              04 SMA-ELSCIA-LEN   PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCIA-TYP   PICTURE  X(02)      VALUE 'SA'.               
              04 SMA-ELSCIA-PTR   POINTER             VALUE NULL.               
              04 SMA-ELSCIA-MVO   PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-COBXIO.                                                    
              04 SMA-COBXIO-DDN   PICTURE  X(08)      VALUE 'COBXIO  '.         
              04 SMA-COBXIO-LEN   PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-COBXIO-TYP   PICTURE  X(02)      VALUE 'DB'.               
              04 SMA-COBXIO-PTR   POINTER             VALUE NULL.               
              04 SMA-COBXIO-MVO   PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-DBPIOPM.                                                   
              04 SMA-DBPIOPM-DDN  PICTURE   X(08)     VALUE 'DBPIOPM'.          
              04 SMA-DBPIOPM-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-DBPIOPM-TYP  PICTURE   X(02)     VALUE 'DB'.               
              04 SMA-DBPIOPM-PTR  POINTER             VALUE NULL.               
              04 SMA-DBPIOPM-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELPCN.                                                     
              04 SMA-ELPCN-DDN    PICTURE  X(08)      VALUE 'ELPCN   '.         
              04 SMA-ELPCN-LEN    PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELPCN-TYP    PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-ELPCN-PTR    POINTER             VALUE NULL.               
              04 SMA-ELPCN-MVO    PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELPCV.                                                     
              04 SMA-ELPCV-DDN    PICTURE  X(08)      VALUE 'ELPCV   '.         
              04 SMA-ELPCV-LEN    PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELPCV-TYP    PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-ELPCV-PTR    POINTER             VALUE NULL.               
              04 SMA-ELPCV-MVO    PICTURE S9(04) COMP VALUE +0022.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELPDE.                                                     
              04 SMA-ELPDE-DDN    PICTURE  X(08)      VALUE 'ELPDE   '.         
              04 SMA-ELPDE-LEN    PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELPDE-TYP    PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-ELPDE-PTR    POINTER             VALUE NULL.               
              04 SMA-ELPDE-MVO    PICTURE S9(04) COMP VALUE +0022.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELPEN.                                                     
              04 SMA-ELPEN-DDN    PICTURE  X(08)      VALUE 'ELPEN   '.         
              04 SMA-ELPEN-LEN    PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELPEN-TYP    PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-ELPEN-PTR    POINTER             VALUE NULL.               
              04 SMA-ELPEN-MVO    PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELPGRPKY.                                                  
              04 SMA-ELPGRPKY-DDN PICTURE  X(08)      VALUE 'ELPGRPKY'.         
              04 SMA-ELPGRPKY-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELPGRPKY-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-ELPGRPKY-PTR POINTER             VALUE NULL.               
              04 SMA-ELPGRPKY-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELPMSGKY.                                                  
              04 SMA-ELPMSGKY-DDN PICTURE  X(08)      VALUE 'ELPMSGKY'.         
              04 SMA-ELPMSGKY-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELPMSGKY-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-ELPMSGKY-PTR POINTER             VALUE NULL.               
              04 SMA-ELPMSGKY-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELPRL.                                                     
              04 SMA-ELPRL-DDN    PICTURE  X(08)      VALUE 'ELPRL   '.         
              04 SMA-ELPRL-LEN    PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELPRL-TYP    PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-ELPRL-PTR    POINTER             VALUE NULL.               
              04 SMA-ELPRL-MVO    PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSATBL.                                                   
              04 SMA-ELSATBL-DDN  PICTURE  X(08)      VALUE 'ELSATBL '.         
              04 SMA-ELSATBL-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSATBL-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSATBL-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSATBL-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCMDSC.                                                  
              04 SMA-ELSCMDSC-DDN PICTURE  X(08)      VALUE 'ELSCMDSC'.         
              04 SMA-ELSCMDSC-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCMDSC-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSCMDSC-PTR POINTER             VALUE NULL.               
              04 SMA-ELSCMDSC-MVO PICTURE S9(04) COMP VALUE +5400.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCMIF.                                                   
              04 SMA-ELSCMIF-DDN  PICTURE  X(08)      VALUE 'ELSCMIF '.         
              04 SMA-ELSCMIF-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCMIF-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSCMIF-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSCMIF-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCOMM.                                                   
              04 SMA-ELSCOMM-DDN  PICTURE  X(08)      VALUE 'ELSCOMM '.         
              04 SMA-ELSCOMM-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCOMM-TYP  PICTURE  X(02)      VALUE 'SA'.               
              04 SMA-ELSCOMM-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSCOMM-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCONIB.                                                  
              04 SMA-ELSCONIB-DDN PICTURE  X(08)      VALUE 'ELSCONIB'.         
              04 SMA-ELSCONIB-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCONIB-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSCONIB-PTR POINTER             VALUE NULL.               
              04 SMA-ELSCONIB-MVO PICTURE S9(04) COMP VALUE +0755.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCONIS.                                                  
              04 SMA-ELSCONIS-DDN PICTURE  X(08)      VALUE 'ELSCONIS'.         
              04 SMA-ELSCONIS-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCONIS-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSCONIS-PTR POINTER             VALUE NULL.               
              04 SMA-ELSCONIS-MVO PICTURE S9(04) COMP VALUE +0755.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCONPB.                                                  
              04 SMA-ELSCONPB-DDN PICTURE  X(08)      VALUE 'ELSCONPB'.         
              04 SMA-ELSCONPB-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCONPB-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSCONPB-PTR POINTER             VALUE NULL.               
              04 SMA-ELSCONPB-MVO PICTURE S9(04) COMP VALUE +0755.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCONPS.                                                  
              04 SMA-ELSCONPS-DDN PICTURE  X(08)      VALUE 'ELSCONPS'.         
              04 SMA-ELSCONPS-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCONPS-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSCONPS-PTR POINTER             VALUE NULL.               
              04 SMA-ELSCONPS-MVO PICTURE S9(04) COMP VALUE +0755.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCSAC.                                                   
              04 SMA-ELSCSAC-DDN  PICTURE  X(08)      VALUE 'ELSCSA'.           
              04 SMA-ELSCSAC-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCSAC-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSCSAC-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSCSAC-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCSADC.                                                  
              04 SMA-ELSCSADC-DDN PICTURE  X(08)      VALUE 'ELSCSAD'.          
              04 SMA-ELSCSADC-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCSADC-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSCSADC-PTR POINTER             VALUE NULL.               
              04 SMA-ELSCSADC-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCSBPC.                                                  
              04 SMA-ELSCSBPC-DDN PICTURE  X(08)      VALUE 'ELSCSBP'.          
              04 SMA-ELSCSBPC-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCSBPC-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSCSBPC-PTR POINTER             VALUE NULL.               
              04 SMA-ELSCSBPC-MVO PICTURE S9(04) COMP VALUE +0050.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCSENC.                                                  
              04 SMA-ELSCSENC-DDN PICTURE  X(08)      VALUE 'ELSCSEN'.          
              04 SMA-ELSCSENC-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCSENC-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSCSENC-PTR POINTER             VALUE NULL.               
              04 SMA-ELSCSENC-MVO PICTURE S9(04) COMP VALUE +0015.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCSPG.                                                   
              04 SMA-ELSCSPG-DDN  PICTURE  X(08)      VALUE 'ELSCSPG'.          
              04 SMA-ELSCSPG-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCSPG-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSCSPG-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSCSPG-MVO  PICTURE S9(04) COMP VALUE +0050.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSCSPTC.                                                  
              04 SMA-ELSCSPTC-DDN PICTURE  X(08)      VALUE 'ELSCSPT'.          
              04 SMA-ELSCSPTC-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSCSPTC-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSCSPTC-PTR POINTER             VALUE NULL.               
              04 SMA-ELSCSPTC-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSELOG.                                                   
              04 SMA-ELSELOG-DDN PICTURE  X(08)       VALUE 'ELSELOG'.          
              04 SMA-ELSELOG-LEN PICTURE S9(04) COMP  VALUE +0000.              
              04 SMA-ELSELOG-TYP PICTURE  X(02)       VALUE 'DA'.               
              04 SMA-ELSELOG-PTR POINTER              VALUE NULL.               
              04 SMA-ELSELOG-MVO PICTURE S9(04) COMP  VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSGRPSP.                                                  
              04 SMA-ELSGRPSP-DDN PICTURE  X(08)      VALUE 'ELSGRPSP'.         
              04 SMA-ELSGRPSP-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSGRPSP-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSGRPSP-PTR POINTER             VALUE NULL.               
              04 SMA-ELSGRPSP-MVO PICTURE S9(04) COMP VALUE +0030.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSIBGR.                                                   
              04 SMA-ELSIBGR-DDN  PICTURE  X(08)      VALUE 'ELSIBGR '.         
              04 SMA-ELSIBGR-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSIBGR-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSIBGR-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSIBGR-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSIDGD.                                                   
              04 SMA-ELSIDGD-DDN  PICTURE  X(08)      VALUE 'ELSIDGD '.         
              04 SMA-ELSIDGD-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSIDGD-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSIDGD-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSIDGD-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSIOPM.                                                   
              04 SMA-ELSIOPM-DDN  PICTURE  X(08)      VALUE 'ELSIOPM '.         
              04 SMA-ELSIOPM-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSIOPM-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSIOPM-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSIOPM-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSIPGN.                                                   
              04 SMA-ELSIPGN-DDN  PICTURE  X(08)      VALUE 'ELSIPGN '.         
              04 SMA-ELSIPGN-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSIPGN-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSIPGN-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSIPGN-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSIPGP.                                                   
              04 SMA-ELSIPGP-DDN  PICTURE  X(08)      VALUE 'ELSIPGP '.         
              04 SMA-ELSIPGP-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSIPGP-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSIPGP-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSIPGP-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSIPGS.                                                   
              04 SMA-ELSIPGS-DDN  PICTURE  X(08)      VALUE 'ELSIPGS '.         
              04 SMA-ELSIPGS-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSIPGS-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSIPGS-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSIPGS-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSIPGT.                                                   
              04 SMA-ELSIPGT-DDN  PICTURE  X(08)      VALUE 'ELSIPGT '.         
              04 SMA-ELSIPGT-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSIPGT-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSIPGT-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSIPGT-MVO  PICTURE S9(04) COMP VALUE +0150.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSKEYS.                                                   
              04 SMA-ELSKEYS-DDN  PICTURE  X(08)      VALUE 'ELSKEYS '.         
              04 SMA-ELSKEYS-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSKEYS-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSKEYS-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSKEYS-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSKTBC.                                                   
              04 SMA-ELSKTBC-DDN  PICTURE  X(08)      VALUE 'ELSKTBC '.         
              04 SMA-ELSKTBC-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSKTBC-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSKTBC-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSKTBC-MVO  PICTURE S9(04) COMP VALUE +0500.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSKTBG.                                                   
              04 SMA-ELSKTBG-DDN  PICTURE  X(08)      VALUE 'ELSKTBG '.         
              04 SMA-ELSKTBG-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSKTBG-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSKTBG-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSKTBG-MVO  PICTURE S9(04) COMP VALUE +0100.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSKTBS.                                                   
              04 SMA-ELSKTBS-DDN  PICTURE  X(08)      VALUE 'ELSKTBS '.         
              04 SMA-ELSKTBS-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSKTBS-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSKTBS-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSKTBS-MVO  PICTURE S9(04) COMP VALUE +0750.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSMEMS.                                                   
              04 SMA-ELSMEMS-DDN  PICTURE  X(08)      VALUE 'ELSMEMS '.         
              04 SMA-ELSMEMS-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSMEMS-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSMEMS-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSMEMS-MVO  PICTURE S9(04) COMP VALUE +0020.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSMHDG.                                                   
              04 SMA-ELSMHDG-DDN  PICTURE  X(08)      VALUE 'ELSMHDG '.         
              04 SMA-ELSMHDG-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSMHDG-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSMHDG-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSMHDG-MVO  PICTURE S9(04) COMP VALUE +0015.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSMOPT.                                                   
              04 SMA-ELSMOPT-DDN  PICTURE  X(08)      VALUE 'ELSMOPT '.         
              04 SMA-ELSMOPT-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSMOPT-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSMOPT-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSMOPT-MVO  PICTURE S9(04) COMP VALUE +0750.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSOUTP.                                                   
              04 SMA-ELSOUTP-DDN  PICTURE  X(08)      VALUE 'ELSOUTP '.         
              04 SMA-ELSOUTP-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSOUTP-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSOUTP-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSOUTP-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSPGMW1.                                                  
              04 SMA-ELSPGMW1-DDN PICTURE  X(08)      VALUE 'ELSPGMW1'.         
              04 SMA-ELSPGMW1-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSPGMW1-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSPGMW1-PTR POINTER             VALUE NULL.               
              04 SMA-ELSPGMW1-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSPGMW2.                                                  
              04 SMA-ELSPGMW2-DDN PICTURE  X(08)      VALUE 'ELSPGMW2'.         
              04 SMA-ELSPGMW2-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSPGMW2-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSPGMW2-PTR POINTER             VALUE NULL.               
              04 SMA-ELSPGMW2-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSPGMW3.                                                  
              04 SMA-ELSPGMW3-DDN PICTURE  X(08)      VALUE 'ELSPGMW3'.         
              04 SMA-ELSPGMW3-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSPGMW3-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSPGMW3-PTR POINTER             VALUE NULL.               
              04 SMA-ELSPGMW3-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSPGMW4.                                                  
              04 SMA-ELSPGMW4-DDN PICTURE  X(08)      VALUE 'ELSPGMW4'.         
              04 SMA-ELSPGMW4-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSPGMW4-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSPGMW4-PTR POINTER             VALUE NULL.               
              04 SMA-ELSPGMW4-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSPLGSW.                                                  
              04 SMA-ELSPLGSW-DDN PICTURE  X(08)      VALUE 'ELSPLGSW'.         
              04 SMA-ELSPLGSW-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSPLGSW-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSPLGSW-PTR POINTER             VALUE NULL.               
              04 SMA-ELSPLGSW-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSPLGTB.                                                  
              04 SMA-ELSPLGTB-DDN PICTURE  X(08)      VALUE 'ELSPLGTB'.         
              04 SMA-ELSPLGTB-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSPLGTB-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSPLGTB-PTR POINTER             VALUE NULL.               
              04 SMA-ELSPLGTB-MVO PICTURE S9(04) COMP VALUE +0025.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSPMCID.                                                  
              04 SMA-ELSPMCID-DDN PICTURE  X(08)      VALUE 'ELSPMCID'.         
              04 SMA-ELSPMCID-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSPMCID-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSPMCID-PTR POINTER             VALUE NULL.               
              04 SMA-ELSPMCID-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSPRVN.                                                   
              04 SMA-ELSPRVN-DDN  PICTURE  X(08)      VALUE 'ELSPRVN '.         
              04 SMA-ELSPRVN-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSPRVN-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSPRVN-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSPRVN-MVO  PICTURE S9(04) COMP VALUE +0050.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSMENU.                                                   
              04 SMA-ELSMENU-DDN  PICTURE  X(08)      VALUE 'ELSQMENU'.         
              04 SMA-ELSMENU-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSMENU-TYP  PICTURE  X(02)      VALUE 'TS'.               
              04 SMA-ELSMENU-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSMENU-MVO  PICTURE S9(04) COMP VALUE +0003.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSPAGE.                                                   
              04 SMA-ELSPAGE-DDN  PICTURE  X(08)      VALUE 'ELSQPAGE'.         
              04 SMA-ELSPAGE-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSPAGE-TYP  PICTURE  X(02)      VALUE 'TS'.               
              04 SMA-ELSPAGE-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSPAGE-MVO  PICTURE S9(04) COMP VALUE +0023.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSRLMED.                                                  
              04 SMA-ELSRLMED-DDN PICTURE  X(08)      VALUE 'ELSRLMED'.         
              04 SMA-ELSRLMED-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSRLMED-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSRLMED-PTR POINTER             VALUE NULL.               
              04 SMA-ELSRLMED-MVO PICTURE S9(04) COMP VALUE +0099.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSRRBL.                                                   
              04 SMA-ELSRRBL-DDN  PICTURE  X(08)      VALUE 'ELSRRBL '.         
              04 SMA-ELSRRBL-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSRRBL-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSRRBL-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSRRBL-MVO  PICTURE S9(04) COMP VALUE +0100.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSSLNR.                                                   
              04 SMA-ELSSLNR-DDN PICTURE   X(08)      VALUE 'ELSSLNR '.         
              04 SMA-ELSSLNR-LEN PICTURE  S9(04) COMP VALUE +0600.              
              04 SMA-ELSSLNR-TYP PICTURE   X(02)      VALUE 'DA'.               
              04 SMA-ELSSLNR-PTR POINTER              VALUE NULL.               
              04 SMA-ELSSLNR-MVO PICTURE  S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSSRTP.                                                   
              04 SMA-ELSSRTP-DDN  PICTURE  X(08)      VALUE 'ELSSRTP '.         
              04 SMA-ELSSRTP-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSSRTP-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSSRTP-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSSRTP-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSSSCB.                                                   
              04 SMA-ELSSSCB-DDN  PICTURE  X(08)      VALUE 'ELSSSCB'.          
              04 SMA-ELSSSCB-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSSSCB-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSSSCB-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSSSCB-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSTCWA.                                                   
              04 SMA-ELSTCWA-DDN  PICTURE  X(08)      VALUE 'ELSTCWA'.          
              04 SMA-ELSTCWA-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSTCWA-TYP  PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-ELSTCWA-PTR  POINTER             VALUE NULL.               
              04 SMA-ELSTCWA-MVO  PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSTWA.                                                    
              04 SMA-ELSTWA-DDN   PICTURE  X(08)      VALUE 'ELSTWA '.          
              04 SMA-ELSTWA-LEN   PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSTWA-TYP   PICTURE  X(02)      VALUE 'SA'.               
              04 SMA-ELSTWA-PTR   POINTER             VALUE NULL.               
              04 SMA-ELSTWA-MVO   PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSWKFL1.                                                  
              04 SMA-ELSWKFL1-DDN PICTURE  X(08)      VALUE 'ELSWKFL1'.         
              04 SMA-ELSWKFL1-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSWKFL1-TYP PICTURE  X(02)      VALUE 'TS'.               
              04 SMA-ELSWKFL1-PTR POINTER             VALUE NULL.               
              04 SMA-ELSWKFL1-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSWKFL2.                                                  
              04 SMA-ELSWKFL2-DDN PICTURE  X(08)      VALUE 'ELSWKFL2'.         
              04 SMA-ELSWKFL2-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSWKFL2-TYP PICTURE  X(02)      VALUE 'TS'.               
              04 SMA-ELSWKFL2-PTR POINTER             VALUE NULL.               
              04 SMA-ELSWKFL2-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSWKFL3.                                                  
              04 SMA-ELSWKFL3-DDN PICTURE  X(08)      VALUE 'ELSWKFL3'.         
              04 SMA-ELSWKFL3-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSWKFL3-TYP PICTURE  X(02)      VALUE 'TS'.               
              04 SMA-ELSWKFL3-PTR POINTER             VALUE NULL.               
              04 SMA-ELSWKFL3-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-ELSWKFL4.                                                  
              04 SMA-ELSWKFL4-DDN PICTURE  X(08)      VALUE 'ELSWKFL4'.         
              04 SMA-ELSWKFL4-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-ELSWKFL4-TYP PICTURE  X(02)      VALUE 'TS'.               
              04 SMA-ELSWKFL4-PTR POINTER             VALUE NULL.               
              04 SMA-ELSWKFL4-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-GCBENPRV.                                                  
              04 SMA-GCBENPRV-DDN PICTURE  X(08)      VALUE 'GCBENPRV'.         
              04 SMA-GCBENPRV-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-GCBENPRV-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-GCBENPRV-PTR POINTER             VALUE NULL.               
              04 SMA-GCBENPRV-MVO PICTURE S9(04) COMP VALUE +0015.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-GCCONTR.                                                   
              04 SMA-GCCONTR-DDN  PICTURE  X(08)      VALUE 'GCCONTR '.         
              04 SMA-GCCONTR-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-GCCONTR-TYP  PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-GCCONTR-PTR  POINTER             VALUE NULL.               
              04 SMA-GCCONTR-MVO  PICTURE S9(04) COMP VALUE +0755.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-GCDATES.                                                   
              04 SMA-GCDATES-DDN  PICTURE  X(08)      VALUE 'GCDATES '.         
              04 SMA-GCDATES-LEN  PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-GCDATES-TYP  PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-GCDATES-PTR  POINTER             VALUE NULL.               
              04 SMA-GCDATES-MVO  PICTURE S9(04) COMP VALUE +0396.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-GCFLDVAL.                                                  
              04 SMA-GCFLDVAL-DDN PICTURE  X(08)      VALUE 'GCFLDVAL'.         
              04 SMA-GCFLDVAL-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-GCFLDVAL-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-GCFLDVAL-PTR POINTER             VALUE NULL.               
              04 SMA-GCFLDVAL-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-GCFLDVL2.                                                  
              04 SMA-GCFLDVL2-DDN PICTURE  X(08)      VALUE 'GCFLDVL2'.         
              04 SMA-GCFLDVL2-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-GCFLDVL2-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-GCFLDVL2-PTR POINTER             VALUE NULL.               
              04 SMA-GCFLDVL2-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-GCGRPSPC.                                                  
              04 SMA-GCGRPSPC-DDN PICTURE  X(08)      VALUE 'GCGRPSPC'.         
              04 SMA-GCGRPSPC-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-GCGRPSPC-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-GCGRPSPC-PTR POINTER             VALUE NULL.               
              04 SMA-GCGRPSPC-MVO PICTURE S9(04) COMP VALUE +0030.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-GCPRVTB2.                                                  
              04 SMA-GCPRVTB2-DDN PICTURE  X(08)      VALUE 'GCPRVTB2'.         
              04 SMA-GCPRVTB2-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-GCPRVTB2-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-GCPRVTB2-PTR POINTER             VALUE NULL.               
              04 SMA-GCPRVTB2-MVO PICTURE S9(04) COMP VALUE +0100.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-GCSYSTBL.                                                  
              04 SMA-GCSYSTBL-DDN PICTURE  X(08)      VALUE 'GCSYSTBL'.         
              04 SMA-GCSYSTBL-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-GCSYSTBL-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-GCSYSTBL-PTR POINTER             VALUE NULL.               
              04 SMA-GCSYSTBL-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-GCTABULR.                                                  
              04 SMA-GCTABULR-DDN PICTURE  X(08)      VALUE 'GCTABULR'.         
              04 SMA-GCTABULR-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-GCTABULR-TYP PICTURE  X(02)      VALUE 'IO'.               
              04 SMA-GCTABULR-PTR POINTER             VALUE NULL.               
              04 SMA-GCTABULR-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-PMCCOMM.                                                   
              04 SMA-PMCCOMM-DDN PICTURE   X(08)      VALUE 'PMCCOMM '.         
              04 SMA-PMCCOMM-LEN PICTURE S9(04) COMP  VALUE +0000.              
              04 SMA-PMCCOMM-TYP PICTURE   X(02)      VALUE 'DA'.               
              04 SMA-PMCCOMM-PTR POINTER              VALUE NULL.               
              04 SMA-PMCCOMM-MVO PICTURE S9(04) COMP  VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-RDMC4306.                                                  
              04 SMA-RDMC4306-DDN PICTURE  X(08)      VALUE 'RDMC4306'.         
              04 SMA-RDMC4306-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-RDMC4306-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-RDMC4306-PTR POINTER             VALUE NULL.               
              04 SMA-RDMC4306-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-RDMC4308.                                                  
              04 SMA-RDMC4308-DDN PICTURE  X(08)      VALUE 'RDMC4308'.         
              04 SMA-RDMC4308-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-RDMC4308-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-RDMC4308-PTR POINTER             VALUE NULL.               
              04 SMA-RDMC4308-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
              03 SMA-RDMC4311.                                                  
              04 SMA-RDMC4311-DDN PICTURE  X(08)      VALUE 'RDMC4311'.         
              04 SMA-RDMC4311-LEN PICTURE S9(04) COMP VALUE +0000.              
              04 SMA-RDMC4311-TYP PICTURE  X(02)      VALUE 'DA'.               
              04 SMA-RDMC4311-PTR POINTER             VALUE NULL.               
              04 SMA-RDMC4311-MVO PICTURE S9(04) COMP VALUE +0000.              
              04 FILLER           PICTURE  X(02)      VALUE SPACES.             
                                                                                
           02 SMA-STG-MGT-TABLE        REDEFINES SMA-DEFINITION.                
              03 SMA-STG-MGT-TBL       OCCURS 76 TIMES                          
                                       ASCENDING KEY SMA-DDN                    
                                       INDEXED BY SMA-STG-MGT-IDX.              
                 04 SMA-DDN            PICTURE  X(08).                          
                 04 SMA-LEN            PICTURE S9(04)          COMP.            
                 04 SMA-TYP            PICTURE  X(02).                          
                    88 SMA-TYP-DATA    VALUE 'DA', 'DB'.                        
                    88 SMA-TYP-DATA-ABOVE-16M                                   
                                       VALUE 'DA'.                              
                    88 SMA-TYP-DATA-BELOW-16M                                   
                                       VALUE 'DB'.                              
                    88 SMA-TYP-IO      VALUE 'IO'.                              
                    88 SMA-TYP-STATIC  VALUE 'SA'.                              
                    88 SMA-TYP-TEMPSTG VALUE 'TS'.                              
                 04 SMA-PTR            POINTER.                                 
                 04 SMA-MVO            PICTURE S9(04)          COMP.            
                 04 FILLER             PICTURE  X(02).                          
                                                                                
