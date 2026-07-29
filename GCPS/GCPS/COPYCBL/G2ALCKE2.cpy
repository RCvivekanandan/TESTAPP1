      ****************************************************************          
      ******************************************************************        
      *****************************************************************         
      **************          N O T E         *************************         
      * LENGTH =150                                                             
      *   THIS COPYLIB MEMBER IS USED IN BOTH OS COBOL AND VS COBOL II          
      * PROGRAMS. ANY MODIFICATIONS MUST BE MADE TO BOTH MEMBERS.               
      * VS COBOL II MEMBERS ARE PREFIXED BY \
      *****************************************************************         
      *****************************************************************         
      *****************************************************************         
           05  COMMAREA2-ALL-LEV-TAB-RECORD.                                    
      ****************************************************************          
      *                                                              *          
      *   G2ALCKE2                                                   *          
      *                                                              *          
      *   THIS COPY MEMBER IS USED IN THE GENERIC CONTRACT SYSTEM.   *          
      *                                                              *          
      *   IT IS ONLY USED IN ALL LEVEL AND INTERNAL TABULAR PROGRAMS *          
      *                                                              *          
      ****************************************************************          
      *****************************************************************         
      *                                                               *         
      *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        *         
      *    *-*         U P D A T E   H I S T O R Y         *-*        *         
      *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        *         
      *                                                               *         
      * CHG #    DATE    BY              DESCRIPTION                  *    CL*96
      * _____  ________  ___  ___________________________________     *    CL*96
      *                                                               *    CL*96
      * D185   10/26/89  GDM  CREATED.                                *    CL*96
      * 11154  10/19/90  NE   CHANGE INCLUDE EXCLUDE FIELD TO 1 BYTE. *         
      * D12009 08/16/91  GDM  INCREASE GCA2-FAM-REL-LVL TO 2 POSITION *         
      * D12009 08/26/91  BSO  DECREASE FILLER BY 1 BYTE TO COMPENSATE *         
      *                       FOR FRL EXPANSION                       *         
      * 14726/ 08/13/97  AB   NEW LENGTH = 150                        *         
      * 15057                 ADDED GCA2-PLAN-CODE                    *         
      *                             GCA2-PKG-CODE                     *         
      *   MILL. + TEXAS       CHANGED GRP-NUMBER TO X(09)             *         
      *                               SECTION-NUM   X(5)              *         
      *                               EFF-DATE      S9(7)             *         
      * 14726/ 10/13/97  FRY  CHANGED FIELD, GCA2-EFFECTIVE-DATE,     *         
      * 15057                  FROM PIC X(08) TO PIC X(06).           *         
      *                                                               *         
      *****************************************************************         
             10  GCA2-CONT-ID.                                                  
               15  GCA2-PLAN-CODE                    PIC X(3).                  
               15  GCA2-GROUP-NUM.                                              
                   20  GCA2-GROUP-NO-1-3             PIC X(3).                  
                   20  GCA2-GRP-NO                   PIC X(6).                  
               15  GCA2-SECTION-NUM.                                            
                   20  GCA2-SEC-NO-1                 PIC X.                     
                   20  GCA2-SECTN-NO                 PIC X(4).                  
               15  GCA2-PKG-CODE                     PIC X(3).                  
               15  GCA2-L-O-B                         PIC X.                    
               15  GCA2-PROV-CTL                      PIC XX.                   
               15  GCA2-FAM-REL-LVL                   PIC XX.                   
               15  GCA2-EFFECTIVE-DT.                                           
                   20  GCA2-EFFDT-CC                      PIC X.                
                   20  GCA2-EFF-DT           COMP-3  PIC S9(5).                 
               15  GCA2-EFFDT-CEN REDEFINES                                     
                    GCA2-EFFECTIVE-DT        COMP-3  PIC S9(7).                 
      *                                                                         
             10  GCA2-EFFECTIVE-DATE                  PIC X(6).                 
             10  GCA2-BEN-PROV-ID                     PIC X(6).                 
             10  GCA2-BEN-PROV-SLOT-NO                PIC X(7).                 
      *                                                                         
             10  GCA2-ALL-LEVEL-TAB-ID                PIC X(6).                 
             10  GCA2-ALL-LEVEL-TAB-SLOT              PIC X(7).                 
             10  GCA2-ALL-LEVEL-TAB-FUNC-CODE         PIC X(4).                 
      *                                                                         
             10  GCA2-INTERNAL-TAB-ID                 PIC X(6).                 
             10  GCA2-INTERNAL-TAB-SLOT               PIC X(7).                 
             10  GCA2-OCCURS-ENTRY-COUNTER            PIC X(7).                 
      *                                                                         
             10  GCA2-FROM-MENU-ID                    PIC X(4).                 
             10  GCA2-ADD-DEL-IND                     PIC X.                    
             10  GCA2-RECORD-POINTER-COMP     COMP    PIC S9(8) SYNC.           
             10  GCA2-RECORD-POINTER REDEFINES                                  
                 GCA2-RECORD-POINTER-COMP     USAGE IS POINTER.                 
      *                                                                         
             10  GCA2-FILLER.                                                   
                 15  GCA2-I-E-INDC                    PIC X.                    
                 15  FILLER                           PIC X(53).                
