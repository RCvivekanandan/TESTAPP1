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
           05  COMMAREA-ALL-LEV-TAB-RECORD.                                     
      ****************************************************************          
      *                                                              *          
      *   G2ALCKEC                                                   *          
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
      * D185   10/23/89  GDM  CREATED.                                *    CL*96
      * 11154  10/19/89  NE   CHANGE INCLUDE EXCLUDE FIELD AS 1 BYTE  *         
      * D12009 08/16/91  GDM  INCREASE GCA-FAM-REL-LVL TO 2 POSITION  *         
      * D12009 08/26/91  BSO  DECREASE FILLER BY 1 BYTE TO COMPENSATE *         
      *                       FOR FRL EXPANSION                       *         
      * 14726/ 08/13/97  AB   NEW LENGTH = 150                        *         
      * 15057                 ADDED GCA-PLAN-CODE                     *         
      *                             GCA-PKG-CODE                      *         
      *   MILL. + TEXAS       CHANGED GRP-NUMBER TO X(09)             *         
      *                               SECTION-NUM   X(5)              *         
      *                               EFF-DATE      S9(7)             *         
      * 14726/ 10/13/97  FRY  CHANGED FIELD, GCA-EFFECTIVE-DATE,      *         
      * 15057                  FROM PIC X(08) TO PIC X(06).           *         
      *                                                               *         
      *****************************************************************         
             10  GCA-CONT-ID.                                                   
               15  GCA-PLAN-CODE                     PIC X(3).                  
               15  GCA-GROUP-NUM.                                               
                   20  GCA-GROUP-NO-1-3              PIC X(3).                  
                   20  GCA-GRP-NO                    PIC X(6).                  
               15  GCA-SECTION-NUM.                                             
                   20  GCA-SEC-NO-1                  PIC X.                     
                   20  GCA-SECTN-NO                  PIC X(4).                  
               15  GCA-PKG-CODE                      PIC X(3).                  
               15  GCA-L-O-B                         PIC X.                     
               15  GCA-PROV-CTL                      PIC XX.                    
               15  GCA-FAM-REL-LVL                   PIC XX.                    
               15  GCA-EFFECTIVE-DT.                                            
                   20  GCA-EFFDT-CC                  PIC X.                     
                   20  GCA-EFF-DT            COMP-3  PIC S9(5).                 
               15  GCA-EFFDT-CEN REDEFINES                                      
                    GCA-EFFECTIVE-DT         COMP-3  PIC S9(7).                 
      *                                                                         
             10  GCA-EFFECTIVE-DATE                  PIC X(6).                  
             10  GCA-BEN-PROV-ID                     PIC X(6).                  
             10  GCA-BEN-PROV-SLOT-NO                PIC X(7).                  
      *                                                                         
             10  GCA-ALL-LEVEL-TAB-ID                PIC X(6).                  
             10  GCA-ALL-LEVEL-TAB-SLOT              PIC X(7).                  
             10  GCA-ALL-LEVEL-TAB-FUNC-CODE         PIC X(4).                  
      *                                                                         
             10  GCA-INTERNAL-TAB-ID                 PIC X(6).                  
             10  GCA-INTERNAL-TAB-SLOT               PIC X(7).                  
             10  GCA-OCCURS-ENTRY-COUNTER            PIC X(7).                  
      *                                                                         
             10  GCA-FROM-MENU-ID                    PIC X(4).                  
             10  GCA-ADD-DEL-IND                     PIC X.                     
             10  GCA-RECORD-POINTER-COMP     COMP    PIC S9(8) SYNC.            
             10  GCA-RECORD-POINTER REDEFINES                                   
                 GCA-RECORD-POINTER-COMP     USAGE IS POINTER.                  
      *                                                                         
             10  GCA-FILLER.                                                    
                 15  GCA-I-E-INDC                       PIC X.                  
                 15  FILLER                             PIC X(53).              
