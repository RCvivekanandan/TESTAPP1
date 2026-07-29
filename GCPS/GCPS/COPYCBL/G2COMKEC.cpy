000100     05  COMMAREA-KEY-RECORD.                                             
000200****************************************************************          
000300******************************************************************        
000400*****************************************************************         
000500**************          N O T E         *************************         
000600*                                                                         
000700*   THIS COPYLIB MEMBER IS USED IN BOTH OS COBOL AND VS COBOL II          
000800* PROGRAMS. ANY MODIFICATIONS MUST BE MADE TO BOTH MEMBERS.               
000900* VS COBOL II MEMBERS ARE PREFIXED BY \
001000*****************************************************************         
001100*****************************************************************         
001101*****************************************************************         
001102*                                                              *          
001103*   THIS COPY MEMBER IS USED IN THE GENERIC CONTRACT SYSTEM.   *          
001104*                                                              *          
001105*   IT IS USED IN THE INQUIRY PORTION OF THE SYSTEM AND WILL   *          
001106*   CONTAIN RECORD KEY FIELDS AND A POINTER TO A RECORD.       *          
001107*                                                              *          
001108*  LENGTH IS 150 BYTES BECAUSE GI-RECORD-POINTER IS COMP SYNC. *          
001109*                                                              *          
001110*   COPY MEMBER NAME - G2COMKEC                                *          
001111*                                                              *          
001112*   THIS VERSION IS FOR COBOL II - IF THIS RDW IS CHANGED      *          
001113*        PLEASE CHANGE ALL VERSIONS OF THIS RDW.               *          
001114*                                                              *          
      ****************************************************************          
      *                                                              *          
      * ============================================================ *          
      * CHG NUM    DATE     WHO             DESCRIPTION              *          
      * -------    -----    ---    --------------------------------  *          
      *                                                              *          
      * ADD REDEFINES FOR DCCTABLE FIELDS USED   FCG     02/19/88    *          
      * D-204   ADD NEW FIELD, GI-RETURN-CODE    FRY      6/13/89    *          
      * D12009 INCREASE FAM-REL-LVL TO 2         GDM     08/16/91    *          
      *         POSITION                                             *          
      *14726/15057 8-22-97  AB     ADDED PLAN-CODE                              
      *                                  PKG-CODE                               
      *                            CHANGED EFF-DATE TO COMP-3 S9(7)             
      * 14726/ 10/14/97  FRY  CHANGED FIELD, GI-EFFECTIVE-DATE,       *         
      * 15057                  FROM PIC X(08) TO PIC X(06).           *         
      *                                                               *         
      *                                                               *         
001125*****************************************************************         
001200       10  GIC-CONTRACT-ID.                                               
001310         15 GIC-PLAN-CODE                     PIC X(3).                   
001320         15 GIC-GROUP-NUM.                                                
001330            17 GIC-GROUP-NO-1-3               PIC X(3).                   
001340            17  GIC-GRP-NO                    PIC X(6).                   
001350         15 GIC-SECTION-NUM.                                              
001360            17 GIC-SEC-NO-1                   PIC X(1).                   
001370            17  GIC-SECTN-NO                  PIC X(4).                   
001380         15 GIC-PKG-CODE                      PIC X(3).                   
001500         15  GIC-L-O-B                         PIC X.                     
001600         15  GIC-PROV-CTL                      PIC XX.                    
001700         15  GIC-FAM-REL-LVL                   PIC XX.                    
001710         15 GIC-EFFECTIVE-DATE.                                           
001720            17 GIC-EFFDT-CC                   PIC X.                      
001730            17 GIC-EFF-DT             COMP-3  PIC S9(5).                  
001740         15 GIC-EFFDT-CEN REDEFINES GIC-EFFECTIVE-DATE                    
001750                                      COMP-3  PIC S9(7).                  
001900***************************************************************           
002000       10  GICT-CONTRACT-TABULAR-ID.                                      
002100         15  GICT-TABULAR-ID                   PIC X(6).                  
002200         15  GICT-SLOT-NUMBER          COMP-3  PIC S9(7).                 
002201       10  DCCTABLE-TABULAR-ID  REDEFINES                                 
002202                                GICT-CONTRACT-TABULAR-ID.                 
002203         15  DCC-TABULAR-ID                    PIC X(6).                  
002204         15  DCC-TABULAR-SLOT-NUMBER   COMP-3  PIC S9(7).                 
002300**************************************************************            
002400       10  GICB-BENEFIT-PROVISION-ID.                                     
002500         15  GICB-BEN-PROV-ID                  PIC X(6).                  
002600         15  GICB-SLOT-NUMBER          COMP-3  PIC S9(7).                 
002601       10  DCCTABLE-TABLE-ID    REDEFINES                                 
002602                                GICB-BENEFIT-PROVISION-ID.                
002603         15  DCC-TABLE-ID                      PIC X(6).                  
002604         15  DCC-TABLE-SLOT-NUMBER     COMP-3  PIC S9(7).                 
002700**************************************************************            
002800       10  GICBT-BENEFIT-PROVISION-TAB-ID.                                
002900         15  GICBT-TABULAR-ID                  PIC X(6).                  
003000         15  GICBT-SLOT-NUMBER         COMP-3  PIC S9(7).                 
003100**************************************************************            
003200       10  GIG-GROUP-SPECIFIC-ID.                                         
003210         15 GIG-PLAN-CODE                     PIC X(3).                   
003220         15 GIG-GROUP-NUM.                                                
003230            17 GIG-GROUP-NO-1-3               PIC X(3).                   
003240            17  GIG-GRP-NO                    PIC X(6).                   
003250         15 GIG-SECTION-NUM.                                              
003260            17 GIG-SEC-NO-1                   PIC X(1).                   
003270            17  GIG-SECTN-NO                  PIC X(4).                   
003280         15 GIG-PKG-CODE                      PIC X(3).                   
003500         15  GIG-FAM-REL-LVL                   PIC XX.                    
003510         15 GIG-EFFECTIVE-DATE.                                           
003520            17 GIG-EFFDT-CC                   PIC X.                      
003530            17 GIG-EFF-DT             COMP-3  PIC S9(5).                  
003540         15 GIG-EFFDT-CEN REDEFINES GIG-EFFECTIVE-DATE                    
003550                                      COMP-3  PIC S9(7).                  
003700**************************************************************            
003800       10  GIGT-GROUP-SPECIFIC-TABULAR-ID.                                
003900         15  GIGT-TABULAR-ID                   PIC X(6).                  
004000         15  GIGT-SLOT-NUMBER          COMP-3  PIC S9(7).                 
004100***************************************************************           
004200       10  GI-MULT-PATH-ID                     PIC X(4).                  
004300       10  GI-EFFECTIVE-DATE                   PIC X(6).                  
004350*****  NECESSARY FOR PROPER ALIGNMENT                                     
004400       10  GI-RECORD-POINTER-COMP   COMP SYNC  PIC 9(8).                  
004401       10  GI-RECORD-POINTER     REDEFINES                                
004402           GI-RECORD-POINTER-COMP              POINTER.                   
004500       10  GI-RETURN-CODE                      PIC X(2).                  
004501           88  GI-GOOD-RETURN                VALUE '00'.                  
004502       10  GI-FILLER                           PIC X(36).                 
