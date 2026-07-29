000100     05  COMMAREA2-KEY-RECORD.                                            
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
001108*  LENGTH IS 150 BYTES BECAUSE GI2-RECORD-POINTER IS COMP SYNC.*          
001109*                                                              *          
001110*   COPY MEMBER NAME - G2COMKE2                                *          
001111*                                                              *          
001112*   THIS COPYBOOK IS USED FOR COBOL II. IF CHANGES ARE MADE    *          
001113*        PLEASE MAKE THEM TO ALL OF THE RDWS.                  *          
001114*                                                              *          
001115****************************************************************          
      *                                                              *          
      * ============================================================ *          
      * CHG NUM    DATE     WHO             DESCRIPTION              *          
      * -------    -----    ---    --------------------------------  *          
      *                                                              *          
001117* ADD REDEFINES FOR DCCTABLE FIELDS USED   FCG     02/19/88    *          
001118* D-204   ADD NEW FIELD, GI2-RETURN-CODE   FRY      6/13/89    *          
001119* D12009 INCREASE FAM-REL-LVL TO 2         GDM     08/16/91    *          
001120*         POSITION                                             *          
001121*14726/15057 8-22-97 AB ADDED PLAN-CODE                                   
001122*                             PKG-CODE                                    
001123*                       CHANGED EFF-DATE TO COMP-3 S9(7)                  
      * 14726/ 10/14/97  FRY  CHANGED FIELD, GI2-EFFECTIVE-DATE,      *         
      * 15057                  FROM PIC X(08) TO PIC X(06).           *         
      *                                                               *         
      *                                                               *         
001126*****************************************************************         
001200       10  GIC2-CONTRACT-ID.                                              
001210         15 GIC2-PLAN-CODE                    PIC X(3).                   
001220         15 GIC2-GROUP-NUM.                                               
001230            17 GIC2-GROUP-NO-1-3              PIC X(3).                   
001240            17  GIC2-GRP-NO                   PIC X(6).                   
001250         15 GIC2-SECTION-NUM.                                             
001260            17 GIC2-SEC-NO-1                  PIC X(1).                   
001270            17  GIC2-SECTN-NO                 PIC X(4).                   
001280         15 GIC2-PKG-CODE                     PIC X(3).                   
001500         15  GIC2-L-O-B                        PIC X.                     
001600         15  GIC2-PROV-CTL                     PIC XX.                    
001700         15  GIC2-FAM-REL-LVL                  PIC XX.                    
001710         15 GIC2-EFFECTIVE-DATE.                                          
001720            17 GIC2-EFFDT-CC                  PIC X.                      
001730            17 GIC2-EFF-DT            COMP-3  PIC S9(5).                  
001740         15 GIC2-EFFDT-CEN REDEFINES GIC2-EFFECTIVE-DATE                  
001750                                      COMP-3  PIC S9(7).                  
001900***************************************************************           
002000       10  GICT2-CONTRACT-TABULAR-ID.                                     
002100         15  GICT2-TABULAR-ID                  PIC X(6).                  
002200         15  GICT2-SLOT-NUMBER         COMP-3  PIC S9(7).                 
002201       10  DCCTABLE2-TABULAR-ID  REDEFINES                                
002202                                GICT2-CONTRACT-TABULAR-ID.                
002203         15  DCC2-TABULAR-ID                   PIC X(6).                  
002204         15  DCC2-TABULAR-SLOT-NUMBER   COMP-3 PIC S9(7).                 
002300**************************************************************            
002400       10  GICB2-BENEFIT-PROVISION-ID.                                    
002500         15  GICB2-BEN-PROV-ID                 PIC X(6).                  
002600         15  GICB2-SLOT-NUMBER         COMP-3  PIC S9(7).                 
002601       10  DCCTABLE2-TABLE-ID    REDEFINES                                
002602                                GICB2-BENEFIT-PROVISION-ID.               
002603         15  DCC2-TABLE-ID                     PIC X(6).                  
002604         15  DCC2-TABLE-SLOT-NUMBER     COMP-3 PIC S9(7).                 
002700**************************************************************            
002800       10  GICBT2-BENEFIT-PROV-TAB-ID.                                    
002900         15  GICBT2-TABULAR-ID                 PIC X(6).                  
003000         15  GICBT2-SLOT-NUMBER        COMP-3  PIC S9(7).                 
003100**************************************************************            
003200       10  GIG2-GROUP-SPECIFIC-ID.                                        
003210         15 GIG2-PLAN-CODE                    PIC X(3).                   
003220         15 GIG2-GROUP-NUM.                                               
003230            17 GIG2-GROUP-NO-1-3              PIC X(3).                   
003240            17  GIG2-GRP-NO                   PIC X(6).                   
003250         15 GIG2-SECTION-NUM.                                             
003260            17 GIG2-SEC-NO-1                  PIC X(1).                   
003270            17  GIG2-SECTN-NO                 PIC X(4).                   
003280         15 GIG2-PKG-CODE                     PIC X(3).                   
003410         15  GIG2-FAM-REL-LVL                  PIC XX.                    
003420         15 GIG2-EFFECTIVE-DATE.                                          
003430            17 GIG2-EFFDT-CC                  PIC X.                      
003440            17 GIG2-EFF-DT            COMP-3  PIC S9(5).                  
003450         15 GIG2-EFFDT-CEN REDEFINES GIG2-EFFECTIVE-DATE                  
003460                                      COMP-3  PIC S9(7).                  
003600**************************************************************            
003700       10  GIGT2-GROUP-SPECIFIC-TAB-ID.                                   
003800         15  GIGT2-TABULAR-ID                  PIC X(6).                  
003900         15  GIGT2-SLOT-NUMBER         COMP-3  PIC S9(7).                 
004000***************************************************************           
004100       10  GI2-MULT-PATH-ID                    PIC X(4).                  
004200       10  GI2-EFFECTIVE-DATE                  PIC X(6).                  
004300       10  GI2-RECORD-POINTER-COMP  COMP SYNC  PIC 9(8).                  
004301       10  GI2-RECORD-POINTER       REDEFINES                             
004302           GI2-RECORD-POINTER-COMP             POINTER.                   
004400       10  GI2-RETURN-CODE                     PIC X(2).                  
004401           88  GI2-GOOD-RETURN               VALUE '00'.                  
004402       10  GI2-FILLER                          PIC X(36).                 
