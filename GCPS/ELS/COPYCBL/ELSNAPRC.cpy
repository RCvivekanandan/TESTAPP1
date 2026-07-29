      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSNAPRC                                        *        
      *    DATE:       04-MAY-1989                                     *        
      *    AUTHOR:     EDWARD G LISS                                   *        
      *    FUNCTION:   ELS ABEND PROCESSING SNAP SHOT RECORD           *        
      *                ROUTING TABLES                                  *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                    TABLE MAINTENANCE HISTORY                   *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      *  1.0  07-JUL-1989 EGL       -CREATED                           *        
      *  1.1  14-AUG-1989 EGL       -CHANGED TABLE TO ELIMINATE EL09   *        
      *                              ABENDS FROM SD REPORTS FOR ALL    *        
      *                              DDNAMES EXCEPT CIA AND IOP        *        
      *  1.2  28-SEP-1989 EGL       -CHANGED TABLE TO PRINT ELSCIA     *        
      *                              FOR SD WHEN AN EL09 ABEND OCCURS. *        
      *  1.3  23-OCT-1989 EGL       -REVISED ABEDN ROUTINE INFO FOR    *        
      *                              BOTH SD AND SSD.                  *        
      *                             -ADDED ABEND CODE EL55             *        
      *  2.0  29-JAN-1990 EGL       -REVISED TABLE STRUCTURE FOR ALL   *        
      *                              POSSIBLE ABEND CODES.             *        
      *                             -ACTIVATED EL24 AND EL25 ABENDS    *        
      *  2.1  07-FEB-1990 EGL       -REMOVED GROUP AND CONTRACT RECS   *        
      *                              FROM EL24 AND EL25 PRINT LIST     *        
      *                             -EL24 AND EL25 SUPPRESSED FROM SD  *        
      *                              REPORT                            *        
      *                                                                *        
      *    LAST GENERATED USING ELSABEND BY R360023                    *        
      *                      ON 90/02/07 AT 11:19                      *        
      *                                                                *        
      *    TABLE LAST UPDATED ON 90/02/07 AT 11.16.51                  *        
      *                       BY R360023                               *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *               FILE TAILORING SERVICES SKELETON                 *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      *  1.0  04-MAY-1989 EGL       CREATED                            *        
      *  1.1  20-OCT-1989 EGL       ADDED ABEND CODE EL55              *        
      *  2.0  26-JAN-1990 EGL       REVISED SKELETON TO HANDLE ALL     *        
      *                             POSSIBLE ABENDS FROM EL00 TO EL99  *        
      *  3.0  19-SEP-2000 AKK       ADDED SUPPORT FOR IPGS TABULAR.    *        
      *                                                                *        
      ******************************************************************        
       01  ART-ABEND-ROUTE-TABLES.                                              
           05  ART-ABEND-ROUTE-AREA.                                            
               10  FILLER           PICTURE X(4)  VALUE 'EL00'.                 
               10  FILLER           PICTURE S9(4) VALUE +1   COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL01'.                 
               10  FILLER           PICTURE S9(4) VALUE +2   COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL02'.                 
               10  FILLER           PICTURE S9(4) VALUE +3   COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL03'.                 
               10  FILLER           PICTURE S9(4) VALUE +4   COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL04'.                 
               10  FILLER           PICTURE S9(4) VALUE +5   COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL05'.                 
               10  FILLER           PICTURE S9(4) VALUE +6   COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL06'.                 
               10  FILLER           PICTURE S9(4) VALUE +7   COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL07'.                 
               10  FILLER           PICTURE S9(4) VALUE +8   COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL08'.                 
               10  FILLER           PICTURE S9(4) VALUE +9   COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL09'.                 
               10  FILLER           PICTURE S9(4) VALUE +10  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL10'.                 
               10  FILLER           PICTURE S9(4) VALUE +11  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL11'.                 
               10  FILLER           PICTURE S9(4) VALUE +12  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL12'.                 
               10  FILLER           PICTURE S9(4) VALUE +13  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL13'.                 
               10  FILLER           PICTURE S9(4) VALUE +14  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL14'.                 
               10  FILLER           PICTURE S9(4) VALUE +15  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL15'.                 
               10  FILLER           PICTURE S9(4) VALUE +16  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL16'.                 
               10  FILLER           PICTURE S9(4) VALUE +17  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL17'.                 
               10  FILLER           PICTURE S9(4) VALUE +18  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL18'.                 
               10  FILLER           PICTURE S9(4) VALUE +19  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL19'.                 
               10  FILLER           PICTURE S9(4) VALUE +20  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL20'.                 
               10  FILLER           PICTURE S9(4) VALUE +21  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL21'.                 
               10  FILLER           PICTURE S9(4) VALUE +22  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL22'.                 
               10  FILLER           PICTURE S9(4) VALUE +23  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL23'.                 
               10  FILLER           PICTURE S9(4) VALUE +24  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL24'.                 
               10  FILLER           PICTURE S9(4) VALUE +25  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL25'.                 
               10  FILLER           PICTURE S9(4) VALUE +26  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL26'.                 
               10  FILLER           PICTURE S9(4) VALUE +27  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL27'.                 
               10  FILLER           PICTURE S9(4) VALUE +28  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL28'.                 
               10  FILLER           PICTURE S9(4) VALUE +29  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL29'.                 
               10  FILLER           PICTURE S9(4) VALUE +30  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL30'.                 
               10  FILLER           PICTURE S9(4) VALUE +31  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL31'.                 
               10  FILLER           PICTURE S9(4) VALUE +32  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL32'.                 
               10  FILLER           PICTURE S9(4) VALUE +33  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL33'.                 
               10  FILLER           PICTURE S9(4) VALUE +34  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL34'.                 
               10  FILLER           PICTURE S9(4) VALUE +35  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL35'.                 
               10  FILLER           PICTURE S9(4) VALUE +36  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL36'.                 
               10  FILLER           PICTURE S9(4) VALUE +37  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL37'.                 
               10  FILLER           PICTURE S9(4) VALUE +38  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL38'.                 
               10  FILLER           PICTURE S9(4) VALUE +39  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL39'.                 
               10  FILLER           PICTURE S9(4) VALUE +40  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL40'.                 
               10  FILLER           PICTURE S9(4) VALUE +41  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL41'.                 
               10  FILLER           PICTURE S9(4) VALUE +42  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL42'.                 
               10  FILLER           PICTURE S9(4) VALUE +43  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL43'.                 
               10  FILLER           PICTURE S9(4) VALUE +44  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL44'.                 
               10  FILLER           PICTURE S9(4) VALUE +45  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL45'.                 
               10  FILLER           PICTURE S9(4) VALUE +46  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL46'.                 
               10  FILLER           PICTURE S9(4) VALUE +47  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL47'.                 
               10  FILLER           PICTURE S9(4) VALUE +48  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL48'.                 
               10  FILLER           PICTURE S9(4) VALUE +49  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL49'.                 
               10  FILLER           PICTURE S9(4) VALUE +50  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL50'.                 
               10  FILLER           PICTURE S9(4) VALUE +51  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL51'.                 
               10  FILLER           PICTURE S9(4) VALUE +52  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL52'.                 
               10  FILLER           PICTURE S9(4) VALUE +53  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL53'.                 
               10  FILLER           PICTURE S9(4) VALUE +54  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL54'.                 
               10  FILLER           PICTURE S9(4) VALUE +55  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL55'.                 
               10  FILLER           PICTURE S9(4) VALUE +56  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL56'.                 
               10  FILLER           PICTURE S9(4) VALUE +57  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL57'.                 
               10  FILLER           PICTURE S9(4) VALUE +58  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL58'.                 
               10  FILLER           PICTURE S9(4) VALUE +59  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL59'.                 
               10  FILLER           PICTURE S9(4) VALUE +60  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL60'.                 
               10  FILLER           PICTURE S9(4) VALUE +61  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL61'.                 
               10  FILLER           PICTURE S9(4) VALUE +62  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL62'.                 
               10  FILLER           PICTURE S9(4) VALUE +63  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL63'.                 
               10  FILLER           PICTURE S9(4) VALUE +64  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL64'.                 
               10  FILLER           PICTURE S9(4) VALUE +65  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL65'.                 
               10  FILLER           PICTURE S9(4) VALUE +66  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL66'.                 
               10  FILLER           PICTURE S9(4) VALUE +67  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL67'.                 
               10  FILLER           PICTURE S9(4) VALUE +68  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL68'.                 
               10  FILLER           PICTURE S9(4) VALUE +69  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL69'.                 
               10  FILLER           PICTURE S9(4) VALUE +70  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL70'.                 
               10  FILLER           PICTURE S9(4) VALUE +71  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL71'.                 
               10  FILLER           PICTURE S9(4) VALUE +72  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL72'.                 
               10  FILLER           PICTURE S9(4) VALUE +73  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL73'.                 
               10  FILLER           PICTURE S9(4) VALUE +74  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL74'.                 
               10  FILLER           PICTURE S9(4) VALUE +75  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL75'.                 
               10  FILLER           PICTURE S9(4) VALUE +76  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL76'.                 
               10  FILLER           PICTURE S9(4) VALUE +77  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL77'.                 
               10  FILLER           PICTURE S9(4) VALUE +78  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL78'.                 
               10  FILLER           PICTURE S9(4) VALUE +79  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL79'.                 
               10  FILLER           PICTURE S9(4) VALUE +80  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL80'.                 
               10  FILLER           PICTURE S9(4) VALUE +81  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL81'.                 
               10  FILLER           PICTURE S9(4) VALUE +82  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL82'.                 
               10  FILLER           PICTURE S9(4) VALUE +83  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL83'.                 
               10  FILLER           PICTURE S9(4) VALUE +84  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL84'.                 
               10  FILLER           PICTURE S9(4) VALUE +85  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL85'.                 
               10  FILLER           PICTURE S9(4) VALUE +86  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL86'.                 
               10  FILLER           PICTURE S9(4) VALUE +87  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL87'.                 
               10  FILLER           PICTURE S9(4) VALUE +88  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL88'.                 
               10  FILLER           PICTURE S9(4) VALUE +89  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL89'.                 
               10  FILLER           PICTURE S9(4) VALUE +90  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL90'.                 
               10  FILLER           PICTURE S9(4) VALUE +91  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL91'.                 
               10  FILLER           PICTURE S9(4) VALUE +92  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL92'.                 
               10  FILLER           PICTURE S9(4) VALUE +93  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL93'.                 
               10  FILLER           PICTURE S9(4) VALUE +94  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL94'.                 
               10  FILLER           PICTURE S9(4) VALUE +95  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL95'.                 
               10  FILLER           PICTURE S9(4) VALUE +96  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL96'.                 
               10  FILLER           PICTURE S9(4) VALUE +97  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL97'.                 
               10  FILLER           PICTURE S9(4) VALUE +98  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL98'.                 
               10  FILLER           PICTURE S9(4) VALUE +99  COMP.              
               10  FILLER           PICTURE X(4)  VALUE 'EL99'.                 
               10  FILLER           PICTURE S9(4) VALUE +100 COMP.              
           05  ART-ABEND-ROUTE-TBL  REDEFINES ART-ABEND-ROUTE-AREA              
                                    OCCURS 100 TIMES                            
                                    ASCENDING KEY ART-ABEND-CODE                
                                    INDEXED BY ART-ABEND-IDX.                   
               10  ART-ABEND-CODE     PICTURE X(4).                             
               10  ART-PRINT-COL      PICTURE S9(4) COMP.                       
           05  ART-MAX-ABEND-CODES  PICTURE S9(4) VALUE +100  COMP.             
           05  ART-ABEND-REC-ROUTE-AREA.                                        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE ' ELS SMA'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y YYYYYYY  YY.......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE ' ELSCIA'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y YYYYYYYYYYY....... YYY  .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'COBXIO'.               
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y            .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'DBPIOPM'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y            .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELPCN'.                
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....Y    .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELPCV'.                
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      ..... Y   .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELPDE'.                
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....  Y  .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELPEN'.                
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....   Y .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELPGRPKY'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELPMSGKY'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELPRL'.                
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....    Y.....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSATBL'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y            .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSCMDSC'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSCMIF'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSCOMM'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSCONIB'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSCONIS'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSCONPB'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSCONPS'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSCSA'.               
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSCSAD'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSCSBP'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSCSEN'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSCSPG'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSCSPT'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSGRPSP'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSIBGR'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSIOPM'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSIPGN'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSIPGS'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSIPGT'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSKEYS'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSKTBC'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......Y     .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y            .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSKTBG'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......Y     .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y            .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSKTBS'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......Y     .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y            .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSMEMS'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......Y     .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y            .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSMHDG'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSMOPT'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSOUTP'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSPGMW1'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSPGMW2'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSPGMW3'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSPGMW4'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSPLGSW'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSPLGTB'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSPRVN'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSQMENU'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y  Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSQPAGE'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y  Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSRRBL'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSSRTP'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSSSCB'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......Y   YY.....YYYYY.....YYYYYYYY.'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   'YYYYYY....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'YYYYYYYYYYYYY....... YYY  .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....YY ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSTCWA'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSTWA'.               
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSWKFL1'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y  Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSWKFL2'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y  Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSWKFL3'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y  Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'ELSWKFL4'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y  Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'GCBENPRV'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....   Y    .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'GCCONTR'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....  Y     .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'GCDATES'.              
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....Y       .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'GCFLDVAL'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....      Y .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'GCFLDVL2'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....       Y.'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'GCGRPSPC'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     ..... Y      .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'GCSYSTBL'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....     Y  .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'GCTABULR'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....    Y   .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y    Y YY    .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'RDMC4306'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'RDMC4308'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
               10  FILLER.                                                      
                 15  FILLER         PICTURE X(8)  VALUE 'RDMC4311'.             
                 15  FILLER         PICTURE X(50) VALUE                         
                   '             .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................'.          
                 15  FILLER         PICTURE X(50) VALUE                         
                   'Y Y          .......      .....     .....        .'.        
                 15  FILLER         PICTURE X(50) VALUE                         
                   '      ....   ...................................YY'.        
           05  ART-ABEND-REC-ROUTE-TBL REDEFINES                                
                                      ART-ABEND-REC-ROUTE-AREA                  
                                      OCCURS 68 TIMES                           
                                      ASCENDING KEY ART-DDNAME                  
                                      INDEXED BY ART-DDNAME-IDX.                
               10  ART-DDNAME         PICTURE X(8).                             
               10  ART-PRINT-SSD-SW   OCCURS 100 TIMES                          
                                      INDEXED BY ART-SSD-IDX                    
                                      PICTURE X.                                
                   88  ART-PRINT-SSD          VALUE 'Y'.                        
                   88  ART-NO-PRINT-SSD       VALUE ' '.                        
                   88  ART-UNDEF-SSD          VALUE '.'.                        
               10  ART-PRINT-SD-SW    OCCURS 100 TIMES                          
                                      INDEXED BY ART-SD-IDX                     
                                      PICTURE X.                                
                   88  ART-PRINT-SD           VALUE 'Y'.                        
                   88  ART-NO-PRINT-SD        VALUE ' '.                        
                   88  ART-UNDEF-SD           VALUE '.'.                        
           05  ART-MAX-DDNAMES        PICTURE S9(4) VALUE 67                    
                                                    COMP SYNC.                  
