      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELSELOGC.                                       *00030000
      *    DATE:       16-MAR-1990                                     *00040000
      *    AUTHOR:     ANNE KEFFER KING.                               *00050000
      *    FUNCTION:   DESIGNED TO CONTAIN DATA FOUND TO MEET          *00060000
      *                'EXCEPTIONAL CONDITIONS' THAT ARE NOT           *00070000
      *                ABENDS.                                         *00080000
      *                                                                *00090000
      ******************************************************************00100000
      *                                                                *00110000
      *                      MAINTENANCE HISTORY                       *00120000
      *                                                                *00130000
      *  MOD     DATE     BY  DRPT                ACTION               *00140000
      * ----- ----------- --- ----- ---------------------------------- *00150000
      * 01.00 15-MAR-1990 AKK       CREATED                            *00160000
      *                                                                *00170000
      * 01.01 14-OCT-1991 JPB       CHANGED LG-C-S-PPM TO LG-C-V-LOGIC *00160000
      *                                                                *00170000
      ***************************************************************** 00180000
       01    LG-LOG-RECORD.                                                     
          02  LG-LOG-LENGTH                    PIC S9(08) COMP.                 
          02  LG-RECORD-TYPE                   PIC X.                           
              88  LG-C-M-ELEMENT                       VALUE '4'.               
              88  LG-C-M-VALUE                         VALUE '5'.               
              88  LG-C-V-LOGIC                         VALUE '6'.               
          02  LG-CODES-MANUAL-INFO.                                             
                05  LG-CV-DE-LOG-LINE.                                          
                   10  LG-LINE-PREFIX.                                          
                       15  LG-RECORD-PREFIX  PIC X(08).                         
                       15  LG-ELEMENT-NAME   PIC X(30).                         
                   10  LG-DE-NUMBER      PIC S999V99 COMP-3.                    
                   10  LG-CODE-VALUE     PIC X(10).                             
