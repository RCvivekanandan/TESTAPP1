000100****************************************************************  00010000
000200*                                                              *  00020000
000300*    COPYBOOK:   ELSGVX1C                                      *  00030003
000400*    DATE:       22-DEC-1993                                   *  00040003
000500*    AUTHOR:     ANNE KEFFER-KING                              *  00050003
      *    FUNCTION:                                                 *          
      *                                                              *          
      *   THIS COPY MEMBER IS USED BY ELS SPECIAL PROVIDER TOPIC     *          
      *   PROCESSING TO KEEP TRACK OF HOW MANY PROVIDERS AND THEIR   *          
      *   NUMBERS HAVE NAMES THAT MATCH WHAT WAS SELECTED BY THE     *          
      *   USER.                                                      *          
      ****************************************************************          
                                                                                
001000****************************************************************  00100000
001100*                                                              *  00110000
001200*                      MAINTENANCE HISTORY                     *  00120000
001300*                                                              *  00130000
001400*  MOD     DATE     BY  DRPT                ACTION             *  00140000
001500* ----- ----------- --- ----- ----------------------------------  00150000
001100* 1.00   22-DEC-93  AKK       CREATED                          *  00110000
001000*                                                              *  00100000
001100* 1.01   20-JUN-06  AKK       EXPAND COPYBOOK TO 2500 OCCURS   *  00110000
001000*                                                              *  00100000
001100* 1.02   22-JUN-06  AKK       EXPAND COPYBOOK TO 5000 OCCURS   *  00110000
001000****************************************************************  00100000
       01  GVX-PROVIDER-NUMBER-OVERLAY.                                         
             05  GVX-NMBR-PRVDR-ENTRS      PIC S9(04) COMP.                     
             05    GVX-PRVDR-NMBR-TBL OCCURS 1 TO 5000 TIMES                    
                              DEPENDING ON GVX-NMBR-PRVDR-ENTRS                 
                                   INDEXED BY GVX-IDX                           
                                              GVX-HOLD-IDX                      
                                              GVX-MAX-IDX.                      
                 10  GVX-PRVDR-NMBR        PIC X(10).                           
