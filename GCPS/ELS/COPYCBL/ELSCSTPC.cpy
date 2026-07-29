      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSCSTPC                                        *        
      *    DATE:       05-JAN-1988                                     *        
      *    AUTHOR:     NINA A. CERVANTES                               *        
      *    FUNCTION:   CONTAINS THE PARAMETER REQUEST LIST FOR EACH    *        
      *                CONTRACT SUMMARY SUBTOPIC.                      *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 30-DEC-1987 NAC       CREATED                            *        
      * 01.01 17-FEB-1988 NAC       INSERTED @ANC FOR SUBTOPIC IHS03.  *        
      * 01.02 02-MAR-1988 REB       ADDITIONAL BENEFIT PROVISIONS      *        
      *                             ADDED FOR IHS SUBTOPIC GROUP 03.   *        
      * 01.03 02-MAR-1988 AKK       CHANGED GPO B AND E TO OUTPATIENT  *        
      *                             VALUES.                            *        
      * 01.04 17-MAR-1988 AKK       CHANGED STRO W TO OUTPATIENT VALUE *        
      *                                                                *        
      * 01.05 25-MAR-1988 AKK       ADDED IHM PARAMETER LIST           *        
      *                                                                *        
      * 01.05 30-MAR-1988 AKK       CHANGED MCOI, MDED AND MLTR TO     *        
      *                             SHOW NO PAYMENT INFO REQUESTED     *        
      ******************************************************************        
       01  WS-IHS-HOSPITAL-SERVICES.                                            
           05  WS-IHS-GROUP-CNT  PIC S9(04) COMP   VALUE +0003.                 
           05  WS-IHS-TBL-CNT    PIC S9(04) COMP   VALUE +0010.                 
           05  WS-IHS-FILLER.                                                   
               07  FILLER    PIC X(14)  VALUE 'IHS01CHC  WIIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS02DRB  AIIY'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS02PVTR AIIY'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS02NRB  AIIY'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS03@ANC  IIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS03XRYI BIIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS03LABI BIIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS03DRGI BIIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS03HMI  BIIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS03MSSI BIIN'.                 
                                                                                
       01  WS-IHM-MED-HOSPITAL-SERVICES.                                        
           05  WS-IHM-GROUP-CNT  PIC S9(04) COMP   VALUE +0003.                 
           05  WS-IHM-TBL-CNT    PIC S9(04) COMP   VALUE +0010.                 
           05  WS-IHM-FILLER.                                                   
               07  FILLER    PIC X(14)  VALUE 'IHS01CHC  WIIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS02MCOI AIIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS02MDED AIIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS02MLTR AIIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS03@ANC  IIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS03XRYI BIIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS03LABI BIIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS03DRGI BIIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS03HMI  BIIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IHS03MSSI BIIN'.                 
                                                                                
       01  WS-IPS-PHYSICIAN-SERVICES.                                           
           05  WS-IPS-GROUP-CNT  PIC S9(04) COMP   VALUE +0003.                 
           05  WS-IPS-TBL-CNT    PIC S9(04) COMP   VALUE +0009.                 
           05  WS-IPS-FILLER.                                                   
               07  FILLER    PIC X(14)  VALUE 'IPS01SRGI CPIY'.                 
               07  FILLER    PIC X(14)  VALUE 'IPS01XRYI EPIY'.                 
               07  FILLER    PIC X(14)  VALUE 'IPS01LABI EPIY'.                 
               07  FILLER    PIC X(14)  VALUE 'IPS01DXTI EPIY'.                 
               07  FILLER    PIC X(14)  VALUE 'IPS01MCHI EPIY'.                 
               07  FILLER    PIC X(14)  VALUE 'IPS02HVD  DPIY'.                 
               07  FILLER    PIC X(14)  VALUE 'IPS03ANSI CPIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IPS03CONI EPIN'.                 
               07  FILLER    PIC X(14)  VALUE 'IPS03ASSI CPIN'.                 
                                                                                
       01  WS-OPS-OUTPATIENT-SERVICES.                                          
           05  WS-OPS-GROUP-CNT  PIC S9(04) COMP   VALUE +0006.                 
           05  WS-OPS-TBL-CNT    PIC S9(04) COMP   VALUE +0020.                 
           05  WS-OPS-FILLER.                                                   
               07  FILLER    PIC X(14)  VALUE 'OPS01ANSO CPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS01ASSO CPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS01CDSO CPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS01COSO CPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS01PODO CPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS01SRGO CPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS02EAER BIOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS02EMER BIOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS03EAC  EPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS03EMC  EPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS04XRYO BIOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS04XRYO EPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS04LABO BIOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS04LABO EPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS04DMPO BIOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS04DMPO EPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS05MCHO BIOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS05MCHO EPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS06CONO EPON'.                 
               07  FILLER    PIC X(14)  VALUE 'OPS06ASOP EPON'.                 
                                                                                
       01  WS-OBS-OBSTERILIZATION.                                              
           05  WS-OBS-GROUP-CNT  PIC S9(04) COMP   VALUE +0003.                 
           05  WS-OBS-TBL-CNT    PIC S9(04) COMP   VALUE +0026.                 
           05  WS-OBS-FILLER.                                                   
               07  FILLER    PIC X(14)  VALUE 'OBS01OBNM WIBY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS01OBNM CPBY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS01OBNS WIBY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS01OBNS CPBY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS01OBND WIBY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS01OBND CPBY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS01OBCM WIBY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS01OBCM CPBY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS01OBCS WIBY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS01OBCS CPBY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS01OBCD WIBY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS01OBCD CPBY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS02EABI WIIY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS02EABI CPIY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS02EABO WIOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS02EABO CPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS02TABI WIIY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS02TABI CPIY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS02TABO WIOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS02TABO CPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS03ESI  CPIY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS03ESO  CPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS03SMNI CPIY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS03SMNO CPOY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS03STRI WIIY'.                 
               07  FILLER    PIC X(14)  VALUE 'OBS03STRO WIOY'.                 
                                                                                
       01  WS-PSY-PSYCHIATRIC.                                                  
           05  WS-PSY-GROUP-CNT  PIC S9(04) COMP   VALUE +0001.                 
           05  WS-PSY-TBL-CNT    PIC S9(04) COMP   VALUE +0012.                 
           05  WS-PSY-FILLER.                                                   
               07  FILLER    PIC X(14)  VALUE 'PSY01GPI  BIIM'.                 
               07  FILLER    PIC X(14)  VALUE 'PSY01GPI  DPIM'.                 
               07  FILLER    PIC X(14)  VALUE 'PSY01GPO  BIOM'.                 
               07  FILLER    PIC X(14)  VALUE 'PSY01GPO  EPOM'.                 
               07  FILLER    PIC X(14)  VALUE 'PSY01IPI  BIIM'.                 
               07  FILLER    PIC X(14)  VALUE 'PSY01IPI  DPIM'.                 
               07  FILLER    PIC X(14)  VALUE 'PSY01IPO  BIOM'.                 
               07  FILLER    PIC X(14)  VALUE 'PSY01IPO  EPOM'.                 
               07  FILLER    PIC X(14)  VALUE 'PSY01PSI  BIIM'.                 
               07  FILLER    PIC X(14)  VALUE 'PSY01PSI  DPIM'.                 
               07  FILLER    PIC X(14)  VALUE 'PSY01PSO  BIOM'.                 
               07  FILLER    PIC X(14)  VALUE 'PSY01PSO  EPOM'.                 
                                                                                
