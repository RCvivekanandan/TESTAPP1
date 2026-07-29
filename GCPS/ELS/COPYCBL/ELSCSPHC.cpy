      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSCSPHC                                        *        
      *    DATE:       26-JAN-1988                                     *        
000500*    AUTHOR:     RICHARD J. LUKETICH                             *        
000600*    FUNCTION:   CONTAINS THE HEADINGS FOR THE VARIOUS BENEFIT   *        
000700*                PROVISIONS USED IN THE SUBTOPICS.               *        
000800*                                                                *        
000900******************************************************************        
001000*                                                                *        
001100*                      MAINTENANCE HISTORY                       *        
001200*                                                                *        
001300*  MOD     DATE     BY  DRPT                ACTION               *        
001400* ----- ----------- --- ----- ---------------------------------- *        
001500* 01.00 26-JAN-1988 RJL       CREATED                            *        
001000* 01.01 03-FEB-1988 REB       USERS WANT TEXT RIGHT-JUSTIFIED    *        
001200* 01.02 14-MAR-1988 REB       ADDED HEADINGS FOR ADDITIONAL      *        
001200*                             PROVISIONS UNDER ANCILLARY GROUP.  *        
001000* 01.03 21-MAR-1988 REB       VERBIAGE CHANGES BY AUGGIE.        *        
      * 01.04 28-MAR-1988 AKK       ADDED HEADINGS FOR MEDICARE DED,   *        
      *                             COINS, AND LIFETIME RESERVE DAYS   *        
001000* 01.05 30-MAR-1988 NAC       VERBIAGE CHANGES BY AUGGIE.        *        
      * 01.06 30-MAR-1988 AKK       VERBIAGE CHANGES PER AUGGIE        *        
      * 01.07 13-APR-1988 NAC       VERBIAGE CHANGES PER AUGGIE        *        
001600******************************************************************        
001700 01  WS-BENEFIT-PROVISION-HEADINGS.                                       
001800     02 WS-NBR-PROVN-HDGS        PICTURE S9(04)  COMP                     
001900                                 VALUE +58.                             00
002000     02 WS-PROVN-HDGS.                                                    
              03 FILLER                PICTURE  X(35)                           
                 VALUE '@ANC                               '.                   
002100        03 FILLER                PICTURE  X(35)                           
002300           VALUE 'ANSI                    ANESTHESIA '.                 00
002310        03 FILLER                PICTURE  X(35)                           
002400           VALUE 'ANSO                    ANESTHESIA '.                 00
002401        03 FILLER                PICTURE  X(35)                           
002402           VALUE 'ASOP   ELECTIVE ADDITIONAL OPINION '.                 00
002403        03 FILLER                PICTURE  X(35)                           
002404           VALUE 'ASSI             ASSISTANT SURGEON '.                 00
002405        03 FILLER                PICTURE  X(35)                           
002406           VALUE 'ASSO             ASSISTANT SURGEON '.                 00
002407        03 FILLER                PICTURE  X(35)                           
002408           VALUE 'CDSO             CONGENITAL DEFECT '.                 00
002409        03 FILLER                PICTURE  X(35)                           
002420           VALUE 'CHC          COORDINATED HOME CARE '.                 00
002421        03 FILLER                PICTURE  X(35)                           
002423           VALUE 'CONI                  CONSULTATION '.                 00
002424        03 FILLER                PICTURE  X(35)                           
002425           VALUE 'CONO                  CONSULTATION '.                 00
002426        03 FILLER                PICTURE  X(35)                           
002440           VALUE 'COSO                      COSMETIC '.                 00
002450        03 FILLER                PICTURE  X(35)                           
002460           VALUE 'DMPO             MEDICAL PROCEDURE '.                 00
002470        03 FILLER                PICTURE  X(35)                           
002500           VALUE 'DRB                   SEMI-PRIVATE '.                 00
002501        03 FILLER                PICTURE  X(35)                           
                 VALUE 'DRGI                      DRUGS IP '.                   
002501        03 FILLER                PICTURE  X(35)                           
002502           VALUE 'DXTI             RADIATION THERAPY '.                 00
002503        03 FILLER                PICTURE  X(35)                           
002504           VALUE 'EABI          ELECTIVE ABORTION IP '.                 00
002505        03 FILLER                PICTURE  X(35)                           
002506           VALUE 'EABO          ELECTIVE ABORTION OP '.                 00
002507        03 FILLER                PICTURE  X(35)                           
002520           VALUE 'EAC      ACCIDENT CARE - PHYSICIAN '.                 00
002521        03 FILLER                PICTURE  X(35)                           
002522           VALUE 'EAER      ACCIDENT ROOM - HOSPITAL '.                 00
002523        03 FILLER                PICTURE  X(35)                           
002524           VALUE 'EMC       MEDICAL CARE - PHYSICIAN '.                 00
002525        03 FILLER                PICTURE  X(35)                           
002540           VALUE 'EMER       MEDICAL ROOM - HOSPITAL '.                 00
002541        03 FILLER                PICTURE  X(35)                           
002542           VALUE 'ESI             ELECTIVE STERIL IP '.                 00
002543        03 FILLER                PICTURE  X(35)                           
002560           VALUE 'ESO             ELECTIVE STERIL OP '.                 00
002570        03 FILLER                PICTURE  X(35)                           
002580           VALUE 'GPI         GROUP PSYCHOTHERAPY IP '.                 00
002590        03 FILLER                PICTURE  X(35)                           
002600           VALUE 'GPO         GROUP PSYCHOTHERAPY OP '.                 00
002601        03 FILLER                PICTURE  X(35)                           
                 VALUE 'HMI      HOSPITAL MISCELLANEOUS IP '.                   
002601        03 FILLER                PICTURE  X(35)                           
002602           VALUE 'HVD                 MEDICAL VISITS '.                 00
002603        03 FILLER                PICTURE  X(35)                           
002604           VALUE 'IPI    INDIVIDUAL PSYCHOTHERAPY IP '.                 00
002605        03 FILLER                PICTURE  X(35)                           
002606           VALUE 'IPO    INDIVIDUAL PSYCHOTHERAPY OP '.                 00
002607        03 FILLER                PICTURE  X(35)                           
002620           VALUE 'LABI                    LABORATORY '.                 00
002621        03 FILLER                PICTURE  X(35)                           
002622           VALUE 'LABO                    LABORATORY '.                 00
002623        03 FILLER                PICTURE  X(35)                           
002624           VALUE 'MCHI                  CHEMOTHERAPY '.                 00
002625        03 FILLER                PICTURE  X(35)                           
002640           VALUE 'MCHO                  CHEMOTHERAPY '.                 00
              03 FILLER                PICTURE  X(35)                           
                 VALUE 'MCOI     MEDICARE COINSURANCE DAYS '.                 00
              03 FILLER                PICTURE  X(35)                           
                 VALUE 'MDED           MEDICARE DEDUCTIBLE '.                 00
              03 FILLER                PICTURE  X(35)                           
                 VALUE 'MLTR MEDICARE LIFETIME RESERVE DAYS'.                 00
002641        03 FILLER                PICTURE  X(35)                           
                 VALUE 'MSSI  MEDICAL/SURGICAL SUPPLIES IP '.                   
002641        03 FILLER                PICTURE  X(35)                           
002642           VALUE 'NRB                        NURSERY '.                 00
002643        03 FILLER                PICTURE  X(35)                           
002660           VALUE 'OBCD COMPLICATED MATERNITY DEPENDNT'.                 00
002670        03 FILLER                PICTURE  X(35)                           
002680           VALUE 'OBCM  COMPLICATED MATERNITY MEMBER '.                 00
002690        03 FILLER                PICTURE  X(35)                           
002700           VALUE 'OBCS  COMPLICATED MATERNITY SPOUSE '.                 00
002701        03 FILLER                PICTURE  X(35)                           
002702           VALUE 'OBND    NORMAL MATERNITY DEPENDENT '.                 00
002703        03 FILLER                PICTURE  X(35)                           
002704           VALUE 'OBNM       NORMAL MATERNITY MEMBER '.                 00
002705        03 FILLER                PICTURE  X(35)                           
002706           VALUE 'OBNS       NORMAL MATERNITY SPOUSE '.                 00
002707        03 FILLER                PICTURE  X(35)                           
002720           VALUE 'PODO                      PODIATRY '.                 00
002721        03 FILLER                PICTURE  X(35)                           
002722           VALUE 'PSI       PSYCHOLOGICAL TESTING IP '.                 00
002723        03 FILLER                PICTURE  X(35)                           
002724           VALUE 'PSO       PSYCHOLOGICAL TESTING OP '.                 00
002725        03 FILLER                PICTURE  X(35)                           
002726           VALUE 'PVTR                       PRIVATE '.                 00
002727        03 FILLER                PICTURE  X(35)                           
002740           VALUE 'SMNI MEDICALLY NECESSARY STERIL IP '.                 00
002741        03 FILLER                PICTURE  X(35)                           
002742           VALUE 'SMNO MEDICALLY NECESSARY STERIL OP '.                 00
002743        03 FILLER                PICTURE  X(35)                           
002760           VALUE 'SRGI                       SURGERY '.                 00
002770        03 FILLER                PICTURE  X(35)                           
002800           VALUE 'SRGO                       SURGERY '.                 00
002801        03 FILLER                PICTURE  X(35)                           
002802           VALUE 'STRI              STERILIZATION IP '.                 00
002803        03 FILLER                PICTURE  X(35)                           
002820           VALUE 'STRO              STERILIZATION OP '.                 00
002830        03 FILLER                PICTURE  X(35)                           
002840           VALUE 'TABI       THERAPEUTIC ABORTION IP '.                 00
002850        03 FILLER                PICTURE  X(35)                           
003000           VALUE 'TABO       THERAPEUTIC ABORTION OP '.                 00
003010        03 FILLER                PICTURE  X(35)                           
003020           VALUE 'XRYI                         X-RAY '.                 00
003030        03 FILLER                PICTURE  X(35)                           
003200           VALUE 'XRYO                         X-RAY '.                 00
005700                                                                          
005800     02 WS-PROVN-HDG-TABLE       REDEFINES WS-PROVN-HDGS.                 
005900        03 WS-PROVN-HDG-TBL      OCCURS 58 TIMES                        00
006100                                 ASCENDING KEY IS WS-PROVN-BP-ID          
006000                                 INDEXED BY WS-BPHT-IDX.                  
006200           04 WS-PROVN-BP-ID     PICTURE  X(05).                          
006500           04 WS-PROVN-HDG       PICTURE  X(30).                          
