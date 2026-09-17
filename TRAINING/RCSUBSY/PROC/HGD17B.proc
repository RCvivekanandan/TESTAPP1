//HGD17B   PROC MIDATS='MIDATS(0)',JBN=XX,               PRODUCTION DAY         
//         SORTWK=40,                                    SORT WORK AREA         
//BKUPINV4 EXEC PGM=SORT,COND=(4035,GT,SR8820)                                  
//SYSOUT   DD SYSOUT=&SYSMSG                                                    
//SYSUDUMP DD SYSOUT=X,HOLD=YES,FREE=CLOSE                                      
//SORTIN   DD DSN=OPER.CNS.BLUECHIP.INVADJ.BKUP(0),DISP=OLD,UNIT=3490E          
//SORTOUT  DD DSN=OPER.BLUECHIP.INVADJ.HGD17B.MTHLY,DISP=(,CATLG),              
//            UNIT=3490E,VOL=(,,,99),                                           
//            DCB=(RECFM=VB,LRECL=474,BLKSIZE=0)                                
//SYSIN    DD DSN=OPER.CONTROL(SORTCOPY),DISP=SHR                               
//********* END OF HGD17B  ***********                                          
