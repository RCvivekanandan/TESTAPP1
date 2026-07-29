$$FILEM DSC ,                                                                   
$$FILEM INPUT=DD01,                                                             
$$FILEM OUTPUT=DD01O,                                                           
$$FILEM PROC=*                                                                  
 IF FLDI(1,,C,'EQ','621'),                                                      
   & FLDI(58,,C,'NE','C'),                                                      
   & FLDI(21,,C,'EQ','POS') THEN DO                                             
    IF RECSOUT() >= 0 THEN DO                                                   
       RETURN                                                                   
    END                                                                         
 END                                                                            
 RETURN DROP                                                                    
/+                                                                              
