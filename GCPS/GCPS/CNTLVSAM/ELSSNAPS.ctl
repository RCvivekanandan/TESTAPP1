 /*DSN=HCV.ELS.ELP.ELSSNAPS                                         */          
 DELETE (HCV.ELS.ELP.ELSSNAPS) CLUSTER                                          
 DEFINE -                                                                       
 CLUSTER ( -                                                                    
 NAME(HCV.ELS.ELP.ELSSNAPS) -                                                   
 CYLINDERS(5 5) -                                                               
 STORCLAS(ONLINE) -                                                             
 NONINDEXED -                                                                   
 BUFFERSPACE(8192) -                                                            
 CONTROLINTERVALSIZE(4096) -                                                    
 RECORDSIZE(1000 4089) -                                                        
 SHAREOPTIONS(2 3) -                                                            
 UNIQUE -                                                                       
 ) -                                                                            
 DATA ( -                                                                       
 NAME(HCV.ELS.ELP.ELSSNAPS.DATA) -                                              
 )                                                                              
 /*  CATLG CARD REMOVED/CONTINUATION REMOVED */                                 
 LISTCAT ENTRIES(HCV.ELS.ELP.ELSSNAPS) ALL                                      
