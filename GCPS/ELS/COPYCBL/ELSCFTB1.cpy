000000******************************************************************        
000010*                                                                *        
000020*    TABLE 1                                                     *        
000030*    LINE OF BUSINESS CONFIDENCE FACTORS TABLE                   *        
000040*                                                                *        
000050******************************************************************        
000060                                                                          
000070 01  CFT1.                                                                
000080     02 CFT1-VALUES.                                                      
000090                                                                          
000100        03 FILLER.                                                        
000110           04 FILLER     PICTURE  X(01)  VALUE '0'.                       
000120           04 FILLER     PICTURE  X(03)  VALUE SPACES.                    
000130           04 FILLER     COMPUTATIONAL-1 VALUE -1.000000E+00.             
000140           04 FILLER     COMPUTATIONAL-1 VALUE -1.000000E+00.             
000150           04 FILLER     COMPUTATIONAL-1 VALUE -1.000000E+00.             
000160           04 FILLER     COMPUTATIONAL-1 VALUE -1.000000E+00.             
000170                                                                          
000180        03 FILLER.                                                        
000190           04 FILLER     PICTURE  X(01)  VALUE '1'.                       
000200           04 FILLER     PICTURE  X(03)  VALUE SPACES.                    
000210           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000220           04 FILLER     COMPUTATIONAL-1 VALUE -1.000000E+00.             
000230           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000240           04 FILLER     COMPUTATIONAL-1 VALUE -1.000000E+00.             
000250                                                                          
000260        03 FILLER.                                                        
000270           04 FILLER     PICTURE  X(01)  VALUE '2'.                       
000280           04 FILLER     PICTURE  X(03)  VALUE SPACES.                    
000290           04 FILLER     COMPUTATIONAL-1 VALUE -1.000000E+00.             
000300           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000310           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000320           04 FILLER     COMPUTATIONAL-1 VALUE -1.000000E+00.             
000330                                                                          
000340        03 FILLER.                                                        
000350           04 FILLER     PICTURE  X(01)  VALUE '3'.                       
000360           04 FILLER     PICTURE  X(03)  VALUE SPACES.                    
000370           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000380           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000390           04 FILLER     COMPUTATIONAL-1 VALUE -1.000000E+00.             
000400           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000410                                                                          
000420        03 FILLER.                                                        
000430           04 FILLER     PICTURE  X(01)  VALUE '4'.                       
000440           04 FILLER     PICTURE  X(03)  VALUE SPACES.                    
000450           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000460           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000470           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000480           04 FILLER     COMPUTATIONAL-1 VALUE -1.000000E+00.             
000490                                                                          
000500        03 FILLER.                                                        
000510           04 FILLER     PICTURE  X(01)  VALUE '5'.                       
000520           04 FILLER     PICTURE  X(03)  VALUE SPACES.                    
000530           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000540           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000550           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000560           04 FILLER     COMPUTATIONAL-1 VALUE -1.000000E+00.             
000570                                                                          
000580        03 FILLER.                                                        
000590           04 FILLER     PICTURE  X(01)  VALUE '6'.                       
000600           04 FILLER     PICTURE  X(03)  VALUE SPACES.                    
000610           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000620           04 FILLER     COMPUTATIONAL-1 VALUE -1.000000E+00.             
000630           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000640           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000650                                                                          
000660        03 FILLER.                                                        
000670           04 FILLER     PICTURE  X(01)  VALUE '7'.                       
000680           04 FILLER     PICTURE  X(03)  VALUE SPACES.                    
000690           04 FILLER     COMPUTATIONAL-1 VALUE -1.000000E+00.             
000700           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000710           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000720           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000730                                                                          
000740        03 FILLER.                                                        
000750           04 FILLER     PICTURE  X(01)  VALUE '8'.                       
000760           04 FILLER     PICTURE  X(03)  VALUE SPACES.                    
000770           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000780           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000790           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000800           04 FILLER     COMPUTATIONAL-1 VALUE  1.000000E+00.             
000810                                                                          
000820     02 CFT1-TABLE       REDEFINES CFT1-VALUES.                           
000830        03 CFT1-TBL      OCCURS 9 TIMES                                   
000840                         INDEXED BY CFT1-IDX.                             
000850           04 CFT1-LOB           PICTURE  X(01).                          
000860           04 FILLER             PICTURE  X(03).                          
000870           04 CFT1-CF-LOB-INST   COMPUTATIONAL-1.                         
000880           04 CFT1-CF-LOB-PROF   COMPUTATIONAL-1.                         
000890           04 CFT1-CF-LOB-BAS    COMPUTATIONAL-1.                         
000900           04 CFT1-CF-LOB-SUP    COMPUTATIONAL-1.                         
000910                                                                          
000920     02 CFT1-COUNTS.                                                      
000930        03 CFT1-NBR-TBL-ENTRIES  PICTURE S9(04)  COMP VALUE     9.        
