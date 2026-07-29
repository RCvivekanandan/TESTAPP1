       CBL DTR                                                                  
00002  IDENTIFICATION DIVISION.                                                 
00003  PROGRAM-ID. GC029000.                                                    
00004  AUTHOR. ROBERT MANN - DECISION CONSULANTS INC.                           
00005  INSTALLATION. HCSC.                                                      
00006  DATE-WRITTEN.  JUL 06,1984                                               
00007  DATE-COMPILED.                                                           
00008 ************************************************************              
00009 *    THIS PROGRAM IS THE DRIVER FOR UPDATING THE PRODUCTION               
00010 *    GROUP SPECIFIC FILE WITH                                             
00011 *    UPDATED TABULAR PROVISION SLOT NUMBERS.                              
00012 ************************************************************              
00013 * UDPATES:                                                                
00014 *    8/6/96  RGO.  ABEND THE PROGRAM AFTER IT IS FINISHED                 
00015 *            PROCESSING, IF AN INVALID ADD HAS BEEN DETECTED              
00016 *            IN GC029070. GIVE A RETURN CODE OF 12                        
00017 *                                                                         
00018 *  14726/    10/08/97  DAU  RECOMPILED PROGRAM TO SUPPORT THE             
00019 *  15057                    YEAR 2000 AND THE EXPANSION OF THE            
00020 *                           GROUP SPECIFIC AND CONTRACT KEY               
00021 *                           TO SUPPORT THE TEXAS MERGER.                  
      *            08-14-02  GTF   RECOMPILE FOR OPID EXPANSION                 
      *                                                            *            
      * P20368     09/11/15  KIKI COMPILE ONLY - BD / TC           *            
      *                           EXTEND - GCGROUPC                *            
      *                                                            *            
      * P22147     05/03/17  TROY COMPILE ONLY - CE                *            
      *                           EXTEND - GCGROUPC                *            
      *                                                            *            
      * P21681     09/28/17  SRI  COMPILE ONLY - UTIL-MANAGEMENT   *            
      *                           IND    - GCGROUPC                *            
      *                                                            *            
      * P-----     08/XX/19  DJD -ADD INFO & ERROR MSGS & COUNTERS *            
      *                          -COMPILE WITH COBOL 6.2           *            
      *                                                            *            
00024 **************************************************************            
00025  ENVIRONMENT DIVISION.                                                    
00026  CONFIGURATION SECTION.                                                   
00027  SOURCE-COMPUTER. IBM-370.                                                
00028  OBJECT-COMPUTER. IBM-370.                                                
00029  INPUT-OUTPUT SECTION.                                                    
00030  FILE-CONTROL.                                                            
00031      SELECT RLSE-GRP-SPEC-FILE                                            
00032                              ASSIGN TO UT-S-GC0290A.                      
00033      EJECT                                                                
00034  DATA DIVISION.                                                           
00035  FILE SECTION.                                                            
00036                                                                           
00037  FD  RLSE-GRP-SPEC-FILE                                                   
00038      LABEL RECORDS ARE STANDARD                                           
00039      RECORDING MODE IS V                                                  
00040      BLOCK CONTAINS 0 RECORDS.                                            
00041  01  RLSE-GRP-SPEC-REC.                                                   
00042      COPY GCWRKDCC.                                                       
00043      COPY GCGROUPC.                                                       
00044      EJECT                                                                
00045  WORKING-STORAGE SECTION.                                                 
00046                                                                           
00047  01  FILLER          PIC X(24)   VALUE                                    
00048                      'GC029000 WORKING STORAGE'.                          
00049                                                                           
       01  WS-MISC.                                                             
           05  FILLER                PIC X(13) VALUE 'CALL-070-CNT:'.           
           05  WS-CALL-GC029070-CNT  PIC 9(9)  COMP-3 VALUE ZERO.               
           05  FILLER                PIC X(15) VALUE 'READ-SPEC-FILE:'.         
           05  WS-READ-SPEC-CNT      PIC 9(9)  COMP-3 VALUE ZERO.               
           05  FILLER                PIC X(02) VALUE '||'.                      
                                                                                
       01  WS-COMPILED.                                                         
           05  WS-COMPILE-DATE            PIC X(8) VALUE 'MM/DD/YY'.            
           05  WS-COMPILE-TIME            PIC X(8) VALUE 'HH.MM.SS'.            
                                                                                
       01  WS-RUN-DATE-TIME.                                                    
           05 WS-RUN-DATE       PIC X(08)    VALUE SPACES.                      
           05 WS-RUN-DATE-FORMAT REDEFINES WS-RUN-DATE.                         
              10 WS-RUN-MM      PIC 99.                                         
              10 SLASH-1        PIC X(01).                                      
              10 WS-RUN-DD      PIC 99.                                         
              10 SLASH-2        PIC X(01).                                      
              10 WS-RUN-YY      PIC 99.                                         
                                                                                
           05 WS-RUN-TIME       PIC X(08)    VALUE SPACES.                      
           05 WS-RUN-TIME-FORMAT REDEFINES WS-RUN-TIME.                         
              10 WS-RUN-HR      PIC 99.                                         
              10 PERIOD-1       PIC X(01).                                      
              10 WS-RUN-MN      PIC 99.                                         
              10 PERIOD-2       PIC X(01).                                      
              10 WS-RUN-SC      PIC 99.                                         
                                                                                
00050  01  SWITCH-AREA.                                                         
00051      05  RG-SW       PIC X   VALUE SPACES.                                
00052          88  EOF-RG          VALUE HIGH-VALUES.                           
00053                                                                           
00054  01  GC029010-IND    PIC X   VALUE 'O'.                                   
00055  01  GC029070-IND    PIC X   VALUE 'O'.                                   
00056  01  GC029080-IND    PIC X   VALUE 'O'.                                   
00057                                                                           
00058  01  ABEND-AT-END    PIC X VALUE SPACES.                                  
00059  01  ABEND-CODE      PIC S9(4)   COMP    VALUE ZEROS.                     
00060      EJECT                                                                
00061  LINKAGE SECTION.                                                         
00062      EJECT                                                                
                                                                                
                                                                                
00063  PROCEDURE DIVISION.                                                      
00064                                                                           
00065  0000-MAINLINE.                                                           
00066                                                                           
           MOVE WHEN-COMPILED          TO WS-COMPILED.                          
                                                                                
           MOVE FUNCTION CURRENT-DATE (5:2) TO WS-RUN-MM.                       
           MOVE FUNCTION CURRENT-DATE (7:2) TO WS-RUN-DD.                       
           MOVE FUNCTION CURRENT-DATE (3:2) TO WS-RUN-YY.                       
           MOVE '/' TO SLASH-1 SLASH-2.                                         
                                                                                
           MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-RUN-HR.                      
           MOVE FUNCTION CURRENT-DATE (11:2) TO WS-RUN-MN.                      
           MOVE FUNCTION CURRENT-DATE (13:2) TO WS-RUN-SC.                      
           MOVE '.' TO PERIOD-1 PERIOD-2.                                       
                                                                                
           DISPLAY '** NOW RUNNING PROGRAM GC029000 **'.                        
           DISPLAY 'COMPILED DATE & TIME = ' WS-COMPILE-DATE                    
                                         ' ' WS-COMPILE-TIME.                   
           DISPLAY 'CURRENT  DATE & TIME = ' WS-RUN-DATE                        
                                         ' ' WS-RUN-TIME.                       
           DISPLAY ' '.                                                         
                                                                                
                                                                                
00067      OPEN INPUT  RLSE-GRP-SPEC-FILE.                                      
00068                                                                           
00069      CALL 'GC029080' USING GC029080-IND.                                  
00070                                                                           
00071      PERFORM 0010-READ-RG-REC THRU 0010-EXIT.                             
00072                                                                           
00073      PERFORM 0020-PROCESS THRU 0020-EXIT                                  
00074          UNTIL EOF-RG.                                                    
00075                                                                           
00076      MOVE 'C'                    TO GC029070-IND                          
00077                                     GC029080-IND.                         
00078                                                                           
00079      IF  GC029010-IND EQUAL SPACES                                        
00080          MOVE 1000       TO ABEND-CODE                                    
00081          DISPLAY '*** ABEND OCCURRED ***'                                 
00082          DISPLAY 'ABEND CODE =' ABEND-CODE                                
00083          DISPLAY 'END OF FILE NOT REACHED'                                
00084          DISPLAY '*** YOU ARE IN PROGRAM 29000TS ***'                     
00085          GO TO 0060-ERROR-RTN.                                            
00086                                                                           
           CALL 'GC029070' USING GC029070-IND.                                  
00088      CALL 'GC029080' USING GC029080-IND.                                  
00089                                                                           
00090      CLOSE RLSE-GRP-SPEC-FILE.                                            
00091                                                                           
00092      IF ABEND-AT-END = 'Y'                                                
              DISPLAY '---------------------------------------------'           
              DISPLAY '*E* ERRORS FOUND. GC029000 ENDING WITH RC=12.'           
              DISPLAY '*E* NOTIFY PROGRAMMER.'                                  
              DISPLAY '---------------------------------------------'           
00095         MOVE 12 TO RETURN-CODE                                            
00096      END-IF.                                                              
00097                                                                           
           DISPLAY ' '.                                                         
           DISPLAY 'COUNTERS:'.                                                 
           DISPLAY 'CALLS TO GC029070   = ' WS-CALL-GC029070-CNT                
           DISPLAY 'READS OF SPEC FILE  = ' WS-READ-SPEC-CNT                    
           DISPLAY 'END OF PROGRAM GC029000'.                                   
00098                                                                           
00099                                                                           
00100      GOBACK.                                                              
00101  0000-EXIT.                                                               
00102      EXIT.                                                                
00103      EJECT                                                                
                                                                                
00104  0010-READ-RG-REC.                                                        
00105                                                                           
00106      READ RLSE-GRP-SPEC-FILE                                              
00107          AT END                                                           
00108              MOVE HIGH-VALUES    TO RG-SW                                 
                   DISPLAY 'GC029000: RLSE-GRP-SPEC-FILE EOF'                   
                           ', REC-CNT = ' WS-READ-SPEC-CNT                      
00109              GO TO 0010-EXIT.                                             
                                                                                
           ADD +1 TO WS-READ-SPEC-CNT.                                          
00110                                                                           
00111  0010-EXIT.                                                               
00112      EXIT.                                                                
00113      EJECT                                                                
                                                                                
00114  0020-PROCESS.                                                            
00115                                                                           
00116      IF  GC029010-IND NOT EQUAL HIGH-VALUES                               
00117          CALL 'GC029010' USING GC029010-IND RLSE-GRP-SPEC-REC.            
00118                                                                           
           ADD +1 TO WS-CALL-GC029070-CNT.                                      
                                                                                
           CALL 'GC029070' USING GC029070-IND RLSE-GRP-SPEC-REC.                
00120                                                                           
00121      IF GC029070-IND = 'E'                                                
00122         MOVE 'Y' TO ABEND-AT-END                                          
00123         MOVE SPACES TO GC029070-IND                                       
ERRMSG        DISPLAY 'GC029000: GC029070-IND = E, SO SETTING ABEND-'           
ERRMSG                'AT-END FLAG TO YES.  DETAILS: '                          
ERRMSG        DISPLAY '  CALL-070-CNT=' WS-CALL-GC029070-CNT                    
ERRMSG                ', -REC=' RLSE-GRP-SPEC-REC(1:50)                         
00124      END-IF.                                                              
00125                                                                           
00126                                                                           
00127      CALL 'GC029080' USING GC029080-IND RLSE-GRP-SPEC-REC.                
00128                                                                           
00129      PERFORM 0010-READ-RG-REC THRU 0010-EXIT.                             
00130                                                                           
00131  0020-EXIT.                                                               
00132      EXIT.                                                                
00133      EJECT                                                                
00134  0060-ERROR-RTN.                                                          
00135                                                                           
00136      CALL 'TSGEND' USING ABEND-CODE.                                      
00137                                                                           
00138  0060-EXIT.                                                               
00139      EXIT.                                                                
