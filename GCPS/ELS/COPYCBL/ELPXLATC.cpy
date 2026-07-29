      ******************************************************************        
      ** ELPXLATC PROVIDES THE FORMAT FOR THE PARAMETERS USED IN      **        
      ** CALLING ELP008X.  ELP008X IS THE ENGLISH LANGUAGE TRANSLATION**        
      ** SUBPROGRAM USED BY ELP008, THE GROUP SUMMARY GENERATOR.      **        
      **                                                              **        
      ** ON THE INITIAL CALL, SET THE XLAT-CALL-CODE TO 'I' FOR       **        
      ** 'INITIAL'.  ALWAYS SPECIFY THE INPUT PREFIX AND INPUT        **        
      ** ELEMENT NAME FIELD.  IF THE DATA ELEMENT IS DISPLAY OR       **        
      ** DISPLAY NUMERIC OF 10 BYTES OR LESS, MOVE ITS CONTENTS       **        
      ** INTO THE CODE VALUE INPUT FIELD.  OTHERWISE, CLEAR THE CODE  **        
      ** VALUE INPUT FIELD TO LOW-VALUES.  DO *NOT* SUPPLY ANY PARMS  **        
      ** OTHER THAN THOSE WITHIN THE GROUP ITEM, 'XLAT-INPUT-PARMS'.  **        
      **                                                              **        
      ** ON SUBSEQUENT CALLS, DO NOT CHANGE ANY OF THE PARM FIELD     **        
      ** CONTENTS.  ELP008X IS USING ALL OF THEM TO KEEP TRACK OF     **        
      ** ITS CURRENT PROCESSING STATUS.  JUST KEEP CALLING THE        **        
      ** ROUTINE AS LONG AS XLAT-RET-CODE = 'M'.                      **        
      **                                                              **        
      ** XLAT-CALL-CODE WILL BE RESET FROM 'I' TO 'S' AUTOMATICALLY   **        
      ** BY ELP008X AFTER IT PROCESSES THE INITIAL CALL.              **        
      ** XLAT-CALL-INITIAL:     THIS IS THE FIRST TIME THE ROUTINE    **        
      **                        IS BEING CALLED FOR THE CURRENT DATA  **        
      **                        ELEMENT.                              **        
      ** XLAT-CALL-SUBSEQUENT:  THIS IS *NOT* THE FIRST TIME THE      **        
      **                        ROUTINE IS BEING CALLED FOR THE       **        
      **                        CURRENT DATA ELEMENT.                 **        
      ** XLAT-CALL-CLOSE:       CLOSE VSAM FILES (TO BE USED AT EOJ). **        
      ******************************************************************        
       01  XLAT-PARMS.                                                          
           05  XLAT-INPUT-PARMS.                                                
               10  XLAT-ELPDE-PATH-KEY.                                         
                   15  XLAT-RECORD-PREFIX-N PIC X(8).                           
                   15  XLAT-ELEMENT-NAME    PIC X(75).                          
               10  XLAT-CODE-VALUE          PIC X(10).                          
               10  XLAT-CALL-CODE           PIC X.                              
                   88  XLAT-CALL-INITIAL                   VALUE 'I'.           
                   88  XLAT-CALL-SUBSEQUENT                VALUE 'S'.           
                   88  XLAT-CALL-CLOSE                     VALUE 'C'.           
      ******************************************************************        
      ** XLAT-RET-MORE-CV:   THE DATA ELEMENT AND CODE VALUE EXIST.   **        
      **                     THE VERBIAGE FIELD CONTAINS VALID DATA.  **        
      **                     AFTER AN INITIAL CALL, IT WILL BE THE    **        
      **                     DATA ELEMENT NAME; OTHERWISE, IT WILL BE **        
      **                     A CODE VALUE DESCRIPTION LINE.  THE ROU- **        
      **                     TINE SHOULD BE INVOKED AGAIN TO GET MORE **        
      **                     DATA FOR THIS DATA ELEMENT/CODE VALUE.   **        
      **                     (NORMAL TRANSLATION IN PROGRESS)         **        
      ** XLAT-RET-END:       WE ARE FINISHED WITH THIS DATA ELEMENT;  **        
      **                     THE VERBIAGE FIELD CONTAINS THE LAST     **        
      **                     (OR ONLY) CODE VALUE DESCRIPTION LINE.   **        
      **                     (NORMAL END OF TRANSLATION #1)           **        
      ** XLAT-RET-NO-CV:     WE ARE FINISHED WITH THIS DATA ELEMENT;  **        
      **                     NO CODE VALUES EXIST ON FILE.            **        
      **                     THE VERBIAGE FIELD CONTAINS A VALID DATA **        
      **                     ELEMENT NAME.                            **        
      **                     (NORMAL END OF TRANSLATION #2)           **        
      ** XLAT-RET-UNDEF-CV:  WE ARE FINISHED WITH THIS DATA ELEMENT;  **        
      **                     CODE VALUE INFORMATION EXISTS FOR THIS   **        
      **                     DATA ELEMENT, BUT THE CODE VALUE SPECI-  **        
      **                     FIED IN THE SEARCH PARMS IS NOT ON FILE. **        
      **                     THE VERBIAGE FIELD CONTAINS A VALID DATA **        
      **                     ELEMENT NAME.                            **        
      **                     (CODE VALUE NOT FOUND WARNING)           **        
      ** XLAT-RET-UNDEF-DE:  DATA ELEMENT NOT ON FILE.                **        
      **                     THE VERBIAGE FIELD SHOULD *NOT* BE USED. **        
      **                     (DATA ELEMENT NOT FOUND ERROR)           **        
      ** XLAT-RET-BAD-PARMS: ONE OF THE FOLLOWING HAPPENED:           **        
      **                     THE ROUTINE WAS INVOKED AGAIN, WITHOUT   **        
      **                     MAKING A NEW INITIAL CALL, AFTER AN      **        
      **                     END-OF-DATA OR NOT-FOUND CONDITION;      **        
      **                     THE CALLER TRIED TO MAKE A NEW INITIAL   **        
      **                     CALL BEFORE COMPLETING ALL OF THE CALLS  **        
      **                     FOR A PREVIOUS DATA ELEMENT; OR          **        
      **                     INVALID PARMS WERE SUPPLIED.             **        
      **                     THE VERBIAGE FIELD SHOULD *NOT* BE USED. **        
      **                     (FATAL ERROR)                            **        
      ** XLAT-RET-FILES-CLOSED:                                       **        
      **                     VSAM FILES SUCCESSFULLY CLOSED.          **        
      ** NOTE THAT ELP008X WILL TERMINATE ABNORMALLY WITHOUT RETURNING**        
      ** TO THE CALLING MODULE IF TSGVSAM INDICATES A HARD I/O ERROR. **        
      ** IN THIS CASE, A DIAGNOSTIC MESSAGE WILL APPEAR ON SYSOUT.    **        
      ******************************************************************        
           05  XLAT-RET-CODE                PIC X.                              
               88  XLAT-RET-MORE-CV                        VALUE 'M'.           
               88  XLAT-RET-END-CV                         VALUE 'E'.           
               88  XLAT-RET-NO-CV                          VALUE 'N'.           
               88  XLAT-RET-UNDEF-CV                       VALUE 'C'.           
               88  XLAT-RET-UNDEF-DE                       VALUE 'D'.           
               88  XLAT-RET-BAD-PARMS                      VALUE 'P'.           
               88  XLAT-RET-FILES-CLOSED                   VALUE 'F'.           
      ******************************************************************        
      ** XLAT-VERBIAGE IS THE FIELD IN WHICH THE VERBAL DESCRIPTION   **        
      ** DATA WILL BE RETURNED.                                       **        
      ******************************************************************        
           05  XLAT-VERBIAGE                PIC X(79).                          
      ******************************************************************        
      ** WORK FIELDS USED WITHIN ELP008X:                             **        
      ******************************************************************        
           05  XLAT-SEARCH-KEY.                                                 
               10  XLAT-SEARCH-GENERIC-ELPCV.                                   
                   15  XLAT-SEARCH-PREFIX-ELEMENT.                              
                       20  XLAT-SEARCH-KEY-PREFIX                               
                                            PIC X(8).                           
                       20  XLAT-SEARCH-KEY-ELEMENT                              
                                            PIC S999V99 COMP-3.                 
                   15  XLAT-SEARCH-KEY-CODE-VALUE                               
                                            PIC X(10).                          
               10  XLAT-SEARCH-KEY-SEQUENCE PIC 99.                             
      ******************************************************************        
      ** XLAT-CV-VALUE-DESC-SUB POINTS TO THE NEXT LINE NUMBER IN THE **        
      ** CURRENT CODE VALUE RECORD.                                   **        
      ******************************************************************        
           05  XLAT-CV-VALUE-DESC-SUB       PIC 99.                             
