      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSVLTGC                                        *        
      *    DATE:       02-SEP-1991                                     *        
      *    AUTHOR:     GEORGE E MOORE                                  *        
      *    FUNCTION:   VARIABLE LEVEL TAG CONSTANT TABLE.              *        
      *                                                                *        
      *                THIS COPY MEMBER CONTAINS THE LABELS THAT       *        
      *                ARE USED TO CLARIFY THE ASSOCIATION OF          *        
      *                INTERNAL TABULARS WITH THE ACCUMULATION         *        
      *                LEVELS IN A VARIABLE LEVEL GROUP.               *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 02-SEP-1991 GEM       CREATED                            *        
      *                                                                *        
      ******************************************************************        
                                                                                
       01  VLT-VARIABLE-LEVEL-AREA.                                             
          05  VLT-VARIABLE-LEVEL-FIELDS.                                        
              10  FILLER                   PIC X(04) VALUE '<A> '.              
              10  FILLER                   PIC X(04) VALUE '<B> '.              
              10  FILLER                   PIC X(04) VALUE '<C> '.              
              10  FILLER                   PIC X(04) VALUE '<D> '.              
              10  FILLER                   PIC X(04) VALUE '<E> '.              
              10  FILLER                   PIC X(04) VALUE '<F> '.              
              10  FILLER                   PIC X(04) VALUE '<G> '.              
              10  FILLER                   PIC X(04) VALUE '<H> '.              
              10  FILLER                   PIC X(04) VALUE '<I> '.              
              10  FILLER                   PIC X(04) VALUE '<J> '.              
              10  FILLER                   PIC X(04) VALUE '<K> '.              
              10  FILLER                   PIC X(04) VALUE '<L> '.              
              10  FILLER                   PIC X(04) VALUE '<M> '.              
              10  FILLER                   PIC X(04) VALUE '<N> '.              
              10  FILLER                   PIC X(04) VALUE '<O> '.              
              10  FILLER                   PIC X(04) VALUE '<P> '.              
              10  FILLER                   PIC X(04) VALUE '<Q> '.              
              10  FILLER                   PIC X(04) VALUE '<R> '.              
              10  FILLER                   PIC X(04) VALUE '<S> '.              
              10  FILLER                   PIC X(04) VALUE '<T> '.              
              10  FILLER                   PIC X(04) VALUE '<U> '.              
              10  FILLER                   PIC X(04) VALUE '<V> '.              
              10  FILLER                   PIC X(04) VALUE '<W> '.              
              10  FILLER                   PIC X(04) VALUE '<X> '.              
              10  FILLER                   PIC X(04) VALUE '<Y> '.              
              10  FILLER                   PIC X(04) VALUE '<Z> '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
              10  FILLER                   PIC X(04) VALUE '    '.              
                                                                                
       01  VLT-VARIABLE-LEVEL-TABLE REDEFINES VLT-VARIABLE-LEVEL-AREA.          
          05  VLT-VARIABLE-LEVEL-ENTRIES                                        
                                 OCCURS 46 TIMES INDEXED BY VLT-INDEX.          
              10  VLT-VARIABLE-LEVEL-TAG   PIC X(04).                           
