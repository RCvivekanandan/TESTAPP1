      *                                                              *          
      ****************************************************************          
      *           GENERIC CONTRACT PROCESSING SYSTEM (GCPS)          *          
      *             AUTOMATED MAP FORWARD (AMF) SUBSYSTEM            *          
      *                                                              *          
      ****  N O T E    N O T E    N O T E    N O T E    N O T E   ****          
      *                                                              *          
      *     IF THIS MEMBER IS UPDATED, THE FOLLOWING COPYMEMBERS     *          
      *     MUST BE CHECKED FOR UPDATE:                             *           
      *          GCAMFKEY, GCAMFKE2, GCAMFTRC, GCAMFTR2              *          
      ****                                                        ****          
      *                                                              *          
      *             COMMUNICATIONS AREA USED FOR PATHING             *          
      *             THRU THE ONLINE PROCESSING.                      *          
      *                                                              *          
      *                                                              *          
      *   COPYBOOK MEMBER:  GAMFCOMC                                 *          
      *                     GCPS-AUTOMATED-MAP-FORWARD-COMMAREA      *          
      *                                                              *          
      *   FIXED LENGTH RECORD:  150 BYTES                            *          
      *                                                              *          
      ****************************************************************          
      *                                                              *          
      ****************************************************************          
      **------------------------------------------------------------**          
      *               M A I N T E N A N C E     L O G                *          
      **------------------------------------------------------------**          
      *                                                              *          
      **-CHG-NUM-* *-DATE-* *WHO*  *--------DESCRIPTION-------------**          
      *                                                              *          
      *  #11665    06/15/92  FRY    CREATED RDW.                     *          
      *                                                              *          
      ****************************************************************          
      *                                                              *          
           05  GAMFCA-PATHING-INFO.                                             
               10  GAMFCA-LAST-CICS-TRANS                  PIC  X(04).          
                   88  GAMFCA-MENU                     VALUE  'GXB1'.           
                   88  GAMFCA-INQUIRY-MAINT-INDEX      VALUE  'GXB2'.           
                   88  GAMFCA-RECORD-SELECT            VALUE  'GXB3'.           
                   88  GAMFCA-COMMON-FIELDS-SELECT     VALUE  'GXB4'.           
                   88  GAMFCA-SLOT-ADD-INQUIRY         VALUE  'GXB5'.           
                   88  GAMFCA-SLOT-DELETE              VALUE  'GXB6'.           
               10  GAMFCA-MENU-OPTION                      PIC  X(01).          
                   88  GAMFCA-ADD-NEW-TRANSACTION      VALUE  '1'.              
                   88  GAMFCA-INQ-MAINT-CONTRACT       VALUE  '2'.              
                   88  GAMFCA-INQ-MAINT-GRPSPEC        VALUE  '3'.              
               10  GAMFCA-TRANS-ID.                                             
                   15  GAMFCA-REC-TYPE                     PIC  X(01).          
                       88  GAMFCA-CON-REC              VALUE  'C'.              
                       88  GAMFCA-GS-REC               VALUE  'G'.              
                   15  GAMFCA-CURR-DATE           COMP-3   PIC S9(05).          
                   15  GAMFCA-SEQUENCE-NO         COMP-3   PIC S9(05).          
                   15  GAMFCA-TYPE-OF-CHG                  PIC  X(01).          
                   15  GAMFCA-ATB-ID                       PIC  X(08).          
               10  GAMFCA-ADD-TRANSACTION-STATUS           PIC  X(01).          
                   88  GAMFCA-ADD-TRANS-NOT-CREATED    VALUE  '0'.              
                   88  GAMFCA-ADD-TRANS-CREATED        VALUE  '1'.              
               10  GAMFCA-GXB2-INDEX-REQUEST               PIC  X(01).          
                   88  GAMFCA-GXB2-INQUIRY-REQUEST     VALUE  'I'.              
                   88  GAMFCA-GXB2-MAINT-REQUEST       VALUE  'M'.              
               10  GAMFCA-CURR-TRANS-BEG-ID                PIC  X(16).          
               10  GAMFCA-CURR-TRANS-END-ID                PIC  X(16).          
               10  GAMFCA-NEXT-TRANS-BEG-ID                PIC  X(16).          
               10  GAMFCA-PRIOR-TRANS-END-ID               PIC  X(16).          
               10  GAMFCA-CURR-PAGE                 COMP   PIC S9(04).          
               10  GAMFCA-LAST-PAGE                 COMP   PIC S9(04).          
               10  GAMFCA-XCTL-NOW                         PIC  X(01).          
               10  FILLER                                  PIC  X(58).          
