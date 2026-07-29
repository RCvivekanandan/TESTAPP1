00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELP005  
00003  PROGRAM-ID.         ELP005.                                         LV001
00004                                                                   ELP005  
00005  AUTHOR.             EDWARD G LISS.                               ELP005  
00006                                                                   ELP005  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELP005  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELP005  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELP005  
00010                      233 N. MICHIGAN AVE                          ELP005  
00011                      CHICAGO, ILLINOIS 60601                      ELP005  
00012                                                                   ELP005  
00013  DATE-WRITTEN.       11-NOV-1989.                                 ELP005  
00014                      ORIGINAL WRITTEN 18-OCT-1985.                ELP005  
00015                                                                   ELP005  
00016  DATE-COMPILED.                                                   ELP005  
00017                                                                   ELP005  
00018  SECURITY.           COPYRIGHT 1988,                              ELP005  
00019                      HEALTH CARE SERVICE CORPORATION              ELP005  
00020      TITLE 'PRINT THE CODES MANUAL'.                              ELP005  
00021 ****************** COMMENTS BY ORIGINAL AUTHOR ****************** ELP005  
00022 ***************************************************************** ELP005  
00023 * THIS PROGRAM PRINTS THE CODES MANUAL FOR THE ENGLISH LANGUAGE * ELP005  
00024 * SUPPORT PROTOTYPE SYSTEM.  THE FIRST SEQUENTIAL PARM RECORD   * ELP005  
00025 * TELLS IT HOW MUCH TO PRINT.  THE OPTIONS ARE THE FOLLOWING:   * ELP005  
00026 *                                                               * ELP005  
00027 * A - ALL RECORDS                                               * ELP005  
00028 * S - SELECTED RECORDS AND DATA ELEMENTS                        * ELP005  
00029 * U - UPDATED RECORDS AND DATA ELEMENTS                         * ELP005  
00030 *                                                               * ELP005  
00031 * IF OPTIONS 'A' OR 'U' ARE CHOSEN, THE PROGRAM WILL GO THROUGH * ELP005  
00032 * THE ENTIRE RECORD LIST FILE.                                  * ELP005  
00033 *                                                               * ELP005  
00034 * FOR OPTION 'A', ELP005 WILL PRINT A REPORT FOR ALL            * ELP005  
00035 * RECORDS ON THE RECORD LIST FILE, WITH THEIR ASSOCIATED        * ELP005  
00036 * DATA ELEMENT AND CODE VALUE RECORD CONTENTS.  FOR OPTION 'U', * ELP005  
00037 *                                                               * ELP005  
00038 * FOR OPTION 'U', ELP005 WILL PRINT A REPORT FOR ALL            * ELP005  
00039 * UPDATED RECORDS ON THE RECORD LIST FILE, TOGETHER WITH        * ELP005  
00040 * THE DATA ELEMENT AND CODE VALUE RECORD CONTENTS ASSOCIATED    * ELP005  
00041 * WITH THOSE UPDATED RECORDS.  FOR THOSE RECORDS ON THE         * ELP005  
00042 * RECORD LIST FILE WHICH WERE NOT UPDATED, ELP005 WILL          * ELP005  
00043 * EXAMINE THE DATA ELEMENTS ASSOCIATED WITH THEM, AND WILL      * ELP005  
00044 * PRINT A REPORT FOR THOSE DATA ELEMENTS THAT ARE FLAGGED       * ELP005  
00045 * AS HAVING BEEN UPDATED.  ALL ASSOCIATED CODE VALUES WILL      * ELP005  
00046 * BE SHOWN.  WHENEVER ELP005 ENCOUNTERS A RECORD LIST RECORD    * ELP005  
00047 * OR DATA ELEMENT RECORD WHICH IS FLAGGED AS HAVING BEEN        * ELP005  
00048 * DELETED, IT WILL PRINT A REPORT SHEET INDICATING WHICH        * ELP005  
00049 * PAGES SHOULD BE REMOVED FROM THE EXISTING CODES MANUAL.       * ELP005  
00050 * DELETED CODE VALUES WILL BE FLAGGED WITH A DETAIL LINE        * ELP005  
00051 * ON THE PAGE FOR THE DATA ELEMENT WITH WHICH THEY ARE          * ELP005  
00052 * ASSOCIATED.                                                   * ELP005  
00053 *                                                               * ELP005  
00054 * NOTE THAT FOR OPTIONS 'A' AND 'U', PROGRAM LOGIC IS           * ELP005  
00055 * DRIVEN BY SEQUENTIAL ACCESS TO THE RECORD LIST FILE.          * ELP005  
00056 * FOR EACH RECORD ON THE RECORD LIST FILE, A START BROWSE       * ELP005  
00057 * IS PERFORMED AGAINST THE DATA ELEMENT FILE TO POINT AT        * ELP005  
00058 * THE FIRST DATA ELEMENT ASSOCIATED WITH THE RECORD LIST        * ELP005  
00059 * RECORD.  EACH OF THESE ARE READ SEQUENTIALLY, AND THE         * ELP005  
00060 * CODE VALUES, IF ANY, ASSOCIATED WITH EACH DATA ELEMENT        * ELP005  
00061 * ARE ACCESSED IN THE SAME MANNER.                              * ELP005  
00062 *                                                               * ELP005  
00063 * FOR OPTION 'S', PROGRAM LOGIC IS DRIVEN BY SEQUENTIAL         * ELP005  
00064 * ACCESS TO THE INPUT PARAMETER FILE.  TWO TYPES OF             * ELP005  
00065 * SELECTION PARMS ARE ALLOWED: SELECTION CAN BE AT THE          * ELP005  
00066 * RECORD LEVEL OR AT THE DATA ELEMENT LEVEL.  IF THE            * ELP005  
00067 * SELECTION IS AT THE RECORD LIST LEVEL, THE RECORD             * ELP005  
00068 * LIST RECORD IS OBTAINED WITH A RANDOM ACCESS, AND             * ELP005  
00069 * ALL ASSOCIATED DATA ELEMENTS AND CODE VALUES ARE              * ELP005  
00070 * OBTAINED WITH START BROWSES AND SEQUENTIAL READS.             * ELP005  
00071 * IF THE SELECTION IS AT THE DATA ELEMENT LEVEL, THE            * ELP005  
00072 * DATA ELEMENT RECORD IS OBTAINED WITH A RANDOM ACCESS,         * ELP005  
00073 * AND ALL ASSOCIATED CODE VALUES ARE OBTAINED WITH A            * ELP005  
00074 * START BROWSE FOLLOWED BY SEQUENTIAL READS.                    * ELP005  
00075 *                                                               * ELP005  
00076 ***************************************************************** ELP005  
00077 /**************************************************************** ELP005  
00078 *                                                               * ELP005  
00079 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * ELP005  
00080 *    *-*         U P D A T E   H I S T O R Y         *-*        * ELP005  
00081 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        * ELP005  
00082 *                                                               * ELP005  
00083 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* ELP005  
00084 *                                                               * ELP005  
00085 *  XXXX      12/13/85  AMJ  ORIGINAL VERSION.                   * ELP005  
00086 *  01.00     11/17/89  EGL  REWRITE (CANIBALIZATION) OF         * ELP005  
00087 *                           PRODUCTION VERSION.                 * ELP005  
00088 *                                                               * ELP005  
00089 ***************************************************************** ELP005  
00090 /                                                                 ELP005  
00091  ENVIRONMENT DIVISION.                                            ELP005  
00092                                                                   ELP005  
00093  CONFIGURATION SECTION.                                           ELP005  
00094                                                                   ELP005  
00095  SOURCE-COMPUTER.  IBM-370.                                       ELP005  
00096                                                                   ELP005  
00097  OBJECT-COMPUTER.  IBM-370.                                       ELP005  
00098                                                                   ELP005  
00099  INPUT-OUTPUT SECTION.                                            ELP005  
00100  FILE-CONTROL.                                                    ELP005  
00101                                                                   ELP005  
00102      SELECT PRINT-FILE                                            ELP005  
00103          ASSIGN TO PRTFILE.                                       ELP005  
00104                                                                   ELP005  
00105      SELECT PARM-FILE                                             ELP005  
00106          ASSIGN TO PARM.                                          ELP005  
00107 /                                                                 ELP005  
00108  DATA DIVISION.                                                   ELP005  
00109  FILE SECTION.                                                    ELP005  
00110                                                                   ELP005  
00111  FD  PARM-FILE                                                    ELP005  
00112      BLOCK CONTAINS  0  RECORDS                                   ELP005  
00113      LABEL RECORDS ARE STANDARD                                   ELP005  
00114      RECORDING MODE IS F                                          ELP005  
00115      RECORD CONTAINS 80 CHARACTERS                                ELP005  
00116      DATA RECORDS ARE PARM-REC-1 PARM-REC-S.                      ELP005  
00117                                                                   ELP005  
00118  01  PARM-REC-1.                                                  ELP005  
00119      05  PARM-1-REQUEST-TYPE    PIC X.                            ELP005  
00120          88  PRINT-ALL          VALUE 'A'.                        ELP005  
00121          88  PRINT-SELECTED     VALUE 'S'.                        ELP005  
00122          88  PRINT-UPDATED      VALUE 'U'.                        ELP005  
00123      05  FILLER                 PIC X(79).                        ELP005  
00124                                                                   ELP005  
00125  01  PARM-REC-S.                                                  ELP005  
00126      05  PARM-S-REQUEST-TYPE    PIC X.                            ELP005  
00127          88  SELECT-BY-RECORD-LIST                                ELP005  
00128                                 VALUE 'R'.                        ELP005  
00129          88  SELECT-BY-DATA-ELEMENT                               ELP005  
00130                                 VALUE 'D'.                        ELP005  
00131      05  PARM-PREFIX-KEY        PIC X(8).                         ELP005  
00132      05  PARM-ELEMENT-999.                                        ELP005  
00133          10  PARM-ELEMENT-NO-999                                  ELP005  
00134                                 PIC 999.                          ELP005  
00135 ******************************************************************ELP005  
00136 ** THE FORMAT FOR THE ELEMENT NUMBER SHOULD BE PIC 999.99;      **ELP005  
00137 ** HOWEVER, WE ARE WATCHING FOR THE SITUATION IN WHICH THE      **ELP005  
00138 ** ELEMENT NUMBER IS ENTERED INSTEAD AS PIC 99999 FOLLOWED      **ELP005  
00139 ** BY A SPACE.  IN EITHER CASE, WE BREAK IT UP PROPERLY AND     **ELP005  
00140 ** REFORMAT IT TO FORM THE VSAM KEY.  TWO DECIMAL PLACES        **ELP005  
00141 ** ARE ALWAYS ASSUMED.                                          **ELP005  
00142 ******************************************************************ELP005  
00143      05  PARM-ELEMENT-SUFFIX-POINT99.                             ELP005  
00144          10  PARM-ELEMENT-POINT PIC X.                            ELP005  
00145              88  POINT-CHAR-ENTERED                               ELP005  
00146                                 VALUE '.'.                        ELP005  
00147          10  PARM-SUFFIX-POINT99.                                 ELP005  
00148              15  PARM-SUFFIX-NO-POINT99                           ELP005  
00149                                 PIC 99.                           ELP005  
00150      05  PARM-ELEMENT-SUFFIX-99SPACE REDEFINES                    ELP005  
00151              PARM-ELEMENT-SUFFIX-POINT99.                         ELP005  
00152          10  PARM-SUFFIX-99SPACE.                                 ELP005  
00153              15  PARM-SUFFIX-NO-99SPACE                           ELP005  
00154                                 PIC 99.                           ELP005  
00155          10  PARM-ELEMENT-SPACE PIC X.                            ELP005  
00156              88  CHAR-NOT-ENTERED                                 ELP005  
00157                                 VALUE LOW-VALUES SPACES.          ELP005  
00158      05  FILLER                 PIC X(65).                        ELP005  
00159 /                                                                 ELP005  
00160  FD  PRINT-FILE                                                   ELP005  
00161      BLOCK CONTAINS  0  RECORDS                                   ELP005  
00162      LABEL RECORDS ARE STANDARD                                   ELP005  
00163      RECORDING MODE IS F                                          ELP005  
00164      RECORD CONTAINS 133 CHARACTERS                               ELP005  
00165      DATA RECORD IS PRT-REC.                                      ELP005  
00166                                                                   ELP005  
00167  01  PRT-REC.                                                     ELP005  
00168      05  PT-CC                  PIC X.                            ELP005  
00169      05  PT-DATA                PIC X(132).                       ELP005  
00170 /                                                                 ELP005  
00171  WORKING-STORAGE SECTION.                                         ELP005  
00172  01  FILLER                         PIC X(32)                     ELP005  
00173        VALUE '*ELP005 WORKING STORAGE BEGINS*'.                   ELP005  
00174                                                                   ELP005  
00175  01  MISC-WORK.                                                   ELP005  
00176      05  PARM-EOF-INDICATOR         PIC X  VALUE 'N'.             ELP005  
00177          88 PARM-EOF                       VALUE 'Y'.             ELP005  
00178          88 PARM-NOT-EOF                   VALUE 'N'.             ELP005  
00179      05  RL-STATUS-INDICATOR        PIC X  VALUE '0'.             ELP005  
00180          88 RL-NOT-FOUND                   VALUE '2'.             ELP005  
00181          88 RL-EOF                         VALUE '1'.             ELP005  
00182          88 RL-OK                          VALUE '0'.             ELP005  
00183      05  DE-STATUS-INDICATOR        PIC X  VALUE '0'.             ELP005  
00184          88 DE-NOT-FOUND                   VALUE '2'.             ELP005  
00185          88 DE-EOF                         VALUE '1'.             ELP005  
00186          88 DE-OK                          VALUE '0'.             ELP005  
00187      05  CV-STATUS-INDICATOR        PIC X  VALUE '0'.             ELP005  
00188          88 CV-NOT-FOUND                   VALUE '2'.             ELP005  
00189          88 CV-EOF                         VALUE '1'.             ELP005  
00190          88 CV-OK                          VALUE '0'.             ELP005  
00191      05  TCIX-INDICATOR             PIC X  VALUE 'N'.             ELP005  
00192          88  TCIX-NEEDED                   VALUE 'Y'.             ELP005  
00193          88  TCIX-NOT-NEEDED               VALUE 'N'.             ELP005  
00194      05  WS-DESC-SUB                PIC S9(4) COMP VALUE +0.      ELP005  
00195      05  WS-CODE-SUB                PIC S9(4) COMP VALUE +0.      ELP005  
00196      05  WS-LINE-NO                 PIC S9(4) COMP VALUE +0.      ELP005  
00197      05  WS-PAGE-NO                 PIC S9(4) COMP VALUE +0.      ELP005  
00198      05  WS-FROM-SUB                PIC S9(4) COMP VALUE +0.      ELP005  
00199      05  WS-TO-SUB                  PIC S9(4) COMP VALUE +0.      ELP005  
00200                                                                   ELP005  
00201 ******************************************************************ELP005  
00202 ** THE FOLLOWING ITEMS ARE USED FOR THE ELKAFPCT SUBROUTINE.    **ELP005  
00203 ** ALL VALUES IN THESE ITEMS ARE IN *PELS* FOR THE AFP PRINTERS **ELP005  
00204 ******************************************************************ELP005  
00205                                                                   ELP005  
00206      05  WS-BASE-LINE-COORD         PIC S9(4) COMP VALUE +0.      ELP005  
00207      05  WS-BASE-LINE-COORD-2       PIC S9(4) COMP VALUE +0.      ELP005  
00208      05  WS-BASE-LINE-LEN           PIC S9(4) COMP VALUE +1800.   ELP005  
00209      05  WS-IN-LINE-COORD           PIC S9(4) COMP VALUE +290.    ELP005  
00210      05  WS-IN-LINE-ZERO            PIC S9(4) COMP VALUE +0.      ELP005  
00211      05  WS-IN-LINE-LEN             PIC S9(4) COMP VALUE +0.      ELP005  
00212      05  WS-LINE-WIDTH              PIC S9(4) COMP VALUE +3.      ELP005  
00213 /*****************************************************************ELP005  
00214 ** IN ORDER FOR THE PAGE BREAKS TO WORK CORRECTLY AND FOR THE   **ELP005  
00215 ** OVERLAY TO BE FILLER IN CORRECTLY, THE MAX-LINES ITEMS MUST  **ELP005  
00216 ** BE HAVE THE CONSTANT RELATIONSHIP DEFINED BELOW.             **ELP005  
00217 ******************************************************************ELP005  
00218      05  MAX-LINES                  PIC S9999 COMP VALUE +50.     ELP005  
00219      05  MAX-LINES-LESS-1           PIC S9999 COMP VALUE +49.     ELP005  
00220      05  MAX-LINES-LESS-2           PIC S9999 COMP VALUE +48.     ELP005  
00221      05  MAX-LINES-LESS-4           PIC S9999 COMP VALUE +46.     ELP005  
00222 ******************************************************************ELP005  
00223      05  CONVERT-SEQ-NO             PIC 999V99     VALUE 0.       ELP005  
00224      05  CONVERT-SEQ-NO-RDF REDEFINES CONVERT-SEQ-NO.             ELP005  
00225          10  CONVERT-SEQ-NO-999     PIC 999.                      ELP005  
00226          10  CONVERT-SEQ-NO-99      PIC 99.                       ELP005  
00227      05  SAVE-CODE-VALUE            PIC X(10) VALUE LOW-VALUES.   ELP005  
00228                                                                   ELP005  
00229  01  WS-PRINT-DATE.                                               ELP005  
00230      05  WS-PRINT-MM                PIC XX    VALUE ZEROES.       ELP005  
00231      05  FILLER                     PIC X     VALUE '/'.          ELP005  
00232      05  WS-PRINT-DD                PIC XX    VALUE ZEROES.       ELP005  
00233      05  FILLER                     PIC X     VALUE '/'.          ELP005  
00234      05  WS-PRINT-YY                PIC XX    VALUE ZEROES.       ELP005  
00235 /                                                                 ELP005  
00236  01  FILLER                         PIC X(16)                     ELP005  
00237        VALUE '****HSCDATES****'.                                  ELP005  
00238                                                                   ELP005  
00239      COPY HSCDATES.                                               ELP005  
00240 /                                                                 ELP005  
00241  01  FILLER                         PIC X(16)                     ELP005  
00242        VALUE '****ELBHIOPM****'.                                  ELP005  
00243                                                                   ELP005  
00244  01  IO-INFO.                                                     ELP005  
00245      COPY ELBHIOPM.                                               ELP005  
00246 /                                                                 ELP005  
00247  01  FILLER                         PIC X(16)                     ELP005  
00248        VALUE '*****ELPRLC*****'.                                  ELP005  
00249                                                                   ELP005  
00250  01  WS-RL-RECORD.                                                ELP005  
00251      COPY ELPRLC.                                                 ELP005  
00252 /                                                                 ELP005  
00253  01  FILLER                         PIC X(16)                     ELP005  
00254        VALUE '*****ELPDEC*****'.                                  ELP005  
00255                                                                   ELP005  
00256  01  WS-DE-RECORD.                                                ELP005  
00257      COPY ELPDEC.                                                 ELP005  
00258 /                                                                 ELP005  
00259  01  FILLER                         PIC X(16)                     ELP005  
00260        VALUE '*****ELPCVC*****'.                                  ELP005  
00261                                                                   ELP005  
00262  01  WS-CV-RECORD.                                                ELP005  
00263      COPY ELPCVC.                                                 ELP005  
00264 /                                                                 ELP005  
00265      COPY ELAFPCNC.                                               ELP005  
00266 /                                                                 ELP005  
00267 *  THESE LINES ARE DIRECTLY RELATED TO THE PAGEDEF AND FORMDEF    ELP005  
00268 *  FOR OVERLAY EL0006.                                            ELP005  
00269 *                                                                 ELP005  
00270 *  CHANGE WITH CAUTION                                            ELP005  
00271 *                                                                 ELP005  
00272  01  WS-TOP-OF-FORM         PIC X         VALUE '1'.              ELP005  
00273  01  EL0006-HEAD-1.                                               ELP005  
00274      05  FILLER             PIC X         VALUE '2'.              ELP005  
00275      05  EL6-RECORD-NAME    PIC X(8)      VALUE SPACES.           ELP005  
00276      05  FILLER             PIC X         VALUE SPACE.            ELP005  
00277      05  EL6-ELEMENT-NUM    PIC 999.99    VALUE ZERO              ELP005  
00278                                           BLANK WHEN ZERO.        ELP005  
00279      05  FILLER             PIC X         VALUE SPACE.            ELP005  
00280      05  EL6-PAGE-NUM       PIC 99        VALUE ZERO              ELP005  
00281                                           BLANK WHEN ZERO.        ELP005  
00282      05  FILLER             PIC X         VALUE SPACE.            ELP005  
00283      05  EL6-PRINT-DATE     PIC X(11)     VALUE SPACES.           ELP005  
00284  01  EL0006-HEAD-2.                                               ELP005  
00285      05  FILLER             PIC X         VALUE '3'.              ELP005  
00286      05  EL6-ELEMENT-NAME   PIC X(75)     VALUE SPACES.           ELP005  
00287  01  EL0006-HEAD-3.                                               ELP005  
00288      05  FILLER             PIC X         VALUE '4'.              ELP005  
00289      05  EL6-SYSTEM-NAME    PIC X(30)     VALUE SPACES.           ELP005  
00290      05  FILLER             PIC X         VALUE SPACE.            ELP005  
00291      05  EL6-ATB-NAME       PIC X(8)      VALUE SPACES.           ELP005  
00292  01  EL0006-HEAD-4.                                               ELP005  
00293      05  FILLER             PIC X         VALUE '5'.              ELP005  
00294      05  FILLER             PIC X(4)      VALUE SPACE.            ELP005  
00295      05  EL6-TAB-IDS        PIC X(96)     VALUE SPACES.           ELP005  
00296      05  EL6-TAB-ID      REDEFINES  EL6-TAB-IDS                   ELP005  
00297                             PIC X(8)                              ELP005  
00298                             OCCURS 12 TIMES.                      ELP005  
00299  01  EL0006-HEAD-5.                                               ELP005  
00300      05  FILLER             PIC X         VALUE '6'.              ELP005  
00301      05  EL6-PRINT-LINE     PIC X(90)     VALUE SPACES.           ELP005  
00302                                                                   ELP005  
00303  01  WS-COL-HEADING.                                              ELP005  
00304      05  FILLER             PIC X         VALUE SPACE.            ELP005  
00305      05  FILLER             PIC X(14)     VALUE '  CODE VALUE  '. ELP005  
00306      05  FILLER             PIC X(75)     VALUE                   ELP005  
00307            '   CODE DESCRIPTION'.                                 ELP005  
00308                                                                   ELP005  
00309  01  WS-COL-HEADING-CONT.                                         ELP005  
00310      05  FILLER             PIC X         VALUE '6'.              ELP005  
00311      05  FILLER             PIC X(14)     VALUE '  CODE VALUE  '. ELP005  
00312      05  FILLER             PIC X(75)     VALUE                   ELP005  
00313            '   CODE DESCRIPTION  (CONTINUED)'.                    ELP005  
00314 /                                                                 ELP005  
00315  01  WS-DETAIL-CODE-LINE.                                         ELP005  
00316      05  FILLER             PIC X         VALUE SPACE.            ELP005  
00317      05  FILLER             PIC XX        VALUE SPACES.           ELP005  
00318      05  DCL-CODE-VALUE     PIC X(10)     VALUE SPACES.           ELP005  
00319      05  DCL-CODE-VALUE-CHARS  REDEFINES  DCL-CODE-VALUE          ELP005  
00320                             OCCURS 10 TIMES                       ELP005  
00321                             PIC X.                                ELP005  
00322      05  FILLER             PIC XX        VALUE SPACES.           ELP005  
00323      05  FILLER             PIC XX        VALUE SPACE.            ELP005  
00324      05  DCL-CODE-DESC      PIC X(75)     VALUE SPACES.           ELP005  
00325                                                                   ELP005  
00326  01  WS-BLANK-CODE-LINE.                                          ELP005  
00327      05  FILLER             PIC X         VALUE SPACE.            ELP005  
00328      05  FILLER             PIC X(14)     VALUE SPACES.           ELP005  
00329      05  FILLER             PIC X(78)     VALUE SPACES.           ELP005  
00330 /                                                                 ELP005  
00331  01  WS-COVER-ALL.                                                ELP005  
00332      05  FILLER        PIC X(35) VALUE SPACES.                    ELP005  
00333      05  FILLER        PIC X(21) VALUE 'COMPLETE CODES MANUAL'.   ELP005  
00334  01  WS-COVER-SELECTED.                                           ELP005  
00335      05  FILLER        PIC X(28) VALUE SPACES.                    ELP005  
00336      05  FILLER        PIC X(21) VALUE 'SELECTED RECORDS AND '.   ELP005  
00337      05  FILLER        PIC X(13) VALUE 'DATA ELEMENTS'.           ELP005  
00338  01  WS-COVER-UPDATES.                                            ELP005  
00339      05  FILLER        PIC X(46) VALUE SPACES.                    ELP005  
00340      05  FILLER        PIC X(7)  VALUE 'UPDATES'.                 ELP005  
00341  01  WS-ELEMENT-DESC-LINE.                                        ELP005  
00342      05  FILLER             PIC X         VALUE SPACE.            ELP005  
00343      05  FILLER             PIC X(4)      VALUE SPACES.           ELP005  
00344      05  WS-ELEMENT-DESC    PIC X(79)     VALUE SPACES.           ELP005  
00345      05  FILLER             PIC X(7)      VALUE SPACES.           ELP005  
00346  01  FOOT1.                                                       ELP005  
00347      05  FILLER             PIC X(20) VALUE                       ELP005  
00348          'DATA ELEMENT: LENGTH'.                                  ELP005  
00349      05  FOOT1-ELEMENT-LGTH PIC ZZ9   VALUE ZEROES.               ELP005  
00350      05  FILLER             PIC X(10) VALUE                       ELP005  
00351          ', FORMAT: '.                                            ELP005  
00352      05  FOOT1-ELEMENT-FMT  PIC X(13) VALUE SPACES.               ELP005  
00353      05  FILLER             PIC X     VALUE SPACES.               ELP005  
00354      05  FOOT1-FORMAT-COMMENT                                     ELP005  
00355                             PIC X(21) VALUE SPACES.               ELP005  
00356  01  DELETE-ELPRL-1.                                              ELP005  
00357      05  FILLER             PIC X(23) VALUE SPACES.               ELP005  
00358      05  FILLER             PIC X(29) VALUE                       ELP005  
00359          'THIS RECORD HAS BEEN DELETED.'.                         ELP005  
00360  01  DELETE-ELPRL-2.                                              ELP005  
00361      05  FILLER             PIC X(20) VALUE SPACES.               ELP005  
00362      05  FILLER             PIC X(34) VALUE                       ELP005  
00363          'REMOVE ALL PAGES WITH THIS PREFIX '.                    ELP005  
00364  01  DELETE-ELPDE-1.                                              ELP005  
00365      05  FILLER             PIC X(22) VALUE SPACES.               ELP005  
00366      05  FILLER             PIC X(35) VALUE                       ELP005  
00367          'THIS DATA ELEMENT HAS BEEN DELETED.'.                   ELP005  
00368  01  DELETE-ELPDE-2.                                              ELP005  
00369      05  FILLER             PIC X(10) VALUE SPACES.               ELP005  
00370      05  FILLER             PIC X(34) VALUE                       ELP005  
00371          'REMOVE ALL PAGES WITH THIS PREFIX '.                    ELP005  
00372      05  FILLER             PIC X(24) VALUE                       ELP005  
00373          'AND DATA ELEMENT NUMBER.'.                              ELP005  
00374  01  WS-CONTINUED-FOLLOWING.                                      ELP005  
00375      05  FILLER             PIC X(55) VALUE SPACES.               ELP005  
00376      05  FILLER             PIC X(35) VALUE                       ELP005  
00377           '*** CONTINUED ON FOLLOWING PAGE ***'.                  ELP005  
00378 /                                                                 ELP005  
00379  01  WS-NO-PREFIX-ERROR.                                          ELP005  
00380      05  FILLER             PIC X(35) VALUE                       ELP005  
00381            'UNABLE TO LOCATE PREFIX RECORD FOR '.                 ELP005  
00382      05  WS-NO-PREFIX-RL    PIC X(8).                             ELP005  
00383      05  FILLER             PIC X  VALUE '.'.                     ELP005  
00384                                                                   ELP005  
00385  01  WS-NO-DATA-ELEMENT-ERROR.                                    ELP005  
00386      05  FILLER             PIC X(34) VALUE                       ELP005  
00387            'UNABLE TO LOCATE DATA ELEMENT FOR '.                  ELP005  
00388      05  WS-NO-PREFIX-DE    PIC X(8).                             ELP005  
00389      05  FILLER             PIC X(9)  VALUE ' ELEMENT '.          ELP005  
00390      05  WS-ELEMENT-NO-DE   PIC 999.99.                           ELP005  
00391                                                                   ELP005  
00392  01  MISSING-PARMS.                                               ELP005  
00393      05  FILLER             PIC X(35) VALUE                       ELP005  
00394      'NO PARAMETER FILE RECORDS          '.                       ELP005  
00395                                                                   ELP005  
00396  01  MISSING-SELECT.                                              ELP005  
00397      05  FILLER             PIC X(35) VALUE                       ELP005  
00398      'NO SELECT RECORDS ON PARAMETER FILE'.                       ELP005  
00399                                                                   ELP005  
00400  01  BAD-OPTION.                                                  ELP005  
00401      05  FILLER             PIC X(35) VALUE                       ELP005  
00402      'PRINT OPTION MUST BE A, S, OR U    '.                       ELP005  
00403                                                                   ELP005  
00404  01  BAD-SELECT.                                                  ELP005  
00405      05  FILLER             PIC X(35) VALUE                       ELP005  
00406      'SELECT OPTION MUST BE R OR D       '.                       ELP005  
00407                                                                   ELP005  
00408  01  BAD-PREFIX.                                                  ELP005  
00409      05  FILLER             PIC X(35) VALUE                       ELP005  
00410      'RECORD LIST FILE KEY MISSING       '.                       ELP005  
00411                                                                   ELP005  
00412  01  BAD-ELEMENT-NUMBER.                                          ELP005  
00413      05  FILLER             PIC X(35) VALUE                       ELP005  
00414      'INVALID DATA ELEMENT NUMBER        '.                       ELP005  
00415                                                                   ELP005  
00416  01  PARM-ERROR-MSG.                                              ELP005  
00417      05  FILLER             PIC X(8)  VALUE 'ELP005  '.           ELP005  
00418      05  FILLER             PIC X(15) VALUE ' INVALID PARM: '.    ELP005  
00419      05  MSG-PARM-ERROR     PIC X(35) VALUE SPACES.               ELP005  
00420      05  FILLER             PIC X(7)  VALUE ' INPUT='.            ELP005  
00421      05  MSG-PARM-VALUE     PIC X(15) VALUE SPACES.               ELP005  
00422                                                                   ELP005  
00423  01  PARM-ABEND-MSG.                                              ELP005  
00424      05  FILLER             PIC X(8)  VALUE 'ELP005  '.           ELP005  
00425      05  FILLER             PIC X(15) VALUE ' ABNORMAL EOJ: '.    ELP005  
00426      05  MSG-PARM-ABEND     PIC X(35) VALUE SPACES.               ELP005  
00427                                                                   ELP005  
00428  01  ABEND-CODE                     PIC S9(04) COMP VALUE ZEROS.  ELP005  
00429      88  TERMINATE-JOB                         VALUE +16.         ELP005  
00430 /                                                                 ELP005  
00431  01  WS-ERROR-LINE-1.                                             ELP005  
00432      05  FILLER                     PIC X(4)  VALUE SPACE.        ELP005  
00433      05  FILLER                     PIC X(85) VALUE ALL '*'.      ELP005  
00434  01  WS-ERROR-LINE-2.                                             ELP005  
00435      05  FILLER                     PIC X(4)  VALUE SPACE.        ELP005  
00436      05  FILLER                     PIC X(2)  VALUE ALL '*'.      ELP005  
00437      05  FILLER                     PIC X(2)  VALUE SPACES.       ELP005  
00438      05  WS-ERROR-PRINT-MSG         PIC X(77) VALUE SPACES.       ELP005  
00439      05  FILLER                     PIC X(2)  VALUE SPACES.       ELP005  
00440      05  FILLER                     PIC X(2)  VALUE ALL '*'.      ELP005  
00441 /                                                                 ELP005  
00442  PROCEDURE DIVISION.                                              ELP005  
00443                                                                   ELP005  
00444  0000-MAINLINE.                                                   ELP005  
00445      PERFORM 0010-INITIALIZATION.                                 ELP005  
00446      PERFORM 1000-MAIN-PROCESSING.                                ELP005  
00447      PERFORM 0500-TERMINATION.                                    ELP005  
00448      GOBACK.                                                      ELP005  
00449                                                                   ELP005  
00450  0010-INITIALIZATION.                                             ELP005  
00451      PERFORM 0020-OPEN-FILES.                                     ELP005  
00452      PERFORM 8000-READ-PARM.                                      ELP005  
00453      IF PARM-EOF                                                  ELP005  
00454          MOVE MISSING-PARMS TO MSG-PARM-ABEND                     ELP005  
00455          SET TERMINATE-JOB TO TRUE                                ELP005  
00456      ELSE                                                         ELP005  
00457          IF PRINT-ALL OR PRINT-SELECTED OR PRINT-UPDATED          ELP005  
00458              PERFORM 0030-PRINT-COVER-SHEET                       ELP005  
00459          ELSE                                                     ELP005  
00460              MOVE BAD-OPTION TO MSG-PARM-ABEND                    ELP005  
00461              SET TERMINATE-JOB TO TRUE                            ELP005  
00462          END-IF                                                   ELP005  
00463      END-IF.                                                      ELP005  
00464      IF TERMINATE-JOB                                             ELP005  
00465          PERFORM 9400-PARM-ABEND.                                 ELP005  
00466                                                                   ELP005  
00467  0020-OPEN-FILES.                                                 ELP005  
00468                                                                   ELP005  
00469 ******************************************************************ELP005  
00470 **    ONLY ONE OPEN IS ISSUED FOR THE VSAM FILE (RL, DE, CV).   **ELP005  
00471 ** THE IO INTERFACE WILL OPEN ALL THE VSAM FILES NEEDED.        **ELP005  
00472 ******************************************************************ELP005  
00473                                                                   ELP005  
00474      OPEN INPUT PARM-FILE                                         ELP005  
00475           OUTPUT PRINT-FILE.                                      ELP005  
00476                                                                   ELP005  
00477      SET RECORD-LIST-FILE TO TRUE.                                ELP005  
00478      SET ELBHIO-OPEN TO TRUE.                                     ELP005  
00479                                                                   ELP005  
00480      CALL  'ELBIOPGM'  USING  IO-INFO.                            ELP005  
00481                                                                   ELP005  
00482      IF ELBHIO-GOOD-RETURN                                        ELP005  
00483          CONTINUE                                                 ELP005  
00484      ELSE                                                         ELP005  
00485          MOVE ELBHIO-FEEDBACK  TO  ABEND-CODE                     ELP005  
00486          PERFORM 9999-ABEND-RTN                                   ELP005  
00487      END-IF.                                                      ELP005  
00488 /                                                                 ELP005  
00489  0030-PRINT-COVER-SHEET.                                          ELP005  
00490      INITIALIZE EL0006-HEAD-1.                                    ELP005  
00491      CALL 'TCDTES' USING HSCDATES.                                ELP005  
00492      MOVE TMO   TO WS-PRINT-MM.                                   ELP005  
00493      MOVE TDA   TO WS-PRINT-DD.                                   ELP005  
00494      MOVE TYR   TO WS-PRINT-YY.                                   ELP005  
00495      MOVE WS-PRINT-DATE TO EL6-PRINT-DATE.                        ELP005  
00496      WRITE PRT-REC FROM WS-TOP-OF-FORM.                           ELP005  
00497      WRITE PRT-REC FROM EL0006-HEAD-1.                            ELP005  
00498      MOVE SPACES TO EL6-PRINT-LINE.                               ELP005  
00499      WRITE PRT-REC FROM EL0006-HEAD-5.                            ELP005  
00500                                                                   ELP005  
00501      PERFORM 4100-PRINT-BLANK-LINE 3 TIMES.                       ELP005  
00502                                                                   ELP005  
00503      IF PRINT-SELECTED                                            ELP005  
00504          MOVE WS-COVER-SELECTED TO PT-DATA                        ELP005  
00505      ELSE                                                         ELP005  
00506          IF PRINT-UPDATED                                         ELP005  
00507              MOVE WS-COVER-UPDATES TO PT-DATA                     ELP005  
00508          ELSE                                                     ELP005  
00509              MOVE WS-COVER-ALL TO PT-DATA.                        ELP005  
00510      MOVE SPACE TO PT-CC.                                         ELP005  
00511      WRITE PRT-REC.                                               ELP005  
00512                                                                   ELP005  
00513  0500-TERMINATION.                                                ELP005  
00514      CLOSE PARM-FILE PRINT-FILE.                                  ELP005  
00515      SET RECORD-LIST-FILE TO TRUE.                                ELP005  
00516      SET ELBHIO-CLOSE TO TRUE.                                    ELP005  
00517      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP005  
00518      IF ELBHIO-GOOD-RETURN                                        ELP005  
00519          CONTINUE                                                 ELP005  
00520      ELSE                                                         ELP005  
00521          MOVE ELBHIO-FEEDBACK  TO  ABEND-CODE                     ELP005  
00522          PERFORM 9999-ABEND-RTN.                                  ELP005  
00523                                                                   ELP005  
00524      IF TCIX-NEEDED                                               ELP005  
00525          MOVE 4  TO  RETURN-CODE                                  ELP005  
00526      ELSE                                                         ELP005  
00527          MOVE 0  TO  RETURN-CODE.                                 ELP005  
00528       TITLE 'PRINT THE CODES MANUAL - MAIN PROCESSING'.           ELP005  
00529  1000-MAIN-PROCESSING.                                            ELP005  
00530 ******************************************************************ELP005  
00531 **  IF PRINT OPTION IS FOR SELECTED ITEMS, THEN THE PARM        **ELP005  
00532 **  INPUT FILE SHOULD CONTAIN AT LEAST ONE SELECTION RECORD.    **ELP005  
00533 **  WITH THIS PRINT OPTION, PROGRAM LOGIC IS DRIVEN BY THE      **ELP005  
00534 **  PARM FILE.  EACH PARM RECORD POINTS AT A RECORD LIST RECORD **ELP005  
00535 **  OR AT A DATA ELEMENT RECORD.                                **ELP005  
00536 **  FOR THE OTHER PRINT OPTIONS, PROGRAM LOGIC IS DRIVEN BY     **ELP005  
00537 **  SEQUENTIAL ACCESS TO THE ELPRL FILE.                        **ELP005  
00538 ******************************************************************ELP005  
00539      IF PRINT-SELECTED                                            ELP005  
00540          PERFORM 8000-READ-PARM                                   ELP005  
00541          IF PARM-EOF                                              ELP005  
00542              MOVE MISSING-SELECT TO MSG-PARM-ABEND                ELP005  
00543              SET TERMINATE-JOB TO TRUE                            ELP005  
00544              PERFORM 9400-PARM-ABEND                              ELP005  
00545          ELSE                                                     ELP005  
00546              PERFORM 1100-ANALYZE-PARM                            ELP005  
00547                  UNTIL PARM-EOF                                   ELP005  
00548      ELSE                                                         ELP005  
00549          PERFORM 1200-PROCESS-ALL.                                ELP005  
00550 /                                                                 ELP005  
00551 ******************************************************************ELP005  
00552 ** PRINT SELECTED PROCESSING STEPS                              **ELP005  
00553 ** A. EDITING OF PARMS - WE ALLOW TWO TYPES OF SELECTION PARMS, **ELP005  
00554 **    ONE WITH A KEY FOR THE RECORD LIST FILE AND ONE WITH A    **ELP005  
00555 **    KEY FOR THE DATA ELEMENT FILE.  BOTH TYPES NEED A         **ELP005  
00556 **    PREFIX FIELD.  THE DATA ELEMENT KEY ALSO NEEDS A DATA     **ELP005  
00557 **    ELEMENT NUMBER, WHICH MUST BE ENTERED AS 999.99 OR 99999. **ELP005  
00558 **    IN EITHER CASE, TWO DECIMAL PLACES ARE ASSUMED.           **ELP005  
00559 ** B. READING THE VSAM FILES - WE USE THE PARM INPUT TO BUILD   **ELP005  
00560 **    THE KEY AND READ THE APPROPRIATE FILE.  IF NOT FOUND,     **ELP005  
00561 **    THE READ ROUTINE WILL PUT OUT AN ERROR MESSAGE.           **ELP005  
00562 ** C. REPORTING THE RESULTS, USING THE APPROPRIATE ROUTINE.     **ELP005  
00563 ** D. OBTAINING THE NEXT PARM RECORD.                           **ELP005  
00564 ******************************************************************ELP005  
00565                                                                   ELP005  
00566  1100-ANALYZE-PARM.                                               ELP005  
00567                                                                   ELP005  
00568      IF SELECT-BY-RECORD-LIST OR SELECT-BY-DATA-ELEMENT           ELP005  
00569          PERFORM 1110-VALIDATE-PREFIX-KEY                         ELP005  
00570      ELSE                                                         ELP005  
00571          MOVE BAD-SELECT TO MSG-PARM-ERROR                        ELP005  
00572          PERFORM 9300-PARM-ERROR-MSG                              ELP005  
00573          PERFORM 8000-READ-PARM                                   ELP005  
00574      END-IF.                                                      ELP005  
00575                                                                   ELP005  
00576  1110-VALIDATE-PREFIX-KEY.                                        ELP005  
00577      IF PARM-PREFIX-KEY = SPACES OR LOW-VALUES OR ZEROES          ELP005  
00578          MOVE BAD-PREFIX TO MSG-PARM-ERROR                        ELP005  
00579          PERFORM 9300-PARM-ERROR-MSG                              ELP005  
00580          PERFORM 8000-READ-PARM                                   ELP005  
00581      ELSE                                                         ELP005  
00582          IF SELECT-BY-DATA-ELEMENT                                ELP005  
00583              PERFORM 1120-VALIDATE-DATA-ELEMENT                   ELP005  
00584          ELSE                                                     ELP005  
00585              IF SELECT-BY-RECORD-LIST                             ELP005  
00586                  MOVE PARM-PREFIX-KEY  TO  ELBHIO-ELPRL-KEY       ELP005  
00587                  PERFORM 8100-READ-ELPRL                          ELP005  
00588                  IF RL-OK AND NOT RL-DELETE                       ELP005  
00589                      PERFORM 1400-REPORT-DE-WITHIN-RL             ELP005  
00590                  ELSE                                             ELP005  
00591                      PERFORM 9000-RL-NOT-FOUND.                   ELP005  
00592                                                                   ELP005  
00593      PERFORM 8000-READ-PARM.                                      ELP005  
00594 /                                                                 ELP005  
00595  1120-VALIDATE-DATA-ELEMENT.                                      ELP005  
00596      IF PARM-ELEMENT-999 IS NUMERIC                               ELP005  
00597          MOVE PARM-ELEMENT-NO-999                                 ELP005  
00598                  TO CONVERT-SEQ-NO-999                            ELP005  
00599          IF POINT-CHAR-ENTERED                                    ELP005  
00600              IF PARM-SUFFIX-POINT99 IS NUMERIC                    ELP005  
00601                  MOVE PARM-SUFFIX-NO-POINT99                      ELP005  
00602                          TO CONVERT-SEQ-NO-99                     ELP005  
00603                  PERFORM 1130-SELECT-BY-DATA-ELEMENT              ELP005  
00604              ELSE                                                 ELP005  
00605                  PERFORM  1190-INVALID-ELEMENT-NUM                ELP005  
00606              END-IF                                               ELP005  
00607          ELSE                                                     ELP005  
00608              IF CHAR-NOT-ENTERED                                  ELP005  
00609                  IF PARM-SUFFIX-99SPACE IS NUMERIC                ELP005  
00610                      MOVE PARM-SUFFIX-NO-99SPACE                  ELP005  
00611                              TO CONVERT-SEQ-NO-99                 ELP005  
00612                      PERFORM 1130-SELECT-BY-DATA-ELEMENT          ELP005  
00613                  ELSE                                             ELP005  
00614                      PERFORM  1190-INVALID-ELEMENT-NUM            ELP005  
00615                  END-IF                                           ELP005  
00616              ELSE                                                 ELP005  
00617                  PERFORM  1190-INVALID-ELEMENT-NUM                ELP005  
00618              END-IF                                               ELP005  
00619          END-IF                                                   ELP005  
00620      ELSE                                                         ELP005  
00621          PERFORM  1190-INVALID-ELEMENT-NUM                        ELP005  
00622      END-IF.                                                      ELP005  
00623                                                                   ELP005  
00624  1130-SELECT-BY-DATA-ELEMENT.                                     ELP005  
00625      MOVE PARM-PREFIX-KEY  TO  ELBHIO-DE-RECORD-PREFIX            ELP005  
00626      MOVE CONVERT-SEQ-NO   TO  ELBHIO-DE-ELEMENT-NBR              ELP005  
00627      PERFORM 8200-READ-ELPDE                                      ELP005  
00628      IF DE-OK AND NOT DE-DELETE                                   ELP005  
00629          PERFORM 1600-GENERATE-RPT-FOR-ELPDE                      ELP005  
00630      ELSE                                                         ELP005  
00631          PERFORM 9100-DE-NOT-FOUND.                               ELP005  
00632                                                                   ELP005  
00633                                                                   ELP005  
00634  1190-INVALID-ELEMENT-NUM.                                        ELP005  
00635      MOVE BAD-ELEMENT-NUMBER TO MSG-PARM-ERROR.                   ELP005  
00636      PERFORM 9300-PARM-ERROR-MSG.                                 ELP005  
00637 /*****************************************************************ELP005  
00638 ** PRINT ALL PROCESSING STEPS:                                  **ELP005  
00639 ** SEQUENTIAL READ OF ELPRL (RECORD LIST FILE)                  **ELP005  
00640 ** CHECK FOR EOF                                                **ELP005  
00641 ** IF PRINT OPTION IS 'ALL', WE GENERATE A REPORT FOR ALL       **ELP005  
00642 ** ELEMENTS AND CODE VALUES ASSOCIATED WITH THE ELPRL RECORD.   **ELP005  
00643 ** IF PRINT OPTION IS 'UPDATED', WE CHECK THE UPDATE AND DELETE **ELP005  
00644 ** FLAGS IN THE ELPRL RECORD.                                   **ELP005  
00645 ** IF THE ELPRL RECORD HAS BEEN UPDATED, WE GENERATE A REPORT   **ELP005  
00646 ** FOR ALL DATA ELEMENTS AND CODE VALUES ASSOCIATED WITH IT.    **ELP005  
00647 ** IF THE ELPRL RECORD HAS BEEN DELETED, WE GENERATE A DELETE   **ELP005  
00648 ** SHEET INDICATING WHAT PAGE RANGE SHOULD BE REMOVED FROM      **ELP005  
00649 ** THE USER'S COPY OF THE MANUAL.                               **ELP005  
00650 ** IF NEITHER OF THE ABOVE, WE CHECK THE FLAGS AT THE DATA      **ELP005  
00651 ** ELEMENT LEVEL, AND GENERATE A REPORT ONLY FOR THOSE ELEMENTS **ELP005  
00652 ** WHOSE FLAGS ARE SET.                                         **ELP005  
00653 ******************************************************************ELP005  
00654                                                                   ELP005  
00655  1200-PROCESS-ALL.                                                ELP005  
00656      PERFORM 8110-READ-NEXT-ELPRL.                                ELP005  
00657      IF RL-RECORD-PREFIX = LOW-VALUES                             ELP005  
00658          PERFORM 8110-READ-NEXT-ELPRL.                            ELP005  
00659      PERFORM 1210-EXAMINE-ELPRL-RECORD                            ELP005  
00660         UNTIL RL-EOF.                                             ELP005  
00661                                                                   ELP005  
00662  1210-EXAMINE-ELPRL-RECORD.                                       ELP005  
00663                                                                   ELP005  
00664      EVALUATE TRUE                                                ELP005  
00665      WHEN RL-DELETE                                               ELP005  
00666           IF PRINT-UPDATED                                        ELP005  
00667                PERFORM 3100-PRINT-DELETED-RL                      ELP005  
00668           END-IF                                                  ELP005  
00669      WHEN PRINT-ALL OR RL-REPRINT                                 ELP005  
00670           MOVE 'Y'  TO  TCIX-INDICATOR                            ELP005  
00671           PERFORM 1400-REPORT-DE-WITHIN-RL                        ELP005  
00672      WHEN OTHER                                                   ELP005  
00673           PERFORM 1400-REPORT-DE-WITHIN-RL                        ELP005  
00674      END-EVALUATE.                                                ELP005  
00675      PERFORM 8110-READ-NEXT-ELPRL.                                ELP005  
00676                                                                   ELP005  
00677 ******************************************************************ELP005  
00678 ** OBTAIN ALL DATA ELEMENTS ASSOCIATED WITH THE CURRENT RECORD  **ELP005  
00679 ** LIST RECORD.  GENERATE APPROPRIATE REPORTS DEPENDING         **ELP005  
00680 ** ON PRINT OPTION AND UPDATE/DELETE FLAGS.                     **ELP005  
00681 ******************************************************************ELP005  
00682                                                                   ELP005  
00683  1400-REPORT-DE-WITHIN-RL.                                        ELP005  
00684                                                                   ELP005  
00685      MOVE RL-RECORD-PREFIX  TO  ELBHIO-DE-RECORD-PREFIX.          ELP005  
00686                                                                   ELP005  
00687      PERFORM 8220-POINT-ELPDE.                                    ELP005  
00688                                                                   ELP005  
00689      IF DE-OK                                                     ELP005  
00690          PERFORM 1500-EXAMINE-ELPDE-RECORD                        ELP005  
00691              UNTIL DE-EOF                                         ELP005  
00692      END-IF.                                                      ELP005  
00693 /*****************************************************************ELP005  
00694 ** IF PRINT OPTION IS FOR ALL RECORDS, OR IF THE RECORD OR DATA **ELP005  
00695 ** ELEMENT HAS BEEN SELECTED BY THE USER, WE GENERATE A REPORT  **ELP005  
00696 ** FOR THE DATA ELEMENT.  THE SAME IS TRUE IF THE REPRINT       **ELP005  
00697 ** FLAGS ARE SET, EITHER AT THE RECORD LIST LEVEL OR AT THE     **ELP005  
00698 ** DATA ELEMENT LEVEL.  IF THE DELETE FLAGS ARE SET AT EITHER   **ELP005  
00699 ** LEVEL, THE REPORT WILL INDICATE ONLY THE FACT THAT THE ITEM  **ELP005  
00700 ** HAS BEEN DELETED.                                            **ELP005  
00701 ******************************************************************ELP005  
00702                                                                   ELP005  
00703  1500-EXAMINE-ELPDE-RECORD.                                       ELP005  
00704                                                                   ELP005  
00705      PERFORM 8210-READ-NEXT-ELPDE.                                ELP005  
00706                                                                   ELP005  
00707      IF DE-OK AND                                                 ELP005  
00708         ELBHIO-DE-RECORD-PREFIX  =  RL-RECORD-PREFIX              ELP005  
00709             PERFORM 1510-SAME-REQUEST                             ELP005  
00710      ELSE                                                         ELP005  
00711         SET DE-EOF TO TRUE                                        ELP005  
00712      END-IF.                                                      ELP005  
00713                                                                   ELP005  
00714  1510-SAME-REQUEST.                                               ELP005  
00715      IF RL-REPRINT OR DE-ENG-NAME-CHG                             ELP005  
00716          MOVE 'Y'  TO  TCIX-INDICATOR                             ELP005  
00717          PERFORM 1600-GENERATE-RPT-FOR-ELPDE                      ELP005  
00718      ELSE                                                         ELP005  
00719          IF DE-REPRINT                                            ELP005  
00720              PERFORM 1600-GENERATE-RPT-FOR-ELPDE                  ELP005  
00721          ELSE                                                     ELP005  
00722              IF DE-DELETE                                         ELP005  
00723                  IF PRINT-UPDATED                                 ELP005  
00724                      MOVE 'Y'  TO  TCIX-INDICATOR                 ELP005  
00725                      PERFORM 3000-PRINT-DELETE-SHEET-ELPDE        ELP005  
00726                  END-IF                                           ELP005  
00727              ELSE                                                 ELP005  
00728                  IF  PRINT-ALL                                    ELP005  
00729                          OR                                       ELP005  
00730                      SELECT-BY-RECORD-LIST                        ELP005  
00731                          OR                                       ELP005  
00732                      SELECT-BY-DATA-ELEMENT                       ELP005  
00733                       PERFORM 1600-GENERATE-RPT-FOR-ELPDE         ELP005  
00734                  ELSE                                             ELP005  
00735                      NEXT SENTENCE.                               ELP005  
00736 /*****************************************************************ELP005  
00737 ** WE ALWAYS HAVE A DESCRIPTIVE HEADER AND A TRAILER LINE FOR   **ELP005  
00738 ** EACH DATA ELEMENT.  ANY CODE VALUES GO BETWEEN THESE.  IF    **ELP005  
00739 ** CODE VALUES AND THEIR DESCRIPTIONS OVERFLOW THE PAGE, WE     **ELP005  
00740 ** PRINT A CONTINUATION TRAILER MESSAGE AND A CONTINUATION      **ELP005  
00741 ** HEADER.  WE ONLY PRINT THE CODE VALUES SUBHEAD IF WE HAVE    **ELP005  
00742 ** FOUND AT LEAST ONE CODE VALUE RECORD ON THE FILE.            **ELP005  
00743 **                                                              **ELP005  
00744 ******************************************************************ELP005  
00745  1600-GENERATE-RPT-FOR-ELPDE.                                     ELP005  
00746                                                                   ELP005  
00747      PERFORM 3200-DATA-ELEMENT-HDR.                               ELP005  
00748                                                                   ELP005  
00749      MOVE DE-RECORD-PREFIX  TO  ELBHIO-CV-RECORD-PREFIX.          ELP005  
00750      MOVE DE-ELEMENT-NBR    TO  ELBHIO-CV-ELEMENT-NBR.            ELP005  
00751                                                                   ELP005  
00752      PERFORM 8310-POINT-ELPCV.                                    ELP005  
00753                                                                   ELP005  
00754      IF CV-OK                                                     ELP005  
00755          PERFORM 1620-PROCESS-NEXT-PCV                            ELP005  
00756      ELSE                                                         ELP005  
00757          PERFORM 3500-PRINT-ELEMENT-INFO.                         ELP005  
00758                                                                   ELP005  
00759  1620-PROCESS-NEXT-PCV.                                           ELP005  
00760      PERFORM 8300-READ-NEXT-ELPCV.                                ELP005  
00761      IF CV-EOF                                                    ELP005  
00762          CONTINUE                                                 ELP005  
00763      ELSE                                                         ELP005  
00764          PERFORM 2000-PRINT-CODE-VALUE                            ELP005  
00765              UNTIL CV-EOF.                                        ELP005  
00766 /*****************************************************************ELP005  
00767 ** NOTICE THAT THERE CAN BE MORE THAN ONE CODE VALUE RECORD     **ELP005  
00768 ** PER CODE VALUE.  THIS IS THE CASE IF THE DESCRIPTION         **ELP005  
00769 ** TAKES UP MORE THAN 12 LINES.  THERE IS A RECORD SEQUENCE NO  **ELP005  
00770 ** IN THE CODE VALUE VSAM KEY, ALTHOUGH WE AREN'T CONCERNED     **ELP005  
00771 ** WITH THAT BECAUSE WE ARE BROWSING UP TO THE NEXT CODE VALUE  **ELP005  
00772 ** BREAK, AND BECAUSE WE DON'T PRINT THE RECORD SEQUENCE NUMBER.**ELP005  
00773 ** OUR PAGE SEQUENCE NUMBER ON THE REPORT IS DIFFERENT.         **ELP005  
00774 ** IT NUMBERS THE REPORT PAGES THAT HAVE TO DO WITH THE CURRENT **ELP005  
00775 ** DATA ELEMENT.                                                **ELP005  
00776 ******************************************************************ELP005  
00777                                                                   ELP005  
00778  2000-PRINT-CODE-VALUE.                                           ELP005  
00779                                                                   ELP005  
00780      MOVE CV-CODE-VALUE TO DCL-CODE-VALUE.                        ELP005  
00781      IF DCL-CODE-VALUE-CHARS (10) = SPACE                         ELP005  
00782           MOVE 10 TO WS-TO-SUB, WS-FROM-SUB                       ELP005  
00783           PERFORM UNTIL WS-FROM-SUB < 1                           ELP005  
00784                IF DCL-CODE-VALUE-CHARS (WS-FROM-SUB) NOT = SPACE  ELP005  
00785                    MOVE DCL-CODE-VALUE-CHARS (WS-FROM-SUB) TO     ELP005  
00786                         DCL-CODE-VALUE-CHARS (WS-TO-SUB)          ELP005  
00787                    MOVE SPACE TO DCL-CODE-VALUE-CHARS             ELP005  
00788                                    (WS-FROM-SUB)                  ELP005  
00789                    SUBTRACT 1 FROM WS-TO-SUB                      ELP005  
00790                END-IF                                             ELP005  
00791                SUBTRACT 1 FROM WS-FROM-SUB                        ELP005  
00792           END-PERFORM                                             ELP005  
00793      END-IF.                                                      ELP005  
00794                                                                   ELP005  
00795      IF CV-DELETE                                                 ELP005  
00796          MOVE CV-CODE-VALUE  TO  SAVE-CODE-VALUE                  ELP005  
00797          PERFORM 8300-READ-NEXT-ELPCV                             ELP005  
00798              UNTIL CV-EOF  OR                                     ELP005  
00799                    CV-CODE-VALUE    NOT  =  SAVE-CODE-VALUE       ELP005  
00800      ELSE                                                         ELP005  
00801          IF WS-LINE-NO NOT < MAX-LINES-LESS-4                     ELP005  
00802              PERFORM 3700-DATA-EL-CONTINUED-HDR                   ELP005  
00803          END-IF                                                   ELP005  
00804          PERFORM 4100-PRINT-BLANK-LINE                            ELP005  
00805          MOVE CV-CODE-VALUE  TO  SAVE-CODE-VALUE                  ELP005  
00806          PERFORM 2100-PRINT-ELPCV-REC                             ELP005  
00807              UNTIL CV-EOF  OR                                     ELP005  
00808                    CV-CODE-VALUE    NOT  =  SAVE-CODE-VALUE       ELP005  
00809      END-IF.                                                      ELP005  
00810                                                                   ELP005  
00811  2100-PRINT-ELPCV-REC.                                            ELP005  
00812                                                                   ELP005  
00813      PERFORM 3400-PRINT-CODE-VALUE-LINE                           ELP005  
00814          VARYING WS-CODE-SUB FROM 1 BY 1                          ELP005  
00815          UNTIL WS-CODE-SUB IS GREATER THAN                        ELP005  
00816              CV-NBR-VALUE-DESC-LINES.                             ELP005  
00817                                                                   ELP005  
00818      PERFORM 8300-READ-NEXT-ELPCV.                                ELP005  
00819 /*****************************************************************ELP005  
00820 ** THIS ROUTINE PRINTS A PAGE INDICATING THAT ALL SHEETS        **ELP005  
00821 ** PERTAINING TO THE CURRENT RECORD OR ELEMENT SHOULD           **ELP005  
00822 ** BE DELETED FROM THE CODES MANUAL.                            **ELP005  
00823 ******************************************************************ELP005  
00824                                                                   ELP005  
00825  3000-PRINT-DELETE-SHEET-ELPDE.                                   ELP005  
00826                                                                   ELP005  
00827      MOVE ZERO               TO WS-PAGE-NO.                       ELP005  
00828      PERFORM 4000-START-NEW-PAGE.                                 ELP005  
00829      WRITE PRT-REC FROM EL0006-HEAD-5.                            ELP005  
00830      PERFORM 4100-PRINT-BLANK-LINE 2 TIMES.                       ELP005  
00831      MOVE DELETE-ELPDE-1 TO WS-ELEMENT-DESC.                      ELP005  
00832      WRITE PRT-REC FROM WS-ELEMENT-DESC-LINE.                     ELP005  
00833      MOVE DELETE-ELPDE-2 TO WS-ELEMENT-DESC.                      ELP005  
00834      WRITE PRT-REC FROM WS-ELEMENT-DESC-LINE.                     ELP005  
00835                                                                   ELP005  
00836  3100-PRINT-DELETED-RL.                                           ELP005  
00837                                                                   ELP005  
00838      PERFORM 3190-INITIALIZE-HEADING.                             ELP005  
00839      MOVE RL-RECORD-PREFIX TO  DE-RECORD-PREFIX.                  ELP005  
00840      PERFORM 4000-START-NEW-PAGE.                                 ELP005  
00841      WRITE PRT-REC FROM EL0006-HEAD-5.                            ELP005  
00842      PERFORM 4100-PRINT-BLANK-LINE 2 TIMES.                       ELP005  
00843      MOVE DELETE-ELPRL-1  TO WS-ELEMENT-DESC.                     ELP005  
00844      WRITE PRT-REC FROM WS-ELEMENT-DESC-LINE.                     ELP005  
00845      MOVE DELETE-ELPRL-2  TO WS-ELEMENT-DESC.                     ELP005  
00846      WRITE PRT-REC FROM WS-ELEMENT-DESC-LINE.                     ELP005  
00847                                                                   ELP005  
00848  3190-INITIALIZE-HEADING.                                         ELP005  
00849                                                                   ELP005  
00850      INITIALIZE DE-RECORD-PREFIX.                                 ELP005  
00851      INITIALIZE DE-ELEMENT-NBR.                                   ELP005  
00852      MOVE ZERO TO WS-PAGE-NO.                                     ELP005  
00853      INITIALIZE DE-ELEMENT-NAME.                                  ELP005  
00854      INITIALIZE DE-COBOL-NAME.                                    ELP005  
00855      INITIALIZE DE-BAL-NAME.                                      ELP005  
00856 /*****************************************************************ELP005  
00857 ** THIS ROUTINE PRINTS THE FIRST HEADER FOR A DATA ELEMENT      **ELP005  
00858 ** RECORD, INCLUDING THE RECURRING DESCRIPTION LINES.           **ELP005  
00859 ******************************************************************ELP005  
00860                                                                   ELP005  
00861  3200-DATA-ELEMENT-HDR.                                           ELP005  
00862                                                                   ELP005  
00863      MOVE +1 TO WS-PAGE-NO.                                       ELP005  
00864      PERFORM  4000-START-NEW-PAGE.                                ELP005  
00865      WRITE PRT-REC FROM EL0006-HEAD-5.                            ELP005  
00866      MOVE +1 TO WS-LINE-NO.                                       ELP005  
00867      PERFORM 3210-PRINT-DE-DESC-LINES                             ELP005  
00868          VARYING WS-DESC-SUB FROM 1 BY 1                          ELP005  
00869          UNTIL WS-DESC-SUB IS GREATER THAN DE-NBR-DESC-LINES.     ELP005  
00870      PERFORM 4100-PRINT-BLANK-LINE.                               ELP005  
00871      PERFORM 3500-PRINT-ELEMENT-INFO.                             ELP005  
00872      PERFORM 4100-PRINT-BLANK-LINE.                               ELP005  
00873      PERFORM 4010-PRINT-NEW-COL-HEADING.                          ELP005  
00874                                                                   ELP005  
00875                                                                   ELP005  
00876 ******************************************************************ELP005  
00877 ** THIS ROUTINE PRINTS A LINE FROM ONE OF THE RECURRING         **ELP005  
00878 ** DESCRIPTION FIELDS IN THE DATA ELEMENT RECORD.               **ELP005  
00879 ******************************************************************ELP005  
00880                                                                   ELP005  
00881  3210-PRINT-DE-DESC-LINES.                                        ELP005  
00882                                                                   ELP005  
00883      MOVE DE-DESC-LINE (WS-DESC-SUB) TO WS-ELEMENT-DESC.          ELP005  
00884      WRITE PRT-REC FROM WS-ELEMENT-DESC-LINE.                     ELP005  
00885      ADD +1 TO WS-LINE-NO.                                        ELP005  
00886 /                                                                 ELP005  
00887  3400-PRINT-CODE-VALUE-LINE.                                      ELP005  
00888                                                                   ELP005  
00889      IF WS-LINE-NO NOT LESS THAN MAX-LINES-LESS-2                 ELP005  
00890          PERFORM 3700-DATA-EL-CONTINUED-HDR.                      ELP005  
00891                                                                   ELP005  
00892      MOVE CV-VALUE-DESC-LINE (WS-CODE-SUB) TO DCL-CODE-DESC.      ELP005  
00893                                                                   ELP005  
00894      WRITE PRT-REC FROM WS-DETAIL-CODE-LINE.                      ELP005  
00895      ADD +1 TO WS-LINE-NO.                                        ELP005  
00896      MOVE SPACES TO DCL-CODE-VALUE.                               ELP005  
00897                                                                   ELP005  
00898  3500-PRINT-ELEMENT-INFO.                                         ELP005  
00899                                                                   ELP005  
00900      MOVE DE-ELEMENT-LENGTH TO FOOT1-ELEMENT-LGTH.                ELP005  
00901      IF DE-ALPHA                                                  ELP005  
00902          MOVE 'ALPHABETIC' TO FOOT1-ELEMENT-FMT                   ELP005  
00903      ELSE                                                         ELP005  
00904          IF DE-ALPHANUM                                           ELP005  
00905              MOVE 'ALPHANUMERIC' TO FOOT1-ELEMENT-FMT             ELP005  
00906          ELSE                                                     ELP005  
00907              IF DE-DATE                                           ELP005  
00908                  MOVE 'DATE' TO FOOT1-ELEMENT-FMT                 ELP005  
00909              ELSE                                                 ELP005  
00910                  IF DE-NUM                                        ELP005  
00911                      MOVE 'NUMERIC' TO FOOT1-ELEMENT-FMT          ELP005  
00912                  ELSE                                             ELP005  
00913                      MOVE DE-ELEMENT-FORMAT                       ELP005  
00914                          TO FOOT1-ELEMENT-FMT.                    ELP005  
00915      MOVE DE-FORMAT-COMMENT TO FOOT1-FORMAT-COMMENT.              ELP005  
00916                                                                   ELP005  
00917      MOVE FOOT1 TO WS-ELEMENT-DESC.                               ELP005  
00918      WRITE PRT-REC FROM WS-ELEMENT-DESC-LINE.                     ELP005  
00919      ADD +1 TO WS-LINE-NO.                                        ELP005  
00920 /*****************************************************************ELP005  
00921 ** THIS ROUTINE PRINTS A CONTINUATION HEADER FOR THOSE          **ELP005  
00922 ** INSTANCES IN WHICH INFORMATION FOR ONE DATA ELEMENT          **ELP005  
00923 ** EXTENDS PAST THE END OF ONE PAGE.                            **ELP005  
00924 ******************************************************************ELP005  
00925                                                                   ELP005  
00926  3700-DATA-EL-CONTINUED-HDR.                                      ELP005  
00927                                                                   ELP005  
00928      PERFORM 4100-PRINT-BLANK-LINE                                ELP005  
00929           UNTIL WS-LINE-NO NOT < MAX-LINES-LESS-1.                ELP005  
00930      WRITE PRT-REC FROM WS-CONTINUED-FOLLOWING.                   ELP005  
00931      PERFORM 4000-START-NEW-PAGE.                                 ELP005  
00932      PERFORM 4020-PRINT-CONT-COL-HEADING.                         ELP005  
00933 /                                                                 ELP005  
00934  4000-START-NEW-PAGE.                                             ELP005  
00935                                                                   ELP005  
00936      MOVE DE-RECORD-PREFIX   TO EL6-RECORD-NAME.                  ELP005  
00937      MOVE DE-ELEMENT-NBR     TO EL6-ELEMENT-NUM.                  ELP005  
00938      MOVE WS-PAGE-NO         TO EL6-PAGE-NUM.                     ELP005  
00939      MOVE DE-ELEMENT-NAME    TO EL6-ELEMENT-NAME.                 ELP005  
00940      MOVE DE-COBOL-NAME      TO EL6-SYSTEM-NAME.                  ELP005  
00941      MOVE DE-BAL-NAME        TO EL6-ATB-NAME.                     ELP005  
00942      MOVE SPACES             TO EL6-PRINT-LINE.                   ELP005  
00943      WRITE PRT-REC FROM WS-TOP-OF-FORM.                           ELP005  
00944      WRITE PRT-REC FROM EL0006-HEAD-1.                            ELP005  
00945      WRITE PRT-REC FROM EL0006-HEAD-2.                            ELP005  
00946      WRITE PRT-REC FROM EL0006-HEAD-3.                            ELP005  
00947      ADD 1 TO WS-PAGE-NO.                                         ELP005  
00948      MOVE ZERO TO WS-LINE-NO.                                     ELP005  
00949                                                                   ELP005  
00950  4010-PRINT-NEW-COL-HEADING.                                      ELP005  
00951 *                                                                 ELP005  
00952 *   THE LINE NUMBER MUST BE CONVERTED TO PELS AND MADE RELATIVE   ELP005  
00953 *   TO PRINT CHANNEL 5.                                           ELP005  
00954 *                                                                 ELP005  
00955      COMPUTE WS-BASE-LINE-COORD = 420 + (40 * WS-LINE-NO).        ELP005  
00956      COMPUTE WS-BASE-LINE-COORD-2 = WS-BASE-LINE-COORD + 80.      ELP005  
00957      COMPUTE WS-IN-LINE-LEN = 2400 - WS-BASE-LINE-COORD.          ELP005  
00958                                                                   ELP005  
00959      CALL 'ELKAFPCT' USING PRT-REC                                ELP005  
00960                            AFPC-DRAW-HOR-LINE-ABS                 ELP005  
00961                            WS-BASE-LINE-COORD                     ELP005  
00962                            WS-IN-LINE-ZERO                        ELP005  
00963                            WS-BASE-LINE-LEN                       ELP005  
00964                            WS-LINE-WIDTH                          ELP005  
00965                            AFPC-DRAW-HOR-LINE-ABS                 ELP005  
00966                            WS-BASE-LINE-COORD-2                   ELP005  
00967                            WS-IN-LINE-ZERO                        ELP005  
00968                            WS-BASE-LINE-LEN                       ELP005  
00969                            WS-LINE-WIDTH                          ELP005  
00970                            AFPC-DRAW-VER-LINE-ABS                 ELP005  
00971                            WS-BASE-LINE-COORD                     ELP005  
00972                            WS-IN-LINE-COORD                       ELP005  
00973                            WS-IN-LINE-LEN                         ELP005  
00974                            WS-LINE-WIDTH.                         ELP005  
00975      WRITE PRT-REC.                                               ELP005  
00976 ********* DO NOT COUNT AFP LINE INTO WS-LINE-NO *********         ELP005  
00977                                                                   ELP005  
00978      MOVE SPACES TO PRT-REC.                                      ELP005  
00979      WRITE PRT-REC.                                               ELP005  
00980      WRITE PRT-REC FROM WS-COL-HEADING.                           ELP005  
00981      MOVE SPACES TO PRT-REC.                                      ELP005  
00982      WRITE PRT-REC.                                               ELP005  
00983      ADD 3 TO WS-LINE-NO.                                         ELP005  
00984 /                                                                 ELP005  
00985  4020-PRINT-CONT-COL-HEADING.                                     ELP005  
00986 *                                                                 ELP005  
00987 *   THE LINE NUMBER MUST BE CONVERTED TO PELS AND MADE RELATIVE   ELP005  
00988 *   TO PRINT CHANNEL 5.                                           ELP005  
00989 *                                                                 ELP005  
00990      COMPUTE WS-BASE-LINE-COORD = 400.                            ELP005  
00991      COMPUTE WS-BASE-LINE-COORD-2 = WS-BASE-LINE-COORD + 60.      ELP005  
00992      COMPUTE WS-IN-LINE-LEN = 2400 - WS-BASE-LINE-COORD.          ELP005  
00993                                                                   ELP005  
00994      CALL 'ELKAFPCT' USING PRT-REC                                ELP005  
00995                            AFPC-DRAW-HOR-LINE-ABS                 ELP005  
00996                            WS-BASE-LINE-COORD-2                   ELP005  
00997                            WS-IN-LINE-ZERO                        ELP005  
00998                            WS-BASE-LINE-LEN                       ELP005  
00999                            WS-LINE-WIDTH                          ELP005  
01000                            AFPC-DRAW-VER-LINE-ABS                 ELP005  
01001                            WS-BASE-LINE-COORD                     ELP005  
01002                            WS-IN-LINE-COORD                       ELP005  
01003                            WS-IN-LINE-LEN                         ELP005  
01004                            WS-LINE-WIDTH.                         ELP005  
01005      WRITE PRT-REC.                                               ELP005  
01006 ********* DO NOT COUNT AFP LINE INTO WS-LINE-NO *********         ELP005  
01007                                                                   ELP005  
01008      WRITE PRT-REC FROM WS-COL-HEADING-CONT.                      ELP005  
01009      MOVE SPACES TO PRT-REC.                                      ELP005  
01010      WRITE PRT-REC.                                               ELP005  
01011      MOVE 2 TO WS-LINE-NO.                                        ELP005  
01012                                                                   ELP005  
01013  4100-PRINT-BLANK-LINE.                                           ELP005  
01014      MOVE SPACES TO PRT-REC.                                      ELP005  
01015      WRITE PRT-REC.                                               ELP005  
01016      ADD 1 TO WS-LINE-NO.                                         ELP005  
01017      TITLE 'PRINT THE CODES MANUAL - I/O ROUTINES'.               ELP005  
01018  8000-READ-PARM.                                                  ELP005  
01019                                                                   ELP005  
01020      READ PARM-FILE                                               ELP005  
01021          AT END                                                   ELP005  
01022              SET PARM-EOF TO TRUE.                                ELP005  
01023                                                                   ELP005  
01024  8100-READ-ELPRL.                                                 ELP005  
01025                                                                   ELP005  
01026      SET RECORD-LIST-FILE TO TRUE.                                ELP005  
01027      SET ELBHIO-READ TO TRUE.                                     ELP005  
01028      MOVE 11       TO  ELBHIO-RECORD-LENGTH.                      ELP005  
01029                                                                   ELP005  
01030      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP005  
01031                                                                   ELP005  
01032      IF ELBHIO-GOOD-RETURN                                        ELP005  
01033          SET RL-OK             TO  TRUE                           ELP005  
01034          MOVE ELBHIO-ELPRL     TO  RECORD-LIST                    ELP005  
01035      ELSE                                                         ELP005  
01036          IF ELBHIO-FEEDBACK  =  16                                ELP005  
01037              SET RL-NOT-FOUND TO TRUE                             ELP005  
01038          ELSE                                                     ELP005  
01039              PERFORM 9900-VSAM-ABEND.                             ELP005  
01040                                                                   ELP005  
01041  8110-READ-NEXT-ELPRL.                                            ELP005  
01042                                                                   ELP005  
01043      SET RECORD-LIST-FILE TO TRUE.                                ELP005  
01044      SET ELBHIO-SEQUENTIAL-GET TO TRUE.                           ELP005  
01045                                                                   ELP005  
01046      CALL 'ELBIOPGM' USING IO-INFO.                               ELP005  
01047                                                                   ELP005  
01048      IF ELBHIO-GOOD-RETURN                                        ELP005  
01049          SET RL-OK              TO  TRUE                          ELP005  
01050          MOVE ELBHIO-ELPRL      TO  RECORD-LIST                   ELP005  
01051      ELSE                                                         ELP005  
01052          IF ELBHIO-REQUEST-TYPE  =  '2' AND                       ELP005  
01053             ELBHIO-FEEDBACK   =  ZERO                             ELP005  
01054               SET RL-EOF TO TRUE                                  ELP005  
01055          ELSE                                                     ELP005  
01056              PERFORM 9900-VSAM-ABEND.                             ELP005  
01057 /                                                                 ELP005  
01058  8200-READ-ELPDE.                                                 ELP005  
01059                                                                   ELP005  
01060      SET DATA-ELEMENT-FILE TO TRUE.                               ELP005  
01061      SET ELBHIO-READ TO TRUE.                                     ELP005  
01062      MOVE 15       TO  ELBHIO-RECORD-LENGTH.                      ELP005  
01063                                                                   ELP005  
01064      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP005  
01065                                                                   ELP005  
01066      IF ELBHIO-GOOD-RETURN                                        ELP005  
01067          MOVE +11 TO DE-NBR-DESC-LINES                            ELP005  
01068          MOVE ELBHIO-ELPDE      TO  DATA-ELEMENT                  ELP005  
01069          SET DE-OK              TO  TRUE                          ELP005  
01070      ELSE                                                         ELP005  
01071          IF ELBHIO-FEEDBACK  =  16                                ELP005  
01072              SET DE-NOT-FOUND TO TRUE                             ELP005  
01073          ELSE                                                     ELP005  
01074              PERFORM 9900-VSAM-ABEND.                             ELP005  
01075                                                                   ELP005  
01076                                                                   ELP005  
01077  8210-READ-NEXT-ELPDE.                                            ELP005  
01078                                                                   ELP005  
01079      SET DATA-ELEMENT-FILE TO TRUE.                               ELP005  
01080      SET ELBHIO-SEQUENTIAL-GET  TO TRUE.                          ELP005  
01081                                                                   ELP005  
01082      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP005  
01083                                                                   ELP005  
01084      IF ELBHIO-GOOD-RETURN                                        ELP005  
01085          MOVE +11               TO  DE-NBR-DESC-LINES             ELP005  
01086          MOVE ELBHIO-ELPDE      TO  DATA-ELEMENT                  ELP005  
01087          SET DE-OK              TO  TRUE                          ELP005  
01088      ELSE                                                         ELP005  
01089          SET DE-EOF             TO  TRUE                          ELP005  
01090          IF ELBHIO-REQUEST-TYPE  NOT  =  '2'                      ELP005  
01091              IF ELBHIO-FEEDBACK  NOT  =  16                       ELP005  
01092                  PERFORM 9900-VSAM-ABEND.                         ELP005  
01093                                                                   ELP005  
01094                                                                   ELP005  
01095  8220-POINT-ELPDE.                                                ELP005  
01096                                                                   ELP005  
01097      SET DATA-ELEMENT-FILE TO TRUE.                               ELP005  
01098      SET ELBHIO-POINT TO TRUE.                                    ELP005  
01099      MOVE 11       TO  ELBHIO-RECORD-LENGTH.                      ELP005  
01100                                                                   ELP005  
01101      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP005  
01102                                                                   ELP005  
01103      IF ELBHIO-REQUEST-TYPE  =  'P'                               ELP005  
01104          SET DE-OK TO TRUE                                        ELP005  
01105      ELSE                                                         ELP005  
01106          IF ELBHIO-REQUEST-TYPE  =  '2'                           ELP005  
01107                     OR                                            ELP005  
01108             ELBHIO-FEEDBACK  =  4                                 ELP005  
01109              SET DE-NOT-FOUND TO TRUE                             ELP005  
01110          ELSE                                                     ELP005  
01111              PERFORM 9900-VSAM-ABEND.                             ELP005  
01112 /                                                                 ELP005  
01113  8300-READ-NEXT-ELPCV.                                            ELP005  
01114                                                                   ELP005  
01115      SET CODE-VALUE-FILE TO TRUE.                                 ELP005  
01116      SET ELBHIO-SEQUENTIAL-GET TO TRUE.                           ELP005  
01117                                                                   ELP005  
01118      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP005  
01119                                                                   ELP005  
01120      IF ELBHIO-GOOD-RETURN                                        ELP005  
01121          MOVE +12               TO  CV-NBR-VALUE-DESC-LINES       ELP005  
01122          MOVE ELBHIO-ELPCV      TO  ELEMENT-CODE-VALUE            ELP005  
01123          IF CV-RECORD-PREFIX = DE-RECORD-PREFIX AND               ELP005  
01124             CV-ELEMENT-NBR = DE-ELEMENT-NBR                       ELP005  
01125                SET CV-OK TO TRUE                                  ELP005  
01126          ELSE                                                     ELP005  
01127             SET CV-EOF TO TRUE                                    ELP005  
01128          END-IF                                                   ELP005  
01129      ELSE                                                         ELP005  
01130          IF ELBHIO-REQUEST-TYPE = '2' AND                         ELP005  
01131             ELBHIO-FEEDBACK = ZERO                                ELP005  
01132                 SET CV-EOF TO TRUE                                ELP005  
01133          ELSE                                                     ELP005  
01134              PERFORM 9900-VSAM-ABEND                              ELP005  
01135          END-IF                                                   ELP005  
01136      END-IF.                                                      ELP005  
01137                                                                   ELP005  
01138  8310-POINT-ELPCV.                                                ELP005  
01139                                                                   ELP005  
01140      SET CODE-VALUE-FILE TO TRUE.                                 ELP005  
01141      SET ELBHIO-POINT TO TRUE.                                    ELP005  
01142      MOVE  15      TO  ELBHIO-RECORD-LENGTH.                      ELP005  
01143                                                                   ELP005  
01144      CALL 'ELBIOPGM'  USING  IO-INFO.                             ELP005  
01145                                                                   ELP005  
01146      IF ELBHIO-REQUEST-TYPE  =  'P'                               ELP005  
01147          SET CV-OK TO TRUE                                        ELP005  
01148      ELSE                                                         ELP005  
01149          IF ELBHIO-REQUEST-TYPE  =  '2'                           ELP005  
01150                     OR                                            ELP005  
01151             ELBHIO-FEEDBACK      =   4                            ELP005  
01152              SET CV-NOT-FOUND TO TRUE                             ELP005  
01153          ELSE                                                     ELP005  
01154              PERFORM 9900-VSAM-ABEND.                             ELP005  
01155      TITLE 'PRINT THE CODES MANUAL - ERROR HANDLING SECTION'.     ELP005  
01156 ******************************************************************ELP005  
01157 ** ERROR MESSAGE FOR SELECTION PARMS                            **ELP005  
01158 ******************************************************************ELP005  
01159  9000-RL-NOT-FOUND.                                               ELP005  
01160      PERFORM 9800-PRINT-ERROR-HEADING.                            ELP005  
01161                                                                   ELP005  
01162      MOVE ELBHIO-ELPRL-KEY   TO WS-NO-PREFIX-RL.                  ELP005  
01163      MOVE WS-NO-PREFIX-ERROR TO WS-ERROR-PRINT-MSG.               ELP005  
01164      WRITE PRT-REC FROM WS-ERROR-LINE-2.                          ELP005  
01165                                                                   ELP005  
01166      PERFORM 9810-PRINT-ERROR-FOOTING.                            ELP005  
01167                                                                   ELP005  
01168  9100-DE-NOT-FOUND.                                               ELP005  
01169      PERFORM 9800-PRINT-ERROR-HEADING.                            ELP005  
01170                                                                   ELP005  
01171      MOVE ELBHIO-DE-RECORD-PREFIX TO WS-NO-PREFIX-DE.             ELP005  
01172      MOVE ELBHIO-DE-ELEMENT-NBR   TO WS-ELEMENT-NO-DE.            ELP005  
01173      MOVE WS-NO-DATA-ELEMENT-ERROR TO WS-ERROR-PRINT-MSG.         ELP005  
01174      WRITE PRT-REC FROM WS-ERROR-LINE-2.                          ELP005  
01175                                                                   ELP005  
01176      PERFORM 9810-PRINT-ERROR-FOOTING.                            ELP005  
01177                                                                   ELP005  
01178  9300-PARM-ERROR-MSG.                                             ELP005  
01179      MOVE PARM-REC-S TO MSG-PARM-VALUE.                           ELP005  
01180      PERFORM 9800-PRINT-ERROR-HEADING.                            ELP005  
01181      MOVE PARM-ERROR-MSG TO WS-ERROR-PRINT-MSG.                   ELP005  
01182      WRITE PRT-REC FROM WS-ERROR-LINE-2.                          ELP005  
01183                                                                   ELP005  
01184      PERFORM 9810-PRINT-ERROR-FOOTING.                            ELP005  
01185                                                                   ELP005  
01186  9400-PARM-ABEND.                                                 ELP005  
01187      DISPLAY PARM-ABEND-MSG.                                      ELP005  
01188      PERFORM 9999-ABEND-RTN.                                      ELP005  
01189 /                                                                 ELP005  
01190  9800-PRINT-ERROR-HEADING.                                        ELP005  
01191      PERFORM 3190-INITIALIZE-HEADING.                             ELP005  
01192      PERFORM 4000-START-NEW-PAGE.                                 ELP005  
01193      WRITE PRT-REC FROM EL0006-HEAD-5.                            ELP005  
01194      PERFORM 4100-PRINT-BLANK-LINE 2 TIMES.                       ELP005  
01195                                                                   ELP005  
01196      WRITE PRT-REC FROM WS-ERROR-LINE-1.                          ELP005  
01197      WRITE PRT-REC FROM WS-ERROR-LINE-1.                          ELP005  
01198                                                                   ELP005  
01199      MOVE SPACES TO WS-ERROR-PRINT-MSG.                           ELP005  
01200      WRITE PRT-REC FROM WS-ERROR-LINE-2.                          ELP005  
01201                                                                   ELP005  
01202  9810-PRINT-ERROR-FOOTING.                                        ELP005  
01203      MOVE SPACES TO WS-ERROR-PRINT-MSG.                           ELP005  
01204      WRITE PRT-REC FROM WS-ERROR-LINE-2.                          ELP005  
01205                                                                   ELP005  
01206      WRITE PRT-REC FROM WS-ERROR-LINE-1.                          ELP005  
01207      WRITE PRT-REC FROM WS-ERROR-LINE-1.                          ELP005  
01208                                                                   ELP005  
01209  9900-VSAM-ABEND.                                                 ELP005  
01210      MOVE ELBHIO-FEEDBACK  TO  ABEND-CODE.                        ELP005  
01211      PERFORM 9999-ABEND-RTN.                                      ELP005  
01212                                                                   ELP005  
01213  9999-ABEND-RTN.                                                  ELP005  
01214      DISPLAY 'CALLING TSGEND FOR ABEND' UPON CONSOLE.             ELP005  
01215      CALL 'TSGEND'  USING  ABEND-CODE.                            ELP005  
