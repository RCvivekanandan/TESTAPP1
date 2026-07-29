00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUPROLG
00003  PROGRAM-ID.           ELUPROLG.                                     LV001
00004                                                                   ELUPROLG
00005  AUTHOR.               NINA CERVANTES.                            ELUPROLG
00006                        RESTRUCTURED BY RICHARD J. LUKETICH.       ELUPROLG
00007                                                                   ELUPROLG
00008  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELUPROLG
00009                        A MUTUAL LEGAL RESERVE COMPANY             ELUPROLG
00010                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELUPROLG
00011                        233 N. MICHIGAN AVE                        ELUPROLG
00012                        CHICAGO, ILLINOIS 60601                    ELUPROLG
00013                                                                   ELUPROLG
00014  DATE-WRITTEN.         19-JUL-1986.                               ELUPROLG
00015                        05-NOV-1993 (RESTRUCTURED).                ELUPROLG
00016                                                                   ELUPROLG
00017  DATE-COMPILED.                                                   ELUPROLG
00018                                                                   ELUPROLG
00019  SECURITY.             COPYRIGHT 1986, 1993,                      ELUPROLG
00020                        HEALTH CARE SERVICE CORPORATION            ELUPROLG
00021                                                                   ELUPROLG
00022 /*****************************************************************ELUPROLG
00023 *                                                                *ELUPROLG
00024 *    System:        English Contract Inquiry                     *ELUPROLG
00025 *    Sub-System:    Topic Output Generation                      *ELUPROLG
00026 *                                                                *ELUPROLG
00027 *    Program ID:    ELUPROLG                                     *ELUPROLG
00028 *    Program Name:  Generate Topic Prolog                        *ELUPROLG
00029 *    Program Type:  Output Generator                             *ELUPROLG
00030 *                                                                *ELUPROLG
00031 *    Description:                                                *ELUPROLG
00032 *       Generate the topic prolog page, which lists the topic    *ELUPROLG
00033 *       and the information used to select the Group Specific    *ELUPROLG
00034 *       and Contract file keys on which the topic information is *ELUPROLG
00035 *       based.                                                   *ELUPROLG
00036 *                                                                *ELUPROLG
00037 *----------------------------------------------------------------*ELUPROLG
00038 *                      Maintenance History                       *ELUPROLG
00039 *----------------------------------------------------------------*ELUPROLG
00040 *                                                                *ELUPROLG
00041 *                                                                *ELUPROLG
00042 * Level    Date     By  Ref    Description                       *ELUPROLG
00043 * ----- ----------- --- ------ --------------------------------- *ELUPROLG
00044 * 02.00 08-Nov-1993 RJL        Restructured.                     *ELUPROLG
00045 *                              Corrected logic error that caused *ELUPROLG
00046 *                              abend related to FRL translation. *ELUPROLG
00047 *                              Improved FRL translation,         *ELUPROLG
00048 *                              including provision for           *ELUPROLG
00049 *                              member/spouse/dependent splits in *ELUPROLG
00050 *                              Medicare-eligible FRLs.           *ELUPROLG
00051 *                              Converted LINKs to static CALLs   *ELUPROLG
00052 *                              where appropriate.                *ELUPROLG
00053 *                                                                *ELUPROLG
00054 *       11-Jan-2000 JP         Added MLDATE routine to convert   *ELUPROLG
00055 *                              termination dates to MM/DD/CCYY   *ELUPROLG
00056 *                              format.                           *ELUPROLG
00057 *                                                                *ELUPROLG
00058 ******************************************************************ELUPROLG
00059 /                                                                 ELUPROLG
00060  ENVIRONMENT DIVISION.                                            ELUPROLG
00061                                                                   ELUPROLG
00062  CONFIGURATION SECTION.                                           ELUPROLG
00063                                                                   ELUPROLG
00064  SOURCE-COMPUTER.    IBM-3033.                                    ELUPROLG
00065  OBJECT-COMPUTER.    IBM-3033.                                    ELUPROLG
00066 /                                                                 ELUPROLG
00067  DATA DIVISION.                                                   ELUPROLG
00068                                                                   ELUPROLG
00069  WORKING-STORAGE SECTION.                                         ELUPROLG
00070                                                                   ELUPROLG
00071  01  WS-HOLD-CONTRACT-POINTERS.                                   ELUPROLG
00072      02 WS-IB-PTR                         POINTER VALUE NULL.     ELUPROLG
00073      02 WS-IS-PTR                         POINTER VALUE NULL.     ELUPROLG
00074      02 WS-PB-PTR                         POINTER VALUE NULL.     ELUPROLG
00075      02 WS-PS-PTR                         POINTER VALUE NULL.     ELUPROLG
00076                                                                   ELUPROLG
00077  01  WS-MAX-VALS.                                                 ELUPROLG
00078      02 WS-MAX-NBR-HDR-LN                 PIC S9(04)     COMP.    ELUPROLG
00079      02 WS-MAX-NBR-DTL-LN                 PIC S9(04)     COMP.    ELUPROLG
00080      02 WS-MAX-NBR-TRL-LN                 PIC S9(04)     COMP.    ELUPROLG
00081                                                                   ELUPROLG
00082  01  WS-FRMT-CNTL.                                                ELUPROLG
00083      02 WS-FRST-INDNT                     PIC S9(04)     COMP.    ELUPROLG
00084      02 WS-SBSQNT-INDNT                   PIC S9(04)     COMP.    ELUPROLG
00085      02 WS-FRST-LN-LEN                    PIC S9(04)     COMP.    ELUPROLG
00086      02 WS-SBSQNT-LN-LEN                  PIC S9(04)     COMP.    ELUPROLG
00087      02 WS-LN-OFST                        PIC S9(04)     COMP.    ELUPROLG
00088                                                                   ELUPROLG
00089  01  WS-WORK-FIELDS.                                              ELUPROLG
00090      02 WS-SUB                            PIC S9(04)     COMP.    ELUPROLG
00091                                                                   ELUPROLG
00092  01  WS-HGADATES-PARMS.                                           ELUPROLG
00093      COPY HGCDAT01.                                               ELUPROLG
00094      COPY MLDATE01.                                               ELUPROLG
00095                                                                   ELUPROLG
00096 ******************************************************************ELUPROLG
00097 *                                                                *ELUPROLG
00098 *    Display Formatting Lines                                    *ELUPROLG
00099 *                                                                *ELUPROLG
00100 ******************************************************************ELUPROLG
00101                                                                   ELUPROLG
00102  01  WS-HDR1.                                                     ELUPROLG
00103      02                                   PIC  X(07)              ELUPROLG
00104         VALUE 'GROUP: '.                                          ELUPROLG
00105      02 WS-HDR1-GROUP-NO                  PIC  X(06).             ELUPROLG
00106      02                                   PIC  X(10)              ELUPROLG
00107         VALUE ' SECTION: '.                                       ELUPROLG
00108      02 WS-HDR1-SECT-NO                   PIC  X(05).             ELUPROLG
00109      02                                   PIC  X(01)              ELUPROLG
00110         VALUE SPACES.                                             ELUPROLG
00111      02 WS-HDR1-FAM-REL                   PIC  X(22).             ELUPROLG
00112      02                                   PIC  X(07)              ELUPROLG
00113         VALUE ' FROM: '.                                          ELUPROLG
00114      02 WS-HDR1-FROM-DATE                 PIC  99/99/99.          ELUPROLG
00115      02                                   PIC  X(05)              ELUPROLG
00116         VALUE ' TO: '.                                            ELUPROLG
00117      02 WS-HDR1-TO-DATE                   PIC  99/99/99.          ELUPROLG
00118                                                                   ELUPROLG
00119  01  WS-TPC-HDR.                                                  ELUPROLG
00120      02                                   PIC  X(47)              ELUPROLG
00121         VALUE 'THE FOLLOWING DISPLAYED BENEFITS ARE FOR: '.       ELUPROLG
00122      02 WS-TPC-DSCRPTN                    PIC  X(80).             ELUPROLG
00123                                                                   ELUPROLG
00124  01  WS-EFFECTIVE-LN.                                             ELUPROLG
00125      02 FILLER                            PIC  X(11)              ELUPROLG
00126         VALUE ' EFFECTIVE '.                                      ELUPROLG
00127      02 WS-FROM-DATE                      PIC  99/99/99.          ELUPROLG
00128      02 FILLER                            PIC  X(06)              ELUPROLG
00129         VALUE ' THRU '.                                           ELUPROLG
00130      02 WS-TO-DATE                        PIC  99/99/9999.        ELUPROLG
00131                                                                   ELUPROLG
00132  01  WS-ALL                               PIC  X(04)              ELUPROLG
00133      VALUE 'ALL '.                                                ELUPROLG
00134                                                                   ELUPROLG
00135  01  WS-AND                               PIC  X(04)              ELUPROLG
00136      VALUE 'AND '.                                                ELUPROLG
00137                                                                   ELUPROLG
00138  01  WS-BAS                               PIC  X(06)              ELUPROLG
00139      VALUE 'BASIC '.                                              ELUPROLG
00140                                                                   ELUPROLG
00141  01  WS-BNFT                              PIC  X(25)              ELUPROLG
00142      VALUE 'BENEFITS ARE TAKEN FROM: '.                           ELUPROLG
00143                                                                   ELUPROLG
00144  01  WS-FRL-CNSDRTNS                      PIC  X(39)              ELUPROLG
00145      VALUE 'FAMILY RELATIONSHIP CONSIDERATIONS FOR '.             ELUPROLG
00146                                                                   ELUPROLG
00147  01  WS-INST                              PIC  X(14)              ELUPROLG
00148      VALUE 'INSTITUTIONAL '.                                      ELUPROLG
00149                                                                   ELUPROLG
00150  01  WS-PROF                              PIC  X(13)              ELUPROLG
00151      VALUE 'PROFESSIONAL '.                                       ELUPROLG
00152                                                                   ELUPROLG
00153  01  WS-PRVDR-CNTRL-CNSDRTNS              PIC  X(28)              ELUPROLG
00154      VALUE 'PROVIDER CONSIDERATIONS FOR '.                        ELUPROLG
00155                                                                   ELUPROLG
00156  01  WS-SUP                               PIC  X(13)              ELUPROLG
00157      VALUE 'SUPPLEMENTAL '.                                       ELUPROLG
00158                                                                   ELUPROLG
00159  01  WS-WITH                              PIC  X(05)              ELUPROLG
00160      VALUE 'WITH '.                                               ELUPROLG
00161                                                                   ELUPROLG
00162 * -- SECTION TITLES (PRE-FORMATTED)                               ELUPROLG
00163                                                                   ELUPROLG
00164  01  WS-GRP-SPCFC-TTL                     PIC  X(79)              ELUPROLG
00165      VALUE '                            GROUP SPECIFIC'.          ELUPROLG
00166                                                                   ELUPROLG
00167  01  WS-CNTRCT-TTL                        PIC  X(79)              ELUPROLG
00168      VALUE '                               CONTRACT'.             ELUPROLG
00169 /             L I N K A G E   S E C T I O N                       ELUPROLG
00170  LINKAGE SECTION.                                                 ELUPROLG
00171                                                                   ELUPROLG
00172  01  DFHCOMMAREA.                                                 ELUPROLG
00173      COPY ELSCOMMC.                                               ELUPROLG
00174 /                                                                 ELUPROLG
00175      COPY ELSCIA2C.                                               ELUPROLG
00176 /                                                                 ELUPROLG
00177      COPY ELSSSCBC.                                               ELUPROLG
00178 /                                                                 ELUPROLG
00179      COPY ELSCMIFC.                                               ELUPROLG
00180 /                                                                 ELUPROLG
00181      COPY ELSCMDSC.                                               ELUPROLG
00182 /                                                                 ELUPROLG
00183      COPY ELSTCWAC.                                               ELUPROLG
00184 /                                                                 ELUPROLG
00185      COPY ELSOUTPC.                                               ELUPROLG
00186 /        G R O U P   S P E C I F I C   R E C O R D                ELUPROLG
00187  01  GROUP-SPECIFIC-RECORD.                                       ELUPROLG
00188      COPY GCGROUPC.                                               ELUPROLG
00189 /        C O N T R A C T   R E C O R D                            ELUPROLG
00190  01  CONTRACT-RECORD.                                             ELUPROLG
00191      COPY GCCONTRC.                                               ELUPROLG
00192 /*****************************************************************ELUPROLG
00193 *                                                                *ELUPROLG
00194 *    Procedure Division                                          *ELUPROLG
00195 *                                                                *ELUPROLG
00196 ******************************************************************ELUPROLG
00197                                                                   ELUPROLG
00198 ******************************************************************ELUPROLG
00199 *                                                                *ELUPROLG
00200 *    Generate Topic Prolog                                       *ELUPROLG
00201 *                                                                *ELUPROLG
00202 ******************************************************************ELUPROLG
00203                                                                   ELUPROLG
00204  PROCEDURE DIVISION.                                              ELUPROLG
00205                                                                   ELUPROLG
00206  0000-GEN-TPC-PRLG.                                               ELUPROLG
00207      IF EIBCALEN = LENGTH OF DFHCOMMAREA                          ELUPROLG
00208      THEN                                                         ELUPROLG
00209         CALL 'ELUINISM'                                           ELUPROLG
00210            USING DFHCOMMAREA                                      ELUPROLG
00211                  ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA         ELUPROLG
00212            END-CALL                                               ELUPROLG
00213         IF ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA = NULL        ELUPROLG
00214         THEN                                                      ELUPROLG
00215            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELUPROLG
00216         ELSE                                                      ELUPROLG
00217            PERFORM 0100-INTLZ                                     ELUPROLG
00218            PERFORM 1000-PRCS                                      ELUPROLG
00219         END-IF                                                    ELUPROLG
00220      ELSE                                                         ELUPROLG
00221         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELUPROLG
00222      END-IF.                                                      ELUPROLG
00223      GOBACK.                                                      ELUPROLG
00224                                                                   ELUPROLG
00225 ******************************************************************ELUPROLG
00226 *                                                                *ELUPROLG
00227 *    Initialize Processing                                       *ELUPROLG
00228 *                                                                *ELUPROLG
00229 *       Establish addressability to all other standard areas     *ELUPROLG
00230 *       needed by the prolog.                                    *ELUPROLG
00231 *                                                                *ELUPROLG
00232 ******************************************************************ELUPROLG
00233                                                                   ELUPROLG
00234  0100-INTLZ.                                                      ELUPROLG
00235      PERFORM A001-ESTAB-ADDR-ELSSSCBC.                            ELUPROLG
00236      PERFORM A002-ESTAB-ADDR-ELSCMIFC.                            ELUPROLG
00237      PERFORM A003-ESTAB-ADDR-ELSOUTPC.                            ELUPROLG
00238      PERFORM A004-ESTAB-ADDR-ELSTCWAC.                            ELUPROLG
00239      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELUPROLG
00240                                                                   ELUPROLG
00241 ******************************************************************ELUPROLG
00242 *                                                                *ELUPROLG
00243 *    Establish Addressability to Selector Status Control Block   *ELUPROLG
00244 *                                                                *ELUPROLG
00245 ******************************************************************ELUPROLG
00246                                                                   ELUPROLG
00247  A001-ESTAB-ADDR-ELSSSCBC.                                        ELUPROLG
00248      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELUPROLG
00249      CALL 'ELUSETAD'                                              ELUPROLG
00250         USING DFHCOMMAREA ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK  ELUPROLG
00251         END-CALL.                                                 ELUPROLG
00252      IF CIA-RC-PTR-NULL                                           ELUPROLG
00253      THEN                                                         ELUPROLG
00254         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELUPROLG
00255         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELUPROLG
00256      END-IF.                                                      ELUPROLG
00257                                                                   ELUPROLG
00258 ******************************************************************ELUPROLG
00259 *                                                                *ELUPROLG
00260 *    Establish Addressability to Codes Manual Interface Block    *ELUPROLG
00261 *                                                                *ELUPROLG
00262 ******************************************************************ELUPROLG
00263                                                                   ELUPROLG
00264  A002-ESTAB-ADDR-ELSCMIFC.                                        ELUPROLG
00265      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELUPROLG
00266      CALL 'ELUSETAD'                                              ELUPROLG
00267         USING DFHCOMMAREA ADDRESS OF CMF-CODES-MANUAL-INTERFACE   ELUPROLG
00268         END-CALL.                                                 ELUPROLG
00269      IF CIA-RC-PTR-NULL                                           ELUPROLG
00270      THEN                                                         ELUPROLG
00271         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELUPROLG
00272         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELUPROLG
00273      END-IF.                                                      ELUPROLG
00274                                                                   ELUPROLG
00275 ******************************************************************ELUPROLG
00276 *                                                                *ELUPROLG
00277 *    Establish Addressability to Output Interface Block          *ELUPROLG
00278 *                                                                *ELUPROLG
00279 ******************************************************************ELUPROLG
00280                                                                   ELUPROLG
00281  A003-ESTAB-ADDR-ELSOUTPC.                                        ELUPROLG
00282      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELUPROLG
00283      CALL 'ELUSETAD'                                              ELUPROLG
00284         USING DFHCOMMAREA ADDRESS OF COF-OUTPUT-INTERFACE         ELUPROLG
00285         END-CALL.                                                 ELUPROLG
00286      IF CIA-RC-PTR-NULL                                           ELUPROLG
00287      THEN                                                         ELUPROLG
00288         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELUPROLG
00289         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELUPROLG
00290      ELSE                                                         ELUPROLG
00291         COMPUTE WS-MAX-NBR-HDR-LN                                 ELUPROLG
00292            = LENGTH OF COF-HDR / LENGTH OF COF-HDR-LINE           ELUPROLG
00293         SET COF-HDR-MAX-IDX TO WS-MAX-NBR-HDR-LN                  ELUPROLG
00294         COMPUTE WS-MAX-NBR-DTL-LN                                 ELUPROLG
00295            = LENGTH OF COF-DTL / LENGTH OF COF-DTL-LINE           ELUPROLG
00296         SET COF-DTL-MAX-IDX TO WS-MAX-NBR-DTL-LN                  ELUPROLG
00297         COMPUTE WS-MAX-NBR-TRL-LN                                 ELUPROLG
00298            = LENGTH OF COF-TRL / LENGTH OF COF-TRL-LINE           ELUPROLG
00299         SET COF-TRL-MAX-IDX TO WS-MAX-NBR-TRL-LN                  ELUPROLG
00300      END-IF.                                                      ELUPROLG
00301                                                                   ELUPROLG
00302 ******************************************************************ELUPROLG
00303 *                                                                *ELUPROLG
00304 *    Establish Addressability to Text Compression Work Area      *ELUPROLG
00305 *                                                                *ELUPROLG
00306 ******************************************************************ELUPROLG
00307                                                                   ELUPROLG
00308  A004-ESTAB-ADDR-ELSTCWAC.                                        ELUPROLG
00309      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELUPROLG
00310      CALL 'ELUSETAD'                                              ELUPROLG
00311         USING DFHCOMMAREA ADDRESS OF TCAR-COMPRESSION-WORK-AREA   ELUPROLG
00312         END-CALL.                                                 ELUPROLG
00313      IF CIA-RC-PTR-NULL                                           ELUPROLG
00314      THEN                                                         ELUPROLG
00315         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELUPROLG
00316         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELUPROLG
00317      END-IF.                                                      ELUPROLG
00318                                                                   ELUPROLG
00319      SET TCAR-FROM-AREA-MAX-IDX TO +1580.                         ELUPROLG
00320      SET TCAR-FROM-LINE-MAX-IDX TO +20.                           ELUPROLG
00321      SET TCAR-TO-AREA-MAX-IDX TO +1580.                           ELUPROLG
00322      SET TCAR-TO-LINE-MAX-IDX TO +20.                             ELUPROLG
00323      SET TCAR-OPF-MAX-IDX TO +20.                                 ELUPROLG
00324      SET TCAR-OPF-DIGIT-MAX-IDX TO +80.                           ELUPROLG
00325 /*****************************************************************ELUPROLG
00326 *                                                                *ELUPROLG
00327 *    Process                                                     *ELUPROLG
00328 *                                                                *ELUPROLG
00329 *       Generate Prolog Output                                   *ELUPROLG
00330 *                                                                *ELUPROLG
00331 ******************************************************************ELUPROLG
00332                                                                   ELUPROLG
00333  1000-PRCS.                                                       ELUPROLG
00334      PERFORM 2000-GEN-INTRDCTRY-INFRMTN.                          ELUPROLG
00335      PERFORM 3000-GEN-GRP-SPCFC-KEY-INFRMTN.                      ELUPROLG
00336      PERFORM 4000-GEN-CNTRCT-KEY-INFRMTN.                         ELUPROLG
00337      CALL 'ELUSBMIF' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELUPROLG
00338                                                                   ELUPROLG
00339 /*****************************************************************ELUPROLG
00340 *                                                                *ELUPROLG
00341 *    Generate Introductory Information                           *ELUPROLG
00342 *                                                                *ELUPROLG
00343 ******************************************************************ELUPROLG
00344                                                                   ELUPROLG
00345  2000-GEN-INTRDCTRY-INFRMTN.                                      ELUPROLG
00346                                                                   ELUPROLG
00347      PERFORM 2100-GEN-PRLG-HDR.                                   ELUPROLG
00348      PERFORM 2200-GEN-TPC-IDNTFCTN.                               ELUPROLG
00349      PERFORM 9020-CALL-ELUOUTPT.                                  ELUPROLG
00350                                                                   ELUPROLG
00351 /*****************************************************************ELUPROLG
00352 *                                                                *ELUPROLG
00353 *    Generate Prolog Header                                      *ELUPROLG
00354 *                                                                *ELUPROLG
00355 ******************************************************************ELUPROLG
00356                                                                   ELUPROLG
00357  2100-GEN-PRLG-HDR.                                               ELUPROLG
00358      MOVE SSB-GRP-NO TO WS-HDR1-GROUP-NO.                         ELUPROLG
00359      MOVE SSB-SECT-NO TO WS-HDR1-SECT-NO.                         ELUPROLG
00360      PERFORM 2110-XLT-FRL-HDR.                                    ELUPROLG
00361      PERFORM 2120-CNVRT-SRVC-DTS.                                 ELUPROLG
00362      SET COF-NEW-PAGE TO TRUE.                                    ELUPROLG
00363      SET COF-HDR-IDX TO 1.                                        ELUPROLG
00364      MOVE WS-HDR1 TO COF-HDR-LINE (COF-HDR-IDX).                  ELUPROLG
00365      MOVE 2 TO COF-NBR-HDR-LINES.                                 ELUPROLG
00366                                                                   ELUPROLG
00367 /*****************************************************************ELUPROLG
00368 *                                                                *ELUPROLG
00369 *    Translate Family Relationship Level for Header              *ELUPROLG
00370 *                                                                *ELUPROLG
00371 ******************************************************************ELUPROLG
00372                                                                   ELUPROLG
00373  2110-XLT-FRL-HDR.                                                ELUPROLG
00374      EVALUATE TRUE                                                ELUPROLG
00375      WHEN SSB-MEDCA-UNDEF                                         ELUPROLG
00376         EVALUATE TRUE                                             ELUPROLG
00377         WHEN SSB-FR-UNDEF                                         ELUPROLG
00378            MOVE '  ALL FAMILY MEMBERS  '   TO WS-HDR1-FAM-REL     ELUPROLG
00379         WHEN SSB-MEMBER                                           ELUPROLG
00380            MOVE '        MEMBER        ' TO WS-HDR1-FAM-REL       ELUPROLG
00381         WHEN SSB-SPOUSE                                           ELUPROLG
00382            MOVE '        SPOUSE        ' TO WS-HDR1-FAM-REL       ELUPROLG
00383         WHEN SSB-DEPENDENT                                        ELUPROLG
00384            MOVE '      DEPENDENT       ' TO WS-HDR1-FAM-REL       ELUPROLG
00385         WHEN OTHER                                                ELUPROLG
00386            SET CIA-AB-PGM-LOGIC TO TRUE                           ELUPROLG
00387            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELUPROLG
00388         END-EVALUATE                                              ELUPROLG
00389      WHEN SSB-MEDCA-ELIG                                          ELUPROLG
00390         EVALUATE TRUE                                             ELUPROLG
00391         WHEN SSB-FR-UNDEF                                         ELUPROLG
00392            MOVE 'ALL MEDICARE ELIGIBLE ' TO WS-HDR1-FAM-REL       ELUPROLG
00393         WHEN SSB-MEMBER                                           ELUPROLG
00394            MOVE '   MEDICARE MEMBER    ' TO WS-HDR1-FAM-REL       ELUPROLG
00395         WHEN SSB-SPOUSE                                           ELUPROLG
00396            MOVE '   MEDICARE SPOUSE    ' TO WS-HDR1-FAM-REL       ELUPROLG
00397         WHEN SSB-DEPENDENT                                        ELUPROLG
00398            MOVE '  MEDICARE DEPENDENT  ' TO WS-HDR1-FAM-REL       ELUPROLG
00399         WHEN OTHER                                                ELUPROLG
00400            SET CIA-AB-PGM-LOGIC TO TRUE                           ELUPROLG
00401            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELUPROLG
00402         END-EVALUATE                                              ELUPROLG
00403      WHEN SSB-MEDCA-INELIG                                        ELUPROLG
00404         EVALUATE TRUE                                             ELUPROLG
00405         WHEN SSB-FR-UNDEF                                         ELUPROLG
00406            MOVE '   ALL NON-MEDICARE   ' TO WS-HDR1-FAM-REL       ELUPROLG
00407         WHEN SSB-MEMBER                                           ELUPROLG
00408            MOVE ' NON-MEDICARE MEMBER  ' TO WS-HDR1-FAM-REL       ELUPROLG
00409         WHEN SSB-SPOUSE                                           ELUPROLG
00410            MOVE ' NON-MEDICARE SPOUSE  ' TO WS-HDR1-FAM-REL       ELUPROLG
00411         WHEN SSB-DEPENDENT                                        ELUPROLG
00412            MOVE 'NON-MEDICARE DEPENDENT' TO WS-HDR1-FAM-REL       ELUPROLG
00413         WHEN OTHER                                                ELUPROLG
00414            SET CIA-AB-PGM-LOGIC TO TRUE                           ELUPROLG
00415            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELUPROLG
00416         END-EVALUATE                                              ELUPROLG
00417      WHEN OTHER                                                   ELUPROLG
00418         SET CIA-AB-PGM-LOGIC TO TRUE                              ELUPROLG
00419         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC               ELUPROLG
00420      END-EVALUATE.                                                ELUPROLG
00421                                                                   ELUPROLG
00422 /*****************************************************************ELUPROLG
00423 *                                                                *ELUPROLG
00424 *    Convert Service From and To Dates                           *ELUPROLG
00425 *                                                                *ELUPROLG
00426 ******************************************************************ELUPROLG
00427                                                                   ELUPROLG
00428  2120-CNVRT-SRVC-DTS.                                             ELUPROLG
00429      MOVE SSB-SRV-FROM-DATE TO HGADATE-JULIAN1.                   ELUPROLG
00430      PERFORM 9030-CNVRT-DT-JLN-GRGRN.                             ELUPROLG
00431      IF HGADATE-RETURN = '00'                                     ELUPROLG
00432      THEN                                                         ELUPROLG
00433         MOVE HGADATE-DATE2 TO WS-HDR1-FROM-DATE                   ELUPROLG
00434      ELSE                                                         ELUPROLG
00435         MOVE ZEROES TO WS-HDR1-FROM-DATE                          ELUPROLG
00436      END-IF.                                                      ELUPROLG
00437                                                                   ELUPROLG
00438      MOVE SSB-SRV-TO-DATE TO HGADATE-JULIAN1.                     ELUPROLG
00439      PERFORM 9030-CNVRT-DT-JLN-GRGRN.                             ELUPROLG
00440      IF HGADATE-RETURN  = '00'                                    ELUPROLG
00441      THEN                                                         ELUPROLG
00442         MOVE HGADATE-DATE2 TO WS-HDR1-TO-DATE                     ELUPROLG
00443      ELSE                                                         ELUPROLG
00444         MOVE ZEROES TO WS-HDR1-TO-DATE                            ELUPROLG
00445      END-IF.                                                      ELUPROLG
00446                                                                   ELUPROLG
00447 /*****************************************************************ELUPROLG
00448 *                                                                *ELUPROLG
00449 *    Generate Topic Identification                               *ELUPROLG
00450 *                                                                *ELUPROLG
00451 *       Topic identification text is taken from the description  *ELUPROLG
00452 *       of the appropriate topic selection level in the          *ELUPROLG
00453 *       following order of precedence:                           *ELUPROLG
00454 *                                                                *ELUPROLG
00455 *          1) Sub-Topic                                          *ELUPROLG
00456 *          2) Modifier 1                                         *ELUPROLG
00457 *          3) Topic                                              *ELUPROLG
00458 *                                                                *ELUPROLG
00459 *       The first available description (based on this order)    *ELUPROLG
00460 *       will be used to identify the topic on the prolog         *ELUPROLG
00461 *       page(s)                                                  *ELUPROLG
00462 *                                                                *ELUPROLG
00463 *       Note the manipulation of SSB-SELECTOR-STATE (condition   *ELUPROLG
00464 *       names prefixed with \
00465 *       states of each of the relevant selectors. This allows    *ELUPROLG
00466 *       the insertion of other steps in the selection process to *ELUPROLG
00467 *       be made without actual source changes to this program.   *ELUPROLG
00468 *       AA recompile wold, however, be necessary.                *ELUPROLG
00469 *                                                                *ELUPROLG
00470 ******************************************************************ELUPROLG
00471                                                                   ELUPROLG
00472  2200-GEN-TPC-IDNTFCTN.                                           ELUPROLG
00473      MOVE 0 TO COF-NBR-DTL-LINES.                                 ELUPROLG
00474      SET COF-DTL-IDX TO COF-NBR-DTL-LINES.                        ELUPROLG
00475      MOVE 0 TO WS-FRST-INDNT                                      ELUPROLG
00476                WS-SBSQNT-INDNT.                                   ELUPROLG
00477                                                                   ELUPROLG
00478      SET SSB-SS-GET-SUBTOPIC TO TRUE.                             ELUPROLG
00479      IF SSB-SEL-DATA-AVAIL (SSB-SELECTOR-STATE)                   ELUPROLG
00480      THEN                                                         ELUPROLG
00481         MOVE SSB-SUB-TOPIC-PHRASE TO WS-TPC-DSCRPTN               ELUPROLG
00482      ELSE                                                         ELUPROLG
00483         SET SSB-SS-GET-MODIFIER-1 TO TRUE                         ELUPROLG
00484         IF SSB-SEL-DATA-AVAIL (SSB-SELECTOR-STATE)                ELUPROLG
00485         THEN                                                      ELUPROLG
00486            MOVE SSB-MODIFIER-1-PHRASE TO WS-TPC-DSCRPTN           ELUPROLG
00487         ELSE                                                      ELUPROLG
00488            MOVE SSB-TOPIC-PHRASE TO WS-TPC-DSCRPTN                ELUPROLG
00489         END-IF                                                    ELUPROLG
00490      END-IF.                                                      ELUPROLG
00491      SET SSB-SS-SELECTION-DONE TO TRUE.                           ELUPROLG
00492                                                                   ELUPROLG
00493      MOVE WS-TPC-HDR TO TCAR-FROM-AREA.                           ELUPROLG
00494      PERFORM 9010-CMPRS-SND-TXT.                                  ELUPROLG
00495                                                                   ELUPROLG
00496 /*****************************************************************ELUPROLG
00497 *                                                                *ELUPROLG
00498 *    Generate Group Specific Key Information                     *ELUPROLG
00499 *                                                                *ELUPROLG
00500 ******************************************************************ELUPROLG
00501                                                                   ELUPROLG
00502  3000-GEN-GRP-SPCFC-KEY-INFRMTN.                                  ELUPROLG
00503      SET CIA-ELSGRPSP-DDN  TO  TRUE.                              ELUPROLG
00504      CALL 'ELUSETAD'                                              ELUPROLG
00505         USING DFHCOMMAREA ADDRESS OF GROUP-SPECIFIC-RECORD        ELUPROLG
00506         END-CALL.                                                 ELUPROLG
00507      IF CIA-RC-OK                                                 ELUPROLG
00508      THEN                                                         ELUPROLG
00509         PERFORM 3100-GEN-GRP-SPCFC-TTL                            ELUPROLG
00510         PERFORM 3200-GEN-GRP-SPCFC-DSCRPTN                        ELUPROLG
00511         PERFORM 9020-CALL-ELUOUTPT                                ELUPROLG
00512      END-IF.                                                      ELUPROLG
00513                                                                   ELUPROLG
00514 ******************************************************************ELUPROLG
00515 *                                                                *ELUPROLG
00516 *    Generate Group Specific Section Title                       *ELUPROLG
00517 *                                                                *ELUPROLG
00518 ******************************************************************ELUPROLG
00519                                                                   ELUPROLG
00520  3100-GEN-GRP-SPCFC-TTL.                                          ELUPROLG
00521      ADD +3 TO COF-NBR-DTL-LINES.                                 ELUPROLG
00522      SET COF-DTL-IDX UP BY 1.                                     ELUPROLG
00523      MOVE SPACES TO COF-DTL-LINE(COF-DTL-IDX).                    ELUPROLG
00524      SET COF-DTL-IDX UP BY 1.                                     ELUPROLG
00525      MOVE WS-GRP-SPCFC-TTL TO COF-DTL-LINE (COF-DTL-IDX).         ELUPROLG
00526      SET COF-DTL-IDX UP BY 1.                                     ELUPROLG
00527      MOVE SPACES TO COF-DTL-LINE(COF-DTL-IDX).                    ELUPROLG
00528                                                                   ELUPROLG
00529 ******************************************************************ELUPROLG
00530 *                                                                *ELUPROLG
00531 *    Generate Group Specific Key Description                     *ELUPROLG
00532 *                                                                *ELUPROLG
00533 ******************************************************************ELUPROLG
00534                                                                   ELUPROLG
00535  3200-GEN-GRP-SPCFC-DSCRPTN.                                      ELUPROLG
00536      MOVE 0 TO WS-FRST-INDNT                                      ELUPROLG
00537                WS-SBSQNT-INDNT.                                   ELUPROLG
00538      MOVE 0 TO TCAR-FROM-SUB.                                     ELUPROLG
00539      SET TCAR-FROM-LINE-IDX TO TCAR-FROM-SUB.                     ELUPROLG
00540      INITIALIZE TCAR-FROM-AREA.                                   ELUPROLG
00541      PERFORM 3210-CNVRT-GRP-SPCFC-DTS.                            ELUPROLG
00542                                                                   ELUPROLG
00543      IF GCG-FAM-REL-LVL = ZERO                                    ELUPROLG
00544      THEN                                                         ELUPROLG
00545         CONTINUE                                                  ELUPROLG
00546      ELSE                                                         ELUPROLG
00547         PERFORM 3220-XLT-GRP-SPCFC-FRL                            ELUPROLG
00548      END-IF.                                                      ELUPROLG
00549                                                                   ELUPROLG
00550      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00551      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).             ELUPROLG
00552      PERFORM 9010-CMPRS-SND-TXT.                                  ELUPROLG
00553      PERFORM 9020-CALL-ELUOUTPT.                                  ELUPROLG
00554                                                                   ELUPROLG
00555 ******************************************************************ELUPROLG
00556 *                                                                *ELUPROLG
00557 *    Convert Group Specific Effective Date                       *ELUPROLG
00558 *                                                                *ELUPROLG
00559 ******************************************************************ELUPROLG
00560                                                                   ELUPROLG
00561  3210-CNVRT-GRP-SPCFC-DTS.                                        ELUPROLG
00562      MOVE GCG-EFF-DT TO HGADATE-JULIAN1.                          ELUPROLG
00563      PERFORM 9030-CNVRT-DT-JLN-GRGRN.                             ELUPROLG
00564      IF HGADATE-RETURN = '00'                                     ELUPROLG
00565      THEN                                                         ELUPROLG
00566         MOVE HGADATE-DATE2 TO WS-FROM-DATE                        ELUPROLG
00567      ELSE                                                         ELUPROLG
00568         MOVE ZEROES TO WS-FROM-DATE                               ELUPROLG
00569      END-IF.                                                      ELUPROLG
00570                                                                   ELUPROLG
00571      MOVE GCG-TERMDT-CEN TO MLDATE-DATE1.                         ELUPROLG
00572      PERFORM 9040-CEN-CNVRT-DT-JLN-GRGRN.                         ELUPROLG
00573      IF MLDATE-RETURN = ZEROES                                    ELUPROLG
00574      THEN                                                         ELUPROLG
00575         MOVE MLDATE-DATE2 TO WS-TO-DATE                           ELUPROLG
00576      ELSE                                                         ELUPROLG
00577         MOVE ZEROES TO WS-TO-DATE                                 ELUPROLG
00578      END-IF.                                                      ELUPROLG
00579                                                                   ELUPROLG
00580      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00581      MOVE WS-EFFECTIVE-LN TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX). ELUPROLG
00582                                                                   ELUPROLG
00583 ******************************************************************ELUPROLG
00584 *                                                                *ELUPROLG
00585 *    Translate Group Specific Family Relationship Level          *ELUPROLG
00586 *                                                                *ELUPROLG
00587 ******************************************************************ELUPROLG
00588                                                                   ELUPROLG
00589  3220-XLT-GRP-SPCFC-FRL.                                          ELUPROLG
00590      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00591      MOVE WS-WITH TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).         ELUPROLG
00592      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00593      MOVE WS-FRL-CNSDRTNS TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX). ELUPROLG
00594                                                                   ELUPROLG
00595      MOVE 'GROUP' TO CMF-RECORD-PREFIX.                           ELUPROLG
00596      MOVE 'FAM-REL-LVL' TO CMF-ELEMENT-SYSTEM-NAME.               ELUPROLG
00597      MOVE GCG-FAM-REL-LVL TO CMF-CODE-VALUE.                      ELUPROLG
00598      CALL 'ELUCMIF' USING DFHEIBLK DFHCOMMAREA END-CALL.          ELUPROLG
00599      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELUPROLG
00600      CALL 'ELUSETAD'                                              ELUPROLG
00601         USING DFHCOMMAREA ADDRESS OF CMF-DESCR END-CALL.          ELUPROLG
00602      PERFORM 9000-APND-CMF-TCWA.                                  ELUPROLG
00603                                                                   ELUPROLG
00604 /*****************************************************************ELUPROLG
00605 *                                                                *ELUPROLG
00606 *    Generate Contract Key Information                           *ELUPROLG
00607 *                                                                *ELUPROLG
00608 ******************************************************************ELUPROLG
00609                                                                   ELUPROLG
00610  4000-GEN-CNTRCT-KEY-INFRMTN.                                     ELUPROLG
00611      SET CIA-ELSCONIB-DDN  TO  TRUE.                              ELUPROLG
00612      CALL 'ELUSETAD'                                              ELUPROLG
00613         USING DFHCOMMAREA WS-IB-PTR END-CALL.                     ELUPROLG
00614                                                                   ELUPROLG
00615      SET CIA-ELSCONIS-DDN  TO  TRUE.                              ELUPROLG
00616      CALL 'ELUSETAD'                                              ELUPROLG
00617         USING DFHCOMMAREA WS-IS-PTR END-CALL.                     ELUPROLG
00618                                                                   ELUPROLG
00619      SET CIA-ELSCONPB-DDN  TO  TRUE.                              ELUPROLG
00620      CALL 'ELUSETAD'                                              ELUPROLG
00621         USING DFHCOMMAREA WS-PB-PTR END-CALL.                     ELUPROLG
00622                                                                   ELUPROLG
00623      SET CIA-ELSCONPS-DDN  TO  TRUE.                              ELUPROLG
00624      CALL 'ELUSETAD'                                              ELUPROLG
00625         USING DFHCOMMAREA WS-PS-PTR END-CALL.                     ELUPROLG
00626                                                                   ELUPROLG
00627      IF WS-IB-PTR = NULL                                          ELUPROLG
00628      THEN                                                         ELUPROLG
00629         IF WS-IS-PTR = NULL                                       ELUPROLG
00630         THEN                                                      ELUPROLG
00631            PERFORM 4001-NO-INST                                   ELUPROLG
00632         ELSE                                                      ELUPROLG
00633            PERFORM 4002-HAS-INST-SUP                              ELUPROLG
00634         END-IF                                                    ELUPROLG
00635      ELSE                                                         ELUPROLG
00636         IF SSB-INST-BAS-L-O-B = '4'                               ELUPROLG
00637         THEN                                                      ELUPROLG
00638            PERFORM 4003-HAS-INST-CMM                              ELUPROLG
00639         ELSE                                                      ELUPROLG
00640            IF WS-IS-PTR = NULL                                    ELUPROLG
00641            THEN                                                   ELUPROLG
00642               PERFORM 4004-HAS-INST-BAS                           ELUPROLG
00643            ELSE                                                   ELUPROLG
00644               PERFORM 4005-HAS-INST-BAS-SUP                       ELUPROLG
00645            END-IF                                                 ELUPROLG
00646         END-IF                                                    ELUPROLG
00647      END-IF.                                                      ELUPROLG
00648                                                                   ELUPROLG
00649 ******************************************************************ELUPROLG
00650 *                                                                *ELUPROLG
00651 *    Generate Contract Key Information                           *ELUPROLG
00652 *    for Inquiries With No Institutional Contract Records Used   *ELUPROLG
00653 *                                                                *ELUPROLG
00654 ******************************************************************ELUPROLG
00655                                                                   ELUPROLG
00656  4001-NO-INST.                                                    ELUPROLG
00657      IF WS-PB-PTR = NULL                                          ELUPROLG
00658      THEN                                                         ELUPROLG
00659         IF WS-PS-PTR = NULL                                       ELUPROLG
00660         THEN                                                      ELUPROLG
00661            CONTINUE                                               ELUPROLG
00662         ELSE                                                      ELUPROLG
00663            PERFORM 4010-GEN-CNTRCT-TTL                            ELUPROLG
00664            PERFORM 4080-GEN-CNTRCT-DSCRPTN-PS                     ELUPROLG
00665         END-IF                                                    ELUPROLG
00666      ELSE                                                         ELUPROLG
00667         IF SSB-PROF-BAS-L-O-B = '4'                               ELUPROLG
00668         THEN                                                      ELUPROLG
00669            PERFORM 4010-GEN-CNTRCT-TTL                            ELUPROLG
00670            PERFORM 4060-GEN-CNTRCT-DSCRPTN-P                      ELUPROLG
00671         ELSE                                                      ELUPROLG
00672            IF WS-PS-PTR = NULL                                    ELUPROLG
00673            THEN                                                   ELUPROLG
00674               PERFORM 4010-GEN-CNTRCT-TTL                         ELUPROLG
00675               PERFORM 4070-GEN-CNTRCT-DSCRPTN-PB                  ELUPROLG
00676            ELSE                                                   ELUPROLG
00677               PERFORM 4010-GEN-CNTRCT-TTL                         ELUPROLG
00678               PERFORM 4070-GEN-CNTRCT-DSCRPTN-PB                  ELUPROLG
00679               PERFORM 4080-GEN-CNTRCT-DSCRPTN-PS                  ELUPROLG
00680            END-IF                                                 ELUPROLG
00681         END-IF                                                    ELUPROLG
00682      END-IF.                                                      ELUPROLG
00683                                                                   ELUPROLG
00684 ******************************************************************ELUPROLG
00685 *                                                                *ELUPROLG
00686 *    Generate Contract Key Information                           *ELUPROLG
00687 *    for Inquiries With Institutional Supplemental Contract      *ELUPROLG
00688 *    Record Used                                                 *ELUPROLG
00689 *                                                                *ELUPROLG
00690 ******************************************************************ELUPROLG
00691                                                                   ELUPROLG
00692  4002-HAS-INST-SUP.                                               ELUPROLG
00693      IF WS-IS-PTR = WS-PS-PTR                                     ELUPROLG
00694      THEN                                                         ELUPROLG
00695         IF WS-PB-PTR = NULL                                       ELUPROLG
00696         THEN                                                      ELUPROLG
00697            PERFORM 4010-GEN-CNTRCT-TTL                            ELUPROLG
00698            PERFORM 4090-GEN-CNTRCT-DSCRPTN-S                      ELUPROLG
00699         ELSE                                                      ELUPROLG
00700            PERFORM 4010-GEN-CNTRCT-TTL                            ELUPROLG
00701            PERFORM 4070-GEN-CNTRCT-DSCRPTN-PB                     ELUPROLG
00702            PERFORM 4090-GEN-CNTRCT-DSCRPTN-S                      ELUPROLG
00703         END-IF                                                    ELUPROLG
00704      ELSE                                                         ELUPROLG
00705         IF WS-PB-PTR = NULL                                       ELUPROLG
00706         THEN                                                      ELUPROLG
00707            IF WS-PS-PTR = NULL                                    ELUPROLG
00708            THEN                                                   ELUPROLG
00709               PERFORM 4010-GEN-CNTRCT-TTL                         ELUPROLG
00710               PERFORM 4050-GEN-CNTRCT-DSCRPTN-IS                  ELUPROLG
00711            ELSE                                                   ELUPROLG
00712               PERFORM 4010-GEN-CNTRCT-TTL                         ELUPROLG
00713               PERFORM 4050-GEN-CNTRCT-DSCRPTN-IS                  ELUPROLG
00714               PERFORM 4080-GEN-CNTRCT-DSCRPTN-PS                  ELUPROLG
00715            END-IF                                                 ELUPROLG
00716         ELSE                                                      ELUPROLG
00717            IF WS-PS-PTR = NULL                                    ELUPROLG
00718            THEN                                                   ELUPROLG
00719               PERFORM 4010-GEN-CNTRCT-TTL                         ELUPROLG
00720               PERFORM 4050-GEN-CNTRCT-DSCRPTN-IS                  ELUPROLG
00721               PERFORM 4070-GEN-CNTRCT-DSCRPTN-PB                  ELUPROLG
00722            ELSE                                                   ELUPROLG
00723               PERFORM 4010-GEN-CNTRCT-TTL                         ELUPROLG
00724               PERFORM 4050-GEN-CNTRCT-DSCRPTN-IS                  ELUPROLG
00725               PERFORM 4070-GEN-CNTRCT-DSCRPTN-PB                  ELUPROLG
00726               PERFORM 4080-GEN-CNTRCT-DSCRPTN-PS                  ELUPROLG
00727            END-IF                                                 ELUPROLG
00728         END-IF                                                    ELUPROLG
00729      END-IF.                                                      ELUPROLG
00730                                                                   ELUPROLG
00731 ******************************************************************ELUPROLG
00732 *                                                                *ELUPROLG
00733 *    Generate Contract Key Information                           *ELUPROLG
00734 *    for Inquiries With Institutional Comprehensive Major        *ELUPROLG
00735 *    Medical Contract Record Used                                *ELUPROLG
00736 *                                                                *ELUPROLG
00737 ******************************************************************ELUPROLG
00738                                                                   ELUPROLG
00739  4003-HAS-INST-CMM.                                               ELUPROLG
00740      IF WS-IB-PTR = WS-PB-PTR                                     ELUPROLG
00741      THEN                                                         ELUPROLG
00742         PERFORM 4010-GEN-CNTRCT-TTL                               ELUPROLG
00743         PERFORM 4020-GEN-CNTRCT-DSCRPTN-C                         ELUPROLG
00744      ELSE                                                         ELUPROLG
00745         IF WS-PB-PTR = NULL                                       ELUPROLG
00746         THEN                                                      ELUPROLG
00747            PERFORM 4010-GEN-CNTRCT-TTL                            ELUPROLG
00748            PERFORM 4030-GEN-CNTRCT-DSCRPTN-I                      ELUPROLG
00749         ELSE                                                      ELUPROLG
00750            PERFORM 4010-GEN-CNTRCT-TTL                            ELUPROLG
00751            PERFORM 4030-GEN-CNTRCT-DSCRPTN-I                      ELUPROLG
00752            PERFORM 4060-GEN-CNTRCT-DSCRPTN-P                      ELUPROLG
00753         END-IF                                                    ELUPROLG
00754      END-IF.                                                      ELUPROLG
00755                                                                   ELUPROLG
00756 ******************************************************************ELUPROLG
00757 *                                                                *ELUPROLG
00758 *    Generate Contract Key Information                           *ELUPROLG
00759 *    for Inquiries With Institutional Basic Contract Record Used *ELUPROLG
00760 *                                                                *ELUPROLG
00761 ******************************************************************ELUPROLG
00762                                                                   ELUPROLG
00763  4004-HAS-INST-BAS.                                               ELUPROLG
00764      IF WS-PB-PTR = NULL                                          ELUPROLG
00765      THEN                                                         ELUPROLG
00766         IF WS-PS-PTR = NULL                                       ELUPROLG
00767         THEN                                                      ELUPROLG
00768            PERFORM 4010-GEN-CNTRCT-TTL                            ELUPROLG
00769            PERFORM 4040-GEN-CNTRCT-DSCRPTN-IB                     ELUPROLG
00770         ELSE                                                      ELUPROLG
00771            PERFORM 4010-GEN-CNTRCT-TTL                            ELUPROLG
00772            PERFORM 4040-GEN-CNTRCT-DSCRPTN-IB                     ELUPROLG
00773            PERFORM 4080-GEN-CNTRCT-DSCRPTN-PS                     ELUPROLG
00774         END-IF                                                    ELUPROLG
00775      ELSE                                                         ELUPROLG
00776         IF WS-PS-PTR = NULL                                       ELUPROLG
00777         THEN                                                      ELUPROLG
00778            PERFORM 4010-GEN-CNTRCT-TTL                            ELUPROLG
00779            PERFORM 4040-GEN-CNTRCT-DSCRPTN-IB                     ELUPROLG
00780            PERFORM 4070-GEN-CNTRCT-DSCRPTN-PB                     ELUPROLG
00781         ELSE                                                      ELUPROLG
00782            PERFORM 4010-GEN-CNTRCT-TTL                            ELUPROLG
00783            PERFORM 4040-GEN-CNTRCT-DSCRPTN-IB                     ELUPROLG
00784            PERFORM 4070-GEN-CNTRCT-DSCRPTN-PB                     ELUPROLG
00785            PERFORM 4080-GEN-CNTRCT-DSCRPTN-PS                     ELUPROLG
00786         END-IF                                                    ELUPROLG
00787      END-IF.                                                      ELUPROLG
00788                                                                   ELUPROLG
00789 ******************************************************************ELUPROLG
00790 *                                                                *ELUPROLG
00791 *    Generate Contract Key Information                           *ELUPROLG
00792 *    for Inquiries With Institutional Basic and Supplemental     *ELUPROLG
00793 *    Major Medical Records Used                                  *ELUPROLG
00794 *                                                                *ELUPROLG
00795 ******************************************************************ELUPROLG
00796                                                                   ELUPROLG
00797  4005-HAS-INST-BAS-SUP.                                           ELUPROLG
00798      IF WS-IS-PTR = WS-PS-PTR                                     ELUPROLG
00799      THEN                                                         ELUPROLG
00800         IF WS-IS-PTR = NULL                                       ELUPROLG
00801         THEN                                                      ELUPROLG
00802            PERFORM 4010-GEN-CNTRCT-TTL                            ELUPROLG
00803            PERFORM 4040-GEN-CNTRCT-DSCRPTN-IB                     ELUPROLG
00804            PERFORM 4090-GEN-CNTRCT-DSCRPTN-S                      ELUPROLG
00805         ELSE                                                      ELUPROLG
00806            PERFORM 4010-GEN-CNTRCT-TTL                            ELUPROLG
00807            PERFORM 4040-GEN-CNTRCT-DSCRPTN-IB                     ELUPROLG
00808            PERFORM 4070-GEN-CNTRCT-DSCRPTN-PB                     ELUPROLG
00809            PERFORM 4090-GEN-CNTRCT-DSCRPTN-S                      ELUPROLG
00810         END-IF                                                    ELUPROLG
00811      ELSE                                                         ELUPROLG
00812         IF WS-PB-PTR = NULL                                       ELUPROLG
00813         THEN                                                      ELUPROLG
00814            IF WS-PS-PTR = NULL                                    ELUPROLG
00815            THEN                                                   ELUPROLG
00816               PERFORM 4010-GEN-CNTRCT-TTL                         ELUPROLG
00817               PERFORM 4040-GEN-CNTRCT-DSCRPTN-IB                  ELUPROLG
00818               PERFORM 4050-GEN-CNTRCT-DSCRPTN-IS                  ELUPROLG
00819            ELSE                                                   ELUPROLG
00820               PERFORM 4010-GEN-CNTRCT-TTL                         ELUPROLG
00821               PERFORM 4040-GEN-CNTRCT-DSCRPTN-IB                  ELUPROLG
00822               PERFORM 4050-GEN-CNTRCT-DSCRPTN-IS                  ELUPROLG
00823               PERFORM 4080-GEN-CNTRCT-DSCRPTN-PS                  ELUPROLG
00824            END-IF                                                 ELUPROLG
00825         ELSE                                                      ELUPROLG
00826            IF WS-PS-PTR = NULL                                    ELUPROLG
00827            THEN                                                   ELUPROLG
00828               PERFORM 4010-GEN-CNTRCT-TTL                         ELUPROLG
00829               PERFORM 4040-GEN-CNTRCT-DSCRPTN-IB                  ELUPROLG
00830               PERFORM 4050-GEN-CNTRCT-DSCRPTN-IS                  ELUPROLG
00831               PERFORM 4070-GEN-CNTRCT-DSCRPTN-PB                  ELUPROLG
00832               CONTINUE                                            ELUPROLG
00833            ELSE                                                   ELUPROLG
00834               PERFORM 4010-GEN-CNTRCT-TTL                         ELUPROLG
00835               PERFORM 4040-GEN-CNTRCT-DSCRPTN-IB                  ELUPROLG
00836               PERFORM 4050-GEN-CNTRCT-DSCRPTN-IS                  ELUPROLG
00837               PERFORM 4070-GEN-CNTRCT-DSCRPTN-PB                  ELUPROLG
00838               PERFORM 4080-GEN-CNTRCT-DSCRPTN-PS                  ELUPROLG
00839            END-IF                                                 ELUPROLG
00840         END-IF                                                    ELUPROLG
00841      END-IF.                                                      ELUPROLG
00842                                                                   ELUPROLG
00843 ******************************************************************ELUPROLG
00844 *                                                                *ELUPROLG
00845 *    Generate Contract Section Title                             *ELUPROLG
00846 *                                                                *ELUPROLG
00847 ******************************************************************ELUPROLG
00848                                                                   ELUPROLG
00849  4010-GEN-CNTRCT-TTL.                                             ELUPROLG
00850      IF COF-NBR-DTL-LINES > (WS-MAX-NBR-DTL-LN - 5)               ELUPROLG
00851      THEN                                                         ELUPROLG
00852         PERFORM 9020-CALL-ELUOUTPT                                ELUPROLG
00853      END-IF.                                                      ELUPROLG
00854      ADD +2 TO COF-NBR-DTL-LINES.                                 ELUPROLG
00855      SET COF-DTL-IDX UP BY 2.                                     ELUPROLG
00856      MOVE WS-CNTRCT-TTL TO COF-DTL-LINE (COF-DTL-IDX).            ELUPROLG
00857                                                                   ELUPROLG
00858 ******************************************************************ELUPROLG
00859 *                                                                *ELUPROLG
00860 *    Generate Contract Description for Single Comprehensive      *ELUPROLG
00861 *    Major Medical Contract                                      *ELUPROLG
00862 *                                                                *ELUPROLG
00863 ******************************************************************ELUPROLG
00864                                                                   ELUPROLG
00865  4020-GEN-CNTRCT-DSCRPTN-C.                                       ELUPROLG
00866      MOVE +1 TO WS-FRST-INDNT.                                    ELUPROLG
00867      MOVE +14 TO WS-SBSQNT-INDNT.                                 ELUPROLG
00868      MOVE 0 TO TCAR-FROM-SUB.                                     ELUPROLG
00869      SET TCAR-FROM-LINE-IDX TO TCAR-FROM-SUB                      ELUPROLG
00870      INITIALIZE TCAR-FROM-AREA.                                   ELUPROLG
00871                                                                   ELUPROLG
00872      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00873      MOVE WS-ALL TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).          ELUPROLG
00874      SET ADDRESS OF CONTRACT-RECORD TO WS-IB-PTR.                 ELUPROLG
00875      PERFORM 4100-GEN-CNTRCT-DSCRPTN.                             ELUPROLG
00876                                                                   ELUPROLG
00877 ******************************************************************ELUPROLG
00878 *                                                                *ELUPROLG
00879 *    Generate Contract Description for Institutional Benefits    *ELUPROLG
00880 *    from Comprehensive Major Medical Contracte                  *ELUPROLG
00881 *                                                                *ELUPROLG
00882 ******************************************************************ELUPROLG
00883                                                                   ELUPROLG
00884  4030-GEN-CNTRCT-DSCRPTN-I.                                       ELUPROLG
00885      MOVE +0 TO WS-FRST-INDNT.                                    ELUPROLG
00886      MOVE +14 TO WS-SBSQNT-INDNT.                                 ELUPROLG
00887      MOVE 0 TO TCAR-FROM-SUB.                                     ELUPROLG
00888      SET TCAR-FROM-LINE-IDX TO TCAR-FROM-SUB                      ELUPROLG
00889      INITIALIZE TCAR-FROM-AREA.                                   ELUPROLG
00890                                                                   ELUPROLG
00891      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00892      MOVE WS-INST TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).         ELUPROLG
00893      SET ADDRESS OF CONTRACT-RECORD TO WS-IB-PTR.                 ELUPROLG
00894      PERFORM 4100-GEN-CNTRCT-DSCRPTN.                             ELUPROLG
00895                                                                   ELUPROLG
00896 ******************************************************************ELUPROLG
00897 *                                                                *ELUPROLG
00898 *    Generage Contract Description for Basic Institutional       *ELUPROLG
00899 *    Benefits from Blue Cross Contract                           *ELUPROLG
00900 *                                                                *ELUPROLG
00901 ******************************************************************ELUPROLG
00902                                                                   ELUPROLG
00903  4040-GEN-CNTRCT-DSCRPTN-IB.                                      ELUPROLG
00904      MOVE +0 TO WS-FRST-INDNT.                                    ELUPROLG
00905      MOVE +14 TO WS-SBSQNT-INDNT.                                 ELUPROLG
00906      MOVE 0 TO TCAR-FROM-SUB.                                     ELUPROLG
00907      SET TCAR-FROM-LINE-IDX TO TCAR-FROM-SUB                      ELUPROLG
00908      INITIALIZE TCAR-FROM-AREA.                                   ELUPROLG
00909                                                                   ELUPROLG
00910      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00911      MOVE WS-INST TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).         ELUPROLG
00912      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00913      MOVE WS-BAS TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).          ELUPROLG
00914      SET ADDRESS OF CONTRACT-RECORD TO WS-IB-PTR.                 ELUPROLG
00915      PERFORM 4100-GEN-CNTRCT-DSCRPTN.                             ELUPROLG
00916                                                                   ELUPROLG
00917 ******************************************************************ELUPROLG
00918 *                                                                *ELUPROLG
00919 *    Generate Contract Description for Supplemental              *ELUPROLG
00920 *    Institutional Benefits from Supplemental Major Medical      *ELUPROLG
00921 *    Contract                                                    *ELUPROLG
00922 *                                                                *ELUPROLG
00923 ******************************************************************ELUPROLG
00924                                                                   ELUPROLG
00925  4050-GEN-CNTRCT-DSCRPTN-IS.                                      ELUPROLG
00926      MOVE +0 TO WS-FRST-INDNT.                                    ELUPROLG
00927      MOVE +14 TO WS-SBSQNT-INDNT.                                 ELUPROLG
00928      MOVE 0 TO TCAR-FROM-SUB.                                     ELUPROLG
00929      SET TCAR-FROM-LINE-IDX TO TCAR-FROM-SUB                      ELUPROLG
00930      INITIALIZE TCAR-FROM-AREA.                                   ELUPROLG
00931                                                                   ELUPROLG
00932      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00933      MOVE WS-INST TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).         ELUPROLG
00934      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00935      MOVE WS-SUP TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).          ELUPROLG
00936      SET ADDRESS OF CONTRACT-RECORD TO WS-IS-PTR.                 ELUPROLG
00937      PERFORM 4100-GEN-CNTRCT-DSCRPTN.                             ELUPROLG
00938                                                                   ELUPROLG
00939 ******************************************************************ELUPROLG
00940 *                                                                *ELUPROLG
00941 *    Generate Contract Description for Professional Benefits     *ELUPROLG
00942 *    from Comprehensive Major Medical Contract                   *ELUPROLG
00943 *                                                                *ELUPROLG
00944 ******************************************************************ELUPROLG
00945                                                                   ELUPROLG
00946  4060-GEN-CNTRCT-DSCRPTN-P.                                       ELUPROLG
00947      MOVE +1 TO WS-FRST-INDNT.                                    ELUPROLG
00948      MOVE +14 TO WS-SBSQNT-INDNT.                                 ELUPROLG
00949      MOVE 0 TO TCAR-FROM-SUB.                                     ELUPROLG
00950      SET TCAR-FROM-LINE-IDX TO TCAR-FROM-SUB                      ELUPROLG
00951      INITIALIZE TCAR-FROM-AREA.                                   ELUPROLG
00952                                                                   ELUPROLG
00953      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00954      MOVE WS-PROF TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).         ELUPROLG
00955      SET ADDRESS OF CONTRACT-RECORD TO WS-PB-PTR.                 ELUPROLG
00956      PERFORM 4100-GEN-CNTRCT-DSCRPTN.                             ELUPROLG
00957                                                                   ELUPROLG
00958 ******************************************************************ELUPROLG
00959 *                                                                *ELUPROLG
00960 *    Generate Contract Description for Basic Professional        *ELUPROLG
00961 *    Benefits from Blue Shield Contract                          *ELUPROLG
00962 *                                                                *ELUPROLG
00963 ******************************************************************ELUPROLG
00964                                                                   ELUPROLG
00965  4070-GEN-CNTRCT-DSCRPTN-PB.                                      ELUPROLG
00966      MOVE +1 TO WS-FRST-INDNT.                                    ELUPROLG
00967      MOVE +14 TO WS-SBSQNT-INDNT.                                 ELUPROLG
00968      MOVE 0 TO TCAR-FROM-SUB.                                     ELUPROLG
00969      SET TCAR-FROM-LINE-IDX TO TCAR-FROM-SUB                      ELUPROLG
00970      INITIALIZE TCAR-FROM-AREA.                                   ELUPROLG
00971                                                                   ELUPROLG
00972      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00973      MOVE WS-PROF TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).         ELUPROLG
00974      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00975      MOVE WS-BAS TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).          ELUPROLG
00976      SET ADDRESS OF CONTRACT-RECORD TO WS-PB-PTR.                 ELUPROLG
00977      PERFORM 4100-GEN-CNTRCT-DSCRPTN.                             ELUPROLG
00978                                                                   ELUPROLG
00979 ******************************************************************ELUPROLG
00980 *                                                                *ELUPROLG
00981 *    Generate Contract Description for Supplemental Professional *ELUPROLG
00982 *    Benefits from Supplemental Major Medical Contract           *ELUPROLG
00983 *                                                                *ELUPROLG
00984 ******************************************************************ELUPROLG
00985                                                                   ELUPROLG
00986  4080-GEN-CNTRCT-DSCRPTN-PS.                                      ELUPROLG
00987      MOVE +1 TO WS-FRST-INDNT.                                    ELUPROLG
00988      MOVE +14 TO WS-SBSQNT-INDNT.                                 ELUPROLG
00989      MOVE 0 TO TCAR-FROM-SUB.                                     ELUPROLG
00990      SET TCAR-FROM-LINE-IDX TO TCAR-FROM-SUB                      ELUPROLG
00991      INITIALIZE TCAR-FROM-AREA.                                   ELUPROLG
00992                                                                   ELUPROLG
00993      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00994      MOVE WS-PROF TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).         ELUPROLG
00995      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
00996      MOVE WS-BAS TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).          ELUPROLG
00997      SET ADDRESS OF CONTRACT-RECORD TO WS-PS-PTR.                 ELUPROLG
00998      PERFORM 4100-GEN-CNTRCT-DSCRPTN.                             ELUPROLG
00999                                                                   ELUPROLG
01000 ******************************************************************ELUPROLG
01001 *                                                                *ELUPROLG
01002 *    Generate Contract Description for Supplemental Benefits     *ELUPROLG
01003 *    from Single Supplemental Major Medical Contract             *ELUPROLG
01004 *                                                                *ELUPROLG
01005 ******************************************************************ELUPROLG
01006                                                                   ELUPROLG
01007  4090-GEN-CNTRCT-DSCRPTN-S.                                       ELUPROLG
01008      MOVE +1 TO WS-FRST-INDNT.                                    ELUPROLG
01009      MOVE +14 TO WS-SBSQNT-INDNT.                                 ELUPROLG
01010      MOVE 0 TO TCAR-FROM-SUB.                                     ELUPROLG
01011      SET TCAR-FROM-LINE-IDX TO TCAR-FROM-SUB                      ELUPROLG
01012      INITIALIZE TCAR-FROM-AREA.                                   ELUPROLG
01013                                                                   ELUPROLG
01014      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
01015      MOVE WS-SUP TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).          ELUPROLG
01016      SET ADDRESS OF CONTRACT-RECORD TO WS-IS-PTR.                 ELUPROLG
01017      PERFORM 4100-GEN-CNTRCT-DSCRPTN.                             ELUPROLG
01018                                                                   ELUPROLG
01019 ******************************************************************ELUPROLG
01020 *                                                                *ELUPROLG
01021 *    Generate Contract Description for Any Contract              *ELUPROLG
01022 *                                                                *ELUPROLG
01023 ******************************************************************ELUPROLG
01024                                                                   ELUPROLG
01025  4100-GEN-CNTRCT-DSCRPTN.                                         ELUPROLG
01026      ADD +1 TO COF-NBR-DTL-LINES.                                 ELUPROLG
01027      SET COF-DTL-IDX UP BY 1.                                     ELUPROLG
01028      MOVE SPACES TO COF-DTL-LINE (COF-DTL-IDX).                   ELUPROLG
01029      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
01030      MOVE WS-BNFT TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).         ELUPROLG
01031      PERFORM 4110-XLAT-CNTRCT-LOB.                                ELUPROLG
01032      PERFORM 4120-CNVRT-CNTRCT-DTS.                               ELUPROLG
01033      IF GCT-FAM-REL-LVL = ZEROS                                   ELUPROLG
01034      THEN                                                         ELUPROLG
01035         IF GCT-PROVDR-CONTROL = ZEROS                             ELUPROLG
01036         THEN                                                      ELUPROLG
01037            CONTINUE                                               ELUPROLG
01038         ELSE                                                      ELUPROLG
01039            SET TCAR-FROM-LINE-IDX UP BY 1                         ELUPROLG
01040            MOVE WS-WITH TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX)    ELUPROLG
01041            PERFORM 4140-XLAT-CNTRCT-PRVDR-CNTL                    ELUPROLG
01042         END-IF                                                    ELUPROLG
01043      ELSE                                                         ELUPROLG
01044         SET TCAR-FROM-LINE-IDX UP BY 1                            ELUPROLG
01045         MOVE WS-WITH TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX)       ELUPROLG
01046         PERFORM 4130-XLAT-CNTRCT-FRL                              ELUPROLG
01047         IF GCT-PROVDR-CONTROL = ZEROS                             ELUPROLG
01048         THEN                                                      ELUPROLG
01049            CONTINUE                                               ELUPROLG
01050         ELSE                                                      ELUPROLG
01051            SET TCAR-FROM-LINE-IDX UP BY 1                         ELUPROLG
01052            MOVE WS-WITH TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX)    ELUPROLG
01053            PERFORM 4140-XLAT-CNTRCT-PRVDR-CNTL                    ELUPROLG
01054         END-IF                                                    ELUPROLG
01055      END-IF.                                                      ELUPROLG
01056                                                                   ELUPROLG
01057      SET TCAR-FROM-LINE-IDX UP BY 1                               ELUPROLG
01058      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX)              ELUPROLG
01059      PERFORM 9010-CMPRS-SND-TXT.                                  ELUPROLG
01060      PERFORM 9020-CALL-ELUOUTPT.                                  ELUPROLG
01061                                                                   ELUPROLG
01062 ******************************************************************ELUPROLG
01063 *                                                                *ELUPROLG
01064 *    Translate Contract Line of Business Indicator               *ELUPROLG
01065 *                                                                *ELUPROLG
01066 ******************************************************************ELUPROLG
01067                                                                   ELUPROLG
01068  4110-XLAT-CNTRCT-LOB.                                            ELUPROLG
01069      MOVE 'CONTRACT' TO CMF-RECORD-PREFIX.                        ELUPROLG
01070      MOVE 'L-O-B' TO CMF-ELEMENT-SYSTEM-NAME.                     ELUPROLG
01071      MOVE GCT-L-O-B TO CMF-CODE-VALUE.                            ELUPROLG
01072      CALL 'ELUCMIF' USING DFHEIBLK DFHCOMMAREA END-CALL.          ELUPROLG
01073      SET CIA-ELSCMDSC-DDN  TO  TRUE.                              ELUPROLG
01074      CALL 'ELUSETAD'                                              ELUPROLG
01075         USING DFHCOMMAREA ADDRESS OF CMF-DESCR END-CALL.          ELUPROLG
01076      PERFORM 9000-APND-CMF-TCWA.                                  ELUPROLG
01077                                                                   ELUPROLG
01078 ******************************************************************ELUPROLG
01079 *                                                                *ELUPROLG
01080 *    Convert Contract Effective and Terminate Dates              *ELUPROLG
01081 *                                                                *ELUPROLG
01082 ******************************************************************ELUPROLG
01083                                                                   ELUPROLG
01084  4120-CNVRT-CNTRCT-DTS.                                           ELUPROLG
01085      MOVE GCT-EFF-DT TO HGADATE-JULIAN1.                          ELUPROLG
01086      PERFORM 9030-CNVRT-DT-JLN-GRGRN.                             ELUPROLG
01087      IF HGADATE-RETURN = '00'                                     ELUPROLG
01088      THEN                                                         ELUPROLG
01089         MOVE HGADATE-DATE2 TO WS-FROM-DATE                        ELUPROLG
01090      ELSE                                                         ELUPROLG
01091         MOVE ZEROES TO WS-FROM-DATE                               ELUPROLG
01092      END-IF.                                                      ELUPROLG
01093                                                                   ELUPROLG
01094      MOVE GCT-TERMDT-CEN TO MLDATE-DATE1.                         ELUPROLG
01095      PERFORM 9040-CEN-CNVRT-DT-JLN-GRGRN.                         ELUPROLG
01096      IF MLDATE-RETURN = ZEROES                                    ELUPROLG
01097      THEN                                                         ELUPROLG
01098         MOVE MLDATE-DATE2 TO WS-TO-DATE                           ELUPROLG
01099      ELSE                                                         ELUPROLG
01100         MOVE ZEROES TO WS-TO-DATE                                 ELUPROLG
01101      END-IF.                                                      ELUPROLG
01102                                                                   ELUPROLG
01103      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
01104      MOVE WS-EFFECTIVE-LN TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX). ELUPROLG
01105                                                                   ELUPROLG
01106 ******************************************************************ELUPROLG
01107 *                                                                *ELUPROLG
01108 *    Translate Contract Family Relationship Level                *ELUPROLG
01109 *                                                                *ELUPROLG
01110 ******************************************************************ELUPROLG
01111                                                                   ELUPROLG
01112  4130-XLAT-CNTRCT-FRL.                                            ELUPROLG
01113      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
01114      MOVE WS-FRL-CNSDRTNS TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX). ELUPROLG
01115      MOVE 'CONTRACT' TO CMF-RECORD-PREFIX.                        ELUPROLG
01116      MOVE 'FAM-REL-LVL' TO CMF-ELEMENT-SYSTEM-NAME.               ELUPROLG
01117      MOVE GCT-FAM-REL-LVL TO CMF-CODE-VALUE.                      ELUPROLG
01118      CALL 'ELUCMIF' USING DFHEIBLK DFHCOMMAREA END-CALL.          ELUPROLG
01119      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELUPROLG
01120      CALL 'ELUSETAD'                                              ELUPROLG
01121         USING DFHCOMMAREA ADDRESS OF CMF-DESCR END-CALL.          ELUPROLG
01122      PERFORM 9000-APND-CMF-TCWA.                                  ELUPROLG
01123                                                                   ELUPROLG
01124 ******************************************************************ELUPROLG
01125 *                                                                *ELUPROLG
01126 *    Translate Contract Provider Control                         *ELUPROLG
01127 *                                                                *ELUPROLG
01128 ******************************************************************ELUPROLG
01129                                                                   ELUPROLG
01130  4140-XLAT-CNTRCT-PRVDR-CNTL.                                     ELUPROLG
01131      SET TCAR-FROM-LINE-IDX UP BY 1.                              ELUPROLG
01132      MOVE WS-PRVDR-CNTRL-CNSDRTNS                                 ELUPROLG
01133        TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX).                    ELUPROLG
01134      MOVE 'CONTRACT' TO CMF-RECORD-PREFIX.                        ELUPROLG
01135      MOVE 'PROVDR-CONTROL' TO CMF-ELEMENT-SYSTEM-NAME.            ELUPROLG
01136      MOVE GCT-PROVDR-CONTROL TO CMF-CODE-VALUE.                   ELUPROLG
01137      CALL 'ELUCMIF' USING DFHEIBLK DFHCOMMAREA END-CALL.          ELUPROLG
01138      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELUPROLG
01139      CALL 'ELUSETAD'                                              ELUPROLG
01140         USING DFHCOMMAREA ADDRESS OF CMF-DESCR END-CALL.          ELUPROLG
01141      PERFORM 9000-APND-CMF-TCWA.                                  ELUPROLG
01142                                                                   ELUPROLG
01143 /*****************************************************************ELUPROLG
01144 *                                                                *ELUPROLG
01145 *    Utility Subroutines                                         *ELUPROLG
01146 *                                                                *ELUPROLG
01147 ******************************************************************ELUPROLG
01148                                                                   ELUPROLG
01149 ******************************************************************ELUPROLG
01150 *                                                                *ELUPROLG
01151 *    Append Codes Manual Description To Text Compression Work    *ELUPROLG
01152 *    Area                                                        *ELUPROLG
01153 *                                                                *ELUPROLG
01154 *       This is an abbreviated form of a method used elsewhere   *ELUPROLG
01155 *       in ECI. This version assumes only short translations     *ELUPROLG
01156 *       appended to short introductory phrases. If EL99 abends   *ELUPROLG
01157 *       begin ocurring based on this assumption, this routine    *ELUPROLG
01158 *       will have to be upgraded to match a more comprehensive   *ELUPROLG
01159 *       model.                                                   *ELUPROLG
01160 *                                                                *ELUPROLG
01161 ******************************************************************ELUPROLG
01162                                                                   ELUPROLG
01163  9000-APND-CMF-TCWA.                                              ELUPROLG
01164      SET CMF-MAX-IDX TO CMF-NBR-DESCR-LINES.                      ELUPROLG
01165      PERFORM WITH TEST AFTER                                      ELUPROLG
01166         VARYING CMF-DESCR-IDX                                     ELUPROLG
01167            FROM 1 BY 1                                            ELUPROLG
01168           UNTIL CMF-DESCR-IDX > CMF-MAX-IDX                       ELUPROLG
01169         SET TCAR-FROM-LINE-IDX UP BY 1                            ELUPROLG
01170         MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                       ELUPROLG
01171           TO TCAR-FROM-LINE (TCAR-FROM-LINE-IDX)                  ELUPROLG
01172         IF TCAR-FROM-LINE-IDX = TCAR-FROM-LINE-MAX-IDX            ELUPROLG
01173         THEN                                                      ELUPROLG
01174            SET CIA-AB-PGM-LOGIC TO TRUE                           ELUPROLG
01175            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELUPROLG
01176         END-IF                                                    ELUPROLG
01177         END-PERFORM.                                              ELUPROLG
01178                                                                   ELUPROLG
01179 ******************************************************************ELUPROLG
01180 *                                                                *ELUPROLG
01181 *    Move Text From Text Compression Work Area To Output         *ELUPROLG
01182 *                                                                *ELUPROLG
01183 ******************************************************************ELUPROLG
01184                                                                   ELUPROLG
01185  9010-CMPRS-SND-TXT.                                              ELUPROLG
01186      COMPUTE WS-FRST-LN-LEN = +79 - WS-FRST-INDNT.                ELUPROLG
01187      COMPUTE WS-SBSQNT-LN-LEN = +79 - WS-SBSQNT-INDNT.            ELUPROLG
01188      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA END-CALL.   ELUPROLG
01189      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELUPROLG
01190      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELUPROLG
01191      MOVE WS-FRST-LN-LEN TO TCAR-OUTPUT-FIELD-1-LEN.              ELUPROLG
01192      MOVE WS-SBSQNT-LN-LEN TO TCAR-OUTPUT-FIELD-2-LEN             ELUPROLG
01193                               TCAR-OUTPUT-FIELD-3-LEN             ELUPROLG
01194                               TCAR-OUTPUT-FIELD-4-LEN             ELUPROLG
01195                               TCAR-OUTPUT-FIELD-5-LEN             ELUPROLG
01196                               TCAR-OUTPUT-FIELD-6-LEN             ELUPROLG
01197                               TCAR-OUTPUT-FIELD-7-LEN             ELUPROLG
01198                               TCAR-OUTPUT-FIELD-8-LEN             ELUPROLG
01199                               TCAR-OUTPUT-FIELD-9-LEN             ELUPROLG
01200                               TCAR-OUTPUT-FIELD-10-LEN            ELUPROLG
01201                               TCAR-OUTPUT-FIELD-11-LEN            ELUPROLG
01202                               TCAR-OUTPUT-FIELD-12-LEN            ELUPROLG
01203                               TCAR-OUTPUT-FIELD-13-LEN            ELUPROLG
01204                               TCAR-OUTPUT-FIELD-14-LEN            ELUPROLG
01205                               TCAR-OUTPUT-FIELD-15-LEN            ELUPROLG
01206                               TCAR-OUTPUT-FIELD-16-LEN            ELUPROLG
01207                               TCAR-OUTPUT-FIELD-17-LEN            ELUPROLG
01208                               TCAR-OUTPUT-FIELD-18-LEN            ELUPROLG
01209                               TCAR-OUTPUT-FIELD-19-LEN            ELUPROLG
01210                               TCAR-OUTPUT-FIELD-20-LEN.           ELUPROLG
01211      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA END-CALL.   ELUPROLG
01212      IF TCAR-OUTPUT-FIELDS-USED > 0                               ELUPROLG
01213      THEN                                                         ELUPROLG
01214         SET TCAR-OPF-USED-IDX TO TCAR-OUTPUT-FIELDS-USED          ELUPROLG
01215         SET TCAR-OPF-IDX TO 1                                     ELUPROLG
01216         ADD +1 TO COF-NBR-DTL-LINES                               ELUPROLG
01217         SET COF-DTL-IDX UP BY 1                                   ELUPROLG
01218         COMPUTE WS-LN-OFST = 80 - WS-FRST-LN-LEN                  ELUPROLG
01219         MOVE TCAR-OPF-DATA (TCAR-OPF-IDX) (1:WS-FRST-LN-LEN)      ELUPROLG
01220           TO COF-DTL-LINE (COF-DTL-IDX) (WS-LN-OFST:)             ELUPROLG
01221                                                                   ELUPROLG
01222         COMPUTE WS-LN-OFST = 80 - WS-SBSQNT-LN-LEN                ELUPROLG
01223         PERFORM WITH TEST BEFORE                                  ELUPROLG
01224            VARYING TCAR-OPF-IDX                                   ELUPROLG
01225               FROM 2 BY 1                                         ELUPROLG
01226              UNTIL TCAR-OPF-IDX > TCAR-OPF-USED-IDX               ELUPROLG
01227            IF COF-DTL-IDX = COF-DTL-MAX-IDX                       ELUPROLG
01228            THEN                                                   ELUPROLG
01229               PERFORM 9020-CALL-ELUOUTPT                          ELUPROLG
01230            END-IF                                                 ELUPROLG
01231            ADD +1 TO COF-NBR-DTL-LINES                            ELUPROLG
01232            SET COF-DTL-IDX UP BY 1                                ELUPROLG
01233            MOVE TCAR-OPF-DATA (TCAR-OPF-IDX) (1:WS-SBSQNT-LN-LEN) ELUPROLG
01234              TO COF-DTL-LINE (COF-DTL-IDX) (WS-LN-OFST:)          ELUPROLG
01235            END-PERFORM                                            ELUPROLG
01236      ELSE                                                         ELUPROLG
01237         CONTINUE                                                  ELUPROLG
01238      END-IF.                                                      ELUPROLG
01239                                                                   ELUPROLG
01240 ******************************************************************ELUPROLG
01241 *                                                                *ELUPROLG
01242 *    Call Page Output Subroutine & Reset Critical Parameters     *ELUPROLG
01243 *                                                                *ELUPROLG
01244 ******************************************************************ELUPROLG
01245                                                                   ELUPROLG
01246  9020-CALL-ELUOUTPT.                                              ELUPROLG
01247      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELUPROLG
01248      INITIALIZE COF-NBR-DTL-LINES.                                ELUPROLG
01249      SET COF-DTL-IDX TO COF-NBR-DTL-LINES.                        ELUPROLG
01250      SET COF-CONTINUE TO TRUE.                                    ELUPROLG
01251                                                                   ELUPROLG
01252 ******************************************************************ELUPROLG
01253 *                                                                *ELUPROLG
01254 *    Convert Date From Julian to Gregorian                       *ELUPROLG
01255 *                                                                *ELUPROLG
01256 ******************************************************************ELUPROLG
01257                                                                   ELUPROLG
01258  9030-CNVRT-DT-JLN-GRGRN.                                         ELUPROLG
01259      MOVE 'CNV' TO HGADATE-FUNC.                                  ELUPROLG
01260      MOVE 'J' TO HGADATE-FORM1.                                   ELUPROLG
01261      MOVE 'M' TO HGADATE-FORM2.                                   ELUPROLG
01262      MOVE ZEROS TO HGADATE-RETURN                                 ELUPROLG
01263                    HGADATE-AMOUNT                                 ELUPROLG
01264                    HGADATE-DATE2.                                 ELUPROLG
01265      EXEC CICS  LINK PROGRAM('HGADATES')                          ELUPROLG
01266          COMMAREA(WS-HGADATES-PARMS)                              ELUPROLG
01267          END-EXEC.                                                ELUPROLG
01268                                                                   ELUPROLG
01269  9040-CEN-CNVRT-DT-JLN-GRGRN.                                     ELUPROLG
01270      MOVE 'CNV' TO  MLDATE-FUNC.                                  ELUPROLG
01271      MOVE 'J' TO  MLDATE-FORM1.                                   ELUPROLG
01272      MOVE 'M' TO  MLDATE-FORM2.                                   ELUPROLG
01273      MOVE ZEROS TO  MLDATE-RETURN                                 ELUPROLG
01274                     MLDATE-AMOUNT.                                ELUPROLG
01275      EXEC CICS  LINK PROGRAM('MLDATEC')                           ELUPROLG
01276          COMMAREA(MLDATE01)                                       ELUPROLG
01277          LENGTH  (28)                                             ELUPROLG
01278          END-EXEC.                                                ELUPROLG
01279                                                                   ELUPROLG
