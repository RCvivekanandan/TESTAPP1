00001  IDENTIFICATION DIVISION.                                         03/27/06
00002  PROGRAM-ID.     GBH1PGM.                                         GBH1PGM 
00003  AUTHOR.         JUNE PON.                                           LV002
00004  DATE-WRITTEN.   09/20/2002.                                      GBH1PGM 
00005  DATE-COMPILED.                                                   GBH1PGM 
00006 ******************************************************************GBH1PGM 
00007 *    GBH1PGM   BENEFIT HIGHLIGHTS PROGRAM                        *GBH1PGM 
00008 *                                                                *GBH1PGM 
00009 *    THIS PROGRAM WILL LINK TO GBIFPGM WHICH WILL DETERMINE RULE *GBH1PGM 
00010 *    VALUES AND RETURN THEM IN COPYBOOK GCBENHLC.                *GBH1PGM 
00011 *    THE VALUES IN THE COPYBOOK WILL THEN BE USED TO BUILD       *GBH1PGM 
00012 *    THE TS QUEUE PAGES.                                         *GBH1PGM 
00013 *    THE FIRST TIME THROUGH, THE FIRST TS QUEUE PAGE WILL BE     *GBH1PGM 
00014 *    DISPLAY. THEN THE USER WILL DETERMINE THROUGH PF KEYS       *GBH1PGM 
00015 *    HOW TO PAGE.                                                *GBH1PGM 
00016 *                                                                *GBH1PGM 
00017 *   FUNC CODE: GBH1                                              *GBH1PGM 
00018 *      MAPSET: GBHSSET                                           *GBH1PGM 
00019 * INPUT FILES: GROUP FILE                                        *GBH1PGM 
00020 *              CONTRACT FILE                                     *GBH1PGM 
00021 *              TABULAR FILE                                      *GBH1PGM 
00022 *                                                                *GBH1PGM 
00023 ******************************************************************GBH1PGM 
00024 *                                                                *GBH1PGM 
00025 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GBH1PGM 
00026 *       *-*         U P D A T E   H I S T O R Y         *-*      *GBH1PGM 
00027 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GBH1PGM 
00028 *                                                                *GBH1PGM 
00029 **-CHG-NUM-* *-DATE-* *WHO* *-----DESCRIPTION--------*            GBH1PGM 
00030 *                                                                *GBH1PGM 
00031 *    XXXX    09/20/02   JP  CREATED SKELETON PROGRAM.            *GBH1PGM 
00032 *                                                                 GBH1PGM 
00033 *            01/06/03   JP  INCREASED MINIMUM LINES OF DATA TO   *GBH1PGM 
00034 *                           15.                                   GBH1PGM 
00035 *                                                                 GBH1PGM 
00036 *            03/20/03   JP  ADD UTILIZATION FIELDS.              *GBH1PGM 
00037 *                                                                 GBH1PGM 
00038 *            03/23/04   JP  PROD FIX FOR BAE ALTERNATIVE GROUPS  *GBH1PGM 
00039 *                                                                 GBH1PGM 
00040 *            05/17/04   JP  add test to allow only bae/blueprint *GBH1PGM 
00041 *            03/15/05   JP  change default for fam ded/opx not fd*GBH1PGM 
00042 *            04/28/05   JP  office coin default for oon          *GBH1PGM 
00043 *            05/02/05   JP  DISPLAY BEN PERIOD QUALIFIER         *GBH1PGM 
00044 *            06/07/05   JP  exclude all grps not = ppo/pos       *GBH1PGM 
00045 *            06/15/05   JP  set min lines to 5; min amt for oalt *GBH1PGM 
00046 *                           ($100,000)                            GBH1PGM 
00047 *            06/18/05   JP  LIMIT TO ILL BAE/BLUEPRINT           *GBH1PGM 
00048 *            06/30/05   JP  prod fix - end browse for accum rule *GBH1PGM 
00049 *            07/20/05   JP  change not applic to call for info   *GBH1PGM 
00050 *            07/28/05   JP  add restrictions for display         *GBH1PGM 
00051 *            08/05/05   JP  add imc/relationship code logic      *GBH1PGM 
00052 *            08/06/05   JP  texas msa logic                      *GBH1PGM 
00053 *            08/30/05   JP  EXCLUDE PROV CTL SPLITS              *GBH1PGM 
00054 *            09/12/05   GF  EXCLUDE ALL NON-JUMBO/BAE/BLUEPRNT   *GBH1PGM 
00055 *                           GRPS. COMMENTED OUT PROCESSING FOR   *GBH1PGM 
00056 *                           RULES 4230, 4240, AND 4250 PER USER  *GBH1PGM 
00057 *                           REQUEST.                             *GBH1PGM 
00058 *            09/13/05   GF  comment out processing for rule      *GBH1PGM 
00059 *                           4280.                                *GBH1PGM 
00060 *            09/22/05   GF  ADD CODE TO CALCULATE PERCENTAGE FOR *GBH1PGM 
00061 *                           MSA SANCTION COINSURANCE (RULE 4340) *GBH1PGM 
00062 *            09/23/05   GF  ADD CODE TO DEFAULT OFFICE VISIT     *GBH1PGM 
00063 *                           PAYMT LVL ONLY IF NO HOSP/MED PAYMT  *GBH1PGM 
00064 *                           LVL AND OFFC VISIT COPAY FOUND.      *GBH1PGM 
00065 *            11/03/05   JP  REGEN FOR APPLIC ID CODE             *GBH1PGM 
00066 *            11/11/05   GF  ADD PRODUCT CODE CHECK FOR TX STAND- *GBH1PGM 
00067 *                           ARD GROUPS.                          *GBH1PGM 
00068 *            11/16/05   JP  correct rule 4198 dupl               *GBH1PGM 
00069 *            12/17/05   JP  application ID logic for BAM         *GBH1PGM 
00069 *            04/05/06  AKK  ADD REMAINING RULES FOR M2B, ALREADY *GBH1PGM 
00069 *                           ADDED 4027, 4021, 4087, AND 4088    * GBH1PGM 
00069 *                           and 4081 (no 4081 added here 4080?    GBH1PGM 
00069 *            06/07/06  AKK  add additional pseudo names for     * GBH1PGM 
00069 *                           benefit hightlight sheet.           * GBH1PGM 
00069 *            10/26/06  AKK  add rule for tx hmo 4084, 4037, 4036* GBH1PGM 
00069 *                           4097, and 4098.                     * GBH1PGM 
00069 *            11/27/06  AKK  add rule for OK 4081 and 4116         GBH1PGM 
00069 *                                                               * GBH1PGM 
00069 *            11/27/06  AKK  add rule for OK 4081 and 4116         GBH1PGM 
00069 *                                                               * GBH1PGM 
00069 *            05/17/07  AKK  add code to bypass groups with        GBH1PGM 
00069 *                           vendor ids coded                      GBH1PGM 
00069 *            09/17/07  AKK  add earthgrn to accepted jumbo        GBH1PGM 
00069 *                           groups                                GBH1PGM 
00069 *            02/14/08  AKK  TX69184 and TX69185 for groups        GBH1PGM 
00069 *                           70885 and 70883 and 69185             GBH1PGM 
00069 *            03/26/08  AKK  REGEN FOR GRBJTABC CHANGE             GBH1PGM 
00069 *                                                               * GBH1PGM 
00069 *            05/12/08  AKK  add code to force pgm to display      GBH1PGM 
00069 *                           BHS for groups specified by bam       GBH1PGM 
00069 *                           team                                * GBH1PGM 
00069 *            05/19/08  AKK  REGEN FOR new ccid GCPS00091          GBH1PGM 
00069 *            08/05/09  JG   ADDED STATEFARM FOR BAM.            * GBH1PGM 
00069 *            10/22/09  JG   Removed the inclusion list.Displaying GBH1PGM 
      *                           BHS for all groups with FRL 00 and            
      *                           prv control 00 except HMO groups.             
      *                           Dec rel change.                               
00069 *            07/15/13  JG   Added logic to fix CQ#18535.Par 6100- GBH1PGM 
0 069 *            04/20/15  JG   Added logic to fix incident 189694    GBH1PGM 
jan23 *            01/03/23  bb   recompile for new vendor in grbjtabc  GBH1PGM 
NSK24 * P56703     06/04/24  NSK  RECOMPILE - PEAQ COPYBOOK EXPANSION           
NSK24 *                           COPY ABM, ACP, ACL, ADL, AOL,                 
NSK24 *                           GCCDRLEN                                      
NSK24 *                                                                         
00070 ******************************************************************GBH1PGM 
00071 /                                                                 GBH1PGM 
00072  ENVIRONMENT DIVISION.                                            GBH1PGM 
00073  DATA DIVISION.                                                   GBH1PGM 
00074  WORKING-STORAGE SECTION.                                         GBH1PGM 
00075                                                                   GBH1PGM 
00076  01  WS-DIAGNOSTICS.                                              GBH1PGM 
00077      05  WS-BEGIN                PIC X(20) VALUE                  GBH1PGM 
00078      '**GBH1PGM WS BEGIN**'.                                      GBH1PGM 
00079      05  WS-PARA-ID              PIC X(4)  VALUE 'GBH1'.          GBH1PGM 
00080      05  WS-ABEND-CODE           PIC X(4)  VALUE 'GBH1'.          GBH1PGM 
00081                                                                   GBH1PGM 
00082  01  WS-MISC.                                                     GBH1PGM 
00083      05  WS-100-AFTER-CPY   PIC X(20)                             GBH1PGM 
00084              VALUE '100% AFTER COPAY    '.                        GBH1PGM 
00085      05  WS-100-PERCENT     PIC X(11) VALUE '      100% '.        GBH1PGM 
00086      05  WS-PK-100          PIC s9(03) COMP-3 VALUE +100.         GBH1PGM 
00087      05  WS-PK-PERCENT      PIC s9(03) COMP-3.                    GBH1PGM 
00088      05  WS-ZERO-DOLLARS    PIC X(11) VALUE '      $0.00'.        GBH1PGM 
00089      05  WS-NOT-APPLIC      PIC X(20)                             GBH1PGM 
00090 *            VALUE '   NOT APPLICABLE   '.                        GBH1PGM 
00091              VALUE ' Call for more info.'.                        GBH1PGM 
00092      05  WS-UNLMTD          PIC X(20)                             GBH1PGM 
00093              VALUE '     UNLIMITED      '.                        GBH1PGM 
00094      05  WS-TEST-COPAY.                                           GBH1PGM 
00095          10  WS-TEST-COPAY-8  PIC X(08) VALUE SPACES.             GBH1PGM 
00096          10  WS-TEST-COPAY-3  PIC X(03) VALUE SPACES.             GBH1PGM 
00097              88 WS-COPAY-DOL-AMT  VALUE '.00'.                    GBH1PGM 
00098      05  WS-UTIL-EDIT.                                            GBH1PGM 
00099          10  FILLER            PIC X(03) VALUE SPACES.            GBH1PGM 
00100          10  FILLER            PIC X(01) VALUE '('.               GBH1PGM 
00101          10  WS-UTIL-EDIT-11   PIC X(11) VALUE SPACES.            GBH1PGM 
00102          10  FILLER            PIC X(01) VALUE ')'.               GBH1PGM 
00103          10  FILLER            PIC X(04) VALUE SPACES.            GBH1PGM 
00104      05  WS-COPAY-EDIT.                                           GBH1PGM 
00105          10  WS-COPAY-EDIT-8    PIC X(08) VALUE SPACES.           GBH1PGM 
00106          10  WS-COPAY-EDIT-12   PIC X(12) VALUE ' copayment  '.   GBH1PGM 
00107      05  WS-TEST-PERC.                                            GBH1PGM 
00108          10  FILLER           PIC X(06) VALUE SPACES.             GBH1PGM 
00109          10  WS-PERC-VAL.                                         GBH1PGM 
00110              15  WS-PERC-VAL-3    PIC X(03) VALUE SPACES.         GBH1PGM 
00111              15  WS-PERC-VAL-1    PIC X(01) VALUE SPACES.         GBH1PGM 
00112                  88 WS-PERCENT-SIGN   VALUE '%'.                  GBH1PGM 
00113          10  FILLER           PIC X(01) VALUE SPACES.             GBH1PGM 
00114      05  WS-PERC-EDIT.                                            GBH1PGM 
00115          10  FILLER             PIC X(02) VALUE SPACES.           GBH1PGM 
00116          10  WS-PERC-EDIT-4     PIC X(04) VALUE SPACES.           GBH1PGM 
00117          10  WS-PERC-EDIT-12    PIC X(12) VALUE ' after copay'.   GBH1PGM 
00118          10  FILLER             PIC X(02) VALUE SPACES.           GBH1PGM 
00119      05  WS-DISP-TIME-QUAL.                                       GBH1PGM 
00120          10  FILLER               PIC X(04) VALUE 'PER '.         GBH1PGM 
00121          10  WS-BEN-PER-STD       PIC X(16) VALUE SPACES.         GBH1PGM 
00122          10  WS-BEN-PER-TQ REDEFINES                              GBH1PGM 
00123              WS-BEN-PER-STD.                                      GBH1PGM 
00124              15  WS-BEN-PER-UNPACK-3  PIC Z(03).                  GBH1PGM 
00125              15  WS-TIME-QUAL         PIC X(13).                  GBH1PGM 
00126      05  WS-BEN-PERIOD        PIC X(02) VALUE ZEROS.              GBH1PGM 
00127      05  WS-BEN-PER-TIME-QUAL PIC X(01) VALUE ZEROS.              GBH1PGM 
00128                                                                   GBH1PGM 
00129                                                                   GBH1PGM 
00130 ** WORKFIELDS AND SWITCHES **                                     GBH1PGM 
00131  01  WS-WORK-FIELDS.                                              GBH1PGM 
00132      05  WS-SERV-DT-CEN           COMP-3  PIC S9(7).              GBH1PGM 
00133      05  WS-ERROR-FOUND-SW       PIC X     VALUE 'N'.             GBH1PGM 
00134          88  ERROR-FD                      VALUE 'Y'.             GBH1PGM 
00135      05  WS-RECORD-FOUND-SW      PIC X     VALUE 'N'.             GBH1PGM 
00136          88  RECORD-FOUND                  VALUE 'Y'.             GBH1PGM 
00137          88  RECORD-NOT-FD                 VALUE 'N'.             GBH1PGM 
LOB123     05  WS-RECORD-FOUND-LOB1-SW PIC X     VALUE 'N'.             GBH1PGM 
LOB123         88  RECORD-FOUND-LOB1             VALUE 'Y'.             GBH1PGM 
LOB123         88  RECORD-NOT-FD-LOB1            VALUE 'N'.             GBH1PGM 
LOB123     05  WS-RECORD-FOUND-LOB2-SW PIC X     VALUE 'N'.             GBH1PGM 
LOB123         88  RECORD-FOUND-LOB2             VALUE 'Y'.             GBH1PGM 
LOB123         88  RECORD-NOT-FD-LOB2            VALUE 'N'.             GBH1PGM 
LOB123     05  WS-RECORD-FOUND-LOB3-SW PIC X     VALUE 'N'.             GBH1PGM 
LOB123         88  RECORD-FOUND-LOB3             VALUE 'Y'.             GBH1PGM 
LOB123         88  RECORD-NOT-FD-LOB3            VALUE 'N'.             GBH1PGM 
00138      05  WS-LOB1-EXISTS-SW       PIC X     VALUE 'N'.             GBH1PGM 
00139          88  LOB1-EXISTS                   VALUE 'Y'.             GBH1PGM 
00138      05  WS-LOB2-EXISTS-SW       PIC X     VALUE 'N'.             GBH1PGM 
00139          88  LOB2-EXISTS                   VALUE 'Y'.             GBH1PGM 
00138      05  WS-LOB3-EXISTS-SW       PIC X     VALUE 'N'.             GBH1PGM 
00139          88  LOB3-EXISTS                   VALUE 'Y'.             GBH1PGM 
00138      05  WS-LOB4-EXISTS-SW       PIC X     VALUE 'N'.             GBH1PGM 
00139          88  LOB4-EXISTS                   VALUE 'Y'.             GBH1PGM 
00138      05  WS-END-OF-READ-SW       PIC X     VALUE 'N'.             GBH1PGM 
00139          88  END-OF-READ                   VALUE 'Y'.             GBH1PGM 
00140      05  WS-EDIT-2ND-VAL-IN-SW     PIC X     VALUE 'N'.           GBH1PGM 
00141          88  EDIT-2ND-VAL-IN                 VALUE 'Y'.           GBH1PGM 
00142      05  WS-EDIT-2ND-VAL-OUT-SW    PIC X     VALUE 'N'.           GBH1PGM 
00143          88  EDIT-2ND-VAL-OUT                VALUE 'Y'.           GBH1PGM 
00144      05  WS-SAVE-VAL-QUAL          PIC X(01) VALUE SPACES.        GBH1PGM 
00145      05  WS-AGE-FIELD.                                            GBH1PGM 
00146          10  FILLER           PIC X(7)  VALUE SPACES.             GBH1PGM 
00147          10  WS-AGE           PIC Z(2)9 VALUE ZEROS.              GBH1PGM 
00148      05  WS-HOLD-WAITING-PERD-LINE.                               GBH1PGM 
00149          10  FILLER               PIC X(03) VALUE SPACES.         GBH1PGM 
00150          10  WS-HOLD-WPL-DESC     PIC X(15) VALUE                 GBH1PGM 
00151                    'WAITING PERIODS'.                             GBH1PGM 
00152          10  FILLER               PIC X(05) VALUE SPACES.         GBH1PGM 
00153          10  FILLER               PIC X(08) VALUE 'MEMBER: '.     GBH1PGM 
00154          10  WS-WPL-MEM-DAYS      PIC X(03) VALUE SPACES.         GBH1PGM 
00155          10  FILLER               PIC X(05) VALUE SPACES.         GBH1PGM 
00156          10  FILLER               PIC X(08) VALUE 'SPOUSE: '.     GBH1PGM 
00157          10  WS-WPL-SPS-DAYS      PIC X(03) VALUE SPACES.         GBH1PGM 
00158          10  FILLER               PIC X(05) VALUE SPACES.         GBH1PGM 
00159          10  FILLER               PIC X(11) VALUE 'DEPENDENT: '.  GBH1PGM 
00160          10  WS-WPL-DEP-DAYS      PIC X(03) VALUE SPACES.         GBH1PGM 
00161          10  FILLER               PIC X(10) VALUE SPACES.         GBH1PGM 
00162      05  WS-SYSID.                                                GBH1PGM 
00163          10  FILLER                           PIC X(01).          GBH1PGM 
00164              88  WS-TEXAS-CICS-REGION         VALUE 'X'.          GBH1PGM 
00165          10  FILLER                           PIC X(03).          GBH1PGM 
00166      05  WS-BASIC-ACCUMS.                                         GBH1PGM 
00167          10  WS-COPAY-FD-SW      PIC X(01) VALUE 'N'.             GBH1PGM 
00168          10  WS-DEDL-FD-SW       PIC X(01) VALUE 'N'.             GBH1PGM 
00169          10  WS-OPX-FD-SW        PIC X(01) VALUE 'N'.             GBH1PGM 
00170          10  WS-COIN-FD-SW       PIC X(01) VALUE 'N'.             GBH1PGM 
00171                                                                   GBH1PGM 
00172      05  WS-PROCESS-CON-GRP-SW   PIC X     VALUE SPACE.           GBH1PGM 
00173          88  WS-PROCESS-CON                VALUE 'C'.             GBH1PGM 
00174          88  WS-PROCESS-GRP                VALUE 'G'.             GBH1PGM 
00175      05  WS-GRP-ABM-FOUND-SW     PIC X     VALUE 'N'.             GBH1PGM 
00176          88  WS-GRP-ABM-FOUND              VALUE 'Y'.             GBH1PGM 
00177      05  WS-GRP-ACL-FOUND-SW     PIC X     VALUE 'N'.             GBH1PGM 
00178          88  WS-GRP-ACL-FOUND              VALUE 'Y'.             GBH1PGM 
00179      05  WS-GRP-ACP-FOUND-SW     PIC X     VALUE 'N'.             GBH1PGM 
00180          88  WS-GRP-ACP-NOT-FD             VALUE 'N'.             GBH1PGM 
00181          88  WS-GRP-ACP-FOUND              VALUE 'Y'.             GBH1PGM 
00182      05  WS-GRP-ADL-FOUND-SW     PIC X     VALUE 'N'.             GBH1PGM 
00183          88  WS-GRP-ADL-FOUND              VALUE 'Y'.             GBH1PGM 
00184      05  WS-GRP-AOL-FOUND-SW     PIC X     VALUE 'N'.             GBH1PGM 
00185          88  WS-GRP-AOL-FOUND              VALUE 'Y'.             GBH1PGM 
00186      05  WS-CON-ABM-FOUND-SW     PIC X     VALUE 'N'.             GBH1PGM 
00187          88  WS-CON-ABM-FOUND              VALUE 'Y'.             GBH1PGM 
00188      05  WS-CON-ACL-FOUND-SW     PIC X     VALUE 'N'.             GBH1PGM 
00189          88  WS-CON-ACL-FOUND              VALUE 'Y'.             GBH1PGM 
00190      05  WS-CON-ACP-FOUND-SW     PIC X     VALUE 'N'.             GBH1PGM 
00191          88  WS-CON-ACP-NOT-FD             VALUE 'N'.             GBH1PGM 
00192          88  WS-CON-ACP-FOUND              VALUE 'Y'.             GBH1PGM 
00193      05  WS-CON-ADL-FOUND-SW     PIC X     VALUE 'N'.             GBH1PGM 
00194          88  WS-CON-ADL-FOUND              VALUE 'Y'.             GBH1PGM 
00195      05  WS-CON-AOL-FOUND-SW     PIC X     VALUE 'N'.             GBH1PGM 
00196          88  WS-CON-AOL-FOUND              VALUE 'Y'.             GBH1PGM 
00197      05  WS-RULE-DONE-SW         PIC X     VALUE 'N'.             GBH1PGM 
00198          88  RULE-DONE                     VALUE 'Y'.             GBH1PGM 
00199          88  RULE-NOT-DONE                 VALUE 'N'.             GBH1PGM 
00200      05  WS-ACCUM-ID-FD-SW       PIC X     VALUE 'N'.             GBH1PGM 
00201          88  ACCUM-ID-FD                   VALUE 'Y'.             GBH1PGM 
00202          88  ACCUM-ID-NOT-FD               VALUE 'N'.             GBH1PGM 
00203      05  WS-RULE-FD-SW           PIC X     VALUE 'N'.             GBH1PGM 
00204          88  RULE-FD                       VALUE 'Y'.             GBH1PGM 
00205          88  RULE-NOT-FD                   VALUE 'N'.             GBH1PGM 
00206      05  WS-OVERALL-FD-SW        PIC X     VALUE 'N'.             GBH1PGM 
00207          88  OVERALL-FD                    VALUE 'Y'.             GBH1PGM 
00208          88  OVERALL-NOT-FD                VALUE 'N'.             GBH1PGM 
00209      05  WS-IN-NET-FD-SW         PIC X     VALUE 'N'.             GBH1PGM 
00210          88  IN-NET-FD                     VALUE 'Y'.             GBH1PGM 
00211          88  IN-NET-NOT-FD                 VALUE 'N'.             GBH1PGM 
00212      05  WS-OUT-NET-FD-SW        PIC X     VALUE 'N'.             GBH1PGM 
00213          88  OUT-NET-FD                    VALUE 'Y'.             GBH1PGM 
00214          88  OUT-NET-NOT-FD                VALUE 'N'.             GBH1PGM 
00215      05  WS-OFFC-LVL-IN-FD       PIC X     VALUE 'N'.             GBH1PGM 
00216          88  OFFC-LVL-IN-FD                VALUE 'Y'.             GBH1PGM 
00217      05  WS-OFFC-COPAY-FD        PIC X     VALUE 'N'.             GBH1PGM 
00218          88  OFFC-COPAY-FD                 VALUE 'Y'.             GBH1PGM 
00219      05  WS-HOSP-MED-FD          PIC X     VALUE 'N'.             GBH1PGM 
00220          88  HOSP-MED-FD                   VALUE 'Y'.             GBH1PGM 
00221      05  WS-SKIP-RULE-SW         PIC X     VALUE 'N'.             GBH1PGM 
00222          88  SKIP-RULE                     VALUE 'Y'.             GBH1PGM 
00223      05  WS-PROD-TYPE-OK-SW      PIC X     VALUE 'N'.             GBH1PGM 
00224          88  PROD-TYPE-OK                  VALUE 'Y'.             GBH1PGM 
00225      05  WS-HMO-GRP-SW           PIC X     VALUE 'N'.             GBH1PGM 
00226          88  NOT-HMO-GRP                   VALUE 'N'.             GBH1PGM 
00227          88  HMO-GRP                       VALUE 'Y'.             GBH1PGM 
00228      05  WS-OCCUR-OK-SW          PIC X     VALUE 'N'.             GBH1PGM 
00229          88  OCCUR-OK                      VALUE 'Y'.             GBH1PGM 
00230                                                                   GBH1PGM 
00231      05  WS-PSEUDO-GRP-NBR       PIC X(09).                       GBH1PGM 
00232          88 JUMBO-PG-GRP         VALUE                            GBH1PGM 
00233          'AANDERSON' 'A D M    ' 'AMERITECH' 'AONCORP  '          GBH1PGM 
00234          'ANDFINACL' 'AUTOZONE ' 'CARPENTER' 'CHI      '          GBH1PGM 
00235          'CHARTERCM' 'CTRANSITA' 'CITY CHG ' 'EIT      '          GBH1PGM 
00236          'COMMONWLT' 'F RES BNK' 'G PACIFIC' 'HEWITT   '          GBH1PGM 
00237          'HOUSE INT' 'ILLPOWERS' 'I T W    ' 'INGERSOLL'          GBH1PGM 
00238          'JOCONIFM ' 'KRAFT    ' 'MCDONALDS' 'NORTHROP '          GBH1PGM 
00239          'PEABDYCOM' 'ROBBOSCH ' 'UNITEDDOM' 'UNITEDAIR'          GBH1PGM 
00240          'UPSERVICE' 'WALMART  ' 'STOFTEXAS' 'TEACHERS '          GBH1PGM 
00241          'TX71778  ' 'HEBUTTS  ' 'TX69184  ' 'TXUTILITY'          GBH1PGM 
00242          'TX58473  ' 'CONTAIRLN' 'TX80946  ' 'TXAMUNIVS'          GBH1PGM 
00243          'HALIBURTN' 'TX05901  ' 'SUIZA    ' 'BRINKER  '          GBH1PGM 
00244          'TX090189 ' 'MCLANE   ' 'TX81566  ' 'OLDCASTLE'          GBH1PGM 
00245          'AMERONOGY' 'LENNOXINT' 'MICHAELS ' 'TX66252  '          GBH1PGM 
00246          'FINAINCOR' 'BROOKSHIR' 'NORTHEAST' 'TX19500  '          GBH1PGM 
00247          'DRESSER  ' 'TX80189  ' 'TX87372  ' 'TX84126  '          GBH1PGM 
00248          'EXPRESJET' 'CENTEX   ' 'BUILDERSF' 'STLUKEHOS'          GBH1PGM 
00249          'SAFETYKLE' 'BMCSOFTWR' 'VEOLIAH20' 'LUBBCKISD'          GBH1PGM 
00250          'TX81472  ' 'TEXWDIND ' 'TX21518  ' 'TRANSOCEA'          GBH1PGM 
00251          'PATTERSON' '92042AUTO' 'COMPUSYST' 'ALCALTEL '          GBH1PGM 
00252          'LAMARCISD' 'DIAMOFFSH' 'TX088284 ' 'REXEL    '          GBH1PGM 
00253          'BENCHELEC' 'TX81510  ' 'TX80950  ' 'TX82265  '          GBH1PGM 
00254          'TX80897  ' 'BAYLORUNI' 'CULLFROST' 'OVERHEAD '          GBH1PGM 
00255          'SOUMETHUN' 'LUFKININD' 'LIFECARE ' 'EWSCRIPS '          GBH1PGM 
00256          'TX89928  ' 'TX88856  ' 'MMIPROD  ' 'FOXWORTH '          GBH1PGM 
00257          'ASSISTLIV' 'FROZENFOD' 'TX093556 ' 'TX82785  '          GBH1PGM 
00258          'HORIZON  ' 'RADIOLOGX' 'ELPELECTR' 'TX22938  '          GBH1PGM 
00259          'CHROMALL ' 'PIPETYLER' 'CHEMILIME' 'NUCORCORP'          GBH1PGM 
00260          'IMCO     ' 'CCI92121 ' 'TX093474 ' 'SMETHRETS'          GBH1PGM 
00261          'SANANTWAT' 'TX085038 ' 'TX092405 ' 'SWAIRLINE'          GBH1PGM 
00261          'SUIZA    ' 'CONAGRA  ' 'TX092405 ' 'BCBSASSOC'          GBH1PGM 
00261          'VICASCINC' 'AFHC70882'.                                 GBH1PGM 
                                                                                
00231      05  WS-PSEUDO-BAM-GRP-NBR       PIC X(09).                   GBH1PGM 
00264          88 BAM-BHS-GRP          VALUE                            GBH1PGM 
                           'HCSC     ' 'BAX22258 ' 'STATEFARM'.                 
                                                                                
    2                                                                   GBH1PGM 
00263      05  WS-PRODUCT-TYPE         PIC X(05).                       GBH1PGM 
00264          88 BAE-BLUEPRNT         VALUE                            GBH1PGM 
00265          'BAE00' 'BAEPP' 'BLALT' 'BLPR0' 'BLVAL'                  GBH1PGM 
00266          'BLEDG' 'BLDEC' 'BLCHS'.                                 GBH1PGM 
00267          88 TX-STANDARD          VALUE                            GBH1PGM 
00268          'BLUE0' 'PPO00'.                                         GBH1PGM 
00269                                                                   GBH1PGM 
00270      05  WS-WRK-VAL-1    COMP-3  PIC S9(7)V99 VALUE ZEROS.        GBH1PGM 
00271      05  WS-WRK-VAL-2    COMP-3  PIC S9(7)V99 VALUE ZEROS.        GBH1PGM 
00272      05  WS-WRK-VAL-DISP         PIC  Z(7)9.99 VALUE ZEROS.       GBH1PGM 
00273      05  WS-WRK-VAL-BUX       REDEFINES WS-WRK-VAL-DISP           GBH1PGM 
00274                                  PIC  $(7)9.99.                   GBH1PGM 
00275      05  WS-SAVE-IADD    COMP-3  PIC S9(7)V99 VALUE ZEROS.        GBH1PGM 
00276      05  WS-SAVE-FADD    COMP-3  PIC S9(7)V99 VALUE ZEROS.        GBH1PGM 
00277      05  WS-HMSA-SAVE-VAL        PIC X(20) VALUE SPACES.          GBH1PGM 
00278      05  WS-HMSI-SAVE-VAL        PIC X(20) VALUE SPACES.          GBH1PGM 
00279      05  WS-HMSO-SAVE-VAL        PIC X(20) VALUE SPACES.          GBH1PGM 
00280      05  WS-HMSA-SAVE-POT        PIC X(02) VALUE SPACES.          GBH1PGM 
00281      05  WS-HMSI-SAVE-POT        PIC X(02) VALUE SPACES.          GBH1PGM 
00282      05  WS-HMSO-SAVE-POT        PIC X(02) VALUE SPACES.          GBH1PGM 
00283      05  WS-MSPA-SAVE-VAL        PIC X(20) VALUE SPACES.          GBH1PGM 
00284      05  WS-MSPI-SAVE-VAL        PIC X(20) VALUE SPACES.          GBH1PGM 
00285      05  WS-MSPO-SAVE-VAL        PIC X(20) VALUE SPACES.          GBH1PGM 
00286      05  WS-MSPA-SAVE-POT        PIC X(02) VALUE SPACES.          GBH1PGM 
00287      05  WS-MSPI-SAVE-POT        PIC X(02) VALUE SPACES.          GBH1PGM 
00288      05  WS-MSPO-SAVE-POT        PIC X(02) VALUE SPACES.          GBH1PGM 
00289                                                                   GBH1PGM 
00290      05  WS-VALUE-LIMIT          PIC  Z(7)9.99 VALUE ZEROS.       GBH1PGM 
00291      05  WS-VALUE-LIMIT-S     REDEFINES WS-VALUE-LIMIT            GBH1PGM 
00292                                  PIC  $(7)9.99.                   GBH1PGM 
00293                                                                   GBH1PGM 
00294      05  WS-VALUE-LIMIT-MIL    PIC  $Z,ZZZ,ZZ9.                   GBH1PGM 
00295                                                                   GBH1PGM 
00296      05  WS-VALUE-LIMIT-V        PIC  Z(7)9V99 VALUE ZEROS.       GBH1PGM 
00297      05  WS-VALUE-LIMIT-FULL  REDEFINES WS-VALUE-LIMIT-V          GBH1PGM 
00298                                  PIC  Z(9)9.                      GBH1PGM 
00299      05  WS-TEMP-VALUE-LIMIT  PIC  X(11) VALUE SPACES.            GBH1PGM 
00300                                                                   GBH1PGM 
00301      05  WS-COINS-PERCENT-LEVEL-1 COMP-3 PIC S9(3) VALUE ZEROS.   GBH1PGM 
00302      05  WS-COINS-PERCENT-LEVEL-2 COMP-3 PIC S9(3) VALUE ZEROS.   GBH1PGM 
00303                                                                   GBH1PGM 
00304      05  WS-PERCENT.                                              GBH1PGM 
00305          10  FILLER           PIC X(6)  VALUE SPACES.             GBH1PGM 
00306          10  WS-PERCENT-VAL   PIC Z(2)9 VALUE ZEROS.              GBH1PGM 
00307          10  WS-PERCENT-SIGN  PIC X     VALUE '%'.                GBH1PGM 
00308      05  WS-PEOPLE.                                               GBH1PGM 
00309          10  FILLER           PIC X(01) VALUE SPACES.             GBH1PGM 
00310          10  WS-PEOPLE-VAL    PIC Z(2)9 VALUE ZEROS.              GBH1PGM 
00311          10  FILLER           PIC X(07) VALUE ' PEOPLE'.          GBH1PGM 
00312      05  WS-VISITS.                                               GBH1PGM 
00313          10  WS-VISITS-VAL    PIC Z(3)9 VALUE ZEROS.              GBH1PGM 
00314          10  FILLER           PIC X(07) VALUE ' VISITS'.          GBH1PGM 
00315      05  WS-VISITS-1.                                             GBH1PGM 
00316          10  WS-VISITS-VAL-1  PIC Z(3)9 VALUE ZEROS.              GBH1PGM 
00317          10  FILLER           PIC X(07) VALUE '  VISIT'.          GBH1PGM 
00318      05  WS-UNITS.                                                GBH1PGM 
00319          10  WS-UNITS-VAL     PIC Z(3)9 VALUE ZEROS.              GBH1PGM 
00320          10  FILLER           PIC X(07) VALUE '  UNITS'.          GBH1PGM 
00321      05  WS-UNITS-1.                                              GBH1PGM 
00322          10  WS-UNITS-VAL-1  PIC Z(3)9 VALUE ZEROS.               GBH1PGM 
00323          10  FILLER           PIC X(07) VALUE '   UNIT'.          GBH1PGM 
00324      05  WS-HOURS.                                                GBH1PGM 
00325          10  WS-HOURS-VAL     PIC Z(3)9 VALUE ZEROS.              GBH1PGM 
00326          10  FILLER           PIC X(07) VALUE '  HOURS'.          GBH1PGM 
00327      05  WS-DAYS.                                                 GBH1PGM 
00328          10  WS-DAYS-VAL      PIC Z(5)9 VALUE ZEROS.              GBH1PGM 
00329          10  FILLER           PIC X(05) VALUE ' DAYS'.            GBH1PGM 
00330 *MQ 10/03                                                         GBH1PGM 
00331      05  WS-CONFINEMENTS.                                         GBH1PGM 
00332          10  WS-CONFINEMENTS-VAL  PIC Z(5)9 VALUE ZEROS.          GBH1PGM 
00333          10  FILLER               PIC X(13) VALUE ' CONFINEMENTS'.GBH1PGM 
00334      05  WS-AGE               PIC 9(03) VALUE ZEROS.              GBH1PGM 
00335                                                                   GBH1PGM 
00336      05  WS-VALUE-LIMIT-DEC   PIC S9(7)V99 VALUE ZEROS.           GBH1PGM 
00337      05  WS-VALUE-LIM REDEFINES WS-VALUE-LIMIT-DEC.               GBH1PGM 
00338          10  WS-VALUE-LIM-1-6 PIC S9(6).                          GBH1PGM 
00339          10  WS-VALUE-LIM-5-7 PIC  9(3).                          GBH1PGM 
00340                                                                   GBH1PGM 
00341      05  WS-HOLD-SECTION-LINE.                                    GBH1PGM 
00342          10  WS-HOLD-SECTION-IND  PIC X(03) VALUE SPACES.         GBH1PGM 
00343          10  FILLER               PIC X(76) VALUE SPACES.         GBH1PGM 
00344      05  WS-RULE              PIC X(06) VALUE SPACES.             GBH1PGM 
00345      05  WS-ACCUM-ID-CT       PIC 9(02) VALUE ZEROS.              GBH1PGM 
00346      05  WS-VAL-QUAL          PIC X(01).                          GBH1PGM 
00347      05  WS-VAL-LMT       COMP-3  PIC S9(7)V99.                   GBH1PGM 
00348      05  WS-REQD-ACCUM-ERROR.                                     GBH1PGM 
00349          10  FILLER               PIC X(42) VALUE                 GBH1PGM 
00350          'REQUIRED ACCUM IDS NOT CODED ON CONTRACT- '.            GBH1PGM 
00351          10  WS-REQD-ACCUMS       PIC X(04).                      GBH1PGM 
00352      05  WS-SAVE-RELAT        PIC X(2)  VALUE SPACES.             GBH1PGM 
00353      05  WS-TEST-PRODUCT-TYPE.                                    GBH1PGM 
00354          10 WS-TEST-PRODUCT-TYPE-4  PIC X(04).                    GBH1PGM 
00355             88 BAE-BLUEPRINT    VALUE  'BAE0'                     GBH1PGM 
00356                                        'BLPR'.                    GBH1PGM 
00357          10 FILLER                  PIC X(05).                    GBH1PGM 
00358      05  WS-POT-TEST                PIC X(02).                    GBH1PGM 
00359             88 POT-OFFICE       VALUE                             GBH1PGM 
00360      '0A' '0B' '0E' '0H' '0J' '0N' '0P' '0Q' '0R'                 GBH1PGM 
00361      '0U' '01' '04' '06' '13'.                                    GBH1PGM 
00362      05  WS-IMC-CODE.                                             GBH1PGM 
00363          10 WS-IMC-SUB       PIC X(01).                           GBH1PGM 
00364          10 WS-IMC-SPS       PIC X(01).                           GBH1PGM 
00365             88  IMC-NO-SPS       VALUE  '0'.                      GBH1PGM 
00366             88  IMC-SPS          VALUE  '1' '2' '3' '4'.          GBH1PGM 
00367          10 WS-IMC-DEP       PIC X(01).                           GBH1PGM 
00368             88  IMC-NO-DEP       VALUE  '0'.                      GBH1PGM 
00369             88  IMC-DEP          VALUE  '1' '2'.                  GBH1PGM 
                  88  IMC-ONE-DEP      VALUE  '1'.                      0000000 
                  88  IMC-MULT-DEP     VALUE  '2'.                      0000000 
00370                                                                   GBH1PGM 
00371  01  WS-DESC-LINES.                                               GBH1PGM 
00372      05  WS-RULE-DUMMY-DESC      PIC X(50)                        GBH1PGM 
00373       VALUE '12345678901234567890123456789012345678901234567890'. GBH1PGM 
00374      05  WS-RULE-4040-DESC-1     PIC X(50)                        GBH1PGM 
00375       VALUE 'The maximum amount of money you pay toward covered'. GBH1PGM 
00376      05  WS-RULE-4040-DESC-2     PIC X(50)                        GBH1PGM 
00377       VALUE ' hospital and medical expenses during any one cale'. GBH1PGM 
00378      05  WS-RULE-4040-DESC-3     PIC X(50)                        GBH1PGM 
00379       VALUE 'ndar year. This amount does not include the deduct'. GBH1PGM 
00380      05  WS-RULE-4040-DESC-4     PIC X(50)                        GBH1PGM 
00381       VALUE 'ible. Other charges are also excluded. See your be'. GBH1PGM 
00382      05  WS-RULE-4040-DESC-5     PIC X(50)                        GBH1PGM 
00383       VALUE 'nefit booklet for more details.                   '. GBH1PGM 
00384                                                                   GBH1PGM 
00385                                                                   GBH1PGM 
00386                                                                   GBH1PGM 
00387                                                                   GBH1PGM 
00388 ******************************************************************GBH1PGM 
00389 *    WT-01   MESSAGE TABLE                                       *GBH1PGM 
00390 ******************************************************************GBH1PGM 
00391  01  FILLER.                                                      GBH1PGM 
00392      05  WS-01-MESSAGE-VALUES.                                    GBH1PGM 
00393                                                                   GBH1PGM 
00394 *----------------------------------------------------------------*GBH1PGM 
00395          10  WS-ERR-ENTRY-O1.                                     GBH1PGM 
00396              15  WS-ERR-CODE-01             PIC X(2)  VALUE '01'. GBH1PGM 
00397              15  WS-ERR-MESSAGE-01.                               GBH1PGM 
00398                  20  FILLER          PIC X(60) VALUE              GBH1PGM 
00399                ' GROUP/SECTION IS NOT FOUND ON GROUP MASTER FILE  GBH1PGM 
00400 -'          '.                                                    GBH1PGM 
00401 *----------------------------------------------------------------*GBH1PGM 
00402          10  WS-ERR-ENTRY-O2.                                     GBH1PGM 
00403              15  WS-ERR-CODE-02             PIC X(2)  VALUE '02'. GBH1PGM 
00404              15  WS-ERR-MESSAGE-02.                               GBH1PGM 
00405                  20  FILLER          PIC X(60) VALUE              GBH1PGM 
00406                ' ERROR CONVERTING DATE OF SERVICE                 GBH1PGM 
00407 -'          '.                                                    GBH1PGM 
00408 *----------------------------------------------------------------*GBH1PGM 
00409          10  WS-ERR-ENTRY-O3.                                     GBH1PGM 
00410              15  WS-ERR-CODE-03             PIC X(2)  VALUE '03'. GBH1PGM 
00411              15  WS-ERR-MESSAGE-03.                               GBH1PGM 
00412                  20  FILLER          PIC X(60) VALUE              GBH1PGM 
00413                ' CONTRACT RECORD NOT FOUND                        GBH1PGM 
00414 -'          '.                                                    GBH1PGM 
00415 *----------------------------------------------------------------*GBH1PGM 
00416          10  WS-ERR-ENTRY-O4.                                     GBH1PGM 
00417              15  WS-ERR-CODE-04             PIC X(2)  VALUE '04'. GBH1PGM 
00418              15  WS-ERR-MESSAGE-04.                               GBH1PGM 
00419                  20  FILLER          PIC X(60) VALUE              GBH1PGM 
00420                ' GROUP SPECIFIC RECORD NOT FOUND                  GBH1PGM 
00421 -'          '.                                                    GBH1PGM 
00422 *----------------------------------------------------------------*GBH1PGM 
00423          10  WS-ERR-ENTRY-O5.                                     GBH1PGM 
00424              15  WS-ERR-CODE-05             PIC X(2)  VALUE '05'. GBH1PGM 
00425              15  WS-ERR-MESSAGE-05.                               GBH1PGM 
00426                  20  FILLER          PIC X(60) VALUE              GBH1PGM 
00427                ' INSUFFICIENT LINES OF DATA RETURNED              GBH1PGM 
00428 -'          '.                                                    GBH1PGM 
00429 *----------------------------------------------------------------*GBH1PGM 
00430          10  WS-ERR-ENTRY-O6.                                     GBH1PGM 
00431              15  WS-ERR-CODE-06             PIC X(2)  VALUE '06'. GBH1PGM 
00432              15  WS-ERR-MESSAGE-06.                               GBH1PGM 
00433                  20  FILLER          PIC X(60) VALUE              GBH1PGM 
00434                ' INTER-RELATIONAL CODES EQUAL ZEROES - GROUP NOT AGBH1PGM 
00435 -'CTIVE     '.                                                    GBH1PGM 
00436 *----------------------------------------------------------------*GBH1PGM 
00437          10  WS-ERR-ENTRY-O7.                                     GBH1PGM 
00438              15  WS-ERR-CODE-07             PIC X(2)  VALUE '07'. GBH1PGM 
00439              15  WS-ERR-MESSAGE-07.                               GBH1PGM 
00440                  20  FILLER          PIC X(60) VALUE              GBH1PGM 
00441                ' PROGRAM NOT AVAILABLE IN TEXAS REGION AT THIS TIMGBH1PGM 
00442 -'E         '.                                                    GBH1PGM 
00443 *----------------------------------------------------------------*GBH1PGM 
00444          10  WS-ERR-ENTRY-O8.                                     GBH1PGM 
00445              15  WS-ERR-CODE-08             PIC X(2)  VALUE '08'. GBH1PGM 
00446              15  WS-ERR-MESSAGE-08.                               GBH1PGM 
00447                  20  FILLER          PIC X(60) VALUE              GBH1PGM 
00448                ' GROUP/SECTION NOT BAE/BLUEPRINT OR JUMBO         GBH1PGM 
00449 -'          '.                                                    GBH1PGM 
00450 *----------------------------------------------------------------*GBH1PGM 
00451          10  WS-ERR-ENTRY-O9.                                     GBH1PGM 
00452              15  WS-ERR-CODE-09             PIC X(2)  VALUE '09'. GBH1PGM 
00453              15  WS-ERR-MESSAGE-09.                               GBH1PGM 
00454                  20  FILLER          PIC X(60) VALUE              GBH1PGM 
00455                ' REQUIRED ACCUM IDS NOT CODED ON CONTRACT         GBH1PGM 
00456 -'          '.                                                    GBH1PGM 
00457 *----------------------------------------------------------------*GBH1PGM 
00458          10  WS-ERR-ENTRY-10.                                     GBH1PGM 
00459              15  WS-ERR-CODE-10             PIC X(2)  VALUE '10'. GBH1PGM 
00460              15  WS-ERR-MESSAGE-10.                               GBH1PGM 
00461                  20  FILLER          PIC X(60) VALUE              GBH1PGM 
00462                'PRODUCT TYPE NOT VALID FOR BHS                    GBH1PGM 
00463 -'          '.                                                    GBH1PGM 
00464                                                                   GBH1PGM 
00465                                                                   GBH1PGM 
00466 *----------------------------------------------------------------*GBH1PGM 
00467          10  WS-ERR-ENTRY-11.                                     GBH1PGM 
00468              15  WS-ERR-CODE-11             PIC X(2)  VALUE '11'. GBH1PGM 
00469              15  WS-ERR-MESSAGE-11.                               GBH1PGM 
00470                  20  FILLER          PIC X(60) VALUE              GBH1PGM 
00471                'CONTRACT HAS PROVIDER CONTROL SPLITS              GBH1PGM 
00472 -'          '.                                                    GBH1PGM 
00473                                                                   GBH1PGM 
00474                                                                   GBH1PGM 
00475 *----------------------------------------------------------------*GBH1PGM 
00476          10  WS-ERR-ENTRY-12.                                     GBH1PGM 
00477              15  WS-ERR-CODE-12             PIC X(2)  VALUE '12'. GBH1PGM 
00478              15  WS-ERR-MESSAGE-12.                               GBH1PGM 
00479                  20  FILLER          PIC X(60) VALUE              GBH1PGM 
00480                'GROUP NOT APPROVED FOR BAM DISPLAY                GBH1PGM 
00481 -'          '.                                                    GBH1PGM 
00482                                                                   GBH1PGM 
00483                                                                   GBH1PGM 
00484 *----------------------------------------------------------------*GBH1PGM 
00485          10  WS-ERR-ENTRY-13.                                     GBH1PGM 
00486              15  WS-ERR-CODE-13             PIC X(2)  VALUE '13'. GBH1PGM 
00487              15  WS-ERR-MESSAGE-13.                               GBH1PGM 
00488                  20  FILLER          PIC X(60) VALUE              GBH1PGM 
00489                '12345678901234567890123456789012345678901234567890GBH1PGM 
00490 -'1234567890'.                                                    GBH1PGM 
00491                                                                   GBH1PGM 
00492                                                                   GBH1PGM 
00493 *----------------------------------------------------------------*GBH1PGM 
LOB123         10  WS-ERR-ENTRY-14.                                     GBH1PGM 
LOB123             15  WS-ERR-CODE-14             PIC X(2)  VALUE '14'. GBH1PGM 
LOB123             15  WS-ERR-MESSAGE-14.                               GBH1PGM 
LOB123                 20  FILLER          PIC X(60) VALUE              GBH1PGM 
LOB123               'GBH1-BENEFIT DTLS NOT AVAILABLE FOR GRP CODED WITHGBH1PGM 
LOB123-' LOB-1,2,3'.                                                    GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123*----------------------------------------------------------------*GBH1PGM 
00494      05  WS-01-MESSAGE-TABLE         REDEFINES                    GBH1PGM 
00495          WS-01-MESSAGE-VALUES         OCCURS 014 TIMES            GBH1PGM 
00496                                      INDEXED BY WS-ERR-IDX.       GBH1PGM 
00497          10  WS-ERROR-ENTRY.                                      GBH1PGM 
00498              15  WS-ERROR-CODE       PIC X(02).                   GBH1PGM 
00499              15  WS-ERROR-MESSAGE    PIC X(60).                   GBH1PGM 
00500                                                                   GBH1PGM 
00501 ******************************************************************GBH1PGM 
00502 **  ACCUM HOLD AREAS                                            **GBH1PGM 
00503 ******************************************************************GBH1PGM 
00504                                                                   GBH1PGM 
00505  01  WS-LOOP-ACL-HOLD.                                            GBH1PGM 
00506  COPY GCTACLC.                                                    GBH1PGM 
00507                                                                   GBH1PGM 
00508  01  WS-LOOP-ACP-HOLD.                                            GBH1PGM 
00509  COPY GCTACPC.                                                    GBH1PGM 
00510                                                                   GBH1PGM 
00511  01  WS-LOOP-ADL-HOLD.                                            GBH1PGM 
00512  COPY GCTADLC.                                                    GBH1PGM 
00513                                                                   GBH1PGM 
00514  01  WS-LOOP-AOL-HOLD.                                            GBH1PGM 
00515  COPY GCTAOLC.                                                    GBH1PGM 
00516                                                                   GBH1PGM 
00517  01  WS-CON-ABM-HOLD.                                             GBH1PGM 
00518  COPY GCTABM2.                                                    GBH1PGM 
00519                                                                   GBH1PGM 
00520  01  WS-CON-ACL-HOLD.                                             GBH1PGM 
00521  COPY GCTACL2.                                                    GBH1PGM 
00522                                                                   GBH1PGM 
00523  01  WS-CON-ACP-HOLD.                                             GBH1PGM 
00524  COPY GCTACP2.                                                    GBH1PGM 
00525                                                                   GBH1PGM 
00526  01  WS-CON-ADL-HOLD.                                             GBH1PGM 
00527  COPY GCTADL2.                                                    GBH1PGM 
00528                                                                   GBH1PGM 
00529  01  WS-CON-AOL-HOLD.                                             GBH1PGM 
00530  COPY GCTAOL2.                                                    GBH1PGM 
00531                                                                   GBH1PGM 
00532  01  WS-GRP-ABM-HOLD.                                             GBH1PGM 
00533  COPY GCTABM3.                                                    GBH1PGM 
00534                                                                   GBH1PGM 
00535  01  WS-GRP-ACL-HOLD.                                             GBH1PGM 
00536  COPY GCTACL3.                                                    GBH1PGM 
00537                                                                   GBH1PGM 
00538  01  WS-GRP-ACP-HOLD.                                             GBH1PGM 
00539  COPY GCTACP3.                                                    GBH1PGM 
00540                                                                   GBH1PGM 
00541  01  WS-GRP-ADL-HOLD.                                             GBH1PGM 
00542  COPY GCTADL3.                                                    GBH1PGM 
00543                                                                   GBH1PGM 
00544  01  WS-GRP-AOL-HOLD.                                             GBH1PGM 
00545  COPY GCTAOL3.                                                    GBH1PGM 
00546                                                                   GBH1PGM 
00547 ******************************************************************GBH1PGM 
00548 **  TABULAR FILE AND I/O PARM AREA.                             **GBH1PGM 
00549 ******************************************************************GBH1PGM 
00550  01  IO-PARM-TABULAR-AREA-1.                                      GBH1PGM 
00551  COPY GCIOPRM5.                                                   GBH1PGM 
00552  COPY GCTABMC.                                                    GBH1PGM 
00553                                                                   GBH1PGM 
00554 ******************************************************************GBH1PGM 
00555 *  ACCUM RULE COPYBOOK                                            GBH1PGM 
00556 ******************************************************************GBH1PGM 
00557  01  WS-ACCMRULE-AREA.                                            GBH1PGM 
00558      COPY GCBENHLA.                                               GBH1PGM 
00559                                                                   GBH1PGM 
00560                                                                   GBH1PGM 
00561                                                                   GBH1PGM 
00562 **********MIL   DATE ROUTINE COMMAREA ****************************GBH1PGM 
00563  COPY MLDATE01.                                                   GBH1PGM 
00564                                                                   GBH1PGM 
00565 *************** DATE ROUTINE COMMAREA ****************************GBH1PGM 
00566  01  HGADATES-COMMAREA.                                           GBH1PGM 
00567  COPY HGCDAT01.                                                   GBH1PGM 
00568                                                                   GBH1PGM 
00569  01  WS-REC-LENGTHS.                                              GBH1PGM 
00570  COPY GCCDRLEN.                                                   GBH1PGM 
00571                                                                   GBH1PGM 
00572  01  WS-CONSTANTS.                                                GBH1PGM 
00573      05  WS-GCCONTRC-KEYLEN      PIC S9(4) COMP VALUE +36.        GBH1PGM 
00574      05  WS-IO-PARM-CONTRACT-LEN PIC S9(8) COMP VALUE +0.         GBH1PGM 
00575      05  WS-IO-PARM-GROUPSPC-LEN PIC S9(8) COMP VALUE +0.         GBH1PGM 
00576      05  WS-IO-PARM-TABULAR-LEN  PIC S9(8) COMP VALUE +0.         GBH1PGM 
00577      05  WS-GCCOMKEC-LENGTH      PIC S9(4) COMP VALUE +150.       GBH1PGM 
00578 /                                                                 GBH1PGM 
00579  01  WS-ACMRULE-RESP1        PIC S9(8) COMP  VALUE +0.            GBH1PGM 
00580 /                                                                 GBH1PGM 
00581 ******************************************************************GBH1PGM 
00582 **  GROUPSPC FILE AND I/O PARM AREA.                            **GBH1PGM 
00583 ******************************************************************GBH1PGM 
00584  01  IO-PARM-GROUPSPC-AREA-1.                                     GBH1PGM 
00585      COPY  GCIOPRM1.                                              GBH1PGM 
00586      COPY  GCGROUPC.                                              GBH1PGM 
00587                                                                   GBH1PGM 
00588 /                                                                 GBH1PGM 
00589 ******************************************************************GBH1PGM 
00590 **  CONTRACT FILE AND I/O PARM AREA.                            **GBH1PGM 
00591 ******************************************************************GBH1PGM 
00592  01  IO-PARM-CONTRACT-AREA-1.                                     GBH1PGM 
00593      COPY  GCIOPRM3.                                              GBH1PGM 
00594      COPY  GCCONTRC.                                              GBH1PGM 
00595                                                                   GBH1PGM 
00596 /                                                                 GBH1PGM 
00597 ******************************************************************GBH1PGM 
00598 ** ATTRIBUTE BYTE SETTINGS                                      **GBH1PGM 
00599 ******************************************************************GBH1PGM 
00600  COPY DFHBMSCA.                                                   GBH1PGM 
00601      02  DFHBMABF                PIC X VALUE '9'.                 GBH1PGM 
00602                                                                   GBH1PGM 
00603 /                                                                 GBH1PGM 
00604 ******************************************************************GBH1PGM 
00605 ** ATTENTION IDENTIFIERS                                        **GBH1PGM 
00606 ******************************************************************GBH1PGM 
00607  COPY DFHAID.                                                     GBH1PGM 
00608                                                                   GBH1PGM 
00609 /                                                                 GBH1PGM 
00610 ******************************************************************GBH1PGM 
00611 ** HEX VALUES FOR MILL DATES.                                   **GBH1PGM 
00612 ******************************************************************GBH1PGM 
00613  COPY HEXCOBOL.                                                   GBH1PGM 
00614                                                                   GBH1PGM 
00615  01  WS-END                      PIC X(16)  VALUE                 GBH1PGM 
00616      '*** W/S ENDS ***'.                                          GBH1PGM 
00617 /                                                                 GBH1PGM 
00618                                                                   GBH1PGM 
00619 **** GRBJPGM ACCUM UTILIZATION COMMAREA **************************GBH1PGM 
00620  01  GRBJPGM-COMMAREA.                                            GBH1PGM 
00621      COPY   GRBJTABC.                                             GBH1PGM 
00622                                                                   GBH1PGM 
00623                                                                   GBH1PGM 
00624                                                                   GBH1PGM 
00625 ******************************************************************GBH1PGM 
00626 ** LINKAGE SECTION FOR COMMAREA RECEIVED/SENT BY BLUESTAR       **GBH1PGM 
00627 ******************************************************************GBH1PGM 
00628  LINKAGE SECTION.                                                 GBH1PGM 
00629                                                                   GBH1PGM 
00630  01  DFHCOMMAREA.                                                 GBH1PGM 
00631      COPY GCBENHLI.                                               GBH1PGM 
00632                                                                   GBH1PGM 
00633 /                                                                 GBH1PGM 
00634  PROCEDURE DIVISION.                                              GBH1PGM 
00635 ******************************************************************GBH1PGM 
00636 **                    M A I N L I N E                            *GBH1PGM 
00637 **                                                               *GBH1PGM 
00638 **                                                               *GBH1PGM 
00639 ******************************************************************GBH1PGM 
00640  0000-MAINLINE.                                                   GBH1PGM 
00641                                                                   GBH1PGM 
00642      MOVE '00' TO GCBHS-ERROR-CODE                                GBH1PGM 
00643                                                                   GBH1PGM 
00644                                                                   GBH1PGM 
00645          EXEC CICS                                                GBH1PGM 
00646               ASSIGN                                              GBH1PGM 
00647               SYSID(WS-SYSID)                                     GBH1PGM 
00648          END-EXEC                                                 GBH1PGM 
00649                                                                   GBH1PGM 
00650 *==> TEMPORARY... IF CALLER IS IN TEXAS REGION, SEND ERROR MSSG.  GBH1PGM 
00651                                                                   GBH1PGM 
00652 *restriction #1                                                   GBH1PGM 
00653 ** 06/29/05 temp to test all grps                                 GBH1PGM 
00654 *        IF WS-TEXAS-CICS-REGION                                  GBH1PGM 
00655 *           MOVE 'Y' TO WS-ERROR-FOUND-SW                         GBH1PGM 
00656 *           SET WS-ERR-IDX TO +07                                 GBH1PGM 
00657 *           GO TO 0000-RETURN                                     GBH1PGM 
00658 *        END-IF                                                   GBH1PGM 
00659                                                                   GBH1PGM 
00660                                                                   GBH1PGM 
00661      MOVE GCBHS-IMC-CODE TO WS-IMC-CODE                           GBH1PGM 
00662                                                                   GBH1PGM 
00663      IF ERROR-FD                                                  GBH1PGM 
00664         GO TO 0000-RETURN                                         GBH1PGM 
00665      END-IF                                                       GBH1PGM 
00666                                                                   GBH1PGM 
00667      PERFORM  2000-PROCESS-CONT-GRPSPEC THRU 2000-EXIT            GBH1PGM 
00668                                                                   GBH1PGM 
00669      IF ERROR-FD                                                  GBH1PGM 
00670         GO TO 0000-RETURN                                         GBH1PGM 
00671      END-IF.                                                      GBH1PGM 
00672                                                                   GBH1PGM 
00673      MOVE GCG-ACCUM-PSEUDO-GRP-NBR TO WS-PSEUDO-GRP-NBR           GBH1PGM 
                                            WS-PSEUDO-BAM-GRP-NBR               
      **added for dec 09 rel                                                    
                                                                                
           IF GCG-DISCOUNT-PRODUCT-TYPE  = 'HMO' OR 'OKH' OR                    
                                           'TXH' OR 'UTH'                       
              MOVE 'Y' TO WS-HMO-GRP-SW                                         
00774         SET WS-ERR-IDX TO +10                                     GBH1PGM 
00775         MOVE 'Y' TO WS-ERROR-FOUND-SW                             GBH1PGM 
           END-IF.                                                              
                                                                                
00669      IF ERROR-FD                                                  GBH1PGM 
00670         GO TO 0000-RETURN                                         GBH1PGM 
00671      END-IF.                                                      GBH1PGM 
      **jg                                                                      
      *Dec 09 rel change display BHS for all,commented out the                  
      *prod-type test                                                           
                                                                                
00674 *    PERFORM  1001-TEST-PROD-TYPE  THRU 1001-EXIT.                GBH1PGM 
00675 *                                                                 GBH1PGM 
00676 *    IF ERROR-FD                                                  GBH1PGM 
00677 *       GO TO 0000-RETURN                                         GBH1PGM 
00678 *    END-IF.                                                      GBH1PGM 
      ****                                                                      
00679                                                                   GBH1PGM 
00680                                                                   GBH1PGM 
00681      PERFORM  2100-LOAD-ACCUMS  THRU 2100-EXIT                    GBH1PGM 
00682                                                                   GBH1PGM 
00683                                                                   GBH1PGM 
00684 *JP 8/5/05 - REMOVE COPAY TEST                                    GBH1PGM 
00685      MOVE 'Y' TO WS-COPAY-FD-SW                                   GBH1PGM 
00686 *JP                                                               GBH1PGM 
      * JG commented for dec 09 Release                                         
00687 *    IF NOT-HMO-GRP                                               GBH1PGM 
00688 *       IF WS-CON-ACP-NOT-FD AND WS-GRP-ACP-NOT-FD                GBH1PGM 
00689 *          MOVE 'Y' TO WS-COPAY-FD-SW                             GBH1PGM 
00690 *       END-IF                                                    GBH1PGM 
00691 *       IF WS-BASIC-ACCUMS = 'YYYY'                               GBH1PGM 
00692 *          CONTINUE                                               GBH1PGM 
00693 *       ELSE                                                      GBH1PGM 
00694 *          SET WS-ERR-IDX TO +09                                  GBH1PGM 
00695 *          MOVE 'Y' TO WS-ERROR-FOUND-SW                          GBH1PGM 
00696 *          MOVE WS-BASIC-ACCUMS TO WS-REQD-ACCUMS                 GBH1PGM 
00697 *          MOVE WS-REQD-ACCUM-ERROR                               GBH1PGM 
00698 *              TO WS-ERROR-MESSAGE (WS-ERR-IDX)                   GBH1PGM 
00699 *       END-IF                                                    GBH1PGM 
00700 *    END-IF.                                                      GBH1PGM 
      * commented for dec release                                               
00701                                                                   GBH1PGM 
00702      IF ERROR-FD                                                  GBH1PGM 
00703         GO TO 0000-RETURN                                         GBH1PGM 
00704      END-IF.                                                      GBH1PGM 
00705                                                                   GBH1PGM 
00706 * Commented out for dec release                                   GBH1PGM 
00707 *restriction #2                                                   GBH1PGM 
      *    if BAM-BHS-GRP                                                       
      *      MOVE SPACES TO WS-PSEUDO-BAM-GRP-NBR                               
      *    else                                                                 
00708 ***  IF GCBHS-BEN-POINTERS  <  +06                                GBH1PGM 
00709      IF GCBHS-BEN-POINTERS  <  +03                                GBH1PGM 
00710         MOVE 'Y' TO WS-ERROR-FOUND-SW                             GBH1PGM 
00711         SET WS-ERR-IDX TO +05                                     GBH1PGM 
           END-IF.                                                              
00712 *    END-IF.                                                      GBH1PGM 
00713                                                                   GBH1PGM 
00714                                                                   GBH1PGM 
00715  0000-RETURN.                                                     GBH1PGM 
00716                                                                   GBH1PGM 
00717                                                                   GBH1PGM 
00718      IF ERROR-FD                                                  GBH1PGM 
00719         MOVE WS-ERROR-CODE (WS-ERR-IDX) TO GCBHS-ERROR-CODE       GBH1PGM 
00720         MOVE WS-ERROR-MESSAGE (WS-ERR-IDX) TO                     GBH1PGM 
00721              GCBHS-ERROR-DESCRIPTION                              GBH1PGM 
00722      END-IF.                                                      GBH1PGM 
00723                                                                   GBH1PGM 
00724      EXEC CICS  RETURN    END-EXEC.                               GBH1PGM 
00725                                                                   GBH1PGM 
00726                                                                   GBH1PGM 
00727  0000-EXIT.                                                       GBH1PGM 
00728      EXIT.                                                        GBH1PGM 
00729 /                                                                 GBH1PGM 
00730                                                                   GBH1PGM 
00731                                                                   GBH1PGM 
00732                                                                   GBH1PGM 
00733 ******************************************************************GBH1PGM 
00734 *                                                                 GBH1PGM 
00735 *                                                                 GBH1PGM 
00736 ******************************************************************GBH1PGM 
00737  1001-TEST-PROD-TYPE.                                             GBH1PGM 
00738                                                                   GBH1PGM 
00739 * ALLOW BHS TO DISPLAY FOR PPO/POS/HMO ONLY.                      GBH1PGM 
00740                                                                   GBH1PGM 
00741      MOVE GCT-PRODUCT-TYPE(1:5) TO WS-PRODUCT-TYPE.               GBH1PGM 
           EVALUATE TRUE                                                        
            WHEN BAM-BHS-GRP                                                    
               CONTINUE                                                         
            WHEN BAE-BLUEPRNT OR TX-STANDARD                                    
               CONTINUE                                                         
            WHEN JUMBO-PG-GRP                                                   
00746          IF GCBHS-BAM-APPLICATION                                 GBH1PGM 
00747             SET WS-ERR-IDX TO +12                                 GBH1PGM 
00748             MOVE 'Y' TO WS-ERROR-FOUND-SW                         GBH1PGM 
00749             GO TO 1001-EXIT                                       GBH1PGM 
               END-IF                                                           
00751 *       ELSE                                                      GBH1PGM 
            WHEN OTHER                                                          
                 SET WS-ERR-IDX TO +08                                    GBH1PG
                 MOVE 'Y' TO WS-ERROR-FOUND-SW                            GBH1PG
                 GO TO 1001-EXIT                                          GBH1PG
            END-EVALUATE.                                                       
00755 *       END-IF                                                    GBH1PGM 
00742 *    IF BAE-BLUEPRNT OR TX-STANDARD                               GBH1PGM 
00743 *      CONTINUE                                                   GBH1PGM 
00744 *    ELSE                                                         GBH1PGM 
00745 *       IF JUMBO-PG-GRP                                           GBH1PGM 
00746 *          IF GCBHS-BAM-APPLICATION                               GBH1PGM 
00747 *             SET WS-ERR-IDX TO +12                               GBH1PGM 
00748 *             MOVE 'Y' TO WS-ERROR-FOUND-SW                       GBH1PGM 
00749 *             GO TO 1001-EXIT                                     GBH1PGM 
00750 *          END-IF                                                 GBH1PGM 
00751 *       ELSE                                                      GBH1PGM 
00752 *          SET WS-ERR-IDX TO +08                                  GBH1PGM 
00753 *          MOVE 'Y' TO WS-ERROR-FOUND-SW                          GBH1PGM 
00754 *          GO TO 1001-EXIT                                        GBH1PGM 
00755 **      END-IF                                                    GBH1PGM 
00756 *    END-IF.                                                      GBH1PGM 
00757                                                                   GBH1PGM 
00758      IF GCG-NEW-POS-IND NOT = '00'                                GBH1PGM 
00759         MOVE 'Y' TO WS-PROD-TYPE-OK-SW                            GBH1PGM 
00760      END-IF                                                       GBH1PGM 
00761                                                                   GBH1PGM 
00762      IF GCG-PARTICIPAT-PROV-OPTION  NOT = '00'                    GBH1PGM 
00763         MOVE 'Y' TO WS-PROD-TYPE-OK-SW                            GBH1PGM 
00764      END-IF                                                       GBH1PGM 
00765                                                                   GBH1PGM 
00766 *GF/912  IF GCG-DISCOUNT-PRODUCT-TYPE       = 'HMO'               GBH1PGM 
00767 *          MOVE 'Y' TO WS-PROD-TYPE-OK-SW                         GBH1PGM 
00768 *          MOVE 'Y' TO WS-HMO-GRP-SW                              GBH1PGM 
00769 *        END-IF                                                   GBH1PGM 
00770                                                                   GBH1PGM 
00771      IF PROD-TYPE-OK                                              GBH1PGM 
00772         CONTINUE                                                  GBH1PGM 
00773      ELSE                                                         GBH1PGM 
00774         SET WS-ERR-IDX TO +10                                     GBH1PGM 
00775         MOVE 'Y' TO WS-ERROR-FOUND-SW                             GBH1PGM 
00776         GO TO 1001-EXIT                                           GBH1PGM 
00777      END-IF.                                                      GBH1PGM 
00778                                                                   GBH1PGM 
00779      IF (GCG-PROV-CONTROL-CONT-BC-IND  = '00')  AND               GBH1PGM 
00780         (GCG-PROV-CONTROL-CONT-BS-IND  = '00')  AND               GBH1PGM 
00781         (GCG-PROV-CONTROL-CONT-MM-IND  = '00')                    GBH1PGM 
00782         CONTINUE                                                  GBH1PGM 
00783      ELSE                                                         GBH1PGM 
00784         SET WS-ERR-IDX TO +11                                     GBH1PGM 
00785         MOVE 'Y' TO WS-ERROR-FOUND-SW                             GBH1PGM 
00786      END-IF.                                                      GBH1PGM 
                                                                                
           if GCG-VENDOR-MULT-ID(1) = '00'                                      
             CONTINUE                                                   GBH1PGM 
00783      ELSE                                                         GBH1PGM 
00784         SET WS-ERR-IDX TO +11                                     GBH1PGM 
00785         MOVE 'Y' TO WS-ERROR-FOUND-SW                             GBH1PGM 
00786      END-IF.                                                      GBH1PGM 
00788                                                                   GBH1PGM 
00789  1001-EXIT.                                                       GBH1PGM 
00790      EXIT.                                                        GBH1PGM 
00791                                                                   GBH1PGM 
00792                                                                   GBH1PGM 
00793                                                                   GBH1PGM 
00794 ******************************************************************GBH1PGM 
00795 *                                                                 GBH1PGM 
00796 *                                                                 GBH1PGM 
00797 ******************************************************************GBH1PGM 
00798  1002-TEST-BASIC-ACCUM-IDS.                                       GBH1PGM 
00799                                                                   GBH1PGM 
00800 * ALLOW BHS TO DISPLAY ONLY IF CERTAIN ACCUM ID VALUES FD.        GBH1PGM 
00801                                                                   GBH1PGM 
00802      IF WS-RULE = '004070' OR '004080'                            GBH1PGM 
00803         MOVE 'Y' TO WS-COPAY-FD-SW                                GBH1PGM 
00804      END-IF                                                       GBH1PGM 
00805                                                                   GBH1PGM 
00806      IF WS-RULE = '004020' OR '004022' OR '004025' OR             GBH1PGM 
00807                   '004030' OR '004032' OR '004035'                GBH1PGM 
00808         MOVE 'Y' TO WS-DEDL-FD-SW                                 GBH1PGM 
00809      END-IF                                                       GBH1PGM 
00810                                                                   GBH1PGM 
00811      IF WS-RULE = '004040' OR '004042' OR '004045' OR             GBH1PGM 
00812                   '004050' OR '004052' OR '004055'                GBH1PGM 
00813         MOVE 'Y' TO WS-OPX-FD-SW                                  GBH1PGM 
00814      END-IF                                                       GBH1PGM 
00815                                                                   GBH1PGM 
00816      IF WS-RULE = '004100' OR '004120' OR '004130' OR             GBH1PGM 
00817                   '004135' OR '004140' OR '004150' OR             GBH1PGM 
00818                   '004210' OR '004215'                            GBH1PGM 
00819         MOVE 'Y' TO WS-COIN-FD-SW                                 GBH1PGM 
00820      END-IF.                                                      GBH1PGM 
00821                                                                   GBH1PGM 
00822                                                                   GBH1PGM 
00823  1002-EXIT.                                                       GBH1PGM 
00824      EXIT.                                                        GBH1PGM 
00825                                                                   GBH1PGM 
00826 ******************************************************************GBH1PGM 
00827 *                                                                 GBH1PGM 
00828 *   READ CONTRACT AND GROUP SPECIFIC FILES                        GBH1PGM 
00829 *     IF GROUP / SECTION NOT FOUND, OR NO RECORD WITH             GBH1PGM 
00830 *        EFFECTIVE AND TERMINATION DATES FOUND TO COINCIDE        GBH1PGM 
00831 *          WITH THE DATE OF SERVICE ENTERED, DISPLAY ERROR.       GBH1PGM 
00832 *                                                                 GBH1PGM 
00833 ******************************************************************GBH1PGM 
00834  2000-PROCESS-CONT-GRPSPEC.                                       GBH1PGM 
00835                                                                   GBH1PGM 
00836      PERFORM  8400-CONV-SERV-DATE THRU 8400-EXIT                  GBH1PGM 
00837                                                                   GBH1PGM 
00838      IF ERROR-FD                                                  GBH1PGM 
00839         GO TO 2000-EXIT.                                          GBH1PGM 
00840                                                                   GBH1PGM 
00841      MOVE SPACES TO WS-TEST-PRODUCT-TYPE                          GBH1PGM 
00842                                                                   GBH1PGM 
00843      MOVE '000'                 TO   GCT-PLAN-CODE                GBH1PGM 
00844      MOVE GCBHS-GROUP-NBR       TO   GCT-GROUP-NUM                GBH1PGM 
00845      MOVE GCBHS-SECT-NUM        TO   GCT-SECTION-NUM              GBH1PGM 
00846      MOVE GCBHS-PACKAGE-CODE    TO   GCT-PKG-CODE                 GBH1PGM 
00847      MOVE '4'                   TO   GCT-L-O-B                    GBH1PGM 
00848      MOVE '00'                  TO   GCT-PROVDR-CONTROL           GBH1PGM 
00849      MOVE '00'                  TO   GCT-FAM-REL-LVL              GBH1PGM 
00850      MOVE  +25                  TO   GCIO3-BROWSE-KEYLEN          GBH1PGM 
00851                                                                   GBH1PGM 
00852                                                                   GBH1PGM 
00853      PERFORM  8000-SEARCH-CONTRACT THRU 8000-EXIT                 GBH1PGM 
00854                                                                   GBH1PGM 
00855      IF ERROR-FD                                                  GBH1PGM 
00856         GO TO 2000-EXIT.                                          GBH1PGM 
00857                                                                   GBH1PGM 
00858      MOVE GCT-PRODUCT-TYPE TO WS-TEST-PRODUCT-TYPE                GBH1PGM 
00859                                                                   GBH1PGM 
00860      IF RECORD-NOT-FD                                             GBH1PGM 
LOB123        PERFORM  8050-SEARCH-CONTRACT-LOB123 THRU 8050-EXIT       GBH1PGM 
LOB123        IF RECORD-FOUND-LOB1 OR                                           
LOB123           RECORD-FOUND-LOB2 OR                                           
LOB123           RECORD-FOUND-LOB3                                              
LOB123           SET WS-ERR-IDX TO +14                                          
LOB123        ELSE                                                              
LOB123           SET WS-ERR-IDX TO +03                                          
LOB123        END-IF                                                            
00861         MOVE 'Y' TO WS-ERROR-FOUND-SW                             GBH1PGM 
LOB123*       SET WS-ERR-IDX TO +03                                     GBH1PGM 
00863         GO TO 2000-EXIT                                           GBH1PGM 
00864      END-IF                                                       GBH1PGM 
00865                                                                   GBH1PGM 
00866 *restriction #3                                                   GBH1PGM 
00867 ** 06/29/05 temp to test all grps                                 GBH1PGM 
00868 *    IF BAE-BLUEPRINT                                             GBH1PGM 
00869 *       CONTINUE                                                  GBH1PGM 
00870 *    ELSE                                                         GBH1PGM 
00871 *       MOVE 'Y' TO WS-ERROR-FOUND-SW                             GBH1PGM 
00872 *       SET WS-ERR-IDX TO +10                                     GBH1PGM 
00873 *       GO TO 2000-EXIT                                           GBH1PGM 
00874 *    END-IF                                                       GBH1PGM 
00875                                                                   GBH1PGM 
00876      MOVE 'N'     TO    WS-RECORD-FOUND-SW                        GBH1PGM 
00877                         WS-END-OF-READ-SW                         GBH1PGM 
00878                                                                   GBH1PGM 
00879      MOVE '000'                 TO   GCG-PLAN-CODE                GBH1PGM 
00880      MOVE GCBHS-GROUP-NBR       TO   GCG-GROUP-NUM                GBH1PGM 
00881      MOVE GCBHS-SECT-NUM        TO   GCG-SECTION-NUM              GBH1PGM 
00882      MOVE GCBHS-PACKAGE-CODE    TO   GCG-PKG-CODE                 GBH1PGM 
00883      MOVE '00'                  TO   GCG-FAM-REL-LVL              GBH1PGM 
00884      MOVE  +22                  TO   GCIO-BROWSE-KEYLEN           GBH1PGM 
00885                                                                   GBH1PGM 
00886      PERFORM  8100-SEARCH-GRPSPEC THRU 8100-EXIT                  GBH1PGM 
00887                                                                   GBH1PGM 
00888      IF RECORD-NOT-FD                                             GBH1PGM 
00889         MOVE 'Y' TO WS-ERROR-FOUND-SW                             GBH1PGM 
00890         SET WS-ERR-IDX TO +04                                     GBH1PGM 
00891         GO TO 2000-EXIT                                           GBH1PGM 
00892      END-IF.                                                      GBH1PGM 
00893                                                                   GBH1PGM 
00894                                                                   GBH1PGM 
00895  2000-EXIT.                                                       GBH1PGM 
00896      EXIT.                                                        GBH1PGM 
00897                                                                   GBH1PGM 
00898 ******************************************************************GBH1PGM 
00899 *                                                                 GBH1PGM 
00900 *                                                                 GBH1PGM 
00901 ******************************************************************GBH1PGM 
00902  2100-LOAD-ACCUMS.                                                GBH1PGM 
00903                                                                   GBH1PGM 
00904                                                                   GBH1PGM 
00905      PERFORM 3000-LOOP-THRU-GRP-ACCUMS  THRU 3000-EXIT            GBH1PGM 
00906         VARYING GCG-INDEX FROM 1 BY 1                             GBH1PGM 
00907            UNTIL GCG-INDEX > 30 OR                                GBH1PGM 
00908                  GCG-TAB-ID (GCG-INDEX) > '#AOL  '.               GBH1PGM 
00909                                                                   GBH1PGM 
00910      PERFORM 3500-LOOP-THRU-CON-ACCUMS  THRU 3500-EXIT            GBH1PGM 
00911         VARYING GCT-TAB-INDEX FROM 1 BY 1                         GBH1PGM 
00912            UNTIL GCT-TAB-INDEX > 18 OR                            GBH1PGM 
00913                  GCT-CON-TAB-ID (GCT-TAB-INDEX) > '#AOL  '.       GBH1PGM 
00914                                                                   GBH1PGM 
00915       SET GCBHS-BEN-IDX TO    1                                   GBH1PGM 
00916       MOVE +1 TO GCBHS-BEN-POINTERS                               GBH1PGM 
00917                                                                   GBH1PGM 
00918      PERFORM 4000-BUILD-RULES THRU 4000-EXIT.                     GBH1PGM 
00919                                                                   GBH1PGM 
00920                                                                   GBH1PGM 
00921                                                                   GBH1PGM 
00922                                                                   GBH1PGM 
00923                                                                   GBH1PGM 
00924  2100-EXIT.                                                       GBH1PGM 
00925      EXIT.                                                        GBH1PGM 
00926                                                                   GBH1PGM 
00927                                                                   GBH1PGM 
00928 ******************************************************************GBH1PGM 
00929 *                                                                 GBH1PGM 
00930 ******************************************************************GBH1PGM 
00931  3000-LOOP-THRU-GRP-ACCUMS.                                       GBH1PGM 
00932                                                                   GBH1PGM 
00933      IF GCG-TAB-ID (GCG-INDEX) = '#ABM  ' OR '#ACL  ' OR          GBH1PGM 
00934                                  '#ACP  ' OR '#ADL  ' OR '#AOL  ' GBH1PGM 
00935         IF GCG-TAB-SLOT-NO (GCG-INDEX) > ZERO                     GBH1PGM 
00936            PERFORM 3100-PROCESS-GRP-ACCUM THRU 3100-EXIT.         GBH1PGM 
00937                                                                   GBH1PGM 
00938  3000-EXIT.                                                       GBH1PGM 
00939      EXIT.                                                        GBH1PGM 
00940 /                                                                 GBH1PGM 
00941 ******************************************************************GBH1PGM 
00942 *                                                                 GBH1PGM 
00943 *  FOR EACH ACCUM FOUND, MOVE IT TO THE GROUP SPECIFIC HOLD ACCUM GBH1PGM 
00944 *  AND SET FOUND SWITCH.                                          GBH1PGM 
00945 *                                                                 GBH1PGM 
00946 ******************************************************************GBH1PGM 
00947  3100-PROCESS-GRP-ACCUM.                                          GBH1PGM 
00948                                                                   GBH1PGM 
00949      MOVE GCG-GRP-SPEC-TAB-ID (GCG-INDEX)                         GBH1PGM 
00950        TO GAA-TABULAR-PROVISION-ID.                               GBH1PGM 
00951                                                                   GBH1PGM 
00952      PERFORM 8600-READ-TABULAR THRU 8600-EXIT.                    GBH1PGM 
00953                                                                   GBH1PGM 
00954      IF GAA-PROVISION-ID = '#ABM  '                               GBH1PGM 
00955         MOVE GAA-RECORD  TO WS-GRP-ABM-HOLD                       GBH1PGM 
00956         MOVE 'Y'         TO WS-GRP-ABM-FOUND-SW                   GBH1PGM 
00957      ELSE                                                         GBH1PGM 
00958      IF GAA-PROVISION-ID = '#ACL  '                               GBH1PGM 
00959         MOVE GAA-RECORD  TO WS-GRP-ACL-HOLD                       GBH1PGM 
00960         MOVE 'Y'         TO WS-GRP-ACL-FOUND-SW                   GBH1PGM 
00961      ELSE                                                         GBH1PGM 
00962      IF GAA-PROVISION-ID = '#ACP  '                               GBH1PGM 
00963         MOVE GAA-RECORD  TO WS-GRP-ACP-HOLD                       GBH1PGM 
00964         MOVE 'Y'         TO WS-GRP-ACP-FOUND-SW                   GBH1PGM 
00965      ELSE                                                         GBH1PGM 
00966      IF GAA-PROVISION-ID = '#ADL  '                               GBH1PGM 
00967         MOVE GAA-RECORD  TO WS-GRP-ADL-HOLD                       GBH1PGM 
00968         MOVE 'Y'         TO WS-GRP-ADL-FOUND-SW                   GBH1PGM 
00969      ELSE                                                         GBH1PGM 
00970      IF GAA-PROVISION-ID = '#AOL  '                               GBH1PGM 
00971         MOVE GAA-RECORD  TO WS-GRP-AOL-HOLD                       GBH1PGM 
00972         MOVE 'Y'         TO WS-GRP-AOL-FOUND-SW.                  GBH1PGM 
00973                                                                   GBH1PGM 
00974  3100-EXIT.                                                       GBH1PGM 
00975      EXIT.                                                        GBH1PGM 
00976 /                                                                 GBH1PGM 
00977 ***************************************************************** GBH1PGM 
00978 *                                                                 GBH1PGM 
00979 *  LOOP THRU THE ACCUMS LOOKING FOR A SLOT GREATER THAN ZERO.     GBH1PGM 
00980 *  IF FOUND, PROCESS THE ACCUM.                                   GBH1PGM 
00981 *                                                                 GBH1PGM 
00982 ***************************************************************** GBH1PGM 
00983  3500-LOOP-THRU-CON-ACCUMS.                                       GBH1PGM 
00984                                                                   GBH1PGM 
00985      IF GCT-CON-TAB-ID (GCT-TAB-INDEX) = '#ABM  ' OR '#ACL  ' OR  GBH1PGM 
00986                              '#ACP  ' OR '#ADL  ' OR '#AOL  '     GBH1PGM 
00987         IF GCT-CON-TAB-SLOT (GCT-TAB-INDEX) > ZERO                GBH1PGM 
00988            PERFORM 3600-PROCESS-CON-ACCUM THRU 3600-EXIT.         GBH1PGM 
00989                                                                   GBH1PGM 
00990  3500-EXIT.                                                       GBH1PGM 
00991      EXIT.                                                        GBH1PGM 
00992 /                                                                 GBH1PGM 
00993 ***************************************************************** GBH1PGM 
00994 *                                                                 GBH1PGM 
00995 *                                                                 GBH1PGM 
00996 *  FOR EACH ACCUM FOUND, MOVE IT TO THE CONTRACT HOLD ACCUM       GBH1PGM 
00997 *  AND SET FOUND SWITCH.                                          GBH1PGM 
00998 *                                                                 GBH1PGM 
00999 *                                                                 GBH1PGM 
01000 ***************************************************************** GBH1PGM 
01001  3600-PROCESS-CON-ACCUM.                                          GBH1PGM 
01002                                                                   GBH1PGM 
01003      MOVE GCT-CON-TAB-ID-SLOT (GCT-TAB-INDEX)                     GBH1PGM 
01004        TO GAA-TABULAR-PROVISION-ID.                               GBH1PGM 
01005                                                                   GBH1PGM 
01006      PERFORM 8600-READ-TABULAR THRU 8600-EXIT                     GBH1PGM 
01007                                                                   GBH1PGM 
01008      IF GAA-PROVISION-ID = '#ABM  '                               GBH1PGM 
01009         MOVE GAA-RECORD  TO WS-CON-ABM-HOLD                       GBH1PGM 
01010         MOVE 'Y'         TO WS-CON-ABM-FOUND-SW                   GBH1PGM 
01011      ELSE                                                         GBH1PGM 
01012      IF GAA-PROVISION-ID = '#ACL  '                               GBH1PGM 
01013         MOVE GAA-RECORD  TO WS-CON-ACL-HOLD                       GBH1PGM 
01014         MOVE 'Y'         TO WS-CON-ACL-FOUND-SW                   GBH1PGM 
01015      ELSE                                                         GBH1PGM 
01016      IF GAA-PROVISION-ID = '#ACP  '                               GBH1PGM 
01017         MOVE GAA-RECORD  TO WS-CON-ACP-HOLD                       GBH1PGM 
01018         MOVE 'Y'         TO WS-CON-ACP-FOUND-SW                   GBH1PGM 
01019      ELSE                                                         GBH1PGM 
01020      IF GAA-PROVISION-ID = '#ADL  '                               GBH1PGM 
01021         MOVE GAA-RECORD  TO WS-CON-ADL-HOLD                       GBH1PGM 
01022         MOVE 'Y'         TO WS-CON-ADL-FOUND-SW                   GBH1PGM 
01023      ELSE                                                         GBH1PGM 
01024      IF GAA-PROVISION-ID = '#AOL  '                               GBH1PGM 
01025         MOVE GAA-RECORD  TO WS-CON-AOL-HOLD                       GBH1PGM 
01026         MOVE 'Y'         TO WS-CON-AOL-FOUND-SW.                  GBH1PGM 
01027                                                                   GBH1PGM 
01028  3600-EXIT.                                                       GBH1PGM 
01029      EXIT.                                                        GBH1PGM 
01030                                                                   GBH1PGM 
01031                                                                   GBH1PGM 
01032 ******************************************************************GBH1PGM 
01033 *                                                                 GBH1PGM 
01034 *  BUILD RULE LINES UNTIL PAGE IS FULL OR END OF RULES.           GBH1PGM 
01035 *  WHEN A PAGE IS FULL, WRITE IT TO TEMPORARY STORAGE AND THEN    GBH1PGM 
01036 *  CONTINUE WITH THE NEXT PAGE.                                   GBH1PGM 
01037 *                                                                 GBH1PGM 
01038 ******************************************************************GBH1PGM 
01039  4000-BUILD-RULES.                                                GBH1PGM 
01040                                                                   GBH1PGM 
01041 *=> LIFETIME MAX                                                  GBH1PGM 
      *=> Added logic to initialize benefit value incident 189694               
      *=> 4/20/15 Rel                                                           
01042       MOVE '004060' TO WS-RULE                                    GBH1PGM 
01043       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01044       IF RULE-DONE                                                GBH1PGM 
01045          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
            ELSE                                                                
               MOVE SPACES TO                                                   
                  GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, GCBHS-COL-IDX, 2)         
01046       END-IF.                                                     GBH1PGM 
01047                                                                   GBH1PGM 
01048 *=> DEDUCTIBLE PER INDIVIDUAL                                     GBH1PGM 
01049       MOVE '004020' TO WS-RULE                                    GBH1PGM 
01050       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01051       IF RULE-DONE                                                GBH1PGM 
01052          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01053          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01054       END-IF.                                                     GBH1PGM 
01055 *=> individual additional inpatient deductible for dean foods     GBH1PGM 
01056       MOVE '004036' TO WS-RULE                                    GBH1PGM 
01057       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01058       IF RULE-DONE                                                GBH1PGM 
01059          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01060          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01061       END-IF.                                                     GBH1PGM 
01062 *=> INDIVIDUAL STAND ALONE DEDUCTIBLE                             GBH1PGM 
01063       MOVE '004022' TO WS-RULE                                    GBH1PGM 
01064       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01065       IF RULE-DONE                                                GBH1PGM 
01066          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01067          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01068       END-IF.                                                     GBH1PGM 
01069 *=> COMBINED INDIVIDUAL DEDUCTIBLE                                GBH1PGM 
01070       MOVE '004025' TO WS-RULE                                    GBH1PGM 
01071       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01072       IF RULE-DONE                                                GBH1PGM 
01073          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01074          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01075       END-IF.                                                     GBH1PGM 
01076                                                                   GBH1PGM 
01077 *=> family additional inpatient deductible for dean foods         GBH1PGM 
01078       MOVE '004037' TO WS-RULE                                    GBH1PGM 
01079       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01080       IF RULE-DONE                                                GBH1PGM 
01081          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01082          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01083       END-IF.                                                     GBH1PGM 
01084                                                                   GBH1PGM 
01085 *=> DEDUCTIBLE PER FAMILY                                         GBH1PGM 
01086       MOVE '004030' TO WS-RULE                                    GBH1PGM 
01087       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01088       IF RULE-DONE                                                GBH1PGM 
01089          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01090          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01091       END-IF.                                                     GBH1PGM 
01092 *=> FAMILY STAND ALONE DEDUCTIBLE                                 GBH1PGM 
01093       MOVE '004032' TO WS-RULE                                    GBH1PGM 
01094       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01095       IF RULE-DONE                                                GBH1PGM 
01096          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01097          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01098       END-IF.                                                     GBH1PGM 
01099 *=> COMBINED FAMILY DEDUCTIBLE                                    GBH1PGM 
01100       MOVE '004035' TO WS-RULE                                    GBH1PGM 
01101       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01102       IF RULE-DONE                                                GBH1PGM 
01103          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01104          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01105       END-IF.                                                     GBH1PGM 
01106                                                                   GBH1PGM 
01107 *=> OUT OF POCKET PER INDIVIDUAL                                  GBH1PGM 
01108       MOVE '004040' TO WS-RULE                                    GBH1PGM 
01109       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01110       IF RULE-DONE                                                GBH1PGM 
01111          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01112          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01113       END-IF.                                                     GBH1PGM 
01114 *=> INDIVIDUAL STAND ALONE OUT OF POCKET                          GBH1PGM 
01115       MOVE '004042' TO WS-RULE                                    GBH1PGM 
01116       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01117       IF RULE-DONE                                                GBH1PGM 
01118          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01119          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01120       END-IF.                                                     GBH1PGM 
01121 *=> COMBINED INDIVIDUAL OUT OF POCKET                             GBH1PGM 
01122       MOVE '004045' TO WS-RULE                                    GBH1PGM 
01123       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01124       IF RULE-DONE                                                GBH1PGM 
01125          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01126          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01127       END-IF.                                                     GBH1PGM 
01128                                                                   GBH1PGM 
01129 *=> OUT OF POCKET PER FAMILY                                      GBH1PGM 
01130       MOVE '004050' TO WS-RULE                                    GBH1PGM 
01131       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01132       IF RULE-DONE                                                GBH1PGM 
01133          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01134          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01135       END-IF.                                                     GBH1PGM 
01136 *=> FAMILY STAND ALONE OUT OF POCKET                              GBH1PGM 
01137       MOVE '004052' TO WS-RULE                                    GBH1PGM 
01138       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01139       IF RULE-DONE                                                GBH1PGM 
01140          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01141          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01142       END-IF.                                                     GBH1PGM 
01143 *=> COMBINED FAMILY OUT OF POCKET                                 GBH1PGM 
01144       MOVE '004055' TO WS-RULE                                    GBH1PGM 
01145       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01146       IF RULE-DONE                                                GBH1PGM 
01147          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01148          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01149       END-IF.                                                     GBH1PGM 
01150                                                                   GBH1PGM 
01151                                                                   GBH1PGM 
01152 *=> HOSPITAL/MEDICAL SURGICAL PAYMENT                             GBH1PGM 
01153       MOVE '004215' TO WS-RULE                                    GBH1PGM 
01154       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01155       IF RULE-DONE                                                GBH1PGM 
01156          MOVE 'Y' TO WS-HOSP-MED-FD                               GBH1PGM 
01157          IF OVERALL-FD OR (IN-NET-FD AND OUT-NET-FD)              GBH1PGM 
01158             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01159          ELSE                                                     GBH1PGM 
01160             IF IN-NET-FD                                          GBH1PGM 
01161                MOVE '004100' TO WS-RULE                           GBH1PGM 
01162                PERFORM 4100-PROC-LOOP THRU 4100-EXIT              GBH1PGM 
01163                IF RULE-DONE                                       GBH1PGM 
01164                   MOVE WS-HMSI-SAVE-VAL TO                        GBH1PGM 
01165                    GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)      GBH1PGM 
01166                   PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT      GBH1PGM 
01167                END-IF                                             GBH1PGM 
01168                MOVE '004210' TO WS-RULE                           GBH1PGM 
01169                PERFORM 4100-PROC-LOOP THRU 4100-EXIT              GBH1PGM 
01170                IF RULE-DONE                                       GBH1PGM 
01171                   MOVE WS-HMSI-SAVE-VAL TO                        GBH1PGM 
01172                    GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)      GBH1PGM 
01173                   PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT      GBH1PGM 
01174                END-IF                                             GBH1PGM 
01175             END-IF                                                GBH1PGM 
01176             IF OUT-NET-FD                                         GBH1PGM 
01177                MOVE '004100' TO WS-RULE                           GBH1PGM 
01178                PERFORM 4100-PROC-LOOP THRU 4100-EXIT              GBH1PGM 
01179                IF RULE-DONE                                       GBH1PGM 
01180                   MOVE WS-HMSO-SAVE-VAL TO                        GBH1PGM 
01181                    GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 2, 1)      GBH1PGM 
01182                   PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT      GBH1PGM 
01183                END-IF                                             GBH1PGM 
01184                MOVE '004210' TO WS-RULE                           GBH1PGM 
01185                PERFORM 4100-PROC-LOOP THRU 4100-EXIT              GBH1PGM 
01186                IF RULE-DONE                                       GBH1PGM 
01187                   MOVE WS-HMSO-SAVE-VAL TO                        GBH1PGM 
01188                    GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 2, 1)      GBH1PGM 
01189                   PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT      GBH1PGM 
01190                END-IF                                             GBH1PGM 
01191             END-IF                                                GBH1PGM 
01192          END-IF                                                   GBH1PGM 
01193       ELSE                                                        GBH1PGM 
01194 *=> HOSPITAL PAYMENT LEVEL                                        GBH1PGM 
01195          MOVE '004100' TO WS-RULE                                 GBH1PGM 
01196          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01197          IF RULE-DONE                                             GBH1PGM 
01198             PERFORM 4502-100-PERC-DFLT-INN THRU 4502-EXIT         GBH1PGM 
01199             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01200          END-IF                                                   GBH1PGM 
01201 *=> MEDICAL SURGICAL PAYMENT LEVEL                                GBH1PGM 
01202          MOVE '004210' TO WS-RULE                                 GBH1PGM 
01203          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01204          IF RULE-DONE                                             GBH1PGM 
01205             PERFORM 4502-100-PERC-DFLT-INN THRU 4502-EXIT         GBH1PGM 
01206             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01207          END-IF                                                   GBH1PGM 
01208       END-IF.                                                     GBH1PGM 
01209 *=> MSA SANCTION COINSURANCE                                      GBH1PGM 
01210       MOVE '004340' TO WS-RULE                                    GBH1PGM 
01211       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01212       IF RULE-DONE                                                GBH1PGM 
01213          PERFORM 4502-100-PERC-DFLT-INN THRU 4502-EXIT            GBH1PGM 
01214          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01215       END-IF.                                                     GBH1PGM 
01216 *=> MSA SANCTION DEDUCTIBLE                                       GBH1PGM 
01217       MOVE '004350' TO WS-RULE                                    GBH1PGM 
01218       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01219       IF RULE-DONE                                                GBH1PGM 
01220          PERFORM 4506-ZERO-DLR-DFLT-MSA THRU 4506-EXIT            GBH1PGM 
01221          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01222       END-IF.                                                     GBH1PGM 
01223                                                                   GBH1PGM 
01224                                                                   GBH1PGM 
01225                                                                   GBH1PGM 
01226                                                                   GBH1PGM 
01227                                                                   GBH1PGM 
01228 *=> OUTPATIENT SURGERY HOSPITAL PAYMENT LEVEL                     GBH1PGM 
01229       MOVE '004120' TO WS-RULE                                    GBH1PGM 
01230       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01231       IF RULE-DONE                                                GBH1PGM 
01232          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01233       END-IF.                                                     GBH1PGM 
01234 *=> OUTPATIENT SURGERY PROFESSIONAL PAYMENT LEVEL                 GBH1PGM 
01235       MOVE '004130' TO WS-RULE                                    GBH1PGM 
01236       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01237       IF RULE-DONE                                                GBH1PGM 
01238          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01239       END-IF.                                                     GBH1PGM 
01240 *=> OUTPATIENT SURGERY B/C AND B/S PAYMENT LEVEL                  GBH1PGM 
01241       MOVE '004135' TO WS-RULE                                    GBH1PGM 
01242       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01243       IF RULE-DONE                                                GBH1PGM 
01244          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01245       END-IF.                                                     GBH1PGM 
01246 *=> OUTPATIENT DIAGNOSTIC HOSPITAL PAYMENT LEVEL                  GBH1PGM 
01247       MOVE '004140' TO WS-RULE                                    GBH1PGM 
01248       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01249       IF RULE-DONE                                                GBH1PGM 
01250          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01251       END-IF.                                                     GBH1PGM 
01252 *=> OUTPATIENT DIAGNOSTIC PROFESSIONAL PAYMENT LEVEL              GBH1PGM 
01253       MOVE '004150' TO WS-RULE                                    GBH1PGM 
01254       PERFORM 4100-PROC-LOOP THRU 4100-EXIT.                      GBH1PGM 
01255       IF RULE-DONE                                                GBH1PGM 
01256          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01257       END-IF.                                                     GBH1PGM 
01258                                                                   GBH1PGM 
01259 *=> PER ADMISSION DEDUCTIBLE                                      GBH1PGM 
01260       MOVE '004110' TO WS-RULE                                    GBH1PGM 
01261       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01262       IF RULE-DONE                                                GBH1PGM 
01263          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01264          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01265       END-IF.                                                     GBH1PGM 
01266                                                                   GBH1PGM 
01267 *=> PER ADMISSION DEDUCTIBLE MAXIMUM                              GBH1PGM 
01268       MOVE '004115' TO WS-RULE                                    GBH1PGM 
01269       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01270       IF RULE-DONE                                                GBH1PGM 
01271          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01272          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01273       END-IF.                                                     GBH1PGM 
01274                                                                   GBH1PGM 
01267 *=> ER DEDUCTIBLE FOR OK                                          GBH1PGM 
01268       MOVE '004116' TO WS-RULE                                    GBH1PGM 
01269       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01270       IF RULE-DONE                                                GBH1PGM 
01271          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01272          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01273       END-IF.                                                     GBH1PGM 
01274                                                                   GBH1PGM 
  275 *=> OFFICE VISIT COPAY                                            GBH1PGM 
01276       MOVE '004080' TO WS-RULE                                    GBH1PGM 
01277       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01278       IF RULE-DONE                                                GBH1PGM 
01279          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01280          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01281          MOVE 'Y' TO WS-OFFC-COPAY-FD                             GBH1PGM 
01  2       END-IF.                                                     GBH1PGM 
                                                                                
  275 *=> OUTPATIENT DEDUCTILBE FOR OK ALSO USED FOR COPAY              GBH1PGM 
01276       MOVE '004081' TO WS-RULE                                    GBH1PGM 
01277       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01278       IF RULE-DONE                                                GBH1PGM 
01279          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01280          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01281          MOVE 'Y' TO WS-OFFC-COPAY-FD                             GBH1PGM 
01  2       END-IF.                                                     GBH1PGM 
                                                                                
01275 *=> ACP OP HOSP ARTIERIOGRAMS, CT SCANS, MRI, EEG, MYELOGRAMS     GBH1PGM 
01275 *=> AND PET                                                       GBH1PGM 
01276       MOVE '004084' TO WS-RULE                                    GBH1PGM 
01277       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01278       IF RULE-DONE                                                GBH1PGM 
01279          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01280          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01281 *        MOVE 'Y' TO WS-OFFC-COPAY-FD                             GBH1PGM 
01282       END-IF.                                                     GBH1PGM 
01283 *=> OFFICE VISIT PAYMENT LEVEL                                    GBH1PGM 
01284       MOVE '004085' TO WS-RULE                                    GBH1PGM 
01285       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01286       IF RULE-DONE                                                GBH1PGM 
01287          PERFORM 4502-100-PERC-DFLT-INN THRU 4502-EXIT            GBH1PGM 
01288          PERFORM 5800-OFFC-COIN-OON     THRU 5800-EXIT            GBH1PGM 
01289       ELSE                                                        GBH1PGM 
01290          PERFORM 5700-OFFC-LVL-DEFAULT THRU 5700-EXIT             GBH1PGM 
01291       END-IF.                                                     GBH1PGM 
01292       PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT                  GBH1PGM 
01293 *=> OFFICE SURGERY PAYMENT LEVEL                                  GBH1PGM 
01294          MOVE '004086' TO WS-RULE                                 GBH1PGM 
01295          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01296          IF RULE-DONE                                             GBH1PGM 
01297             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01298          END-IF                                                   GBH1PGM 
01299                                                                   GBH1PGM 
01300 *=> PER ADMIT COPAY MAXIMUM PER CONFINEMENT                       GBH1PGM 
01301          MOVE '004098' TO WS-RULE                                 GBH1PGM 
01302          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01303          IF RULE-DONE                                             GBH1PGM 
01304             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01305          END-IF                                                   GBH1PGM 
01306                                                                   GBH1PGM 
01307 *=> PER ADMIT COPAY PER DAY ACCUM IDS                             GBH1PGM 
01308          MOVE '004097' TO WS-RULE                                 GBH1PGM 
01309          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01310          IF RULE-DONE                                             GBH1PGM 
01311             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01312          END-IF                                                   GBH1PGM 
01313                                                                   GBH1PGM 
01314 *=> COPAY - SPECIALIST OFFICE VISIT                               GBH1PGM 
01315       MOVE '004091' TO WS-RULE                                    GBH1PGM 
01316       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01317       IF RULE-DONE                                                GBH1PGM 
01318          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01319          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01320       END-IF.                                                     GBH1PGM 
01321 *=> COPAY - OUTPATIENT SURGERY                                    GBH1PGM 
01322       MOVE '004092' TO WS-RULE                                    GBH1PGM 
01323       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01324       IF RULE-DONE                                                GBH1PGM 
01325          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01326          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01327       END-IF.                                                     GBH1PGM 
01328 *=> COPAY - OUTPATIENT MENTAL HEALTH AND SUBSTANCE ABUSE          GBH1PGM 
01329       MOVE '004093' TO WS-RULE                                    GBH1PGM 
01330       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01331       IF RULE-DONE                                                GBH1PGM 
01332          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01333          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01334       END-IF.                                                     GBH1PGM 
01335 *=> COPAY - URGENT CARE FACILITY                                  GBH1PGM 
01336       MOVE '004094' TO WS-RULE                                    GBH1PGM 
01337       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01338       IF RULE-DONE                                                GBH1PGM 
01339          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01340          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01341       END-IF.                                                     GBH1PGM 
01342 *=> COPAY - OUTPATIENT HOSPITAL                                   GBH1PGM 
01343       MOVE '004095' TO WS-RULE                                    GBH1PGM 
01344       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01345       IF RULE-DONE                                                GBH1PGM 
01346          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01347          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01348       END-IF.                                                     GBH1PGM 
01349 *=> COPAY - URGENT CARE PROFESSIONAL                              GBH1PGM 
01350       MOVE '004096' TO WS-RULE                                    GBH1PGM 
01351       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01352       IF RULE-DONE                                                GBH1PGM 
01353          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01354          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01355       END-IF.                                                     GBH1PGM 
01356                                                                   GBH1PGM 
01357 *=> WELL CARE COPAY                                               GBH1PGM 
01358       MOVE '004090' TO WS-RULE                                    GBH1PGM 
01359       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01360       IF RULE-DONE                                                GBH1PGM 
01361          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01362          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01363       END-IF.                                                     GBH1PGM 
01364 *=> WELL CARE PAYMENT LEVEL                                       GBH1PGM 
01365       MOVE '004300' TO WS-RULE                                    GBH1PGM 
01366       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01367       IF RULE-DONE                                                GBH1PGM 
01368          PERFORM 4502-100-PERC-DFLT-INN THRU 4502-EXIT            GBH1PGM 
01369          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01370       END-IF.                                                     GBH1PGM 
01371 *=> WELL ADULT CARE PAYMENT LEVEL                                 GBH1PGM 
01372       MOVE '004305' TO WS-RULE                                    GBH1PGM 
01373       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01374       IF RULE-DONE                                                GBH1PGM 
01375          PERFORM 4502-100-PERC-DFLT-INN THRU 4502-EXIT            GBH1PGM 
01376          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01377       END-IF.                                                     GBH1PGM 
01378 *=> WELL ADULT CARE MAXIMUM                                       GBH1PGM 
01379       MOVE '004310' TO WS-RULE                                    GBH1PGM 
01380       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01381       IF RULE-DONE                                                GBH1PGM 
01382          PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT             GBH1PGM 
01383          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01384       END-IF.                                                     GBH1PGM 
01385 *=> WELL ADULT CARE BENEFIT PERIOD MAXIMUM                        GBH1PGM 
01386       MOVE '004315' TO WS-RULE                                    GBH1PGM 
01387       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01388       IF RULE-DONE                                                GBH1PGM 
01389          PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT             GBH1PGM 
01390          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01391       END-IF.                                                     GBH1PGM 
01392 *=> WELL CHILD CARE PAYMENT LEVEL                                 GBH1PGM 
01393       MOVE '004325' TO WS-RULE                                    GBH1PGM 
01394       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01395       IF RULE-DONE                                                GBH1PGM 
01396          PERFORM 4502-100-PERC-DFLT-INN THRU 4502-EXIT            GBH1PGM 
01397          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01398       END-IF.                                                     GBH1PGM 
01399 *=> WELL CHILD CARE MAXIMUM                                       GBH1PGM 
01400       MOVE '004320' TO WS-RULE                                    GBH1PGM 
01401       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01402       IF RULE-DONE                                                GBH1PGM 
01403          PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT             GBH1PGM 
01404          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01405       END-IF.                                                     GBH1PGM 
01406                                                                   GBH1PGM 
01407                                                                   GBH1PGM 
01408 *=> EMERGENCY ROOM COPAY                                          GBH1PGM 
01409       MOVE '004070' TO WS-RULE                                    GBH1PGM 
01410       PERFORM 4100-PROC-LOOP THRU 4100-EXIT                       GBH1PGM 
01411       IF RULE-DONE                                                GBH1PGM 
01412          PERFORM 4501-ZERO-DLR-DFLT THRU 4501-EXIT                GBH1PGM 
01413          PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT               GBH1PGM 
01414       END-IF.                                                     GBH1PGM 
01415 *=> EMERGENCY ACCIDENT CARE HOSPITAL PAYMENT LEVEL                GBH1PGM 
01416          MOVE '004160' TO WS-RULE                                 GBH1PGM 
01417          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01418          IF RULE-DONE                                             GBH1PGM 
01419             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01420          END-IF                                                   GBH1PGM 
01421 *=> OUTPATIENT SURGERY PROFESSIONAL PAYMENT LEVEL                 GBH1PGM 
01422          MOVE '004170' TO WS-RULE                                 GBH1PGM 
01423          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01424          IF RULE-DONE                                             GBH1PGM 
01425             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01426          END-IF                                                   GBH1PGM 
01427 *=> EAC B/C AND B/S PAYMENT LEVEL                                 GBH1PGM 
01428          MOVE '004175' TO WS-RULE                                 GBH1PGM 
01429          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01430          IF RULE-DONE                                             GBH1PGM 
01431             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01432          END-IF                                                   GBH1PGM 
01433 *=> EMERGENCY MEDICAL CARE HOSPITAL PAYMENT LEVEL                 GBH1PGM 
01434          MOVE '004180' TO WS-RULE                                 GBH1PGM 
01435          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01436          IF RULE-DONE                                             GBH1PGM 
01437             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01438          END-IF                                                   GBH1PGM 
01439 *=> EMERGENCY MEDICAL CARE PROFESSIONAL PAYMENT LEVEL             GBH1PGM 
01440          MOVE '004190' TO WS-RULE                                 GBH1PGM 
01441          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01442          IF RULE-DONE                                             GBH1PGM 
01443             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01444          END-IF                                                   GBH1PGM 
01445 *=> EMC B/C AND B/S PAYMENT LEVEL                                 GBH1PGM 
01446          MOVE '004195' TO WS-RULE                                 GBH1PGM 
01447          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01448          IF RULE-DONE                                             GBH1PGM 
01449             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01450          END-IF                                                   GBH1PGM 
01451 *=> EAC/EMC B/C PAYMENT LEVEL                                     GBH1PGM 
01452          MOVE '004196' TO WS-RULE                                 GBH1PGM 
01453          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01454          IF RULE-DONE                                             GBH1PGM 
01455             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01456          END-IF                                                   GBH1PGM 
01457 *=> EAC/EMC B/S PAYMENT LEVEL                                     GBH1PGM 
01458          MOVE '004197' TO WS-RULE                                 GBH1PGM 
01459          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01460          IF RULE-DONE                                             GBH1PGM 
01461             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01462          END-IF                                                   GBH1PGM 
01463 *=> EAC/EMC B/C AND B/S PAYMENT LEVEL                             GBH1PGM 
01464          MOVE '004198' TO WS-RULE                                 GBH1PGM 
01465          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01466          IF RULE-DONE                                             GBH1PGM 
01467             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01468          END-IF                                                   GBH1PGM 
01469 *=> SUPPLEMENTAL ACCIDENT CARE (90 DAYS MAXIMUM)                  GBH1PGM 
01470          MOVE '004200' TO WS-RULE                                 GBH1PGM 
01471          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01472          IF RULE-DONE                                             GBH1PGM 
01473             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01474          END-IF                                                   GBH1PGM 
01475                                                                   GBH1PGM 
01476 *=> OTHER COVERED SERVICES PAYMENT LEVEL                          GBH1PGM 
01477          MOVE '004330' TO WS-RULE                                 GBH1PGM 
01478          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01479          IF RULE-DONE                                             GBH1PGM 
01480             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01481          END-IF                                                   GBH1PGM 
01482 *=> NON PLAN PAYMENT LEVEL                                        GBH1PGM 
01483          MOVE '004380' TO WS-RULE                                 GBH1PGM 
01484          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01485          IF RULE-DONE                                             GBH1PGM 
01486             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01487          END-IF                                                   GBH1PGM 
01488                                                                   GBH1PGM 
01489 *---MISC MAXES---------------------------------------             GBH1PGM 
01490                                                                   GBH1PGM 
01491 *=> THERAPY MAXIMUM COMBINED (PT, OT, ST)                         GBH1PGM 
01492          MOVE '004220' TO WS-RULE                                 GBH1PGM 
01493          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01494          IF RULE-DONE                                             GBH1PGM 
01495             PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT          GBH1PGM 
01496             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01497          END-IF                                                   GBH1PGM 
01498 *=> FUNCTIONAL OCCUPATIONAL THERAPY MAXIMUM                       GBH1PGM 
01499 *GF 9/12 MOVE '004230' TO WS-RULE                                 GBH1PGM 
01500 *        PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01501 *        IF RULE-DONE                                             GBH1PGM 
01502 *           PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT          GBH1PGM 
01503 *           PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01504 *        END-IF                                                   GBH1PGM 
01505 *=> PHYSICAL / MECHANO THERAPY MAXIMUM                            GBH1PGM 
01506 *        MOVE '004240' TO WS-RULE                                 GBH1PGM 
01507 *        PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01508 *        IF RULE-DONE                                             GBH1PGM 
01509 *           PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT          GBH1PGM 
01510 *           PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01511 *        END-IF                                                   GBH1PGM 
01512 *=> SPEECH THERAPY MAXIMUM                                        GBH1PGM 
01513 *        MOVE '004250' TO WS-RULE                                 GBH1PGM 
01514 *        PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01515 *        IF RULE-DONE                                             GBH1PGM 
01516 *           PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT          GBH1PGM 
01517 *           PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01518 *GF 9/12 END-IF                                                   GBH1PGM 
01519 *=> TMJ LIFETIME MAXIMUM                                          GBH1PGM 
01520          MOVE '004260' TO WS-RULE                                 GBH1PGM 
01521          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01522          IF RULE-DONE                                             GBH1PGM 
01523             PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT          GBH1PGM 
01524             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01525          END-IF                                                   GBH1PGM 
01526 *=> PRIVATE DUTY NURSING MAXIMUM                                  GBH1PGM 
01527          MOVE '004270' TO WS-RULE                                 GBH1PGM 
01528          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01529          IF RULE-DONE                                             GBH1PGM 
01530             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01531          END-IF                                                   GBH1PGM 
01532 *=> SKILLED NURSING BENEFIT PERIOD MAXIMUM                        GBH1PGM 
01533          MOVE '004275' TO WS-RULE                                 GBH1PGM 
01534          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01535          IF RULE-DONE                                             GBH1PGM 
01536             PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT          GBH1PGM 
01537             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01538          END-IF                                                   GBH1PGM 
01539 *=> CHIROPRACTIC SERVICES MAXIMUM                                 GBH1PGM 
01540 *        MOVE '004280' TO WS-RULE                                 GBH1PGM 
01541 *        PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01542 *        IF RULE-DONE                                             GBH1PGM 
01543 *           PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT          GBH1PGM 
01544 *           PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01545 *        END-IF                                                   GBH1PGM 
01546 *=> CHIROPRACTOR PROVIDER MAXIMUM                                 GBH1PGM 
01547          MOVE '004290' TO WS-RULE                                 GBH1PGM 
01548          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01549          IF RULE-DONE                                             GBH1PGM 
01550             PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT          GBH1PGM 
01551             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01552          END-IF                                                   GBH1PGM 
01553 *                                                                 GBH1PGM 
01554 *=> HEARING AID BENEFIT PERIOD MAXIMUM                            GBH1PGM 
01555          MOVE '004370' TO WS-RULE                                 GBH1PGM 
01556          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01557          IF RULE-DONE                                             GBH1PGM 
01558             PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT          GBH1PGM 
01559             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01560          END-IF                                                   GBH1PGM 
01561 *=> CONTACT LENSES BENEFIT PERIOD MAXIMUM                         GBH1PGM 
01562          MOVE '004390' TO WS-RULE                                 GBH1PGM 
01563          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01564          IF RULE-DONE                                             GBH1PGM 
01565             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01566          END-IF                                                   GBH1PGM 
01567 *=> FRAMES BENEFIT PERIOD MAXIMUM                                 GBH1PGM 
01568          MOVE '004392' TO WS-RULE                                 GBH1PGM 
01569          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01570          IF RULE-DONE                                             GBH1PGM 
01571             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01572          END-IF                                                   GBH1PGM 
01573 *=> VISION EXAM BENEFIT PERIOD MAXIMUM                            GBH1PGM 
01574          MOVE '004394' TO WS-RULE                                 GBH1PGM 
01575          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01576          IF RULE-DONE                                             GBH1PGM 
01577             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01578          END-IF                                                   GBH1PGM 
01579 *=> LENSES BENEFIT PERIOD MAXIMUM                                 GBH1PGM 
01580          MOVE '004396' TO WS-RULE                                 GBH1PGM 
01581          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01582          IF RULE-DONE                                             GBH1PGM 
01583             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01584          END-IF                                                   GBH1PGM 
01585 *=> VISION HARDWARE BENEFIT PERIOD MAXIMUM                        GBH1PGM 
01586          MOVE '004396' TO WS-RULE                                 GBH1PGM 
01587          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01588          IF RULE-DONE                                             GBH1PGM 
01589             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01590          END-IF                                                   GBH1PGM 
01591 *=> HOSPICE CARE BENEFIT PERIOD MAXIMUM                           GBH1PGM 
01592          MOVE '005040' TO WS-RULE                                 GBH1PGM 
01593          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01594          IF RULE-DONE                                             GBH1PGM 
01595             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01596             PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT          GBH1PGM 
01597          END-IF                                                   GBH1PGM 
01598 *=> COORDINATED HOME CARE BENEFIT PERIOD MAXIMUM                  GBH1PGM 
01599          MOVE '005050' TO WS-RULE                                 GBH1PGM 
01600          PERFORM 4100-PROC-LOOP THRU 4100-EXIT                    GBH1PGM 
01601          IF RULE-DONE                                             GBH1PGM 
01602             PERFORM 4503-INN-MAX-NA-DFLT  THRU 4503-EXIT          GBH1PGM 
01603             PERFORM 4500-INDEX-AND-UTIL THRU 4500-EXIT            GBH1PGM 
01604          END-IF.                                                  GBH1PGM 
01605                                                                   GBH1PGM 
01606                                                                   GBH1PGM 
01607                                                                   GBH1PGM 
01608                                                                   GBH1PGM 
01609  4000-EXIT.                                                       GBH1PGM 
01610      EXIT.                                                        GBH1PGM 
01611                                                                   GBH1PGM 
01612 *P4100-PROC-LOOP.                                                 GBH1PGM 
01613  4100-PROC-LOOP.                                                  GBH1PGM 
01614                                                                   GBH1PGM 
01615       MOVE  0       TO WS-ACCUM-ID-CT                             GBH1PGM 
01616       MOVE  +0      TO WS-PK-PERCENT                              GBH1PGM 
01617       MOVE 'N'      TO WS-RULE-DONE-SW                            GBH1PGM 
01618       MOVE 'N'      TO WS-RULE-FD-SW                              GBH1PGM 
01619       MOVE 'N' TO WS-OVERALL-FD-SW                                GBH1PGM 
01620       MOVE 'N' TO WS-IN-NET-FD-SW                                 GBH1PGM 
01621       MOVE 'N' TO WS-OUT-NET-FD-SW                                GBH1PGM 
01622       MOVE 'N'      TO WS-SKIP-RULE-SW                            GBH1PGM 
01623                                                                   GBH1PGM 
01624       PERFORM 4200-SEARCH-RULE THRU 4200-EXIT                     GBH1PGM 
01625       IF RULE-FD                                                  GBH1PGM 
01626          CONTINUE                                                 GBH1PGM 
01627       ELSE                                                        GBH1PGM 
01628          GO TO 4100-END-BROWSE                                    GBH1PGM 
01629       END-IF                                                      GBH1PGM 
01630                                                                   GBH1PGM 
01631       PERFORM 5000-PROC-ACCUM-ID THRU 5000-EXIT                   GBH1PGM 
01632          VARYING GCBHA-TYPE-X FROM 1 BY 1                         GBH1PGM 
01633            UNTIL GCBHA-TYPE-X > GCBHA-TYPE-CTR                    GBH1PGM 
01634               OR RULE-DONE.                                       GBH1PGM 
01635                                                                   GBH1PGM 
01636       IF SKIP-RULE                                                GBH1PGM 
01637          GO TO 4100-END-BROWSE                                    GBH1PGM 
01638       END-IF                                                      GBH1PGM 
01639                                                                   GBH1PGM 
01640       IF (GCBHA-TYPE-X > GCBHA-TYPE-CTR)                          GBH1PGM 
01641           AND (WS-ACCUM-ID-CT > 0)                                GBH1PGM 
01642          MOVE 'Y' TO WS-RULE-DONE-SW                              GBH1PGM 
01643       END-IF                                                      GBH1PGM 
01644                                                                   GBH1PGM 
01645       IF RULE-DONE                                                GBH1PGM 
01646          MOVE GCBHA-RULE-DESC-1                                   GBH1PGM 
01647               TO GCBHS-BENEFIT-TITLE(GCBHS-BEN-IDX)               GBH1PGM 
01648       END-IF.                                                     GBH1PGM 
01649                                                                   GBH1PGM 
01650                                                                   GBH1PGM 
01651                                                                   GBH1PGM 
01652  4100-END-BROWSE.                                                 GBH1PGM 
01653                                                                   GBH1PGM 
01654                                                                   GBH1PGM 
01655      PERFORM 4310-END-BROWSE-ACMRULE-FILE THRU 4310-EXIT.         GBH1PGM 
01656                                                                   GBH1PGM 
01657                                                                   GBH1PGM 
01658                                                                   GBH1PGM 
01659  4100-EXIT.                                                       GBH1PGM 
01660      EXIT.                                                        GBH1PGM 
01661                                                                   GBH1PGM 
01662                                                                   GBH1PGM 
01663                                                                   GBH1PGM 
01664  4200-SEARCH-RULE.                                                GBH1PGM 
01665                                                                   GBH1PGM 
01666 **== ACCUM ID RULE NUMBER                                         GBH1PGM 
01667      MOVE WS-RULE             TO  GCBHA-RULE-KEY                  GBH1PGM 
01668      MOVE +0                  TO  WS-ACMRULE-RESP1                GBH1PGM 
01669                                                                   GBH1PGM 
01670      PERFORM 4300-BROWSE-ACMRULE-FILE                             GBH1PGM 
01671                             THRU 4300-EXIT.                       GBH1PGM 
01672                                                                   GBH1PGM 
01673 **== EVALUATE ACMRULE RESPONSE CODE AFTER STARTBROWSE             GBH1PGM 
01674      IF WS-ACMRULE-RESP1 = DFHRESP(NOTFND)                        GBH1PGM 
01675 *        MOVE 'Y' TO WS-ERROR-FOUND-SW                            GBH1PGM 
01676 *        SET WS-ERR-IDX TO +01                                    GBH1PGM 
01677 *        PERFORM 9000-800-RETURN THRU 9000-800-EXIT               GBH1PGM 
01678          GO TO  4200-EXIT                                         GBH1PGM 
01679      ELSE                                                         GBH1PGM 
01680         PERFORM 4400-READ-ACMRULE-FILE THRU 4400-EXIT             GBH1PGM 
01681      END-IF.                                                      GBH1PGM 
01682                                                                   GBH1PGM 
01683 **== EVALUATE ACMRULE RESPONSE CODE AFTER READNEXT                GBH1PGM 
01684 *    EVALUATE  WS-ACMRULE-RESP1                                   GBH1PGM 
01685 *       WHEN DFHRESP(NORMAL)                                      GBH1PGM 
01686 *         CONTINUE                                                GBH1PGM 
01687 *       WHEN OTHER                                                GBH1PGM 
01688 *         MOVE 'Y' TO WS-ERROR-FOUND-SW                           GBH1PGM 
01689 *         SET WS-ERR-IDX TO +02                                   GBH1PGM 
01690 *         PERFORM 9000-800-RETURN THRU 9000-800-EXIT              GBH1PGM 
01691 *    END-EVALUATE.                                                GBH1PGM 
01692                                                                   GBH1PGM 
01693                                                                   GBH1PGM 
01694  4200-EXIT.                                                       GBH1PGM 
01695      EXIT.                                                        GBH1PGM 
01696                                                                   GBH1PGM 
01697                                                                   GBH1PGM 
01698 ***************************************************************   GBH1PGM 
01699 * STARTBROWSE ON THE ACCUM RULE VSAM FILE                         GBH1PGM 
01700 ***************************************************************   GBH1PGM 
01701  4300-BROWSE-ACMRULE-FILE.                                        GBH1PGM 
01702                                                                   GBH1PGM 
01703      EXEC CICS STARTBR GTEQ                                       GBH1PGM 
01704                DATASET ('GCACMRUL')                               GBH1PGM 
01705                RIDFLD  (GCBHA-RULE-KEY)                           GBH1PGM 
01706                RESP    (WS-ACMRULE-RESP1)                         GBH1PGM 
01707                END-EXEC.                                          GBH1PGM 
01708                                                                   GBH1PGM 
01709                                                                   GBH1PGM 
01710  4300-EXIT.                                                       GBH1PGM 
01711      EXIT.                                                        GBH1PGM 
01712 /                                                                 GBH1PGM 
01713 ***************************************************************   GBH1PGM 
01714 *   ENDBROWSE ON THE ACCUM RULE VSAM FILE                         GBH1PGM 
01715 ***************************************************************   GBH1PGM 
01716  4310-END-BROWSE-ACMRULE-FILE.                                    GBH1PGM 
01717                                                                   GBH1PGM 
01718      EXEC CICS                                                    GBH1PGM 
01719                ENDBR DATASET ('GCACMRUL')                         GBH1PGM 
01720                RESP    (WS-ACMRULE-RESP1)                         GBH1PGM 
01721      END-EXEC.                                                    GBH1PGM 
01722                                                                   GBH1PGM 
01723                                                                   GBH1PGM 
01724  4310-EXIT.                                                       GBH1PGM 
01725      EXIT.                                                        GBH1PGM 
01726 /                                                                 GBH1PGM 
01727 ************************************************************      GBH1PGM 
01728 *                                                          *      GBH1PGM 
01729 *                                                          *      GBH1PGM 
01730 ************************************************************      GBH1PGM 
01731  4400-READ-ACMRULE-FILE.                                          GBH1PGM 
01732                                                                   GBH1PGM 
01733      EXEC CICS READNEXT                                           GBH1PGM 
01734                DATASET ('GCACMRUL')                               GBH1PGM 
01735                INTO    (WS-ACCMRULE-AREA)                         GBH1PGM 
01736                RIDFLD  (GCBHA-RULE-KEY)                           GBH1PGM 
01737                RESP    (WS-ACMRULE-RESP1)                         GBH1PGM 
01738                END-EXEC.                                          GBH1PGM 
01739                                                                   GBH1PGM 
01740      IF  WS-RULE   =  GCBHA-RULE-KEY                              GBH1PGM 
01741          MOVE 'Y' TO WS-RULE-FD-SW                                GBH1PGM 
01742       END-IF.                                                     GBH1PGM 
01743                                                                   GBH1PGM 
01744                                                                   GBH1PGM 
01745  4400-EXIT.                                                       GBH1PGM 
01746      EXIT.                                                        GBH1PGM 
01747                                                                   GBH1PGM 
01748                                                                   GBH1PGM 
01749 *P4500-                                                           GBH1PGM 
01750  4500-INDEX-AND-UTIL.                                             GBH1PGM 
01751                                                                   GBH1PGM 
01752 *     IF RULE-DONE                                                GBH1PGM 
01753 ****     PERFORM 5400-PERF-UTZ  THRU 5400-EXIT                    GBH1PGM 
01754                                                                   GBH1PGM 
01755          SET GCBHS-BEN-IDX UP BY 1                                GBH1PGM 
01756          ADD +1 TO GCBHS-BEN-POINTERS.                            GBH1PGM 
01757 *     END-IF.                                                     GBH1PGM 
01758                                                                   GBH1PGM 
01759                                                                   GBH1PGM 
01760      IF GCBHS-BEN-POINTERS = +50                                  GBH1PGM 
01761         GO TO 0000-RETURN                                         GBH1PGM 
01762      END-IF.                                                      GBH1PGM 
01763                                                                   GBH1PGM 
01764      PERFORM 4505-SECOND-VAL  THRU  4505-EXIT.                    GBH1PGM 
01765                                                                   GBH1PGM 
01766                                                                   GBH1PGM 
01767                                                                   GBH1PGM 
01768                                                                   GBH1PGM 
01769                                                                   GBH1PGM 
01770  4500-EXIT.                                                       GBH1PGM 
01771      EXIT.                                                        GBH1PGM 
01772                                                                   GBH1PGM 
01773                                                                   GBH1PGM 
01774 *P4501-                                                           GBH1PGM 
01775  4501-ZERO-DLR-DFLT.                                              GBH1PGM 
01776                                                                   GBH1PGM 
01777       IF OVERALL-NOT-FD                                           GBH1PGM 
01778          IF OUT-NET-FD AND IN-NET-NOT-FD                          GBH1PGM 
01779             IF WS-SAVE-VAL-QUAL = '5'                             GBH1PGM 
01780 *JP 7/28/05    MOVE WS-ZERO-DOLLARS         TO                    GBH1PGM 
01781                MOVE WS-NOT-APPLIC           TO                    GBH1PGM 
01782                GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)          GBH1PGM 
01783             ELSE                                                  GBH1PGM 
01784                MOVE WS-NOT-APPLIC           TO                    GBH1PGM 
01785                GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)          GBH1PGM 
01786             END-IF                                                GBH1PGM 
01787          END-IF                                                   GBH1PGM 
01788          IF IN-NET-FD AND OUT-NET-NOT-FD                          GBH1PGM 
01789             MOVE WS-NOT-APPLIC           TO                       GBH1PGM 
01790             GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 2, 1)             GBH1PGM 
01791          END-IF                                                   GBH1PGM 
01792       END-IF.                                                     GBH1PGM 
01793                                                                   GBH1PGM 
01794                                                                   GBH1PGM 
01795  4501-EXIT.                                                       GBH1PGM 
01796      EXIT.                                                        GBH1PGM 
01797                                                                   GBH1PGM 
01798                                                                   GBH1PGM 
01799 *P4502-                                                           GBH1PGM 
01800  4502-100-PERC-DFLT-INN.                                          GBH1PGM 
01801                                                                   GBH1PGM 
01802       IF OVERALL-NOT-FD AND OUT-NET-FD AND IN-NET-NOT-FD          GBH1PGM 
01803 *JP 7/28 MOVE WS-100-PERCENT          TO                          GBH1PGM 
01804          MOVE WS-NOT-APPLIC           TO                          GBH1PGM 
01805          GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)                GBH1PGM 
01806          IF WS-RULE = '004210'                                    GBH1PGM 
01807 *JP 7/28    MOVE WS-100-PERCENT TO WS-MSPI-SAVE-VAL               GBH1PGM 
01808             MOVE WS-NOT-APPLIC  TO WS-MSPI-SAVE-VAL               GBH1PGM 
01809          END-IF                                                   GBH1PGM 
01810       END-IF.                                                     GBH1PGM 
01811                                                                   GBH1PGM 
01812  4502-EXIT.                                                       GBH1PGM 
01813      EXIT.                                                        GBH1PGM 
01814                                                                   GBH1PGM 
01815                                                                   GBH1PGM 
01816 *P4503-                                                           GBH1PGM 
01817  4503-INN-MAX-NA-DFLT.                                            GBH1PGM 
01818                                                                   GBH1PGM 
01819       IF OVERALL-NOT-FD                                           GBH1PGM 
01820          IF OUT-NET-FD AND IN-NET-NOT-FD                          GBH1PGM 
01821             MOVE WS-NOT-APPLIC           TO                       GBH1PGM 
01822             GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)             GBH1PGM 
01823          END-IF                                                   GBH1PGM 
01824       END-IF.                                                     GBH1PGM 
01825                                                                   GBH1PGM 
01826                                                                   GBH1PGM 
01827  4503-EXIT.                                                       GBH1PGM 
01828      EXIT.                                                        GBH1PGM 
01829                                                                   GBH1PGM 
01830                                                                   GBH1PGM 
01831                                                                   GBH1PGM 
01832                                                                   GBH1PGM 
01833  4505-SECOND-VAL.                                                 GBH1PGM 
01834                                                                   GBH1PGM 
01835                                                                   GBH1PGM 
01836      IF (GCBHA-ACCUM-TYPE (1) = '#ABM  ')   AND                   GBH1PGM 
01837         OVERALL-FD AND  (GCBHA-ID-CTR > 1)                        GBH1PGM 
01838         CONTINUE                                                  GBH1PGM 
01839      ELSE                                                         GBH1PGM 
01840         GO TO 4505-EXIT                                           GBH1PGM 
01841      END-IF                                                       GBH1PGM 
01842                                                                   GBH1PGM 
01843                                                                   GBH1PGM 
01844       MOVE  0       TO WS-ACCUM-ID-CT                             GBH1PGM 
01845       MOVE 'N'      TO WS-RULE-DONE-SW                            GBH1PGM 
01846                                                                   GBH1PGM 
01847       PERFORM VARYING GCBHA-ID-X FROM 2 BY 1                      GBH1PGM 
01848 **      UNTIL (GCBHA-ID-X > GCBHA-ID-CTR)                         GBH1PGM 
01849         UNTIL (GCBHA-ID-X > 3)                                    GBH1PGM 
01850           IF GCBHA-ACCUM-ID (GCBHA-ID-X) NOT = SPACES             GBH1PGM 
01851              IF WS-CON-ABM-FOUND                                  GBH1PGM 
01852                 MOVE WS-CON-ABM-HOLD TO GAA-RECORD                GBH1PGM 
01853                 PERFORM 5101-ABM-INNER-LOOP THRU 5101-EXIT        GBH1PGM 
01854              END-IF                                               GBH1PGM 
01855              IF WS-GRP-ABM-FOUND                                  GBH1PGM 
01856                 MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                GBH1PGM 
01857                 PERFORM 5101-ABM-INNER-LOOP THRU 5101-EXIT        GBH1PGM 
01858              END-IF                                               GBH1PGM 
01859           END-IF                                                  GBH1PGM 
01860       END-PERFORM.                                                GBH1PGM 
01861                                                                   GBH1PGM 
01862       IF WS-ACCUM-ID-CT > 0                                       GBH1PGM 
01863          IF OUT-NET-FD AND IN-NET-NOT-FD                          GBH1PGM 
01864             MOVE WS-NOT-APPLIC           TO                       GBH1PGM 
01865             GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)             GBH1PGM 
01866          END-IF                                                   GBH1PGM 
01867          MOVE GCBHA-RULE-DESC-1                                   GBH1PGM 
01868               TO GCBHS-BENEFIT-TITLE(GCBHS-BEN-IDX)               GBH1PGM 
01869 ****     PERFORM 5400-PERF-UTZ  THRU 5400-EXIT                    GBH1PGM 
01870                                                                   GBH1PGM 
01871          SET GCBHS-BEN-IDX UP BY 1                                GBH1PGM 
01872          ADD +1 TO GCBHS-BEN-POINTERS                             GBH1PGM 
01873       END-IF.                                                     GBH1PGM 
01874                                                                   GBH1PGM 
01875                                                                   GBH1PGM 
01876      IF GCBHS-BEN-POINTERS = +50                                  GBH1PGM 
01877         GO TO 0000-RETURN                                         GBH1PGM 
01878      END-IF.                                                      GBH1PGM 
01879                                                                   GBH1PGM 
01880                                                                   GBH1PGM 
01881  4505-EXIT.                                                       GBH1PGM 
01882      EXIT.                                                        GBH1PGM 
01883                                                                   GBH1PGM 
01884                                                                   GBH1PGM 
01885 *P4506-                                                           GBH1PGM 
01886  4506-ZERO-DLR-DFLT-MSA.                                          GBH1PGM 
01887                                                                   GBH1PGM 
01888       IF OVERALL-NOT-FD                                           GBH1PGM 
01889          IF OUT-NET-FD AND IN-NET-NOT-FD                          GBH1PGM 
01890             IF WS-TEXAS-CICS-REGION                               GBH1PGM 
01891                MOVE 'NO PENALTY IN-NETWK'   TO                    GBH1PGM 
01892                GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)          GBH1PGM 
01893             ELSE                                                  GBH1PGM 
01894                IF WS-SAVE-VAL-QUAL = '5'                          GBH1PGM 
01895 *******           MOVE WS-ZERO-DOLLARS         TO                 GBH1PGM 
01896                   MOVE WS-NOT-APPLIC           TO                 GBH1PGM 
01897                   GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)       GBH1PGM 
01898                ELSE                                               GBH1PGM 
01899                  MOVE WS-NOT-APPLIC           TO                  GBH1PGM 
01900                  GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)        GBH1PGM 
01901                END-IF                                             GBH1PGM 
01902             END-IF                                                GBH1PGM 
01903          END-IF                                                   GBH1PGM 
01904          IF IN-NET-FD AND OUT-NET-NOT-FD                          GBH1PGM 
01905             MOVE WS-NOT-APPLIC           TO                       GBH1PGM 
01906             GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 2, 1)             GBH1PGM 
01907          END-IF                                                   GBH1PGM 
01908       END-IF.                                                     GBH1PGM 
01909                                                                   GBH1PGM 
01910 **TEXAS - EVEN IF MSA OVERALL CODED, NO PENALTY ASSESSED INN      GBH1PGM 
01911 **      - FLIP OVERALL TO OON, ADD MSG TO INN                     GBH1PGM 
01912       IF OVERALL-FD AND WS-TEXAS-CICS-REGION                      GBH1PGM 
01913          MOVE GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1) TO        GBH1PGM 
01914            GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 2, 1)              GBH1PGM 
01915          MOVE 'NO PENALTY IN-NETWK'   TO                          GBH1PGM 
01916            GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)              GBH1PGM 
01917          MOVE 'N' TO GCBHS-BEN-COMB-FLAG (GCBHS-BEN-IDX)          GBH1PGM 
01918       END-IF.                                                     GBH1PGM 
01919                                                                   GBH1PGM 
01920                                                                   GBH1PGM 
01921                                                                   GBH1PGM 
01922  4506-EXIT.                                                       GBH1PGM 
01923      EXIT.                                                        GBH1PGM 
01924                                                                   GBH1PGM 
01925                                                                   GBH1PGM 
01926                                                                   GBH1PGM 
01927                                                                   GBH1PGM 
01928 *5300-SETUP-UTZ.                                                  GBH1PGM 
01929 *                                                                 GBH1PGM 
01930 *                                                                 GBH1PGM 
01931 *>>INITIALIZE GRBJPGM COMMAREA                                    GBH1PGM 
01932 *     MOVE SPACES   TO GRBJ-ACCUM-DATA                            GBH1PGM 
01933 *                                                                 GBH1PGM 
01934 *     MOVE GCG-ACCUM-PSEUDO-GRP-NBR TO GRBJ-PSEUDO-GROUP          GBH1PGM 
01935 *     MOVE GCG-ACCUM-PSEUDO-SECTION-NBR TO GRBJ-PSEUDO-SECTION    GBH1PGM 
01936 *     MOVE GRBF-SUBSCRIBER-NBR     TO  GRBJ-MEMBER-NUM            GBH1PGM 
01937 *     MOVE GRBF-PATIENT-LAST-NAME  TO  GRBJ-PATIENT-LAST-NAME     GBH1PGM 
01938 *     MOVE GRBF-PATIENT-FIRST-NAME TO GRBJ-PATIENT-FIRST-NAME     GBH1PGM 
01939 *     MOVE WS-PAT-BIRTH-DATE-CEN   TO GRBJ-PATIENT-BIRTH-DT       GBH1PGM 
01940 *     MOVE GRBF-PAT-RELATIONSHIP   TO GRBJ-PATIENT-REL            GBH1PGM 
01941 *     MOVE GRBF-GENDER             TO GRBJ-SEX                    GBH1PGM 
01942 *     MOVE '00'                    TO GRBJ-ERROR-CODE             GBH1PGM 
01943 *                                                                 GBH1PGM 
01944 ***   DATE OF SERVICE CONVERTED TO  => MMDDCCYY                   GBH1PGM 
01945 *     MOVE 'CNV'          TO MLDATE-FUNC                          GBH1PGM 
01946 *     MOVE 'Y'            TO MLDATE-FORM1                         GBH1PGM 
01947 *     MOVE GRBF-DATE-OF-SERVICE  TO MLDATE-DATE1                  GBH1PGM 
01948 *     MOVE 'M'            TO MLDATE-FORM2                         GBH1PGM 
01949 *     EXEC CICS LINK PROGRAM ('MLDATEC')                          GBH1PGM 
01950 *               COMMAREA(MLDATE01)                                GBH1PGM 
01951 *     END-EXEC.                                                   GBH1PGM 
01952 *     MOVE MLDATE-DATE2   TO GRBJ-SVC-DATE                        GBH1PGM 
01953 *                                                                 GBH1PGM 
01954 *     MOVE GRBF-CON-PLAN-CODE      TO GRBJ-CON-PLAN-CODE          GBH1PGM 
01955 *     MOVE GRBF-CON-GROUP-NUM      TO GRBJ-CON-GROUP-NUM          GBH1PGM 
01956 *     MOVE GRBF-CON-SECTION-NUM    TO GRBJ-CON-SECTION-NUM        GBH1PGM 
01957 *     MOVE GRBF-CON-PKG-CODE       TO GRBJ-CON-PKG-CODE           GBH1PGM 
01958 *     MOVE GRBF-CON-L-O-B          TO GRBJ-CON-L-O-B              GBH1PGM 
01959 *     MOVE GRBF-CON-PROVDR-CONTROL TO GRBJ-CON-PROVDR-CONTROL     GBH1PGM 
01960 *     MOVE GRBF-CON-FAM-REL-LVL    TO GRBJ-CON-FAM-REL-LVL        GBH1PGM 
01961 *     MOVE GRBF-CON-EFFDT-CEN      TO GRBJ-CON-EFFDT-CEN          GBH1PGM 
01962 *                                                                 GBH1PGM 
01963 *                                                                 GBH1PGM 
01964 *5300-EXIT.                                                       GBH1PGM 
01965 *    EXIT.                                                        GBH1PGM 
01966                                                                   GBH1PGM 
01967 *5400-PERF-UTZ.                                                   GBH1PGM 
01968 *                                                                 GBH1PGM 
01969 *     IF ACCUM-TYPE (GCBHA-TYPE-X, RULE-X) =                      GBH1PGM 
01970 *        '#ABM  ' OR '#ADL  ' OR '#AOL  '                         GBH1PGM 
01971 *        CONTINUE                                                 GBH1PGM 
01972 *     ELSE                                                        GBH1PGM 
01973 *        GO TO 5400-EXIT                                          GBH1PGM 
01974 *     END-IF                                                      GBH1PGM 
01975 *                                                                 GBH1PGM 
01976 *                                                                 GBH1PGM 
01977 *     EXEC CICS LINK PROGRAM ('GRBJPGM')                          GBH1PGM 
01978 *          COMMAREA(GRBJPGM-COMMAREA)                             GBH1PGM 
01979 *          LENGTH  (LENGTH OF GRBJPGM-COMMAREA)                   GBH1PGM 
01980 *     END-EXEC.                                                   GBH1PGM 
01981 *                                                                 GBH1PGM 
01982 *     IF GRBJ-ERROR-CODE NOT = '00'                               GBH1PGM 
01983 *        MOVE GRBJ-ERROR-DESCRIPTION                              GBH1PGM 
01984 *                  TO GRB8-ERROR-DESCRIPTION                      GBH1PGM 
01985 *        GO TO 5400-EXIT                                          GBH1PGM 
01986 *     END-IF.                                                     GBH1PGM 
01987 *                                                                 GBH1PGM 
01988 *     PERFORM VARYING GRBJ-INDEX       FROM 1 BY 1                GBH1PGM 
01989 *       UNTIL GRBJ-INDEX  > 2                                     GBH1PGM 
01990 *       SET GCBHS-COL-IDX     TO GRBJ-INDEX                       GBH1PGM 
01991 *       IF GRBJ-ACCUMULATED-AMT (GRBJ-INDEX, 1) NUMERIC           GBH1PGM 
01992 *          AND GRBJ-VALUE-LIMIT (GRBJ-INDEX) NUMERIC              GBH1PGM 
01993 *          PERFORM 5500-DISP-UTZ-AMTS   THRU 5500-EXIT            GBH1PGM 
01994 *       ELSE                                                      GBH1PGM 
01995 *          IF GRBF-ACCUM-ID (GRBF-DTL-IDX) = '#ACL  '             GBH1PGM 
01996 *             MOVE 'N/A                 '                         GBH1PGM 
01997 *               TO GRBF-ACCUM-AMT(GRBF-DTL-IDX)                   GBH1PGM 
01998 *             MOVE 'N/A                 '                         GBH1PGM 
01999 *               TO GRBF-REMAIN-AMT(GRBF-DTL-IDX)                  GBH1PGM 
02000 *          END-IF                                                 GBH1PGM 
02001 *       END-IF                                                    GBH1PGM 
02002 *     END-PERFORM.                                                GBH1PGM 
02003 *                                                                 GBH1PGM 
02004 *>>INITIALIZE GRBJPGM COMMAREA                                    GBH1PGM 
02005 *     MOVE SPACES   TO GRBJ-ACCUM-DATA.                           GBH1PGM 
02006 *                                                                 GBH1PGM 
02007 *5400-EXIT.                                                       GBH1PGM 
02008 *    EXIT.                                                        GBH1PGM 
02009                                                                   GBH1PGM 
02010 *5500-DISP-UTZ-AMTS.                                              GBH1PGM 
02011 *                                                                 GBH1PGM 
02012 *    IF (GRBF-ACCUM-ID (GRBF-DTL-IDX) = '#ACL  ' OR               GBH1PGM 
02013 *        '#ACP  ')   OR                                           GBH1PGM 
02014 *       (GRBF-ACCUM-SLOT( GRBF-DTL-IDX) = ZEROES)                 GBH1PGM 
02015 *         MOVE 'N/A                 '                             GBH1PGM 
02016 *           TO GRBF-ACCUM-AMT(GRBF-DTL-IDX)                       GBH1PGM 
02017 *         MOVE 'N/A                 '                             GBH1PGM 
02018 *           TO GRBF-REMAIN-AMT(GRBF-DTL-IDX)                      GBH1PGM 
02019 *       GO TO 5500-EXIT                                           GBH1PGM 
02020 *    END-IF                                                       GBH1PGM 
02021 *                                                                 GBH1PGM 
02022 *     EVALUATE GRBJ-VALUE-QUALIFIER (GRBJ-INDEX)                  GBH1PGM 
02023 *       WHEN '2'                                                  GBH1PGM 
02024 *         MOVE GRBJ-ACCUMULATED-AMT (GRBJ-INDEX, 1)               GBH1PGM 
02025 *                                  TO  WS-VALUE-LIMIT-V           GBH1PGM 
02026 *         IF GRBJ-ACCUMULATED-AMT (GRBJ-INDEX, 1) = .01           GBH1PGM 
02027 *           MOVE WS-VALUE-LIMIT-FULL  TO  WS-VISITS-VAL-1         GBH1PGM 
02028 *           MOVE WS-VISITS-1  TO                                  GBH1PGM 
02029 *    JP     GCBHS-BEN-UTIL-VAL(GCBHS-COL-IDX, GCBHS-BEN-IDX)      GBH1PGM 
02030 *         ELSE                                                    GBH1PGM 
02031 *           MOVE WS-VALUE-LIMIT-FULL  TO  WS-VISITS-VAL           GBH1PGM 
02032 *           MOVE WS-VISITS TO GRBF-ACCUM-AMT(GRBF-DTL-IDX)        GBH1PGM 
02033 *         END-IF                                                  GBH1PGM 
02034 *      WHEN '3'                                                   GBH1PGM 
02035 *        MOVE GRBJ-ACCUMULATED-AMT (GRBJ-INDEX, 1)                GBH1PGM 
02036 *                                 TO  WS-VALUE-LIMIT-V            GBH1PGM 
02037 *        MOVE WS-VALUE-LIMIT-FULL   TO  WS-DAYS-VAL               GBH1PGM 
02038 *        MOVE WS-DAYS  TO GRBF-ACCUM-AMT(GRBF-DTL-IDX)            GBH1PGM 
02039 *      WHEN '4'                                                   GBH1PGM 
02040 *        MOVE GRBJ-ACCUMULATED-AMT (GRBJ-INDEX, 1)                GBH1PGM 
02041 *                                 TO  WS-VALUE-LIMIT-V            GBH1PGM 
02042 *         IF GRBJ-ACCUMULATED-AMT (GRBJ-INDEX, 1) = .01           GBH1PGM 
02043 *           MOVE WS-VALUE-LIMIT-FULL  TO  WS-VISITS-VAL-1         GBH1PGM 
02044 *           MOVE WS-VISITS-1                                      GBH1PGM 
02045 *             TO GRBF-ACCUM-AMT(GRBF-DTL-IDX)                     GBH1PGM 
02046 *         ELSE                                                    GBH1PGM 
02047 *           MOVE WS-VALUE-LIMIT-FULL  TO  WS-VISITS-VAL           GBH1PGM 
02048 *           MOVE WS-VISITS TO GRBF-ACCUM-AMT(GRBF-DTL-IDX)        GBH1PGM 
02049 *         END-IF                                                  GBH1PGM 
02050 *      WHEN '5'                                                   GBH1PGM 
02051 *        MOVE GRBJ-ACCUMULATED-AMT (GRBJ-INDEX, 1)                GBH1PGM 
02052 *                    TO  WS-VALUE-LIMIT-MIL-CENTS-2               GBH1PGM 
02053 *        MOVE WS-VALUE-LIMIT-MIL-CENTS-2                          GBH1PGM 
02054 *               TO GRBF-ACCUM-AMT(GRBF-DTL-IDX)                   GBH1PGM 
02055 *      WHEN '6'                                                   GBH1PGM 
02056 *        MOVE GRBJ-ACCUMULATED-AMT (GRBJ-INDEX, 1)                GBH1PGM 
02057 *                                 TO  WS-VALUE-LIMIT-V            GBH1PGM 
02058 *        MOVE WS-VALUE-LIMIT-FULL  TO  WS-UTZ-CONF-VAL            GBH1PGM 
02059 *        MOVE WS-UTZ-CONFINEMENTS                                 GBH1PGM 
02060 *               TO GRBF-ACCUM-AMT(GRBF-DTL-IDX)                   GBH1PGM 
02061 *      WHEN '7'                                                   GBH1PGM 
02062 *        MOVE GRBJ-ACCUMULATED-AMT (GRBJ-INDEX, 1)                GBH1PGM 
02063 *                                 TO  WS-VALUE-LIMIT-DEC          GBH1PGM 
02064 *        MOVE WS-VALUE-LIM-5-7      TO  WS-PEOPLE-VAL             GBH1PGM 
02065 *        MOVE WS-PEOPLE TO GRBF-ACCUM-AMT(GRBF-DTL-IDX)           GBH1PGM 
02066 *      WHEN OTHER                                                 GBH1PGM 
02067 *        MOVE GRBJ-ACCUMULATED-AMT (GRBJ-INDEX, 1)                GBH1PGM 
02068 *                                 TO  WS-VALUE-LIMIT              GBH1PGM 
02069 *        MOVE WS-VALUE-LIMIT                                      GBH1PGM 
02070 *            TO GRBF-ACCUM-AMT(GRBF-DTL-IDX)                      GBH1PGM 
02071 *     END-EVALUATE.                                               GBH1PGM 
02072 *                                                                 GBH1PGM 
02073 *     COMPUTE WS-REMAINING = GRBJ-VALUE-LIMIT(GRBJ-INDEX)         GBH1PGM 
02074 *            -  GRBJ-ACCUMULATED-AMT (GRBJ-INDEX, 1)              GBH1PGM 
02075 *                                                                 GBH1PGM 
02076 *                                                                 GBH1PGM 
02077 *     EVALUATE GRBJ-VALUE-QUALIFIER (GRBJ-INDEX)                  GBH1PGM 
02078 *       WHEN '2'                                                  GBH1PGM 
02079 *         MOVE WS-REMAINING        TO  WS-VALUE-LIMIT-V           GBH1PGM 
02080 *          IF WS-REMAINING                         = .01          GBH1PGM 
02081 *            MOVE WS-VALUE-LIMIT-FULL  TO  WS-VISITS-VAL-1        GBH1PGM 
02082 *            MOVE WS-VISITS-1                                     GBH1PGM 
02083 *               TO GRBF-REMAIN-AMT(GRBF-DTL-IDX)                  GBH1PGM 
02084 *          ELSE                                                   GBH1PGM 
02085 *            MOVE WS-VALUE-LIMIT-FULL  TO  WS-VISITS-VAL          GBH1PGM 
02086 *           MOVE WS-VISITS TO GRBF-REMAIN-AMT(GRBF-DTL-IDX)       GBH1PGM 
02087 *          END-IF                                                 GBH1PGM 
02088 *       WHEN '3'                                                  GBH1PGM 
02089 *         MOVE WS-REMAINING        TO  WS-VALUE-LIMIT-V           GBH1PGM 
02090 *         MOVE WS-VALUE-LIMIT-FULL   TO  WS-DAYS-VAL              GBH1PGM 
02091 *         MOVE WS-DAYS  TO GRBF-REMAIN-AMT(GRBF-DTL-IDX)          GBH1PGM 
02092 *       WHEN '4'                                                  GBH1PGM 
02093 *         MOVE WS-REMAINING        TO  WS-VALUE-LIMIT-V           GBH1PGM 
02094 *          IF WS-REMAINING                         = .01          GBH1PGM 
02095 *            MOVE WS-VALUE-LIMIT-FULL  TO  WS-VISITS-VAL-1        GBH1PGM 
02096 *            MOVE WS-VISITS-1                                     GBH1PGM 
02097 *               TO GRBF-REMAIN-AMT(GRBF-DTL-IDX)                  GBH1PGM 
02098 *          ELSE                                                   GBH1PGM 
02099 *            MOVE WS-VALUE-LIMIT-FULL  TO  WS-VISITS-VAL          GBH1PGM 
02100 *            MOVE WS-VISITS TO GRBF-REMAIN-AMT(GRBF-DTL-IDX)      GBH1PGM 
02101 *          END-IF                                                 GBH1PGM 
02102 *       WHEN '5'                                                  GBH1PGM 
02103 *         MOVE WS-REMAINING  TO WS-VALUE-LIMIT-MIL-CENTS-2        GBH1PGM 
02104 *         MOVE WS-VALUE-LIMIT-MIL-CENTS-2                         GBH1PGM 
02105 *                TO GRBF-REMAIN-AMT(GRBF-DTL-IDX)                 GBH1PGM 
02106 *       WHEN '6'                                                  GBH1PGM 
02107 *         MOVE WS-REMAINING        TO  WS-VALUE-LIMIT-V           GBH1PGM 
02108 *         MOVE WS-VALUE-LIMIT-FULL  TO  WS-UTZ-CONF-VAL           GBH1PGM 
02109 *         MOVE WS-UTZ-CONFINEMENTS                                GBH1PGM 
02110 *                TO GRBF-REMAIN-AMT(GRBF-DTL-IDX)                 GBH1PGM 
02111 *       WHEN '7'                                                  GBH1PGM 
02112 *         MOVE WS-REMAINING        TO  WS-VALUE-LIMIT-DEC         GBH1PGM 
02113 *         MOVE WS-VALUE-LIM-5-7      TO  WS-PEOPLE-VAL            GBH1PGM 
02114 *         MOVE WS-PEOPLE TO GRBF-REMAIN-AMT(GRBF-DTL-IDX)         GBH1PGM 
02115 *       WHEN OTHER                                                GBH1PGM 
02116 *         MOVE WS-REMAINING        TO  WS-VALUE-LIMIT             GBH1PGM 
02117 *         MOVE WS-VALUE-LIMIT                                     GBH1PGM 
02118 *             TO GRBF-REMAIN-AMT(GRBF-DTL-IDX)                    GBH1PGM 
02119 *                                                                 GBH1PGM 
02120 *     END-EVALUATE.                                               GBH1PGM 
02121 *                                                                 GBH1PGM 
02122 *                                                                 GBH1PGM 
02123 *5500-EXIT.                                                       GBH1PGM 
02124 *    EXIT.                                                        GBH1PGM 
02125                                                                   GBH1PGM 
02126                                                                   GBH1PGM 
02127                                                                   GBH1PGM 
02128                                                                   GBH1PGM 
02129  5000-PROC-ACCUM-ID.                                              GBH1PGM 
02130                                                                   GBH1PGM 
02131       EVALUATE GCBHA-ACCUM-TYPE (GCBHA-TYPE-X)                    GBH1PGM 
02132         WHEN '#ABM  '                                             GBH1PGM 
02133           PERFORM 5100-ABM-LOOP THRU 5100-EXIT                    GBH1PGM 
02134         WHEN '#ACL  '                                             GBH1PGM 
02135           PERFORM 5200-ACL-LOOP THRU 5200-EXIT                    GBH1PGM 
02136         WHEN '#ACP  '                                             GBH1PGM 
02137           PERFORM 5300-ACP-LOOP THRU 5300-EXIT                    GBH1PGM 
02138         WHEN '#ADL  '                                             GBH1PGM 
02139           PERFORM 5400-ADL-LOOP THRU 5400-EXIT                    GBH1PGM 
02140         WHEN '#AOL  '                                             GBH1PGM 
02141           PERFORM 5500-AOL-LOOP THRU 5500-EXIT                    GBH1PGM 
02142         WHEN OTHER                                                GBH1PGM 
02143           GO TO 5000-EXIT                                         GBH1PGM 
02144       END-EVALUATE.                                               GBH1PGM 
02145                                                                   GBH1PGM 
02146 *JP-REMOVE FOR EG RULE 4070= ERCI ON ADL & ERCO ON ACP            GBH1PGM 
02147 *     IF (GCBHA-ID-X > GCBHA-ID-CTR) AND (WS-ACCUM-ID-CT > 0)     GBH1PGM 
02148 *        MOVE 'Y' TO WS-RULE-DONE-SW                              GBH1PGM 
02149 *     END-IF.                                                     GBH1PGM 
02150                                                                   GBH1PGM 
02151                                                                   GBH1PGM 
02152  5000-EXIT.                                                       GBH1PGM 
02153      EXIT.                                                        GBH1PGM 
02154                                                                   GBH1PGM 
02155                                                                   GBH1PGM 
02156  5100-ABM-LOOP.                                                   GBH1PGM 
02157                                                                   GBH1PGM 
02158       PERFORM VARYING GCBHA-ID-X FROM 1 BY 1                      GBH1PGM 
02159 **8/18  UNTIL (GCBHA-ID-X > GCBHA-ID-CTR) OR RULE-DONE            GBH1PGM 
02160         UNTIL (GCBHA-ID-X > 3 )           OR RULE-DONE            GBH1PGM 
02161                 OR SKIP-RULE                                      GBH1PGM 
02162           IF GCBHA-ACCUM-ID (GCBHA-ID-X) NOT = SPACES             GBH1PGM 
02163              IF WS-CON-ABM-FOUND                                  GBH1PGM 
02164                 MOVE WS-CON-ABM-HOLD TO GAA-RECORD                GBH1PGM 
02165                 PERFORM 5101-ABM-INNER-LOOP THRU 5101-EXIT        GBH1PGM 
02166              END-IF                                               GBH1PGM 
02167              IF RULE-NOT-DONE                                     GBH1PGM 
02168                 IF WS-GRP-ABM-FOUND                               GBH1PGM 
02169                    MOVE WS-GRP-ABM-HOLD TO GAA-RECORD             GBH1PGM 
02170                    PERFORM 5101-ABM-INNER-LOOP THRU 5101-EXIT     GBH1PGM 
02171                 END-IF                                            GBH1PGM 
02172              END-IF                                               GBH1PGM 
02173           END-IF                                                  GBH1PGM 
02174       END-PERFORM.                                                GBH1PGM 
02175                                                                   GBH1PGM 
02176                                                                   GBH1PGM 
02177                                                                   GBH1PGM 
02178  5100-EXIT.                                                       GBH1PGM 
02179      EXIT.                                                        GBH1PGM 
02180                                                                   GBH1PGM 
02181                                                                   GBH1PGM 
02182  5101-ABM-INNER-LOOP.                                             GBH1PGM 
02183                                                                   GBH1PGM 
02184       MOVE 'N' TO WS-ACCUM-ID-FD-SW                               GBH1PGM 
02185                                                                   GBH1PGM 
02186       PERFORM VARYING GAA-INDEX  FROM 1 BY 1                      GBH1PGM 
02187         UNTIL GAA-INDEX = GAA-ENTRY-COUNT OR RULE-DONE            GBH1PGM 
02188 ****                                      OR ACCUM-ID-FD          GBH1PGM 
02189          IF GAA-BAMA-ACCUMID (GAA-INDEX) =                        GBH1PGM 
02190             GCBHA-ACCUM-ID (GCBHA-ID-X)                           GBH1PGM 
02191             MOVE 'Y'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02192             IF WS-RULE = '004060'  AND                            GBH1PGM 
02193                GAA-BAMA-VALUE-LIMIT (GAA-INDEX)                   GBH1PGM 
02194                        > +0100000.00                              GBH1PGM 
02195                MOVE 'Y' TO WS-SKIP-RULE-SW                        GBH1PGM 
02196                GO TO 5101-EXIT                                    GBH1PGM 
02197             END-IF                                                GBH1PGM 
02198 **jp 9/30/05                                                      GBH1PGM 
02199             IF WS-RULE = '004270'  AND                            GBH1PGM 
02200                GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX)               GBH1PGM 
02201                   NOT  = '5'                                      GBH1PGM 
02202                MOVE 'Y' TO WS-SKIP-RULE-SW                        GBH1PGM 
02203                GO TO 5101-EXIT                                    GBH1PGM 
02204             END-IF                                                GBH1PGM 
02205             PERFORM 6000-OCCUR-TESTS  THRU 6000-EXIT              GBH1PGM 
02206             IF OCCUR-OK                                           GBH1PGM 
02207                PERFORM 5102-LOAD-ABM-VAL THRU 5102-EXIT           GBH1PGM 
02208 ****        MOVE 'Y' TO WS-ACCUM-ID-FD-SW                         GBH1PGM 
02209                ADD +1 TO WS-ACCUM-ID-CT                           GBH1PGM 
02210             END-IF                                                GBH1PGM 
02211          END-IF                                                   GBH1PGM 
02212       END-PERFORM.                                                GBH1PGM 
02213                                                                   GBH1PGM 
02214                                                                   GBH1PGM 
02215  5101-EXIT.                                                       GBH1PGM 
02216      EXIT.                                                        GBH1PGM 
02217                                                                   GBH1PGM 
02218                                                                   GBH1PGM 
02219                                                                   GBH1PGM 
02220  5102-LOAD-ABM-VAL.                                               GBH1PGM 
02221                                                                   GBH1PGM 
02222       PERFORM 5600-SET-COL-IDX THRU 5600-EXIT.                    GBH1PGM 
02223                                                                   GBH1PGM 
02224       MOVE GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX)                   GBH1PGM 
02225            TO WS-VAL-QUAL                                         GBH1PGM 
02226       MOVE GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX)                   GBH1PGM 
02227          TO WS-SAVE-VAL-QUAL                                      GBH1PGM 
02228       MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)                       GBH1PGM 
02229            TO WS-VAL-LMT                                          GBH1PGM 
02230       MOVE GAA-BAMA-BENEFIT-PERIOD (GAA-INDEX)                    GBH1PGM 
02231            TO WS-BEN-PERIOD                                       GBH1PGM 
02232       MOVE GAA-BAMA-BEN-PER-TIME-QUAL (GAA-INDEX)                 GBH1PGM 
02233            TO WS-BEN-PER-TIME-QUAL                                GBH1PGM 
02234       MOVE GAA-BAMA-BEN-PER-TIME-FCTR (GAA-INDEX)                 GBH1PGM 
02235            TO WS-BEN-PER-UNPACK-3                                 GBH1PGM 
02236                                                                   GBH1PGM 
02237       PERFORM 5900-VALUE-QUALIFIER-FMT THRU 5900-EXIT.            GBH1PGM 
02238       PERFORM 5901-DISP-BEN-PERIOD     THRU 5901-EXIT.            GBH1PGM 
02239                                                                   GBH1PGM 
02240       SET GRBJ-INDEX TO GCBHS-COL-IDX                             GBH1PGM 
02241       MOVE GAA-PROVISION-ID                                       GBH1PGM 
02242         TO GRBJ-ACCUM-TYPE(GRBJ-INDEX)                            GBH1PGM 
02243       MOVE GAA-PROVISION-SLOT-NO                                  GBH1PGM 
02244         TO GRBJ-ACCUM-SLOT (GRBJ-INDEX)                           GBH1PGM 
02245       SET  GRBJ-ACCUM-OCCURRENCE (GRBJ-INDEX) TO GAA-INDEX        GBH1PGM 
02246       MOVE GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX)   TO              GBH1PGM 
02247            GRBJ-VALUE-QUALIFIER (GRBJ-INDEX)                      GBH1PGM 
02248       MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)  TO                   GBH1PGM 
02249            GRBJ-VALUE-LIMIT (GRBJ-INDEX).                         GBH1PGM 
02250                                                                   GBH1PGM 
02251                                                                   GBH1PGM 
02252  5102-EXIT.                                                       GBH1PGM 
02253      EXIT.                                                        GBH1PGM 
02254                                                                   GBH1PGM 
02255  5200-ACL-LOOP.                                                   GBH1PGM 
02256                                                                   GBH1PGM 
02257       PERFORM VARYING GCBHA-ID-X FROM 1 BY 1                      GBH1PGM 
02258 **8/18  UNTIL (GCBHA-ID-X > GCBHA-ID-CTR) OR RULE-DONE            GBH1PGM 
02259         UNTIL (GCBHA-ID-X > 3 )           OR RULE-DONE            GBH1PGM 
02260         IF GCBHA-ACCUM-ID (GCBHA-ID-X) NOT = SPACES               GBH1PGM 
02261           IF WS-CON-ACL-FOUND                                     GBH1PGM 
02262              MOVE WS-CON-ACL-HOLD TO GAB-RECORD                   GBH1PGM 
02263              PERFORM 5201-ACL-INNER-LOOP THRU 5201-EXIT           GBH1PGM 
02264           END-IF                                                  GBH1PGM 
02265           IF RULE-NOT-DONE                                        GBH1PGM 
02266              IF WS-GRP-ACL-FOUND                                  GBH1PGM 
02267                 MOVE WS-GRP-ACL-HOLD TO GAB-RECORD                GBH1PGM 
02268                 PERFORM 5201-ACL-INNER-LOOP THRU 5201-EXIT        GBH1PGM 
02269              END-IF                                               GBH1PGM 
02270           END-IF                                                  GBH1PGM 
02271         END-IF                                                    GBH1PGM 
02272       END-PERFORM.                                                GBH1PGM 
02273                                                                   GBH1PGM 
02274                                                                   GBH1PGM 
02275                                                                   GBH1PGM 
02276  5200-EXIT.                                                       GBH1PGM 
02277      EXIT.                                                        GBH1PGM 
02278                                                                   GBH1PGM 
02279  5201-ACL-INNER-LOOP.                                             GBH1PGM 
02280                                                                   GBH1PGM 
02281       MOVE 'N' TO WS-ACCUM-ID-FD-SW                               GBH1PGM 
02282                                                                   GBH1PGM 
02283       PERFORM VARYING GAB-INDEX  FROM 1 BY 1                      GBH1PGM 
02284         UNTIL GAB-INDEX = GAB-ENTRY-COUNT OR RULE-DONE            GBH1PGM 
02285 ***                                       OR ACCUM-ID-FD          GBH1PGM 
02286          IF GAB-COINS-ACCUMID (GAB-INDEX) =                       GBH1PGM 
02287             GCBHA-ACCUM-ID (GCBHA-ID-X)                           GBH1PGM 
02288             MOVE 'Y'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02289             PERFORM 6000-OCCUR-TESTS  THRU 6000-EXIT              GBH1PGM 
02290             IF OCCUR-OK                                           GBH1PGM 
02291                PERFORM 5202-LOAD-ACL-VAL THRU 5202-EXIT           GBH1PGM 
02292 ****           MOVE 'Y' TO WS-ACCUM-ID-FD-SW                      GBH1PGM 
02293                ADD +1 TO WS-ACCUM-ID-CT                           GBH1PGM 
02294                PERFORM 1002-TEST-BASIC-ACCUM-IDS THRU 1002-EXIT   GBH1PGM 
02295             END-IF                                                GBH1PGM 
02296          END-IF                                                   GBH1PGM 
02297       END-PERFORM.                                                GBH1PGM 
02298                                                                   GBH1PGM 
02299                                                                   GBH1PGM 
02300  5201-EXIT.                                                       GBH1PGM 
02301      EXIT.                                                        GBH1PGM 
02302                                                                   GBH1PGM 
02303  5202-LOAD-ACL-VAL.                                               GBH1PGM 
02304                                                                   GBH1PGM 
02305       PERFORM 5600-SET-COL-IDX THRU 5600-EXIT.                    GBH1PGM 
02306                                                                   GBH1PGM 
02307       IF WS-RULE = '004340'                                       GBH1PGM 
02308          COMPUTE WS-PK-PERCENT = WS-PK-100 -                      GBH1PGM 
02309                  GAB-COINS-PERCENT-LEVEL (GAB-INDEX)              GBH1PGM 
02310          MOVE WS-PK-PERCENT TO WS-PERCENT-VAL                     GBH1PGM 
02311       ELSE                                                        GBH1PGM 
02312          MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)                 GBH1PGM 
02313                 TO WS-PERCENT-VAL                                 GBH1PGM 
02314       END-IF.                                                     GBH1PGM 
02315                                                                   GBH1PGM 
02316       MOVE WS-PERCENT    TO                                       GBH1PGM 
02317         GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, GCBHS-COL-IDX, 1).    GBH1PGM 
02318                                                                   GBH1PGM 
02319       IF GAB-COINS-ACCUMID (GAB-INDEX) =                          GBH1PGM 
02320          'HMSA' OR 'HMSI' OR 'HMSO' OR                            GBH1PGM 
02321          'MSPA' OR 'MSPI' OR 'MSPO'                               GBH1PGM 
02322          PERFORM 5203-SAVE-ACL THRU 5203-EXIT                     GBH1PGM 
02323       END-IF.                                                     GBH1PGM 
02324                                                                   GBH1PGM 
02325                                                                   GBH1PGM 
02326                                                                   GBH1PGM 
02327  5202-EXIT.                                                       GBH1PGM 
02328      EXIT.                                                        GBH1PGM 
02329                                                                   GBH1PGM 
02330                                                                   GBH1PGM 
02331                                                                   GBH1PGM 
02332  5203-SAVE-ACL.                                                   GBH1PGM 
02333                                                                   GBH1PGM 
02334                                                                   GBH1PGM 
02335       EVALUATE GAB-COINS-ACCUMID (GAB-INDEX)                      GBH1PGM 
02336         WHEN 'HMSA'                                               GBH1PGM 
02337           MOVE WS-PERCENT TO WS-HMSA-SAVE-VAL                     GBH1PGM 
02338           MOVE GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)           GBH1PGM 
02339             TO WS-HMSA-SAVE-POT                                   GBH1PGM 
02340         WHEN 'HMSI'                                               GBH1PGM 
02341           MOVE WS-PERCENT TO WS-HMSI-SAVE-VAL                     GBH1PGM 
02342           MOVE GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)           GBH1PGM 
02343             TO WS-HMSI-SAVE-POT                                   GBH1PGM 
02344         WHEN 'HMSO'                                               GBH1PGM 
02345           MOVE WS-PERCENT TO WS-HMSO-SAVE-VAL                     GBH1PGM 
02346           MOVE GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)           GBH1PGM 
02347             TO WS-HMSO-SAVE-POT                                   GBH1PGM 
02348         WHEN 'MSPA'                                               GBH1PGM 
02349           MOVE WS-PERCENT TO WS-MSPA-SAVE-VAL                     GBH1PGM 
02350           MOVE GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)           GBH1PGM 
02351             TO WS-MSPA-SAVE-POT                                   GBH1PGM 
02352         WHEN 'MSPI'                                               GBH1PGM 
02353           MOVE WS-PERCENT TO WS-MSPI-SAVE-VAL                     GBH1PGM 
02354           MOVE GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)           GBH1PGM 
02355             TO WS-MSPI-SAVE-POT                                   GBH1PGM 
02356         WHEN 'MSPO'                                               GBH1PGM 
02357           MOVE WS-PERCENT TO WS-MSPO-SAVE-VAL                     GBH1PGM 
02358           MOVE GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)           GBH1PGM 
02359             TO WS-MSPO-SAVE-POT                                   GBH1PGM 
02360       END-EVALUATE.                                               GBH1PGM 
02361                                                                   GBH1PGM 
02362                                                                   GBH1PGM 
02363                                                                   GBH1PGM 
02364  5203-EXIT.                                                       GBH1PGM 
02365      EXIT.                                                        GBH1PGM 
02366                                                                   GBH1PGM 
02367                                                                   GBH1PGM 
02368  5300-ACP-LOOP.                                                   GBH1PGM 
02369                                                                   GBH1PGM 
02370       PERFORM VARYING GCBHA-ID-X FROM 1 BY 1                      GBH1PGM 
02371 **8/18  UNTIL (GCBHA-ID-X > GCBHA-ID-CTR) OR RULE-DONE            GBH1PGM 
02372         UNTIL (GCBHA-ID-X > 3 )           OR RULE-DONE            GBH1PGM 
02373          IF GCBHA-ACCUM-ID (GCBHA-ID-X) NOT = SPACES              GBH1PGM 
02374           IF WS-CON-ACP-FOUND                                     GBH1PGM 
02375              MOVE WS-CON-ACP-HOLD TO GAF-RECORD                   GBH1PGM 
02376              PERFORM 5301-ACP-INNER-LOOP THRU 5301-EXIT           GBH1PGM 
02377           END-IF                                                  GBH1PGM 
02378           IF RULE-NOT-DONE                                        GBH1PGM 
02379              IF WS-GRP-ACP-FOUND                                  GBH1PGM 
02380                 MOVE WS-GRP-ACP-HOLD TO GAF-RECORD                GBH1PGM 
02381                 PERFORM 5301-ACP-INNER-LOOP THRU 5301-EXIT        GBH1PGM 
02382              END-IF                                               GBH1PGM 
02383           END-IF                                                  GBH1PGM 
02384          END-IF                                                   GBH1PGM 
02385       END-PERFORM.                                                GBH1PGM 
02386                                                                   GBH1PGM 
02387                                                                   GBH1PGM 
02388                                                                   GBH1PGM 
02389  5300-EXIT.                                                       GBH1PGM 
02390      EXIT.                                                        GBH1PGM 
02391                                                                   GBH1PGM 
02392  5301-ACP-INNER-LOOP.                                             GBH1PGM 
02393                                                                   GBH1PGM 
02394       MOVE 'N' TO WS-ACCUM-ID-FD-SW                               GBH1PGM 
02395                                                                   GBH1PGM 
02396       PERFORM VARYING GAF-INDEX  FROM 1 BY 1                      GBH1PGM 
02397         UNTIL GAF-INDEX = GAF-ENTRY-COUNT OR RULE-DONE            GBH1PGM 
02398 ***                                       OR ACCUM-ID-FD          GBH1PGM 
02399          IF GAF-COPAY-ACCUMID (GAF-INDEX) =                       GBH1PGM 
02400             GCBHA-ACCUM-ID (GCBHA-ID-X)                           GBH1PGM 
02401             MOVE 'Y'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02402             PERFORM 6000-OCCUR-TESTS  THRU 6000-EXIT              GBH1PGM 
02403             IF OCCUR-OK                                           GBH1PGM 
02404                PERFORM 5302-LOAD-ACP-VAL THRU 5302-EXIT           GBH1PGM 
02405 ***            MOVE 'Y' TO WS-ACCUM-ID-FD-SW                      GBH1PGM 
02406                ADD +1 TO WS-ACCUM-ID-CT                           GBH1PGM 
02407                PERFORM 1002-TEST-BASIC-ACCUM-IDS THRU 1002-EXIT   GBH1PGM 
02408             END-IF                                                GBH1PGM 
02409          END-IF                                                   GBH1PGM 
02410       END-PERFORM.                                                GBH1PGM 
02411                                                                   GBH1PGM 
02412                                                                   GBH1PGM 
02413  5301-EXIT.                                                       GBH1PGM 
02414      EXIT.                                                        GBH1PGM 
02415                                                                   GBH1PGM 
02416  5302-LOAD-ACP-VAL.                                               GBH1PGM 
02417                                                                   GBH1PGM 
02418       PERFORM 5600-SET-COL-IDX THRU 5600-EXIT.                    GBH1PGM 
02419                                                                   GBH1PGM 
02420       MOVE GAF-COPAY-VALUE-QUALIFIER(GAF-INDEX)                   GBH1PGM 
02421            TO WS-VAL-QUAL                                         GBH1PGM 
02422       MOVE GAF-COPAY-VALUE-QUALIFIER(GAF-INDEX)                   GBH1PGM 
02423          TO WS-SAVE-VAL-QUAL                                      GBH1PGM 
02424       MOVE GAF-COPAY-VALUE-LIMIT(GAF-INDEX)                       GBH1PGM 
02425            TO WS-VAL-LMT                                          GBH1PGM 
02426                                                                   GBH1PGM 
02427       PERFORM 5900-VALUE-QUALIFIER-FMT THRU 5900-EXIT.            GBH1PGM 
02428                                                                   GBH1PGM 
02429 *     SET GRBJ-INDEX TO GCBHS-COL-IDX                             GBH1PGM 
02430 *     MOVE GAF-PROVISION-ID                                       GBH1PGM 
02431 *       TO GRBJ-ACCUM-TYPE(GRBJ-INDEX)                            GBH1PGM 
02432 *     MOVE GAF-PROVISION-SLOT-NO                                  GBH1PGM 
02433 *       TO GRBJ-ACCUM-SLOT (GRBJ-INDEX)                           GBH1PGM 
02434 *     SET  GRBJ-ACCUM-OCCURRENCE (GRBJ-INDEX) TO GAF-INDEX        GBH1PGM 
02435 *     MOVE GAF-COPAY-VALUE-QUALIFIER(GAF-INDEX)   TO              GBH1PGM 
02436 *          GRBJ-VALUE-QUALIFIER (GRBJ-INDEX)                      GBH1PGM 
02437 *     MOVE GAF-COPAY-VALUE-LIMIT(GAF-INDEX)  TO                   GBH1PGM 
02438 *          GRBJ-VALUE-LIMIT (GRBJ-INDEX).                         GBH1PGM 
02439                                                                   GBH1PGM 
02440                                                                   GBH1PGM 
02441  5302-EXIT.                                                       GBH1PGM 
02442      EXIT.                                                        GBH1PGM 
02443                                                                   GBH1PGM 
02444                                                                   GBH1PGM 
02445                                                                   GBH1PGM 
02446  5400-ADL-LOOP.                                                   GBH1PGM 
02447                                                                   GBH1PGM 
02448       PERFORM VARYING GCBHA-ID-X FROM 1 BY 1                      GBH1PGM 
02449 **8/18  UNTIL (GCBHA-ID-X > GCBHA-ID-CTR) OR RULE-DONE            GBH1PGM 
02450         UNTIL (GCBHA-ID-X > 3 )           OR RULE-DONE            GBH1PGM 
02451          IF GCBHA-ACCUM-ID (GCBHA-ID-X) NOT = SPACES              GBH1PGM 
02452           IF WS-CON-ADL-FOUND                                     GBH1PGM 
02453              MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD             GBH1PGM 
02454              PERFORM 5401-ADL-INNER-LOOP THRU 5401-EXIT           GBH1PGM 
02455           END-IF                                                  GBH1PGM 
02456           IF RULE-NOT-DONE                                        GBH1PGM 
02457              IF WS-GRP-ADL-FOUND                                  GBH1PGM 
02458                 MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD          GBH1PGM 
02459                 PERFORM 5401-ADL-INNER-LOOP THRU 5401-EXIT        GBH1PGM 
02460              END-IF                                               GBH1PGM 
02461           END-IF                                                  GBH1PGM 
02462          END-IF                                                   GBH1PGM 
02463       END-PERFORM.                                                GBH1PGM 
02464                                                                   GBH1PGM 
02465                                                                   GBH1PGM 
02466                                                                   GBH1PGM 
02467  5400-EXIT.                                                       GBH1PGM 
02468      EXIT.                                                        GBH1PGM 
02469                                                                   GBH1PGM 
02470                                                                   GBH1PGM 
02471  5401-ADL-INNER-LOOP.                                             GBH1PGM 
02472                                                                   GBH1PGM 
02473       MOVE 'N' TO WS-ACCUM-ID-FD-SW                               GBH1PGM 
02474                                                                   GBH1PGM 
02475       PERFORM VARYING GAC-INDEX  FROM 1 BY 1                      GBH1PGM 
02476         UNTIL GAC-INDEX = GAC-ENTRY-COUNT OR RULE-DONE            GBH1PGM 
02477 ***                                       OR ACCUM-ID-FD          GBH1PGM 
02478          IF GAC-DEDL-ACCUMID (GAC-INDEX) =                        GBH1PGM 
02479             GCBHA-ACCUM-ID (GCBHA-ID-X)                           GBH1PGM 
02480             MOVE 'Y'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02481             PERFORM 6000-OCCUR-TESTS  THRU 6000-EXIT              GBH1PGM 
02482             IF OCCUR-OK                                           GBH1PGM 
02483                PERFORM 5402-LOAD-ADL-VAL THRU 5402-EXIT           GBH1PGM 
02484 ***            MOVE 'Y' TO WS-ACCUM-ID-FD-SW                      GBH1PGM 
02485                ADD +1 TO WS-ACCUM-ID-CT                           GBH1PGM 
02486                PERFORM 1002-TEST-BASIC-ACCUM-IDS THRU 1002-EXIT   GBH1PGM 
02487             END-IF                                                GBH1PGM 
02488          END-IF                                                   GBH1PGM 
02489       END-PERFORM.                                                GBH1PGM 
02490                                                                   GBH1PGM 
02491                                                                   GBH1PGM 
02492  5401-EXIT.                                                       GBH1PGM 
02493      EXIT.                                                        GBH1PGM 
02494                                                                   GBH1PGM 
02495                                                                   GBH1PGM 
02496                                                                   GBH1PGM 
02497  5402-LOAD-ADL-VAL.                                               GBH1PGM 
02498                                                                   GBH1PGM 
02499       PERFORM 5600-SET-COL-IDX THRU 5600-EXIT.                    GBH1PGM 
02500       MOVE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)                   GBH1PGM 
02501            TO WS-VAL-QUAL                                         GBH1PGM 
02502       MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                       GBH1PGM 
02503            TO WS-VAL-LMT                                          GBH1PGM 
02504                                                                   GBH1PGM 
02505       MOVE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)                   GBH1PGM 
02506          TO WS-SAVE-VAL-QUAL                                      GBH1PGM 
02507                                                                   GBH1PGM 
02508                                                                   GBH1PGM 
02509       PERFORM 5900-VALUE-QUALIFIER-FMT THRU 5900-EXIT.            GBH1PGM 
02510                                                                   GBH1PGM 
02511                                                                   GBH1PGM 
02512       SET GRBJ-INDEX TO GCBHS-COL-IDX                             GBH1PGM 
02513       MOVE GAC-PROVISION-ID                                       GBH1PGM 
02514         TO GRBJ-ACCUM-TYPE(GRBJ-INDEX)                            GBH1PGM 
02515       MOVE GAC-PROVISION-SLOT-NO                                  GBH1PGM 
02516         TO GRBJ-ACCUM-SLOT (GRBJ-INDEX)                           GBH1PGM 
02517       SET  GRBJ-ACCUM-OCCURRENCE (GRBJ-INDEX) TO GAC-INDEX        GBH1PGM 
02518       MOVE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)   TO              GBH1PGM 
02519            GRBJ-VALUE-QUALIFIER (GRBJ-INDEX)                      GBH1PGM 
02520       MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)  TO                   GBH1PGM 
02521            GRBJ-VALUE-LIMIT (GRBJ-INDEX).                         GBH1PGM 
02522                                                                   GBH1PGM 
02523  5402-EXIT.                                                       GBH1PGM 
02524      EXIT.                                                        GBH1PGM 
02525                                                                   GBH1PGM 
02526                                                                   GBH1PGM 
02527                                                                   GBH1PGM 
02528  5500-AOL-LOOP.                                                   GBH1PGM 
02529                                                                   GBH1PGM 
02530       PERFORM VARYING GCBHA-ID-X FROM 1 BY 1                      GBH1PGM 
02531 **8/18  UNTIL (GCBHA-ID-X > GCBHA-ID-CTR) OR RULE-DONE            GBH1PGM 
02532         UNTIL (GCBHA-ID-X > 3 )           OR RULE-DONE            GBH1PGM 
02533          IF GCBHA-ACCUM-ID (GCBHA-ID-X) NOT = SPACES              GBH1PGM 
02534           IF WS-CON-AOL-FOUND                                     GBH1PGM 
02535              MOVE WS-CON-AOL-HOLD TO WS-LOOP-AOL-HOLD             GBH1PGM 
02536              PERFORM 5501-AOL-INNER-LOOP THRU 5501-EXIT           GBH1PGM 
02537           END-IF                                                  GBH1PGM 
02538           IF RULE-NOT-DONE                                        GBH1PGM 
02539              IF WS-GRP-AOL-FOUND                                  GBH1PGM 
02540                 MOVE WS-GRP-AOL-HOLD TO WS-LOOP-AOL-HOLD          GBH1PGM 
02541                 PERFORM 5501-AOL-INNER-LOOP THRU 5501-EXIT        GBH1PGM 
02542              END-IF                                               GBH1PGM 
02543           END-IF                                                  GBH1PGM 
02544          END-IF                                                   GBH1PGM 
02545       END-PERFORM.                                                GBH1PGM 
02546                                                                   GBH1PGM 
02547                                                                   GBH1PGM 
02548                                                                   GBH1PGM 
02549  5500-EXIT.                                                       GBH1PGM 
02550      EXIT.                                                        GBH1PGM 
02551                                                                   GBH1PGM 
02552                                                                   GBH1PGM 
02553  5501-AOL-INNER-LOOP.                                             GBH1PGM 
02554                                                                   GBH1PGM 
02555       MOVE 'N' TO WS-ACCUM-ID-FD-SW                               GBH1PGM 
02556                                                                   GBH1PGM 
02557       PERFORM VARYING GAD-INDEX  FROM 1 BY 1                      GBH1PGM 
02558         UNTIL GAD-INDEX = GAD-ENTRY-COUNT OR RULE-DONE            GBH1PGM 
02559 ***                                       OR ACCUM-ID-FD          GBH1PGM 
02560          IF GAD-O-P-X-ACCUMID(GAD-INDEX) =                        GBH1PGM 
02561             GCBHA-ACCUM-ID (GCBHA-ID-X)                           GBH1PGM 
02562             MOVE 'Y'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02563             PERFORM 6000-OCCUR-TESTS  THRU 6000-EXIT              GBH1PGM 
02564             IF OCCUR-OK                                           GBH1PGM 
02565                PERFORM 5502-LOAD-AOL-VAL THRU 5502-EXIT           GBH1PGM 
02566 ***            MOVE 'Y' TO WS-ACCUM-ID-FD-SW                      GBH1PGM 
02567                ADD +1 TO WS-ACCUM-ID-CT                           GBH1PGM 
02568                PERFORM 1002-TEST-BASIC-ACCUM-IDS THRU 1002-EXIT   GBH1PGM 
02569             END-IF                                                GBH1PGM 
02570          END-IF                                                   GBH1PGM 
02571       END-PERFORM.                                                GBH1PGM 
02572                                                                   GBH1PGM 
02573                                                                   GBH1PGM 
02574  5501-EXIT.                                                       GBH1PGM 
02575      EXIT.                                                        GBH1PGM 
02576                                                                   GBH1PGM 
02577                                                                   GBH1PGM 
02578                                                                   GBH1PGM 
02579  5502-LOAD-AOL-VAL.                                               GBH1PGM 
02580                                                                   GBH1PGM 
02581       PERFORM 5600-SET-COL-IDX THRU 5600-EXIT.                    GBH1PGM 
02582                                                                   GBH1PGM 
02583       MOVE GAD-O-P-X-VALUE-QUALIFIER(GAD-INDEX)                   GBH1PGM 
02584            TO WS-VAL-QUAL                                         GBH1PGM 
02585       MOVE GAD-O-P-X-VALUE-QUALIFIER(GAD-INDEX)                   GBH1PGM 
02586          TO WS-SAVE-VAL-QUAL                                      GBH1PGM 
02587       MOVE GAD-O-P-X-VALUE-LIMIT(GAD-INDEX)                       GBH1PGM 
02588            TO WS-VAL-LMT                                          GBH1PGM 
02589       PERFORM 5900-VALUE-QUALIFIER-FMT THRU 5900-EXIT.            GBH1PGM 
02590                                                                   GBH1PGM 
02591                                                                   GBH1PGM 
02592       SET GRBJ-INDEX TO GCBHS-COL-IDX                             GBH1PGM 
02593       MOVE GAD-PROVISION-ID                                       GBH1PGM 
02594         TO GRBJ-ACCUM-TYPE(GRBJ-INDEX)                            GBH1PGM 
02595       MOVE GAD-PROVISION-SLOT-NO                                  GBH1PGM 
02596         TO GRBJ-ACCUM-SLOT (GRBJ-INDEX)                           GBH1PGM 
02597       SET  GRBJ-ACCUM-OCCURRENCE (GRBJ-INDEX) TO GAD-INDEX        GBH1PGM 
02598       MOVE GAD-O-P-X-VALUE-QUALIFIER(GAD-INDEX)   TO              GBH1PGM 
02599            GRBJ-VALUE-QUALIFIER (GRBJ-INDEX)                      GBH1PGM 
02600       MOVE GAD-O-P-X-VALUE-LIMIT(GAD-INDEX)  TO                   GBH1PGM 
02601            GRBJ-VALUE-LIMIT (GRBJ-INDEX).                         GBH1PGM 
02602                                                                   GBH1PGM 
02603  5502-EXIT.                                                       GBH1PGM 
02604      EXIT.                                                        GBH1PGM 
02605                                                                   GBH1PGM 
02606                                                                   GBH1PGM 
02607                                                                   GBH1PGM 
02608                                                                   GBH1PGM 
02609  5600-SET-COL-IDX.                                                GBH1PGM 
02610                                                                   GBH1PGM 
02611                                                                   GBH1PGM 
02612 * COLUMN #1 PLUS COMB-FLAG = OVERALL (SHARED IIN/OON)             GBH1PGM 
02613       IF GCBHA-ID-X = 1                                           GBH1PGM 
02614          SET GCBHS-COL-IDX TO 1                                   GBH1PGM 
02615          MOVE 'Y' TO GCBHS-BEN-COMB-FLAG (GCBHS-BEN-IDX)          GBH1PGM 
02616          MOVE 'Y' TO WS-RULE-DONE-SW                              GBH1PGM 
02617          MOVE 'Y' TO WS-OVERALL-FD-SW                             GBH1PGM 
02618       END-IF                                                      GBH1PGM 
02619 * COLUMN #1 + NO COMB-FLAG = IN-NETWORK                           GBH1PGM 
02620       IF GCBHA-ID-X = 2                                           GBH1PGM 
02621          SET GCBHS-COL-IDX TO 1                                   GBH1PGM 
02622          MOVE 'Y' TO WS-IN-NET-FD-SW                              GBH1PGM 
02623       END-IF                                                      GBH1PGM 
02624 * COLUMN #2 = OUT-OF-NETWORK                                      GBH1PGM 
02625       IF GCBHA-ID-X = 3                                           GBH1PGM 
02626          SET GCBHS-COL-IDX TO 2                                   GBH1PGM 
02627          MOVE 'Y' TO WS-OUT-NET-FD-SW                             GBH1PGM 
02628       END-IF.                                                     GBH1PGM 
02629                                                                   GBH1PGM 
02630                                                                   GBH1PGM 
02631  5600-EXIT.                                                       GBH1PGM 
02632      EXIT.                                                        GBH1PGM 
02633                                                                   GBH1PGM 
02634                                                                   GBH1PGM 
02635                                                                   GBH1PGM 
02636  5700-OFFC-LVL-DEFAULT.                                           GBH1PGM 
02637                                                                   GBH1PGM 
02638      MOVE 'OFFICE VISIT PAYMENT LEVEL'                            GBH1PGM 
02639        TO GCBHS-BENEFIT-TITLE(GCBHS-BEN-IDX)                      GBH1PGM 
02640      IF HOSP-MED-FD AND OFFC-COPAY-FD                             GBH1PGM 
02641         MOVE WS-NOT-APPLIC           TO                           GBH1PGM 
02642         GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)                 GBH1PGM 
02643         MOVE 'Y' TO                                               GBH1PGM 
02644                 GCBHS-BEN-COMB-FLAG (GCBHS-BEN-IDX)               GBH1PGM 
02645         GO TO 5700-EXIT                                           GBH1PGM 
02646      END-IF                                                       GBH1PGM 
02647      IF WS-HMSA-SAVE-VAL NOT = SPACES                             GBH1PGM 
02648         MOVE WS-HMSA-SAVE-POT TO WS-POT-TEST                      GBH1PGM 
02649         IF POT-OFFICE                                             GBH1PGM 
02650            MOVE WS-HMSA-SAVE-VAL TO                               GBH1PGM 
02651            GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)              GBH1PGM 
02652            MOVE 'Y' TO                                            GBH1PGM 
02653                 GCBHS-BEN-COMB-FLAG (GCBHS-BEN-IDX)               GBH1PGM 
02654            MOVE 'Y' TO WS-OFFC-LVL-IN-FD                          GBH1PGM 
02655         END-IF                                                    GBH1PGM 
02656      END-IF                                                       GBH1PGM 
02657      IF WS-HMSI-SAVE-VAL NOT = SPACES                             GBH1PGM 
02658         MOVE WS-HMSI-SAVE-POT TO WS-POT-TEST                      GBH1PGM 
02659         IF POT-OFFICE                                             GBH1PGM 
02660            MOVE WS-HMSI-SAVE-VAL TO                               GBH1PGM 
02661            GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)              GBH1PGM 
02662            MOVE 'Y' TO WS-OFFC-LVL-IN-FD                          GBH1PGM 
02663         END-IF                                                    GBH1PGM 
02664      END-IF                                                       GBH1PGM 
02665      IF WS-HMSO-SAVE-VAL NOT = SPACES                             GBH1PGM 
02666         MOVE WS-HMSO-SAVE-POT TO WS-POT-TEST                      GBH1PGM 
02667         IF POT-OFFICE                                             GBH1PGM 
02668            MOVE WS-HMSO-SAVE-VAL TO                               GBH1PGM 
02669            GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 2, 1)              GBH1PGM 
02670         END-IF                                                    GBH1PGM 
02671      END-IF                                                       GBH1PGM 
02672                                                                   GBH1PGM 
02673      IF WS-MSPA-SAVE-VAL NOT = SPACES                             GBH1PGM 
02674         MOVE WS-MSPA-SAVE-POT TO WS-POT-TEST                      GBH1PGM 
02675         IF POT-OFFICE                                             GBH1PGM 
02676            MOVE WS-MSPA-SAVE-VAL TO                               GBH1PGM 
02677            GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)              GBH1PGM 
02678            MOVE 'Y' TO                                            GBH1PGM 
02679                 GCBHS-BEN-COMB-FLAG (GCBHS-BEN-IDX)               GBH1PGM 
02680            MOVE 'Y' TO WS-OFFC-LVL-IN-FD                          GBH1PGM 
02681         END-IF                                                    GBH1PGM 
02682      END-IF                                                       GBH1PGM 
02683      IF WS-MSPI-SAVE-VAL NOT = SPACES                             GBH1PGM 
02684         MOVE WS-MSPI-SAVE-POT TO WS-POT-TEST                      GBH1PGM 
02685         IF POT-OFFICE OR (WS-MSPI-SAVE-POT = SPACES)              GBH1PGM 
02686            MOVE WS-MSPI-SAVE-VAL TO                               GBH1PGM 
02687            GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)              GBH1PGM 
02688            MOVE 'Y' TO WS-OFFC-LVL-IN-FD                          GBH1PGM 
02689         END-IF                                                    GBH1PGM 
02690      END-IF                                                       GBH1PGM 
02691      IF WS-MSPO-SAVE-VAL NOT = SPACES                             GBH1PGM 
02692         MOVE WS-MSPO-SAVE-POT TO WS-POT-TEST                      GBH1PGM 
02693         IF POT-OFFICE                                             GBH1PGM 
02694            MOVE WS-MSPO-SAVE-VAL TO                               GBH1PGM 
02695            GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 2, 1)              GBH1PGM 
02696         END-IF                                                    GBH1PGM 
02697      END-IF.                                                      GBH1PGM 
02698                                                                   GBH1PGM 
02699      IF OFFC-LVL-IN-FD                                            GBH1PGM 
02700         GO TO 5700-EXIT                                           GBH1PGM 
02701      ELSE                                                         GBH1PGM 
02702         IF OFFC-COPAY-FD                                          GBH1PGM 
02703 *gf 9/12   MOVE WS-100-AFTER-CPY TO                               GBH1PGM 
02704            MOVE WS-NOT-APPLIC           TO                        GBH1PGM 
02705            GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)              GBH1PGM 
02706         ELSE                                                      GBH1PGM 
02707 *JP 7/29   MOVE WS-100-PERCENT          TO                        GBH1PGM 
02708            MOVE WS-NOT-APPLIC           TO                        GBH1PGM 
02709            GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 1, 1)              GBH1PGM 
02710         END-IF                                                    GBH1PGM 
02711      END-IF.                                                      GBH1PGM 
02712                                                                   GBH1PGM 
02713                                                                   GBH1PGM 
02714  5700-EXIT.                                                       GBH1PGM 
02715      EXIT.                                                        GBH1PGM 
02716                                                                   GBH1PGM 
02717                                                                   GBH1PGM 
02718                                                                   GBH1PGM 
02719  5800-OFFC-COIN-OON.                                              GBH1PGM 
02720                                                                   GBH1PGM 
02721      IF OVERALL-NOT-FD AND OUT-NET-NOT-FD AND IN-NET-FD           GBH1PGM 
02722         IF WS-HMSO-SAVE-VAL NOT = SPACES                          GBH1PGM 
02723            MOVE WS-HMSO-SAVE-POT TO WS-POT-TEST                   GBH1PGM 
02724            IF POT-OFFICE                                          GBH1PGM 
02725               MOVE WS-HMSO-SAVE-VAL TO                            GBH1PGM 
02726               GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 2, 1)           GBH1PGM 
02727            END-IF                                                 GBH1PGM 
02728         END-IF                                                    GBH1PGM 
02729         IF WS-MSPO-SAVE-VAL NOT = SPACES                          GBH1PGM 
02730            MOVE WS-MSPO-SAVE-POT TO WS-POT-TEST                   GBH1PGM 
02731            IF POT-OFFICE                                          GBH1PGM 
02732               MOVE WS-MSPO-SAVE-VAL TO                            GBH1PGM 
02733               GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, 2, 1)           GBH1PGM 
02734            END-IF                                                 GBH1PGM 
02735         END-IF                                                    GBH1PGM 
02736      END-IF.                                                      GBH1PGM 
02737                                                                   GBH1PGM 
02738  5800-EXIT.                                                       GBH1PGM 
02739      EXIT.                                                        GBH1PGM 
02740                                                                   GBH1PGM 
02741                                                                   GBH1PGM 
02742                                                                   GBH1PGM 
02743  5900-VALUE-QUALIFIER-FMT.                                        GBH1PGM 
02744                                                                   GBH1PGM 
02745      EVALUATE WS-VAL-QUAL                                         GBH1PGM 
02746                                                                   GBH1PGM 
02747       WHEN '2'                                                    GBH1PGM 
02748         MOVE WS-VAL-LMT  TO  WS-VALUE-LIMIT-V                     GBH1PGM 
02749         IF WS-VAL-LMT  = .01                                      GBH1PGM 
02750           MOVE WS-VALUE-LIMIT-FULL  TO  WS-VISITS-VAL-1           GBH1PGM 
02751           MOVE WS-VISITS-1   TO                                   GBH1PGM 
02752          GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, GCBHS-COL-IDX, 1)    GBH1PGM 
02753         ELSE                                                      GBH1PGM 
02754           MOVE WS-VALUE-LIMIT-FULL   TO  WS-VISITS-VAL            GBH1PGM 
02755           MOVE WS-VISITS     TO                                   GBH1PGM 
02756          GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, GCBHS-COL-IDX, 1)    GBH1PGM 
02757         END-IF                                                    GBH1PGM 
02758       WHEN '3'                                                    GBH1PGM 
02759         MOVE WS-VAL-LMT  TO  WS-VALUE-LIMIT-V                     GBH1PGM 
02760         MOVE WS-VALUE-LIMIT-FULL   TO  WS-DAYS-VAL                GBH1PGM 
02761         MOVE WS-DAYS       TO                                     GBH1PGM 
02762          GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, GCBHS-COL-IDX, 1)    GBH1PGM 
02763       WHEN '4'                                                    GBH1PGM 
02764         MOVE WS-VAL-LMT  TO  WS-VALUE-LIMIT-V                     GBH1PGM 
02765         IF WS-VAL-LMT  = .01                                      GBH1PGM 
02766           MOVE WS-VALUE-LIMIT-FULL  TO  WS-UNITS-VAL-1            GBH1PGM 
02767           MOVE WS-UNITS-1   TO                                    GBH1PGM 
02768          GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, GCBHS-COL-IDX, 1)    GBH1PGM 
02769         ELSE                                                      GBH1PGM 
02770           MOVE WS-VALUE-LIMIT-FULL   TO  WS-UNITS-VAL             GBH1PGM 
02771           MOVE WS-UNITS      TO                                   GBH1PGM 
02772          GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, GCBHS-COL-IDX, 1)    GBH1PGM 
02773         END-IF                                                    GBH1PGM 
02774       WHEN '5'                                                    GBH1PGM 
02775         IF WS-VAL-LMT =                                           GBH1PGM 
02776              +9999999.99  OR +9999999.00 OR +0999999.99           GBH1PGM 
02777           MOVE WS-UNLMTD   TO                                     GBH1PGM 
02778           GCBHS-BENEFIT-VALUE(GCBHS-BEN-IDX, GCBHS-COL-IDX, 1)    GBH1PGM 
02779         ELSE                                                      GBH1PGM 
02780           MOVE WS-VAL-LMT  TO  WS-VALUE-LIMIT-MIL                 GBH1PGM 
02781           MOVE WS-VALUE-LIMIT-MIL TO                              GBH1PGM 
02782           GCBHS-BENEFIT-VALUE(GCBHS-BEN-IDX, GCBHS-COL-IDX, 1)    GBH1PGM 
02783         END-IF                                                    GBH1PGM 
02784       WHEN '6'                                                    GBH1PGM 
02785         MOVE WS-VAL-LMT            TO  WS-VALUE-LIMIT-V           GBH1PGM 
02786         MOVE WS-VALUE-LIMIT-FULL   TO  WS-CONFINEMENTS-VAL        GBH1PGM 
02787         MOVE WS-CONFINEMENTS       TO                             GBH1PGM 
02788           GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, GCBHS-COL-IDX, 1)   GBH1PGM 
02789       WHEN '7'                                                    GBH1PGM 
02790          MOVE WS-VAL-LMT        TO WS-VALUE-LIMIT-DEC             GBH1PGM 
02791          MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL                 GBH1PGM 
02792          MOVE WS-PEOPLE         TO                                GBH1PGM 
02793            GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, GCBHS-COL-IDX, 1)  GBH1PGM 
02794       WHEN 'B'                                                    GBH1PGM 
02795         MOVE WS-VAL-LMT  TO  WS-VALUE-LIMIT-V                     GBH1PGM 
02796         MOVE WS-VALUE-LIMIT-FULL   TO  WS-HOURS-VAL               GBH1PGM 
02797         MOVE WS-HOURS      TO                                     GBH1PGM 
02798          GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, GCBHS-COL-IDX, 1)    GBH1PGM 
02799       WHEN OTHER                                                  GBH1PGM 
02800         MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)                     GBH1PGM 
02801                                    TO  WS-VALUE-LIMIT             GBH1PGM 
02802         MOVE WS-VALUE-LIMIT   TO                                  GBH1PGM 
02803            GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, GCBHS-COL-IDX, 1)  GBH1PGM 
02804      END-EVALUATE.                                                GBH1PGM 
02805                                                                   GBH1PGM 
02806 *    IF WS-BEN-PERIOD = '0C'                                      GBH1PGM 
02807 *       AND WS-BEN-PER-TIME-QUAL NOT = '0'                        GBH1PGM 
02808 *       AND GCBHA-ACCUM-TYPE (GCBHA-TYPE-X) = '#ABM  '            GBH1PGM 
02809 *       EVALUATE WS-BEN-PER-TIME-QUAL                             GBH1PGM 
02810 *         WHEN '1'                                                GBH1PGM 
02811 *          MOVE ' DAY(S)'    TO  WS-TIME-QUAL                     GBH1PGM 
02812 *         WHEN '2'                                                GBH1PGM 
02813 *          MOVE ' WEEK(S)'   TO  WS-TIME-QUAL                     GBH1PGM 
02814 *         WHEN '3'                                                GBH1PGM 
02815 *          MOVE ' MONTH(S)'  TO  WS-TIME-QUAL                     GBH1PGM 
02816 *         WHEN '4'                                                GBH1PGM 
02817 *          MOVE ' YEAR(S)'   TO  WS-TIME-QUAL                     GBH1PGM 
02818 *       END-EVALUATE                                              GBH1PGM 
02819 *       MOVE WS-DISP-TIME-QUAL TO                                 GBH1PGM 
02820 *          GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, GCBHS-COL-IDX, 2)  GBH1PGM 
02821 *    END-IF.                                                      GBH1PGM 
02822                                                                   GBH1PGM 
02823                                                                   GBH1PGM 
02824  5900-EXIT.                                                       GBH1PGM 
02825      EXIT.                                                        GBH1PGM 
02826                                                                   GBH1PGM 
02827                                                                   GBH1PGM 
02828  5901-DISP-BEN-PERIOD.                                            GBH1PGM 
02829                                                                   GBH1PGM 
02830      EVALUATE WS-BEN-PERIOD                                       GBH1PGM 
02831        WHEN '0A'                                                  GBH1PGM 
02832            MOVE SPACES TO WS-BEN-PER-STD                          GBH1PGM 
02833            MOVE 'CARE INTERVAL   ' TO WS-BEN-PER-STD              GBH1PGM 
02834        WHEN '0B'                                                  GBH1PGM 
02835            MOVE SPACES TO WS-BEN-PER-STD                          GBH1PGM 
02836            MOVE 'LIFETIME        ' TO WS-BEN-PER-STD              GBH1PGM 
02837        WHEN '0I'                                                  GBH1PGM 
02838            MOVE SPACES TO WS-BEN-PER-STD                          GBH1PGM 
02839            MOVE 'OCCURRENCE      ' TO WS-BEN-PER-STD              GBH1PGM 
02840        WHEN '0K'                                                  GBH1PGM 
02841            MOVE SPACES TO WS-BEN-PER-STD                          GBH1PGM 
02842            MOVE 'MONTH           ' TO WS-BEN-PER-STD              GBH1PGM 
02843        WHEN '0Y'                                                  GBH1PGM 
02844            MOVE SPACES TO WS-BEN-PER-STD                          GBH1PGM 
02845            MOVE 'VISIT/UNIT      ' TO WS-BEN-PER-STD              GBH1PGM 
02846        WHEN '0C'                                                  GBH1PGM 
02847          IF  WS-BEN-PER-TIME-QUAL NOT = '0'                       GBH1PGM 
02848           EVALUATE WS-BEN-PER-TIME-QUAL                           GBH1PGM 
02849            WHEN '1'                                               GBH1PGM 
02850             MOVE ' DAY(S)'    TO  WS-TIME-QUAL                    GBH1PGM 
02851            WHEN '2'                                               GBH1PGM 
02852             MOVE ' WEEK(S)'   TO  WS-TIME-QUAL                    GBH1PGM 
02853            WHEN '3'                                               GBH1PGM 
02854             MOVE ' MONTH(S)'  TO  WS-TIME-QUAL                    GBH1PGM 
02855            WHEN '4'                                               GBH1PGM 
02856             MOVE ' YEAR(S)'   TO  WS-TIME-QUAL                    GBH1PGM 
02857           END-EVALUATE                                            GBH1PGM 
02858          END-IF                                                   GBH1PGM 
02859        WHEN OTHER                                                 GBH1PGM 
02860           GO TO 5901-EXIT                                         GBH1PGM 
02861      END-EVALUATE.                                                GBH1PGM 
02862                                                                   GBH1PGM 
02863      MOVE WS-DISP-TIME-QUAL TO                                    GBH1PGM 
02864        GCBHS-BENEFIT-VALUE (GCBHS-BEN-IDX, GCBHS-COL-IDX, 2).     GBH1PGM 
02865                                                                   GBH1PGM 
02866                                                                   GBH1PGM 
02867  5901-EXIT.                                                       GBH1PGM 
02868      EXIT.                                                        GBH1PGM 
02869                                                                   GBH1PGM 
02870                                                                   GBH1PGM 
02871                                                                   GBH1PGM 
02872 ******************************************************************GBH1PGM 
02873 *                                                                 GBH1PGM 
02874 *  TEST ACCUM OCCURRENCE FOR CERTAIN CONDITIONS                   GBH1PGM 
02875 *                                                                 GBH1PGM 
02876 ******************************************************************GBH1PGM 
02877  6000-OCCUR-TESTS.                                                GBH1PGM 
02878                                                                   GBH1PGM 
02879      EVALUATE GCBHA-ACCUM-TYPE (GCBHA-TYPE-X)                     GBH1PGM 
02880        WHEN '#ABM  '                                              GBH1PGM 
02881         MOVE GAA-BAMA-RELATIONSHIP-IND (GAA-INDEX)                GBH1PGM 
02882                 TO WS-SAVE-RELAT                                  GBH1PGM 
02883        WHEN '#ACL  '                                              GBH1PGM 
02884         MOVE GAB-COINS-RELATIONSHIP-IND (GAB-INDEX)               GBH1PGM 
02885                 TO WS-SAVE-RELAT                                  GBH1PGM 
02886        WHEN '#ACP  '                                              GBH1PGM 
02887         MOVE GAF-COPAY-RELATIONSHIP-IND (GAF-INDEX)               GBH1PGM 
02888                 TO WS-SAVE-RELAT                                  GBH1PGM 
02889        WHEN '#ADL  '                                              GBH1PGM 
02890         MOVE GAC-DEDL-RELATIONSHIP-IND (GAC-INDEX)                GBH1PGM 
02891                 TO WS-SAVE-RELAT                                  GBH1PGM 
02892        WHEN '#AOL  '                                              GBH1PGM 
02893         MOVE GAD-O-P-X-RELATIONSHIP-IND (GAD-INDEX)               GBH1PGM 
02894                 TO WS-SAVE-RELAT                                  GBH1PGM 
02895       END-EVALUATE.                                               GBH1PGM 
02896                                                                   GBH1PGM 
02897       IF WS-SAVE-RELAT  NOT = '00'                                GBH1PGM 
02898          PERFORM 6100-IMC-REL-TEST  THRU 6100-EXIT                GBH1PGM 
02899       END-IF.                                                     GBH1PGM 
02900                                                                   GBH1PGM 
02901                                                                   GBH1PGM 
02902                                                                   GBH1PGM 
02903                                                                   GBH1PGM 
02904  6000-EXIT.                                                       GBH1PGM 
02905      EXIT.                                                        GBH1PGM 
02910                                                                   GBH1PGM 
02911  6100-IMC-REL-TEST.                                               GBH1PGM 
02912                                                                   GBH1PGM 
02913      EVALUATE WS-SAVE-RELAT                                       GBH1PGM 
02914        WHEN '01'                                                  GBH1PGM 
02915          IF IMC-SPS OR IMC-DEP                                    GBH1PGM 
02916             MOVE 'N'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02917          END-IF                                                   GBH1PGM 
02918        WHEN '02'                                                  GBH1PGM 
02919          IF IMC-NO-SPS AND IMC-NO-DEP                             GBH1PGM 
02920             MOVE 'N'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02921          END-IF                                                   GBH1PGM 
02922        WHEN '03'                                                  GBH1PGM 
02923          CONTINUE                                                 GBH1PGM 
02924        WHEN '04'                                                  GBH1PGM 
02925          IF IMC-NO-SPS OR IMC-DEP                                 GBH1PGM 
02926             MOVE 'N'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02927          END-IF                                                   GBH1PGM 
02928        WHEN '05'                                                  GBH1PGM 
02929          IF IMC-SPS OR IMC-NO-DEP                                 GBH1PGM 
02930             MOVE 'N'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02931          END-IF                                                   GBH1PGM 
02932        WHEN '06'                                                  GBH1PGM 
02933          IF IMC-NO-SPS OR IMC-NO-DEP                              GBH1PGM 
02934             MOVE 'N'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02935          END-IF                                                   GBH1PGM 
02936        WHEN '07'                                                  GBH1PGM 
02937          IF IMC-SPS OR IMC-DEP                                    GBH1PGM 
02938             MOVE 'N'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02939          END-IF                                                   GBH1PGM 
02940        WHEN '08'                                                  GBH1PGM 
02941          IF IMC-NO-SPS OR IMC-DEP                                 GBH1PGM 
02942             MOVE 'N'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02943          END-IF                                                   GBH1PGM 
02944        WHEN '09'                                                  GBH1PGM 
02945          IF IMC-SPS OR IMC-NO-DEP                                 GBH1PGM 
02946             MOVE 'N'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02947          END-IF                                                   GBH1PGM 
02948        WHEN '10'                                                  GBH1PGM 
02949          IF IMC-NO-SPS OR IMC-NO-DEP                              GBH1PGM 
02950             MOVE 'N'    TO WS-OCCUR-OK-SW                         GBH1PGM 
02951          END-IF                                                   GBH1PGM 
      *Added logic for rel-code 11-16 to fix CQ#18535                   0000000 
             WHEN '11'                                                  0000000 
               CONTINUE                                                         
      *        IF IMC-SPS OR IMC-DEP                                    0000000 
      *           MOVE 'N'    TO WS-OCCUR-OK-SW                         0000000 
      *        END-IF                                                   0000000 
      *IF IMC INDICATES SPOUSE AND CHILD OR CHILDREN SKIP THIS OCCURS   0000000 
      * REL IND 12 IS FOR MEMBER AND EITHER SPUSE OR ONE DEP AKK 1/3/7  0000000 
             WHEN '12'                                                  0000000 
               IF IMC-MULT-DEP OR (IMC-SPS AND IMC-DEP)                 0000000 
                  MOVE 'N'    TO WS-OCCUR-OK-SW                         0000000 
01361          END-IF                                                   7928774 
             WHEN '13'                                                  0000000 
               IF IMC-NO-SPS OR IMC-NO-DEP                              0000000 
                  MOVE 'N'    TO WS-OCCUR-OK-SW                         0000000 
               END-IF                                                   0000000 
             WHEN '14'                                                  0000000 
               IF IMC-SPS OR IMC-DEP                                    0000000 
                  MOVE 'N'    TO WS-OCCUR-OK-SW                         0000000 
               END-IF                                                   0000000 
      *12 and 15 are almost the same, but use different fam and         0000000 
      * single codes, see grbfpgm for  explanation 1/23/07   AKK        0000000 
             WHEN '15'                                                  0000000 
               IF IMC-MULT-DEP                                          0000000 
                  OR                                                            
                  (IMC-SPS AND IMC-DEP)                                         
                  OR                                                            
                  (IMC-NO-SPS AND IMC-NO-DEP)                                   
                   MOVE 'N'    TO WS-OCCUR-OK-SW                        0000000 
01361          END-IF                                                   7928774 
01357        WHEN '16'                                                  7928774 
01358          IF (IMC-NO-SPS AND IMC-NO-DEP)                           7928774 
                  OR                                                            
                  (IMC-SPS AND IMC-NO-DEP)                                      
                  OR                                                            
                  (IMC-NO-SPS AND IMC-ONE-DEP)                                  
01359              MOVE 'N'    TO WS-OCCUR-OK-SW                        7928774 
01360          END-IF                                                   7928774 
02952      END-EVALUATE.                                                GBH1PGM 
02953                                                                   GBH1PGM 
02954                                                                   GBH1PGM 
02955                                                                   GBH1PGM 
02956  6100-EXIT.                                                       GBH1PGM 
02957      EXIT.                                                        GBH1PGM 
02958                                                                   GBH1PGM 
02959                                                                   GBH1PGM 
02960 ******************************************************************GBH1PGM 
02961 *                                                                 GBH1PGM 
02962 *  BROWSE CONTRACT FOR THE DATE OF SERVICE ENTERED                GBH1PGM 
02963 *                                                                 GBH1PGM 
02964 ******************************************************************GBH1PGM 
02965  8000-SEARCH-CONTRACT.                                            GBH1PGM 
02966                                                                   GBH1PGM 
02967      PERFORM 8001-CONTRACT-STARTBR  THRU 8001-EXIT                GBH1PGM 
02968                                                                   GBH1PGM 
02969      IF GCIO3-RETURN-CODE NOT = '0'                               GBH1PGM 
02970         GO TO 8000-EXIT.                                          GBH1PGM 
02971                                                                   GBH1PGM 
02972      IF GCT-PLAN-CODE         NOT =  '000'                OR      GBH1PGM 
02973         GCT-GROUP-NUM         NOT =   GCBHS-GROUP-NBR     OR      GBH1PGM 
02974         GCT-SECTION-NUM       NOT =   GCBHS-SECT-NUM      OR      GBH1PGM 
02975         GCT-PKG-CODE          NOT =   GCBHS-PACKAGE-CODE  OR      GBH1PGM 
02976         GCT-L-O-B             NOT =  '4'                  OR      GBH1PGM 
02977         GCT-PROVDR-CONTROL    NOT =  '00'                 OR      GBH1PGM 
02978         GCT-FAM-REL-LVL       NOT =  '00'                         GBH1PGM 
LOB123            PERFORM 8003-CONTRACT-ENDBR THRU 8003-EXIT                    
02979             GO TO 8000-EXIT                                       GBH1PGM 
02980      END-IF.                                                      GBH1PGM 
02981                                                                   GBH1PGM 
LOB123     MOVE 'Y'                 TO WS-LOB4-EXISTS-SW                GBH1PGM 
02982      IF (WS-SERV-DT-CEN   >=  GCT-EFFDT-CEN)  AND                 GBH1PGM 
02983         (WS-SERV-DT-CEN   <=  GCT-TERMDT-CEN)                     GBH1PGM 
02984            MOVE 'Y' TO WS-RECORD-FOUND-SW                         GBH1PGM 
02985         IF GCT-INTER-REL-CD = ZEROES                              GBH1PGM 
02986            MOVE 'Y' TO WS-ERROR-FOUND-SW                          GBH1PGM 
02987            SET WS-ERR-IDX TO +06                                  GBH1PGM 
02988         END-IF                                                    GBH1PGM 
02989      END-IF.                                                      GBH1PGM 
02990                                                                   GBH1PGM 
02991      PERFORM 8002-CONTRACT-READ-NEXT  THRU 8002-EXIT              GBH1PGM 
02992         UNTIL  RECORD-FOUND  OR  END-OF-READ.                     GBH1PGM 
02993                                                                   GBH1PGM 
02994                                                                   GBH1PGM 
02995      PERFORM 8003-CONTRACT-ENDBR THRU 8003-EXIT.                  GBH1PGM 
02996                                                                   GBH1PGM 
02997  8000-EXIT.                                                       GBH1PGM 
02998      EXIT.                                                        GBH1PGM 
02999                                                                   GBH1PGM 
03000 ******************************************************************GBH1PGM 
03001 *                                                                 GBH1PGM 
03002 *  CONTRACT FILE START BROWSE                                     GBH1PGM 
03003 *                                                                 GBH1PGM 
03004 ******************************************************************GBH1PGM 
03005  8001-CONTRACT-STARTBR.                                           GBH1PGM 
03006                                                                   GBH1PGM 
03007      COMPUTE  WS-IO-PARM-CONTRACT-LEN      =                      GBH1PGM 
03008               GC-GCIOPARM-LEN              +                      GBH1PGM 
03009               GC-GCCONTR-FIXED-LEN         +                      GBH1PGM 
03010             ( GC-GCCONTR-VARY-MAX-OCUR     *                      GBH1PGM 
03011               GC-GCCONTR-VARY-LEN ).                              GBH1PGM 
03012                                                                   GBH1PGM 
03013                                                                   GBH1PGM 
03014      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GBH1PGM 
03015                                   TO GCT-COUNT-BEN-PROVN-POINTERS.GBH1PGM 
03016      MOVE 'SB '                   TO GCIO3-FILE-ACCESS-CODE.      GBH1PGM 
03017      MOVE 'GTE'                   TO GCIO3-BROWSE-QUAL-CODE       GBH1PGM 
03018      MOVE 'GCCONTR '              TO GCIO3-FILE-DDNAME.           GBH1PGM 
03019      MOVE '1'                     TO GCIO3-IO-AREA-TO-USE.        GBH1PGM 
03020      MOVE GCT-CONTRACT-ID         TO GCIO3-FILE-KEY.              GBH1PGM 
03021                                                                   GBH1PGM 
03022      EXEC CICS LINK   PROGRAM ('GCIOPGM')                         GBH1PGM 
03023                       COMMAREA(IO-PARM-CONTRACT-AREA-1)           GBH1PGM 
03024                       LENGTH  (WS-IO-PARM-CONTRACT-LEN)           GBH1PGM 
03025                       END-EXEC.                                   GBH1PGM 
03026                                                                   GBH1PGM 
03027  8001-EXIT.                                                       GBH1PGM 
03028      EXIT.                                                        GBH1PGM 
03029                                                                   GBH1PGM 
03030 ******************************************************************GBH1PGM 
03031 *                                                                 GBH1PGM 
03032 *  CONTRACT FILE READ NEXT                                        GBH1PGM 
03033 *                                                                 GBH1PGM 
03034 ******************************************************************GBH1PGM 
03035  8002-CONTRACT-READ-NEXT.                                         GBH1PGM 
03036                                                                   GBH1PGM 
03037                                                                   GBH1PGM 
03038      MOVE 'RN '                   TO GCIO3-FILE-ACCESS-CODE.      GBH1PGM 
03039      MOVE 'GCCONTR '              TO GCIO3-FILE-DDNAME.           GBH1PGM 
03040      MOVE '1'                     TO GCIO3-IO-AREA-TO-USE.        GBH1PGM 
03041                                                                   GBH1PGM 
03042      EXEC CICS LINK   PROGRAM ('GCIOPGM')                         GBH1PGM 
03043                       COMMAREA(IO-PARM-CONTRACT-AREA-1)           GBH1PGM 
03044                       LENGTH  (WS-IO-PARM-CONTRACT-LEN)           GBH1PGM 
03045                       END-EXEC.                                   GBH1PGM 
03046                                                                   GBH1PGM 
03047      IF GCIO3-END-OF-FILE                                         GBH1PGM 
03048         MOVE 'Y' TO WS-END-OF-READ-SW                             GBH1PGM 
03049         GO TO 8002-EXIT                                           GBH1PGM 
03050      END-IF                                                       GBH1PGM 
03051                                                                   GBH1PGM 
03052      IF GCT-PLAN-CODE         NOT =  '000'                OR      GBH1PGM 
03053         GCT-GROUP-NUM         NOT =   GCBHS-GROUP-NBR     OR      GBH1PGM 
03054         GCT-SECTION-NUM       NOT =   GCBHS-SECT-NUM      OR      GBH1PGM 
03055         GCT-PKG-CODE          NOT =   GCBHS-PACKAGE-CODE  OR      GBH1PGM 
03056         GCT-L-O-B             NOT =  '4'                  OR      GBH1PGM 
03057         GCT-PROVDR-CONTROL    NOT =  '00'                 OR      GBH1PGM 
03058         GCT-FAM-REL-LVL       NOT =  '00'                         GBH1PGM 
03059             MOVE 'Y' TO WS-END-OF-READ-SW                         GBH1PGM 
03060             GO TO 8002-EXIT                                       GBH1PGM 
03061      END-IF.                                                      GBH1PGM 
03062                                                                   GBH1PGM 
03063      IF (WS-SERV-DT-CEN   >=  GCT-EFFDT-CEN)  AND                 GBH1PGM 
03064         (WS-SERV-DT-CEN   <=  GCT-TERMDT-CEN)                     GBH1PGM 
03065            MOVE 'Y' TO WS-RECORD-FOUND-SW                         GBH1PGM 
03066         IF GCT-INTER-REL-CD = ZEROES                              GBH1PGM 
03067            MOVE 'Y' TO WS-ERROR-FOUND-SW                          GBH1PGM 
03068            SET WS-ERR-IDX TO +06                                  GBH1PGM 
03069         END-IF                                                    GBH1PGM 
03070      END-IF.                                                      GBH1PGM 
03071                                                                   GBH1PGM 
03072                                                                   GBH1PGM 
03073  8002-EXIT.                                                       GBH1PGM 
03074      EXIT.                                                        GBH1PGM 
03075                                                                   GBH1PGM 
03076 ******************************************************************GBH1PGM 
03077 *                                                                 GBH1PGM 
03078 *  CONTRACT FILE END   BROWSE                                     GBH1PGM 
03079 *                                                                 GBH1PGM 
03080 ******************************************************************GBH1PGM 
03081  8003-CONTRACT-ENDBR.                                             GBH1PGM 
03082                                                                   GBH1PGM 
03083      MOVE 'EB '                   TO GCIO3-FILE-ACCESS-CODE.      GBH1PGM 
03084      MOVE 'GCCONTR '              TO GCIO3-FILE-DDNAME.           GBH1PGM 
03085      MOVE '1'                     TO GCIO3-IO-AREA-TO-USE.        GBH1PGM 
03086                                                                   GBH1PGM 
03087      EXEC CICS LINK   PROGRAM ('GCIOPGM')                         GBH1PGM 
03088                       COMMAREA(IO-PARM-CONTRACT-AREA-1)           GBH1PGM 
03089                       LENGTH  (WS-IO-PARM-CONTRACT-LEN)           GBH1PGM 
03090                       END-EXEC.                                   GBH1PGM 
03091                                                                   GBH1PGM 
03092  8003-EXIT.                                                       GBH1PGM 
03093      EXIT.                                                        GBH1PGM 
03094                                                                   GBH1PGM 
03095                                                                   GBH1PGM 
LOB123******************************************************************GBH1PGM 
LOB123*                                                                 GBH1PGM 
LOB123*  BROWSE CONTRACT FOR THE DATE OF SERVICE ENTERED FOR LOB123     GBH1PGM 
LOB123*                                                                 GBH1PGM 
LOB123******************************************************************GBH1PGM 
LOB123 8050-SEARCH-CONTRACT-LOB123.                                     GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123     MOVE 'N'                   TO   WS-END-OF-READ-SW            GBH1PGM 
LOB123                                     WS-LOB1-EXISTS-SW            GBH1PGM 
LOB123                                     WS-LOB2-EXISTS-SW            GBH1PGM 
LOB123                                     WS-LOB3-EXISTS-SW            GBH1PGM 
LOB123     MOVE 'N'                   TO   WS-RECORD-FOUND-LOB1-SW      GBH1PGM 
LOB123                                     WS-RECORD-FOUND-LOB2-SW      GBH1PGM 
LOB123                                     WS-RECORD-FOUND-LOB3-SW      GBH1PGM 
LOB123     MOVE '000'                 TO   GCT-PLAN-CODE                GBH1PGM 
LOB123     MOVE GCBHS-GROUP-NBR       TO   GCT-GROUP-NUM                GBH1PGM 
LOB123     MOVE GCBHS-SECT-NUM        TO   GCT-SECTION-NUM              GBH1PGM 
LOB123     MOVE GCBHS-PACKAGE-CODE    TO   GCT-PKG-CODE                 GBH1PGM 
LOB123     MOVE LOW-VALUES            TO   GCT-L-O-B                    GBH1PGM 
LOB123     MOVE LOW-VALUES            TO   GCT-PROVDR-CONTROL           GBH1PGM 
LOB123     MOVE LOW-VALUES            TO   GCT-FAM-REL-LVL              GBH1PGM 
LOB123     MOVE  +25                  TO   GCIO3-BROWSE-KEYLEN          GBH1PGM 
LOB123     PERFORM 8001-CONTRACT-STARTBR  THRU 8001-EXIT                GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123     IF GCIO3-RETURN-CODE NOT = '0'                               GBH1PGM 
LOB123        GO TO 8050-EXIT.                                          GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123     IF GCT-PLAN-CODE         NOT =  '000'                OR      GBH1PGM 
LOB123        GCT-GROUP-NUM         NOT =   GCBHS-GROUP-NBR     OR      GBH1PGM 
LOB123        GCT-SECTION-NUM       NOT =   GCBHS-SECT-NUM      OR      GBH1PGM 
LOB123        GCT-PKG-CODE          NOT =   GCBHS-PACKAGE-CODE          GBH1PGM 
LOB123            GO TO 8050-EXIT                                       GBH1PGM 
LOB123     END-IF.                                                      GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123     EVALUATE GCT-L-O-B                                                   
LOB123       WHEN '1'                                                           
LOB123        MOVE 'Y' TO WS-LOB1-EXISTS-SW                             GBH1PGM 
LOB123       WHEN '2'                                                           
LOB123        MOVE 'Y' TO WS-LOB2-EXISTS-SW                             GBH1PGM 
LOB123       WHEN '3'                                                           
LOB123        MOVE 'Y' TO WS-LOB3-EXISTS-SW                             GBH1PGM 
LOB123     END-EVALUATE                                                 GBH1PGM 
LOB123     IF (WS-SERV-DT-CEN   >=  GCT-EFFDT-CEN)  AND                 GBH1PGM 
LOB123        (WS-SERV-DT-CEN   <=  GCT-TERMDT-CEN)                     GBH1PGM 
LOB123        EVALUATE GCT-L-O-B                                                
LOB123          WHEN '1'                                                        
LOB123           MOVE 'Y' TO WS-RECORD-FOUND-LOB1-SW                    GBH1PGM 
LOB123          WHEN '2'                                                        
LOB123           MOVE 'Y' TO WS-RECORD-FOUND-LOB2-SW                    GBH1PGM 
LOB123          WHEN '3'                                                        
LOB123           MOVE 'Y' TO WS-RECORD-FOUND-LOB3-SW                    GBH1PGM 
LOB123        END-EVALUATE                                              GBH1PGM 
LOB123     END-IF.                                                      GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123     PERFORM 8052-CONTRACT-READ-NEXT-LOB123  THRU 8052-EXIT       GBH1PGM 
LOB123        UNTIL  RECORD-FOUND-LOB1   OR                                     
LOB123               RECORD-FOUND-LOB2   OR                                     
LOB123               RECORD-FOUND-LOB3   OR                                     
LOB123               END-OF-READ.                                               
LOB123                                                                  GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123     PERFORM 8003-CONTRACT-ENDBR THRU 8003-EXIT.                  GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123 8050-EXIT.                                                       GBH1PGM 
LOB123     EXIT.                                                        GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123******************************************************************GBH1PGM 
LOB123*                                                                 GBH1PGM 
LOB123*  CONTRACT FILE READ NEXT                                        GBH1PGM 
LOB123*                                                                 GBH1PGM 
LOB123******************************************************************GBH1PGM 
LOB123 8052-CONTRACT-READ-NEXT-LOB123.                                  GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123     MOVE 'RN '                   TO GCIO3-FILE-ACCESS-CODE.      GBH1PGM 
LOB123     MOVE 'GCCONTR '              TO GCIO3-FILE-DDNAME.           GBH1PGM 
LOB123     MOVE '1'                     TO GCIO3-IO-AREA-TO-USE.        GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123     EXEC CICS LINK   PROGRAM ('GCIOPGM')                         GBH1PGM 
LOB123                      COMMAREA(IO-PARM-CONTRACT-AREA-1)           GBH1PGM 
LOB123                      LENGTH  (WS-IO-PARM-CONTRACT-LEN)           GBH1PGM 
LOB123                      END-EXEC.                                   GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123     IF GCIO3-END-OF-FILE                                         GBH1PGM 
LOB123        MOVE 'Y' TO WS-END-OF-READ-SW                             GBH1PGM 
LOB123        GO TO 8052-EXIT                                           GBH1PGM 
LOB123     END-IF                                                       GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123     IF GCT-PLAN-CODE         NOT =  '000'                OR      GBH1PGM 
LOB123        GCT-GROUP-NUM         NOT =   GCBHS-GROUP-NBR     OR      GBH1PGM 
LOB123        GCT-SECTION-NUM       NOT =   GCBHS-SECT-NUM      OR      GBH1PGM 
LOB123        GCT-PKG-CODE          NOT =   GCBHS-PACKAGE-CODE          GBH1PGM 
LOB123            MOVE 'Y' TO WS-END-OF-READ-SW                         GBH1PGM 
LOB123            GO TO 8052-EXIT                                       GBH1PGM 
LOB123     END-IF.                                                      GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123     EVALUATE GCT-L-O-B                                                   
LOB123       WHEN '1'                                                           
LOB123        MOVE 'Y' TO WS-LOB1-EXISTS-SW                             GBH1PGM 
LOB123       WHEN '2'                                                           
LOB123        MOVE 'Y' TO WS-LOB2-EXISTS-SW                             GBH1PGM 
LOB123       WHEN '3'                                                           
LOB123        MOVE 'Y' TO WS-LOB3-EXISTS-SW                             GBH1PGM 
LOB123     END-EVALUATE                                                 GBH1PGM 
LOB123     IF (WS-SERV-DT-CEN   >=  GCT-EFFDT-CEN)  AND                 GBH1PGM 
LOB123        (WS-SERV-DT-CEN   <=  GCT-TERMDT-CEN)                     GBH1PGM 
LOB123        EVALUATE GCT-L-O-B                                                
LOB123          WHEN '1'                                                        
LOB123           MOVE 'Y' TO WS-RECORD-FOUND-LOB1-SW                    GBH1PGM 
LOB123          WHEN '2'                                                        
LOB123           MOVE 'Y' TO WS-RECORD-FOUND-LOB2-SW                    GBH1PGM 
LOB123          WHEN '3'                                                        
LOB123           MOVE 'Y' TO WS-RECORD-FOUND-LOB3-SW                    GBH1PGM 
LOB123        END-EVALUATE                                              GBH1PGM 
LOB123     END-IF.                                                      GBH1PGM 
LOB123                                                                  GBH1PGM 
LOB123 8052-EXIT.                                                       GBH1PGM 
LOB123     EXIT.                                                        GBH1PGM 
LOB123                                                                  GBH1PGM 
03096 ******************************************************************GBH1PGM 
03097 *                                                                 GBH1PGM 
03098 *  BROWSE GROUP SPECIFIC FOR THE DATE OF SERVICE ENTERED          GBH1PGM 
03099 *                                                                 GBH1PGM 
03100 ******************************************************************GBH1PGM 
03101  8100-SEARCH-GRPSPEC.                                             GBH1PGM 
03102                                                                   GBH1PGM 
03103      PERFORM 8101-GROUP-SPEC-STARTBR THRU 8101-EXIT               GBH1PGM 
03104                                                                   GBH1PGM 
03105      IF GCIO-RETURN-CODE NOT = '0'                                GBH1PGM 
03106         GO TO 8100-EXIT.                                          GBH1PGM 
03107                                                                   GBH1PGM 
03108      IF GCG-PLAN-CODE         NOT =  '000'                OR      GBH1PGM 
03109         GCG-GROUP-NUM         NOT =   GCBHS-GROUP-NBR     OR      GBH1PGM 
03110         GCG-SECTION-NUM       NOT =   GCBHS-SECT-NUM      OR      GBH1PGM 
03111         GCG-PKG-CODE          NOT =   GCBHS-PACKAGE-CODE  OR      GBH1PGM 
03112         GCG-FAM-REL-LVL       NOT =  '00'                         GBH1PGM 
03113             GO TO 8100-EXIT                                       GBH1PGM 
03114      END-IF.                                                      GBH1PGM 
03115                                                                   GBH1PGM 
03116      IF (WS-SERV-DT-CEN   >=  GCG-EFFDT-CEN)  AND                 GBH1PGM 
03117         (WS-SERV-DT-CEN   <=  GCG-TERMDT-CEN)                     GBH1PGM 
03118           MOVE 'Y' TO WS-RECORD-FOUND-SW                          GBH1PGM 
03119         IF GCG-INTER-RELATIONAL-CODE  = ZEROES                    GBH1PGM 
03120            MOVE 'Y' TO WS-ERROR-FOUND-SW                          GBH1PGM 
03121            SET WS-ERR-IDX TO +06                                  GBH1PGM 
03122         END-IF                                                    GBH1PGM 
03123      END-IF.                                                      GBH1PGM 
03124                                                                   GBH1PGM 
03125                                                                   GBH1PGM 
03126      PERFORM 8102-GROUP-SPEC-READ-NEXT THRU 8102-EXIT             GBH1PGM 
03127         UNTIL  RECORD-FOUND  OR  END-OF-READ.                     GBH1PGM 
03128                                                                   GBH1PGM 
03129      PERFORM 8103-GROUP-SPEC-ENDBR THRU 8103-EXIT.                GBH1PGM 
03130                                                                   GBH1PGM 
03131                                                                   GBH1PGM 
03132  8100-EXIT.                                                       GBH1PGM 
03133      EXIT.                                                        GBH1PGM 
03134                                                                   GBH1PGM 
03135                                                                   GBH1PGM 
03136 ******************************************************************GBH1PGM 
03137 *                                                                 GBH1PGM 
03138 *  START BROWSE ON GROUP SPECIFIC.                                GBH1PGM 
03139 *                                                                 GBH1PGM 
03140 ******************************************************************GBH1PGM 
03141  8101-GROUP-SPEC-STARTBR.                                         GBH1PGM 
03142                                                                   GBH1PGM 
03143      COMPUTE  WS-IO-PARM-GROUPSPC-LEN      =                      GBH1PGM 
03144               GC-GCIOPARM-LEN              +                      GBH1PGM 
03145               GC-GCGRPSPC-FIXED-LEN        +                      GBH1PGM 
03146             ( GC-GCGRPSPC-VARY-MAX-OCUR    *                      GBH1PGM 
03147               GC-GCGRPSPC-VARY-LEN ).                             GBH1PGM 
03148                                                                   GBH1PGM 
03149                                                                   GBH1PGM 
03150      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GBH1PGM 
03151                                   TO GCG-COUNT-TAB-PROVN-POINTERS.GBH1PGM 
03152      MOVE 'SB '                   TO GCIO-FILE-ACCESS-CODE.       GBH1PGM 
03153      MOVE 'GTE'                   TO GCIO-BROWSE-QUAL-CODE        GBH1PGM 
03154      MOVE 'GCGRPSPC'              TO GCIO-FILE-DDNAME.            GBH1PGM 
03155      MOVE '1'                     TO GCIO-IO-AREA-TO-USE.         GBH1PGM 
03156      MOVE GCG-GRP-SPECIF-ID       TO GCIO-FILE-KEY.               GBH1PGM 
03157                                                                   GBH1PGM 
03158      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         GBH1PGM 
03159                       COMMAREA(IO-PARM-GROUPSPC-AREA-1)           GBH1PGM 
03160                       LENGTH  (WS-IO-PARM-GROUPSPC-LEN)           GBH1PGM 
03161                       END-EXEC.                                   GBH1PGM 
03162                                                                   GBH1PGM 
03163  8101-EXIT.                                                       GBH1PGM 
03164      EXIT.                                                        GBH1PGM 
03165                                                                   GBH1PGM 
03166                                                                   GBH1PGM 
03167                                                                   GBH1PGM 
03168 ******************************************************************GBH1PGM 
03169 *                                                                 GBH1PGM 
03170 *  GROUP SPECIFIC FILE READ NEXT RECORD                           GBH1PGM 
03171 *                                                                 GBH1PGM 
03172 ******************************************************************GBH1PGM 
03173  8102-GROUP-SPEC-READ-NEXT.                                       GBH1PGM 
03174                                                                   GBH1PGM 
03175      MOVE 'RN '                   TO GCIO-FILE-ACCESS-CODE.       GBH1PGM 
03176      MOVE 'GCGRPSPC'              TO GCIO-FILE-DDNAME.            GBH1PGM 
03177      MOVE '1'                     TO GCIO-IO-AREA-TO-USE.         GBH1PGM 
03178                                                                   GBH1PGM 
03179      EXEC CICS LINK   PROGRAM ('GCIOPGM')                         GBH1PGM 
03180                       COMMAREA(IO-PARM-GROUPSPC-AREA-1)           GBH1PGM 
03181                       LENGTH  (WS-IO-PARM-GROUPSPC-LEN)           GBH1PGM 
03182                       END-EXEC.                                   GBH1PGM 
03183                                                                   GBH1PGM 
03184      IF GCIO-END-OF-FILE                                          GBH1PGM 
03185         MOVE 'Y' TO WS-END-OF-READ-SW                             GBH1PGM 
03186         GO TO 8102-EXIT                                           GBH1PGM 
03187      END-IF                                                       GBH1PGM 
03188                                                                   GBH1PGM 
03189      IF GCG-PLAN-CODE         NOT =  '000'                OR      GBH1PGM 
03190         GCG-GROUP-NUM         NOT =   GCBHS-GROUP-NBR     OR      GBH1PGM 
03191         GCG-SECTION-NUM       NOT =   GCBHS-SECT-NUM      OR      GBH1PGM 
03192         GCG-PKG-CODE          NOT =   GCBHS-PACKAGE-CODE  OR      GBH1PGM 
03193         GCG-FAM-REL-LVL       NOT =  '00'                         GBH1PGM 
03194             MOVE 'Y' TO WS-END-OF-READ-SW                         GBH1PGM 
03195             GO TO 8102-EXIT                                       GBH1PGM 
03196      END-IF.                                                      GBH1PGM 
03197                                                                   GBH1PGM 
03198      IF (WS-SERV-DT-CEN   >=  GCG-EFFDT-CEN)  AND                 GBH1PGM 
03199         (WS-SERV-DT-CEN   <=  GCG-TERMDT-CEN)                     GBH1PGM 
03200           MOVE 'Y' TO WS-RECORD-FOUND-SW                          GBH1PGM 
03201         IF GCG-INTER-RELATIONAL-CODE  = ZEROES                    GBH1PGM 
03202            MOVE 'Y' TO WS-ERROR-FOUND-SW                          GBH1PGM 
03203            SET WS-ERR-IDX TO +06                                  GBH1PGM 
03204            GO TO 8102-EXIT                                        GBH1PGM 
03205         END-IF                                                    GBH1PGM 
03206      END-IF.                                                      GBH1PGM 
03207                                                                   GBH1PGM 
03208 **restriction #4                                                  GBH1PGM 
03209 *    IF RECORD-FOUND                                              GBH1PGM 
03210 *       IF GCG-PARTICIPAT-PROV-OPTION = '00'                      GBH1PGM 
03211 *          AND GCG-NEW-POS-IND        = '00'                      GBH1PGM 
03212 *          MOVE 'Y' TO WS-ERROR-FOUND-SW                          GBH1PGM 
03213 *          SET WS-ERR-IDX TO +10                                  GBH1PGM 
03214 *       END-IF                                                    GBH1PGM 
03215 *    END-IF.                                                      GBH1PGM 
03216                                                                   GBH1PGM 
03217                                                                   GBH1PGM 
03218                                                                   GBH1PGM 
03219  8102-EXIT.                                                       GBH1PGM 
03220      EXIT.                                                        GBH1PGM 
03221                                                                   GBH1PGM 
03222                                                                   GBH1PGM 
03223 ******************************************************************GBH1PGM 
03224 *                                                                 GBH1PGM 
03225 *  END   BROWSE ON GROUP SPECIFIC.                                GBH1PGM 
03226 *                                                                 GBH1PGM 
03227 ******************************************************************GBH1PGM 
03228  8103-GROUP-SPEC-ENDBR.                                           GBH1PGM 
03229                                                                   GBH1PGM 
03230      MOVE 'EB '                   TO GCIO-FILE-ACCESS-CODE.       GBH1PGM 
03231      MOVE 'GCGRPSPC'              TO GCIO-FILE-DDNAME.            GBH1PGM 
03232      MOVE '1'                     TO GCIO-IO-AREA-TO-USE.         GBH1PGM 
03233                                                                   GBH1PGM 
03234      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         GBH1PGM 
03235                       COMMAREA(IO-PARM-GROUPSPC-AREA-1)           GBH1PGM 
03236                       LENGTH  (WS-IO-PARM-GROUPSPC-LEN)           GBH1PGM 
03237                       END-EXEC.                                   GBH1PGM 
03238                                                                   GBH1PGM 
03239  8103-EXIT.                                                       GBH1PGM 
03240      EXIT.                                                        GBH1PGM 
03241                                                                   GBH1PGM 
03242                                                                   GBH1PGM 
03243                                                                   GBH1PGM 
03244 /                                                                 GBH1PGM 
03245 ******************************************************************GBH1PGM 
03246 *                                                                 GBH1PGM 
03247 *    CONVERT DATE OF SERVICE                                      GBH1PGM 
03248 *                                                                 GBH1PGM 
03249 ******************************************************************GBH1PGM 
03250  8400-CONV-SERV-DATE.                                             GBH1PGM 
03251                                                                   GBH1PGM 
03252                                                                   GBH1PGM 
03253      MOVE 'CNV'  TO  MLDATE-FUNC.                                 GBH1PGM 
03254      MOVE 'Y'    TO  MLDATE-FORM1.                                GBH1PGM 
03255      MOVE GCBHS-DATE-OF-SERVICE   TO  MLDATE-DATE1.               GBH1PGM 
03256      MOVE 'J'    TO  MLDATE-FORM2.                                GBH1PGM 
03257      EXEC CICS LINK PROGRAM ('MLDATEC')                           GBH1PGM 
03258                     COMMAREA (MLDATE01)                           GBH1PGM 
03259                     END-EXEC.                                     GBH1PGM 
03260      MOVE MLDATE-JUL2   TO WS-SERV-DT-CEN                         GBH1PGM 
03261                                                                   GBH1PGM 
03262      IF MLDATE-RETURN NOT  = '00'                                 GBH1PGM 
03263         MOVE 'Y' TO WS-ERROR-FOUND-SW                             GBH1PGM 
03264         SET WS-ERR-IDX TO +02                                     GBH1PGM 
03265      END-IF.                                                      GBH1PGM 
03266                                                                   GBH1PGM 
03267                                                                   GBH1PGM 
03268  8400-EXIT.                                                       GBH1PGM 
03269      EXIT.                                                        GBH1PGM 
03270                                                                   GBH1PGM 
03271                                                                   GBH1PGM 
03272                                                                   GBH1PGM 
03273                                                                   GBH1PGM 
03274                                                                   GBH1PGM 
03275 ****************************************************************  GBH1PGM 
03276  8600-READ-TABULAR.                                               GBH1PGM 
03277                                                                   GBH1PGM 
03278      COMPUTE   WS-IO-PARM-TABULAR-LEN     =                       GBH1PGM 
03279                GC-GCIOPARM-LEN            +                       GBH1PGM 
03280                GC-GCTABULR-ABM-FIXED-LEN  +                       GBH1PGM 
03281               (GC-GCTABULR-ABM-VARY-LEN   *                       GBH1PGM 
03282                GC-GCTABULR-ABM-VARY-MAX-OCUR).                    GBH1PGM 
03283                                                                   GBH1PGM 
03284      MOVE 'RD '                    TO GCIO5-FILE-ACCESS-CODE.     GBH1PGM 
03285      MOVE 'GCTABULR'               TO GCIO5-FILE-DDNAME.          GBH1PGM 
03286      MOVE '1'                      TO GCIO5-IO-AREA-TO-USE.       GBH1PGM 
03287      MOVE GAA-TABULAR-PROVISION-ID TO GCIO5-FILE-KEY.             GBH1PGM 
03288                                                                   GBH1PGM 
03289      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR                           GBH1PGM 
03290        TO GAA-ENTRY-COUNT.                                        GBH1PGM 
03291                                                                   GBH1PGM 
03292      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GBH1PGM 
03293         COMMAREA(IO-PARM-TABULAR-AREA-1)                          GBH1PGM 
03294         LENGTH(WS-IO-PARM-TABULAR-LEN) END-EXEC.                  GBH1PGM 
03295                                                                   GBH1PGM 
03296      IF NOT GCIO5-GOOD-RETURN                                     GBH1PGM 
03297         MOVE '1BF3'  TO  WS-ABEND-CODE                            GBH1PGM 
03298         PERFORM 9999-ABEND.                                       GBH1PGM 
03299                                                                   GBH1PGM 
03300  8600-EXIT.                                                       GBH1PGM 
03301      EXIT.                                                        GBH1PGM 
03302                                                                   GBH1PGM 
03303                                                                   GBH1PGM 
03304 /                                                                 GBH1PGM 
03305 ******************************************************************GBH1PGM 
03306 * THIS ERROR CAN BE INVOKED BY A NUMBER OF DIFFERENT REQUESTS    *GBH1PGM 
03307 * THE PROGRAMMER SHOULD CHECK THE WS-PARA-ID FIELD IN THE        *GBH1PGM 
03308 * DUMP TO DETERMINE WHAT CODE CAUSED THIS ABEND.                 *GBH1PGM 
03309 ******************************************************************GBH1PGM 
03310  9999-ABEND.                                                      GBH1PGM 
03311                                                                   GBH1PGM 
03312      MOVE '9999' TO WS-PARA-ID.                                   GBH1PGM 
03313                                                                   GBH1PGM 
03314      EXEC CICS ABEND  ABCODE(WS-ABEND-CODE)                       GBH1PGM 
03315                       END-EXEC.                                   GBH1PGM 
03316                                                                   GBH1PGM 
03317  9999-EXIT.                                                       GBH1PGM 
03318      EXIT.                                                        GBH1PGM 
